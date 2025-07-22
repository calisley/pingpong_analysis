# Linked IRT Analysis: Bayesian Hierarchical 2PL with Stan
# -------------------------------------------------------

# 0. Load libraries and data
library(dplyr)
library(tidyr)
library(rstan)
library(here)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

pre_test <- readRDS(here("data", "clean", "pre_tests.rds"))
grades   <- readRDS(here("data", "clean", "graded_posttests.rds"))
classes<- readRDS(here("data/generated/classes.rds")) %>% select(
  class_id = ref_id,
  name
)

# 1. Clean pre-test anchor form
pre_test_clean <- pre_test %>%
  select(class_id, student_id, correct_q1:correct_q11) %>%
  rename_with(~ sub("^correct_q", "pretest_q", .x),
              correct_q1:correct_q11) %>%
  mutate(across(pretest_q1:pretest_q11, as.numeric)) %>% 
  group_by(class_id,student_id) %>%
  summarize(across(contains("pretest"), ~max(.))) %>%
  distinct() %>% ungroup()

# 2. Stack GAN vs. Stats
gan   <- grades %>% filter(process == "gan")   %>% select(-consent)
stats <- grades %>% filter(process == "stats") %>% select(-consent)

full_df <- bind_rows(
  gan   %>% mutate(process = "gan"),
  stats %>% mutate(process = "stats")
)


# 4. Build wide data with unique item naming + pre-test anchors
wide_link <- full_df %>%
  select(process, class_id, student_id, question_number, correct) %>%
  group_by(process, class_id, student_id, question_number) %>%
  summarize(correct = max(correct, na.rm = TRUE), .groups = "drop") %>%
  # create a unique “item” name by pasting class & question:
  mutate(item = paste0("class", class_id, "_q", question_number)) %>%
  select(process, student_id, item, correct, class_id) %>%
  pivot_wider(
    names_from  = item,
    values_from = correct,
    values_fill = list(correct = NA)
  ) %>%
  left_join(pre_test_clean, by = c("class_id","student_id")) %>%
  arrange(process, class_id, student_id)

# 5. Identify eligible classes:
#    – all stats classes
#    –  at least 2 students (needed for one-off calibration)
class_meta <- wide_link %>%
  group_by(process, class_id) %>%
  summarise(n_students = n(), .groups = "drop")

eligible_classes <- class_meta %>%
  filter(n_students>1) %>%#no variance
  pull(class_id)

# 6. Subset data to eligible classes
wide_link2 <- wide_link %>%
  filter(class_id %in% eligible_classes) %>%
  distinct()
ai_df    <- wide_link2 %>% filter(process == "gan")
stats_df <- wide_link2 %>% filter(process == "stats")


# 7. Extract response matrix and group vector; drop globally constant items
resp_mat  <- wide_link2 %>%
  select(-process, -student_id, -class_id) %>%
  as.matrix() 
group_vec <- wide_link2$class_id

varying    <- apply(resp_mat, 2, function(x) length(unique(na.omit(x))) > 1)
resp_mat2  <- resp_mat[, varying, drop = FALSE]


# 7. Prepare data list for Stan
N    <- nrow(resp_mat2)                 # total persons
J    <- ncol(resp_mat2)                 # total items
G    <- length(unique(group_vec))       # number of groups/classes
grp  <- as.integer(factor(group_vec))   # group index per person
y    <- resp_mat2 + 0                  # ensure 0/1, NA will be handled
# convert y into long format for Stan:
long_idx <- which(!is.na(y), arr.ind = TRUE)
I <- nrow(long_idx)
person_id <- long_idx[,1]
item_id   <- long_idx[,2]
resp      <- y[long_idx]
stan_data <- list(
  N        = N,
  J        = J,
  G        = G,
  I        = I,
  P        = person_id,
  Q        = item_id,
  Y        = resp,
  grp      = grp,
  mu0_mean = 0,
  mu0_sd   = 1,
  sd0_scale = 1,
  a_mean   = 0,
  a_sd     = 0.5
)
stan_code = "
data {
  int<lower=1> N;                // persons
  int<lower=1> J;                // items
  int<lower=1> G;                // classes/groups
  int<lower=1> I;                // total observations
  int<lower=1,upper=N> P[I];     // person index for each response
  int<lower=1,upper=J> Q[I];     // item index for each response
  int<lower=0,upper=1> Y[I];     // binary responses
  int<lower=1,upper=G> grp[N];   // class for each person

  real a_mean;                   // prior mean on log discrimination
  real<lower=0> a_sd;            // prior SD   on log discrimination
  real<lower=0> sd0_scale;       // scale for student-SD prior (matches sd0_scale)
}

parameters {
  // item parameters (non-centered)
  vector[J] b_raw;               // raw difficulties
  vector[J] a_raw;               // raw log-discriminations

  // class-level hierarchy
  real<lower=0> sigma_grp;       // SD of class means
  vector[G] mu_grp;              // class means

  // student abilities
  real<lower=0> sigma_theta;     // SD of student deviations
  vector[N] theta_raw;           // raw student latent draws
}

transformed parameters {
  vector[J] b = b_raw;                              // difficulties
  vector<lower=0>[J] a = exp(a_raw * a_sd + a_mean); // discriminations

  vector[N] theta;
  for (n in 1:N)
    theta[n] = mu_grp[grp[n]] + sigma_theta * theta_raw[n];
}

model {
  // item priors
  b_raw        ~ normal(0, 1);
  a_raw        ~ normal(0, 1);

  // class-level pooling
  sigma_grp    ~ normal(0, 1);          // half-normal
  mu_grp       ~ normal(0, sigma_grp);

  // student abilities
  sigma_theta  ~ normal(0, sd0_scale);
  theta_raw    ~ normal(0, 1);

  // likelihood
  for (i in 1:I) {
    real eta = a[Q[i]] * (theta[P[i]] - b[Q[i]]);
    Y[i] ~ bernoulli_logit(eta);
  }
}

generated quantities {
  vector[J] info_at0;
  for (j in 1:J) {
    real p = inv_logit(a[j] * (0 - b[j]));
    info_at0[j] = a[j]^2 * p * (1 - p);
  }
}

"

# 9. Compile and sample
fit <- stan(
  model_code = stan_code,
  data       = stan_data,
  iter       = 2000,
  warmup     = 1000,
  chains     = 4,
  control    = list(adapt_delta = 0.95)
)


# new 2! ------------------------------------------------------------------

# indices for AI and Stats items

colnames_all2 <- colnames(resp_mat2)
stats_ids     <- unique(stats$class_id)
stats_pattern <- paste0("^class", stats_ids, "_", collapse = "|")
pre_pattern   <- "^pretest_q"
ai_items      <- colnames_all2[!grepl(pre_pattern, colnames_all2) & !grepl(stats_pattern, colnames_all2)]
stats_items   <- colnames_all2[grepl(stats_pattern, colnames_all2)]
pre_items     <- colnames_all2[grepl(pre_pattern, colnames_all2)]

#7. Extract posterior draws ----------------------------------------------
a_mat <- as.matrix(fit, pars = "a")  # draws × J
b_mat <- as.matrix(fit, pars = "b")  # draws × J

# map matrix columns to item names
a_items <- colnames_all2  # order matches Stan parameters
b_items <- colnames_all2

# 8. Discrimination & Difficulty Plots (AI only) --------------------------
# subset columns
ai_idx     <- which(a_items %in% ai_items)
a_ai_mat   <- a_mat[, ai_idx]
b_ai_mat   <- b_mat[, ai_idx]

stat_idx     <- which(a_items %in% stats_items)
a_stat_mat   <- a_mat[, stat_idx]
b_stat_mat   <- b_mat[, stat_idx]

pretest_idx <- which(a_items %in% pre_items)



a_df <- as.data.frame(a_stat_mat) %>%
  pivot_longer(everything(), names_to = "item", values_to = "a")
b_df <- as.data.frame(b_stat_mat) %>%
  pivot_longer(everything(), names_to = "item", values_to = "b")

# discrimination density
ggplot(a_df, aes(x = a)) +
  geom_density() +
  labs(title = "Stats Post-Test Discrimination (a)", x = "a") +
  theme_minimal()

# difficulty density
ggplot(b_df, aes(x = b)) +
  geom_density() +
  labs(title = "Stats Post-Test Difficulty (b)", x = "b") +
  theme_minimal()

ai_persons <- which(wide_link2$process == "stats")
theta_ai_mat <- theta_mat[, ai_persons]
theta_df <- as.data.frame(theta_ai_mat) %>%
  pivot_longer(everything(), names_to = "student", values_to = "theta")

# posterior density of θ for AI test takers
ggplot(theta_df, aes(x = theta)) +
  geom_density() +
  labs(
    title = expression("Posterior Distribution of " * theta * " for Stats Test Takers"),
    x     = expression(theta),
    y     = "Density"
  ) +
  theme_minimal()

#standout classes
class_sizes_ai <- ai_df %>% count(class_id, name = "n_students")
big_ai_ids     <- class_sizes_ai %>% filter(n_students > 50) %>% pull(class_id)

# extract b draws for items in each large AI class
df_list_b <- lapply(big_ai_ids, function(cid) {
  idx_items <- grep(paste0("^class", cid, "_"), a_items)
  df <- as.data.frame(b_mat[, idx_items]) %>%
    pivot_longer(everything(), names_to = "item", values_to = "b") %>%
    mutate(class_id = cid)
  df
})
b_ai_df <- bind_rows(df_list_b) %>%
  left_join(classes, by = "class_id")

ggplot(b_ai_df, aes(x = b, color = name, fill = name)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ name, scales = "free_y") +
  labs(
    title = "Difficulty (b) Distributions for Large AI Classes (n > 50)",
    x     = "b",
    color = "Class",
    fill  = "Class"
  ) +
  theme_minimal()

# 9. ICC Curves (Pretest) -------------------------------------------------
theta_grid <- seq(-3, 3, length.out = 100)
icc_list_pre <- lapply(pretest_idx, function(j) {
  a_j <- mean(a_mat[, j])
  b_j <- mean(b_mat[, j])
  p   <- plogis(a_j * (theta_grid - b_j))
  data.frame(item = a_items[j], theta = theta_grid, p = p)
})
icc_df_pre <- bind_rows(icc_list_pre)

ggplot(icc_df_pre, aes(x = theta, y = p, color = item)) +
  geom_line() +
  labs(
    title = "Item Characteristic Curves: Pre-Test Items",
    x     = expression(theta),
    y     = "P(correct)"
  ) +
  theme_minimal()

# 9.1 ICC Curves (Stats) -------------------------------------------------
# theta_grid <- seq(-3, 3, length.out = 100)
# icc_list_stats <- lapply(stat_idx, function(j) {
#   a_j <- mean(a_mat[, j])
#   b_j <- mean(b_mat[, j])
#   p   <- plogis(a_j * (theta_grid - b_j))
#   data.frame(item = a_items[j], theta = theta_grid, p = p)
# })
# icc_df_stats <- bind_rows(icc_list_stats)
# 
# ggplot(icc_df_stats, aes(x = theta, y = p, color = item)) +
#   geom_line() +
#   labs(
#     title = "Item Characteristic Curves: AP-Stats Items",
#     x     = expression(theta),
#     y     = "P(correct)"
#   ) +
#   theme_minimal()

# 10. Test Information Curves ----------------------------------------------
# 10a. Define θ grid
theta_grid <- seq(-3, 3, length.out = 100)

# 10b. For each posterior draw for AI exams, compute total test information at each θ
ai_classes  <- sort(unique(ai_df$class_id))
D           <- nrow(a_mat)
T           <- length(theta_grid)
C           <- length(ai_classes)

# Prepare a 3‐d array: draws × theta × class
info_class  <- array(0, dim = c(D, T, C),
                     dimnames = list(NULL, NULL, ai_classes))

for (ci in seq_along(ai_classes)) {
  cid      <- ai_classes[ci]
  # locate columns in the full `a_mat`/`b_mat`
  cols     <- grep(paste0("^class", cid, "_"), a_items)
  for (d in 1:D) {
    a_d_c  <- a_mat[d, cols]
    b_d_c  <- b_mat[d, cols]
    # vectorized across theta
    info_class[d, , ci] <-
      sapply(theta_grid, function(th) {
        p_j <- plogis(a_d_c * (th - b_d_c))
        sum(a_d_c^2 * p_j * (1 - p_j))
      })
  }
}

info_avg_draws <- apply(info_class, c(1,2), mean)  # D × T

info_mean  <- colMeans(info_avg_draws)
info_lower <- apply(info_avg_draws, 2, quantile, probs = 0.025)
info_upper <- apply(info_avg_draws, 2, quantile, probs = 0.975)

tinfo_df <- tibble(
  theta = theta_grid,
  mean  = info_mean,
  lower = info_lower,
  upper = info_upper
)

# 10d. Plot the “average‐across‐exams” TIC with 95% bands ----------
ggplot(tinfo_df, aes(x = theta, y = mean)) +
  geom_line(size = 1,   color = "steelblue") +  
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill= "steelblue") +
  labs(
    title = "Average AI Test Information Curve",
    x     = expression(theta),
    y     = "Information"
  ) +
  theme_minimal()

#10.1.a
process_list <- list(
  gan     = sort(unique(ai_df$class_id)),
  stats   = sort(unique(stats_df$class_id)),
  pretest = sort(unique(wide_link2$class_id))  # pre-test items exist for every class
)

# 3. For each process, compute draws × θ averaged across forms
tinfo_list <- lapply(names(process_list), function(proc) {
  classes <- process_list[[proc]]
  C       <- length(classes)
  D       <- nrow(a_mat)
  T       <- length(theta_grid)
  
  # a 3-d array: draws × θ × class
  info_class <- array(0, dim = c(D, T, C),
                      dimnames = list(NULL, NULL, classes))
  
  for (ci in seq_along(classes)) {
    cid  <- classes[ci]
    # pick out item‐columns for this class/process
    cols <- if (proc=="pretest") {
      grep("^pretest_q", colnames(resp_mat2))
    } else {
      grep(paste0("^class", cid, "_"), colnames(resp_mat2))
    }
    
    for (d in 1:D) {
      a_d <- a_mat[d, cols]
      b_d <- b_mat[d, cols]
      info_class[d, , ci] <- sapply(theta_grid, function(th) {
        p_j <- plogis(a_d * (th - b_d))
        sum(a_d^2 * p_j * (1 - p_j))
      })
    }
  }
  
  # average across classes *within* each draw → D × T
  info_avg_draws <- apply(info_class, c(1,2), mean)
  
  # summarize over draws
  mean_info  <- colMeans(info_avg_draws)
  lower_info <- apply(info_avg_draws, 2, quantile, probs = 0.025)
  upper_info <- apply(info_avg_draws, 2, quantile, probs = 0.975)
  
  tibble(
    theta = theta_grid,
    mean  = mean_info,
    lower = lower_info,
    upper = upper_info,
    process = proc
  )
})

# 4. Combine into one data.frame
tinfo_all <- bind_rows(tinfo_list)

# 5. Plot all three on one figure
ggplot(tinfo_all, aes(x = theta, y = mean, color = process)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = process),
              alpha = 0.2, color = NA) +
  scale_color_manual(
    values = c(gan = "steelblue", stats = "darkorange", pretest = "forestgreen")
  ) +
  scale_fill_manual(
    values = c(gan = "steelblue", stats = "darkorange", pretest = "forestgreen")
  ) +
  labs(
    title = "Average Test Information Curves\nGAN vs Stats vs Pre-test",
    x     = expression(theta),
    y     = "Information"
  ) +
  theme_minimal()



# 11. Sample TICs for Representative AI Classes --------------------------

# 11a. Identify top 4 largest AI classes
class_sizes_ai <- ai_df %>% count(class_id, name = "n_students")
top4_ai       <- class_sizes_ai %>% arrange(desc(n_students)) %>% slice_head(n = 4) %>% pull(class_id)

# 11b. For each top class, pick 5 random posterior draws and compute class‐specific TICs
sampled_curves <- lapply(top4_ai, function(cid) {
  # which item‐columns belong to this class?
  item_cols <- grep(paste0("^class", cid, "_"), a_items)
  # pick 5 draws at random
  draw_ids  <- sample(nrow(a_mat), 5)
  # build a data.frame of (θ, info, draw, class_id)
  do.call(bind_rows, lapply(draw_ids, function(d) {
    a_d <- a_mat[d, item_cols]
    b_d <- b_mat[d, item_cols]
    info  <- sapply(theta_grid, function(th) {
      p_j <- plogis(a_d * (th - b_d))
      sum(a_d^2 * p_j * (1 - p_j))
    })
    tibble(
      theta    = theta_grid,
      info     = info,
      draw     = factor(d),   # label by draw
      class_id = cid
    )
  }))
})

class_tic_df <- bind_rows(sampled_curves) %>%
  left_join(classes, by = "class_id")

# 11c. Plot sample TICs faceted by class
ggplot(class_tic_df, aes(x = theta, y = info, group = interaction(class_id, draw))) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ name, scales = "free_y") +
  labs(
    title = "Sample Test Information Curves\nTop 4 AI Classes",
    x     = expression(theta),
    y     = "Information"
  ) +
  theme_minimal()

# 11. Sample TICs for Representative AI Classes ---------------------------



# new ---------------------------------------------------------------------

library(bayesplot)

############################## Rhat Plot #######################################
posterior <- as.array(fit)
# Rhat summary
rhat_vals <- rhat(fit)
print(rhat_vals)
# Plot Rhat values
mcmc_rhat(rhat_vals) + ggtitle("Rhat Diagnostics for All Parameters")

##### Dist of a b and \theta ####

# 1. Extract posterior draws from your full fit
a_mat     <- as.matrix(fit, pars = "a")      # draws × J
b_mat     <- as.matrix(fit, pars = "b")      # draws × J
theta_mat <- as.matrix(fit, pars = "theta")  # draws × N_students

# 2. Subset to post-test items only
post_idx     <- which(resp_cols %in% posttest_cols)
a_post_mat   <- a_mat[, post_idx, drop = FALSE]
b_post_mat   <- b_mat[, post_idx, drop = FALSE]

# 3. Pivot to long format
a_df <- as.data.frame(a_post_mat) %>%
  pivot_longer(everything(), names_to = "item", values_to = "a")
b_df <- as.data.frame(b_post_mat) %>%
  pivot_longer(everything(), names_to = "item", values_to = "b")
theta_df <- as.data.frame(theta_mat) %>%
  pivot_longer(everything(), names_to = "student", values_to = "theta")


# Discrimination (a) — post-test only
ggplot(a_df, aes(x = a)) +
  geom_density() +
  labs(
    title = "Posterior Distribution of Post-Test Discrimination (a)",
    x     = "a"
  )

# Difficulty (b) — post-test only
ggplot(b_df, aes(x = b)) +
  geom_density() +
  labs(
    title = "Posterior Distribution of Post-test Difficulty (b)",
    x     = "b"
  )

# Ability (θ) — all students (unchanged)
ggplot(theta_df, aes(x = theta)) +
  geom_density() +
  labs(
    title = expression("Posterior Distribution of " * theta * " (Student Ability)"),
    x     = expression(theta),
    y     = "Density"
  )

#####



#### New difficulty plot: Looks good but what does it mean? #####

# --- 1. Extract b draws and make long tibble  -----------------------------
b_mat <- as.matrix(fit, pars = "b")   # dims = iterations × J
b_df_long <- as.data.frame(b_mat) %>%
  mutate(.iter = row_number()) %>%
  pivot_longer(
    cols      = - .iter,
    names_to  = "param",
    values_to = "b"
  ) %>%
  # extract item index from "b[123]"
  mutate(
    item_idx = as.integer(str_extract(param, "(?<=\\[)\\d+(?=\\])")),
    item     = colnames(resp_mat2)[item_idx]
  ) %>%
  select(-param, -item_idx)

# --- 2. Tag each draw with its class_id, drop pre-test items ---------------
b_df_long <- b_df_long %>%
  mutate(
    class_id = str_extract(item, "(?<=^class)[:alnum:]+(?=_q)")
  ) %>%
  filter(!is.na(class_id))

# --- 3. Find the “big” classes (>50 students) ------------------------------
class_sizes <- tibble(
  student  = seq_along(group_vec),
  class_id = group_vec
) %>% count(class_id, name = "n_students")

big_classes <- class_sizes %>%
  filter(n_students > 50) %>%
  pull(class_id)

# --- 4. Sample a few items per big class ----------------------------------
items_to_plot <- b_df_long %>%
  filter(class_id %in% big_classes) 


# --- 5. Bring in human‐readable class names -------------------------------
# assume you have a tibble `classes` with columns class_id and name
b_sample <- items_to_plot %>%
  left_join(classes, by = "class_id")   # adds `name` column

ggplot(b_sample, aes(x = b, color = name, fill = name)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ name, scales = "free") +
  labs(
    title = "Posterior Distributions of Item Difficulty in Select Classes",
    x     = expression(b),
    y     = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                # legend no longer needed
    strip.text       = element_text(size = 10)
  )
#####


##### GOOD #####
# 15. Item Characteristic Curves (pre-test only)
pre_idx <- grep("^pretest_q", colnames(resp_mat2))
theta_grid <- seq(-3, 3, length.out = 100)
icc_list <- lapply(pre_idx, function(j) {
  a_j <- mean(a_samples[, j])
  b_j <- mean(b_samples[, j])
  p <- plogis(a_j * (theta_grid - b_j))
  data.frame(item = colnames(resp_mat2)[j], theta = theta_grid, p = p)
})
icc_df <- bind_rows(icc_list)
ggplot(icc_df, aes(x = theta, y = p, color = item)) +
  geom_line() + labs(title = "Item Characteristic Curves: Pre-test Items", x = expression(theta), y = "P(correct)")

##### 

# assume tif_list/tif_df are built as before:
tif_list <- lapply(seq_len(J), function(j) {
  a_j <- mean(a_samples[, j])
  b_j <- mean(b_samples[, j])
  p   <- plogis(a_j * (theta_grid - b_j))
  info <- a_j^2 * p * (1 - p)
  data.frame(
    theta    = theta_grid,
    info     = info,
    item     = colnames(resp_mat2)[j]
  )
})
tif_df <- bind_rows(tif_list)

# 1. extract class_id from item names of the form "class<id>_q<number>"
tif_df <- tif_df %>%
  mutate(
    class_id = str_extract(item, "(?<=^class)[:alnum:]+(?=_q)"),
    # anything not matching "class..._q" will be NA
  ) %>%
  # 2. drop pretest anchors (they won't match the regex, so class_id == NA)
  filter(!is.na(class_id))

# 3. sum info per class and theta
tif_by_class <- tif_df %>%
  group_by(class_id, theta) %>%
  summarise(
    form_info = sum(info),
    .groups   = "drop"
  )

# 4. plot: one TIF curve per class
ggplot(tif_by_class, aes(x = theta, y = form_info, color = class_id, group = class_id)) +
  geom_line() +
  labs(
    title = "Test Information Function by Class",
    x     = expression(theta),
    y     = "Information"
  ) +
  theme_minimal()




# 17. Posterior Predictive Checks
# generate y_rep if not already in generated quantities
# assume y_rep exists in fit
y_rep <- as.matrix(fit, pars = "y_rep")
# (a) overall proportion correct
overall_ppc <- apply(y_rep, 1, mean)
ppc_dens_overlay(y = resp, yrep = y_rep[1:50, ]) + labs(title = "PPC: Overall Proportion Correct")
# (b) by class
ppc_by_class <- function(class_id) {
  inds <- which(grp == class_id)
  obs <- rowMeans(resp_mat2[inds, ], na.rm = TRUE)
  rep <- apply(y_rep[, inds], 1, mean)
  ppc_scatter_avg_hist(obs, rep) + ggtitle(paste0("PPC by Class: ", class_id))
}
lapply(unique(grp), ppc_by_class)
# 18. Sensitivity to priors
# Re-fit with wider/narrower priors on a and sigma_theta
# e.g., a_sd = 1.0 and a_sd = 0.25
for (sd_val in c(0.25, 1.0)) {
  stan_data$a_sd <- sd_val
  fit_sens <- stan(model_code = stan_code, data = stan_data,
                   iter = 1000, warmup = 500, chains = 2, control = list(adapt_delta = 0.9))
  rhat_sens <- rhat(fit_sens)
  print(paste("Sensitivity a_sd=", sd_val))
  print(rhat_sens)
}



# Old ---------------------------------------------------------------------




# 10. Extract and summarize
post <- rstan::extract(fit)
# Item parameters
b_post  <- apply(post$b, 2, mean)
a_post  <- apply(post$a, 2, mean)
# Group summaries (mean discrimination, reliability r0)
test_info0 <- colSums(post$info_at0)      # sum over items in each draw
r0_draws   <- test_info0 / (test_info0 + 1)
r0_mean    <- mean(r0_draws)

# 11. Per-class metrics
library(purrr)
results_per_class <- map_dfr(1:G, function(g) {
  # which rows (persons) belong to class g
  in_class <- which(grp == g)
  
  # which items (columns) have at least one non-NA in that class
  items_used <- which(colSums(!is.na(resp_mat2[in_class, , drop = FALSE])) > 0)
  
  # class-specific test information at theta = 0
  p0 <- plogis(a_post[items_used] * (0 - b_post[items_used]))
  I0_class <- sum(a_post[items_used]^2 * p0 * (1 - p0))
  
  # reliability = I0 / (I0 + 1)
  r0_class <- I0_class / (I0_class + 1)
  
  tibble(
    class_id   = unique(group_vec)[g],
    n_students = length(in_class),
    n_items    = length(items_used),     # should be 11 + exam length
    mean_a     = mean(a_post[items_used], na.rm = TRUE),
    I0         = I0_class,
    r0         = r0_class
  )
})

# 12. Aggregate by process
summary_by_process <- results_per_class %>%
  left_join(class_meta %>% select(class_id, process), by = "class_id") %>%
  group_by(process) %>%
  summarise(
    classes     = n(),
    avg_n_items = mean(n_items),
    avg_mean_a  = mean(mean_a),
    avg_r0      = mean(r0),
    sd_r0       = sd(r0)
  )

print(summary_by_process)


# a) Build a long tibble of posterior θ draws for every student
theta_mat <- post$theta     # dim = iterations × N
iter   <- nrow(theta_mat)
theta_df <- as_tibble(theta_mat) %>%
  mutate(.iter = row_number()) %>%
  pivot_longer(-.iter, names_to="student_idx", values_to="theta") %>%
  mutate(
    stud_idx = as.integer(sub("^V", "", student_idx)),
    process  = factor(group_vec[stud_idx], levels=unique(group_vec)) %>%
      recode_factor(!!!setNames(class_meta$process, class_meta$class_id))
  )

# b) Posterior summaries by process
theta_summary <- theta_df %>%
  group_by(process, .iter) %>%
  summarise(mean_theta = mean(theta), .groups="drop") %>%
  group_by(process) %>%
  summarise(
    avg_mean_theta = mean(mean_theta),
    median_theta   = median(mean_theta),
    lower_2.5      = quantile(mean_theta, 0.025),
    upper_97.5     = quantile(mean_theta, 0.975)
  )

print(theta_summary)


# c) Density plot of θ by process
ggplot(theta_df, aes(x=theta, color=process, fill=process)) +
  geom_density(alpha=0.2) +
  labs(
    title = "Posterior Ability Densities: GAN vs Stats",
    x     = expression(theta),
    y     = "Density"
  ) +
  theme_minimal()


# d) Per‐process reliability (r₀) table
reliability_by_process <- results_per_class %>%
  left_join(class_meta %>% select(class_id, process), by="class_id") %>%
  group_by(process) %>%
  summarise(
    mean_r0 = mean(r0),
    sd_r0   = sd(r0),
    n_forms = n()
  )

print(reliability_by_process)


# e) Test‐Information Functions (TIFs)

#  - Compute item‐information curves for each item over a grid of theta
theta_grid <- seq(-3, 3, length = 61)
#  - post$info_at0 gave info@0; we need full item info curves:
item_info_curves <- lapply(seq_along(b_post), function(j) {
  p_θ <- plogis(a_post[j] * (theta_grid - b_post[j]))
  info <- a_post[j]^2 * p_θ * (1 - p_θ)
  tibble(item = j, theta = theta_grid, info = info)
})
item_info_df <- bind_rows(item_info_curves)
item_names <- colnames(resp_mat2)
item_info_df <- item_info_df %>%
  mutate(
    class_id = ifelse(
      grepl("^class", item_names[item]),
      sub("^class([^_]+)_q\\d+$", "\\1", item_names[item]),
      NA_character_
    ),
    process = ifelse(
      is.na(class_id),
      "pretest",
      class_meta$process[match(class_id, class_meta$class_id)]
    )
  )

# c) Compute per‐form (per class_id) TIF curves
form_tif <- item_info_df %>%
  filter(process %in% c("gan","stats")) %>%
  group_by(process, class_id, theta) %>%
  summarise(form_info = sum(info), .groups="drop")

# d) Average across forms within each process
avg_form_tif <- form_tif %>%
  group_by(process, theta) %>%
  summarise(mean_info = mean(form_info),  # average TIF per form
            sd_info   = sd(form_info),    # variability across forms
            .groups="drop")

# e) Plot with ribbons to show variability
ggplot(avg_form_tif, aes(x=theta, y=mean_info, color=process, fill=process)) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin = mean_info - sd_info, ymax = mean_info + sd_info),
              alpha=0.2, color=NA) +
  labs(
    title = "Average Test Information Function per Form: GAN vs Stats",
    x     = expression(theta),
    y     = "Information (per form)"
  ) +
  theme_minimal()



# Other -------------------------------------------------------------------

### Pretest difficulty

sum_b <- summary(fit, pars = "b")$summary
# turn into a data.frame with item names
df_b <- as.data.frame(sum_b) %>%
  mutate(item = resp_cols) %>%
  select(item, mean, `2.5%`, `97.5%`)

# 3. Split into pre/post
df_pre  <- df_b %>% filter(item %in% pretest_cols)
df_post <- df_b %>% filter(item %in% posttest_cols)

# 4a. Base‐R plot for Pre-test difficulties
with(df_pre, {
  x <- seq_along(item)
  plot(x, mean,
       ylim = range(`2.5%`, `97.5%`),
       xaxt = "n",
       xlab = "Pre-test item",
       ylab = "Difficulty (b)",
       pch = 19)
  arrows(x, `2.5%`, x, `97.5%`, angle = 90, code = 3, length = 0.02)
  axis(1, at = x, labels = item)
})



