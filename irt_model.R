# Linked IRT Analysis: Bayesian Hierarchical 2PL Data Preparation
# -------------------------------------------------------------

# 0. Load libraries
library(rstan)
library(here)
library(ggplot2)
library(stringr)
library(bayesplot)
library(tidyverse)
library(posterior) 
library(tidybayes)
library(patchwork)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# irt_model.R -------------------------------------------------------------


# 1. Read data
data_dir <- here("data", "clean")
pre_test  <- readRDS(file.path(data_dir, "pre_tests.rds"))
grades    <- readRDS(file.path(data_dir, "graded_posttests.rds"))

class_names <- readRDS(here("data", "generated", "classes.rds")) %>%
  select(class_id = ref_id, name)

# 2. Clean and pivot pre-test data
pre_test_long <- pre_test %>%
  select(class_id, student_id, starts_with("correct_q")) %>%
  rename_with(~ str_replace(.x, "^correct_q", "pretest_q")) %>%
  mutate(across(starts_with("pretest_q"), as.numeric)) %>%
  group_by(class_id, student_id) %>%
  summarise(across(starts_with("pretest_q"), ~max(.)), .groups = "drop") %>%
  pivot_longer(
    cols      = starts_with("pretest_q"),
    names_to  = "question_number",
    values_to = "correct"
  ) %>%
  filter(!is.na(correct)) 

# 3. Stack GAN vs. Stats post-test data
full_df <- grades %>%
  filter(process %in% c("gan", "stats")) %>%
  mutate(
    process         = factor(process, levels = c("gan", "stats")),
    question_number = as.character(question_number),
    correct         = as.numeric(correct)
  ) %>%
  select(class_id, student_id, question_number, question_id, correct, process) 

# 4. Combine pre- and post-test, then index
students <- full_df %>% distinct(class_id, student_id)
# add explicit process variable for pretest
pretest_filtered <- pre_test_long %>%
  inner_join(students, by = c("class_id", "student_id")) %>%
  mutate(process = "pretest") %>%
  group_by(class_id, student_id, question_number, process) %>%
  summarize(correct = max(correct),
            .groups = "drop") %>%
  mutate(question_id = question_number)

# add explicit process variable for posttest (already has it)
full_df <- full_df %>%
  mutate(process = as.character(process))

# stack all with explicit process column
long_df <- bind_rows(full_df, pretest_filtered) %>%
  group_by(class_id) %>%
  filter(length(unique(student_id)) > 1) %>% #drop classes with only one student
  ungroup() %>%
  mutate(
    question_id_factor = as.integer(factor(question_id)),
    student_id_factor  = as.integer(factor(student_id)),
    class_id_factor    = as.integer(factor(class_id)),
    process            = factor(process, levels = c("pretest", "gan", "stats")),
    process_id         = as.integer(process)
  )


# 5. Prepare Stan data dimensions
nn_people  <- n_distinct(long_df$student_id_factor)
nn_items   <- n_distinct(long_df$question_id_factor)
nn_classes <- n_distinct(long_df$class_id_factor)
nn_obs     <- nrow(long_df)
nn_processes <- length(levels(long_df$process))

# 6. Build index vectors and response
person_id   <- long_df$student_id_factor
item_id     <- long_df$question_id_factor
resp        <- long_df$correct
classes_obs <- long_df$class_id_factor  # one class per response
processes   <- long_df$process_id 

# 7. Stan data list matching model variable names\
stan_data <- list(
  n_people    = nn_people,
  n_items     = nn_items,
  n_classes   = nn_classes,
  n_obs       = nn_obs,
  n_process   = nn_processes,
  person      = person_id,
  item        = item_id,
  classes_obs = classes_obs,
  response    = resp,
  process     = processes,
  mu0_mean    = 0,
  mu0_sd      = 1,
  sd0_scale   = 1,
  a_mean      = 0,
  a_sd        = 0.5
)

# 8. Stan model code
stan_code <- "
  data {
  int<lower=1> n_obs;
  int<lower=1> n_people;
  int<lower=1> n_items;
  int<lower=1> n_classes;
  int<lower=1> n_process;                
  int<lower=1,upper=n_people> person[n_obs];
  int<lower=1,upper=n_items>   item[n_obs];
  int<lower=1,upper=n_classes> classes_obs[n_obs];
  int<lower=1,upper=n_process> process[n_obs];  
  int<lower=0,upper=1>         response[n_obs];
}

parameters {
  // discrimination (log‐normal) hyperpriors
  real        mu_a;
  real<lower=0> sigma_a;

  // difficulty hyperpriors
  real        mu_b;
  real<lower=0> sigma_b;

  // class‐level intercept scale
  real<lower=0> sigma_class;
  
  // person ability scale
  real<lower=0> sigma_theta;

  // raw (non‐centered) class effects
  vector[n_classes] mu_class_raw;
  
  // raw (non‐centered) abilities
  vector[n_people] theta_raw;

  // raw (non‐centered) discrimination deviations
  vector[n_items] log_a_raw;

  // raw (non‐centered) difficulty deviations
  vector[n_items] b_raw;
  
  // exam‐generation process shifts for AI and Stats exams only
  // (pretest is baseline, so we only need n_process – 1 parameters)
  vector[n_process-1] delta_b;
}

transformed parameters {
  vector[n_items] a;         // discrimination parameters
  vector[n_people] theta;    // person abilities
  vector[n_classes] mu_class; // class effects
  vector[n_items] b;         // item difficulties

  // non-centered transforms
  a         = exp(mu_a + sigma_a * log_a_raw);
  theta     = sigma_theta * theta_raw;
  mu_class  = sigma_class * mu_class_raw;
  b         = mu_b + sigma_b * b_raw;
}

model {
  // Hyperpriors
  mu_a        ~ normal(0, 1);
  sigma_a     ~ normal(0, 1);
  mu_b        ~ normal(0, 2.5);
  sigma_b     ~ normal(0, 2.5);
  sigma_class ~ normal(0, 1);
  sigma_theta ~ normal(0, 1);

  // Non‐centered priors
  mu_class_raw ~ normal(0, 1);
  theta_raw    ~ normal(0, 1);
  log_a_raw    ~ normal(0, 1);
  b_raw        ~ normal(0, 1);

  // Prior on difficulty shifts for gan & stats
  delta_b     ~ normal(0,1);
  
  // Likelihood
  for (n in 1:n_obs) {
    int p = process[n];
    real shift = (p > 1) ? delta_b[p-1] : 0;  // pretest=0, ai=δ₁, stats=δ₂
    real eta = a[item[n]] * (
                 (theta[person[n]] + mu_class[classes_obs[n]])
                 - (b[item[n]] + shift)
               );
    response[n] ~ bernoulli_logit(eta);
}

}
"

# 9. Compile and sample
fit <- stan(
  model_code = stan_code,
  data       = stan_data,
  iter       = 4000,
  warmup     = 2000,
  chains     = 4,
  control    = list(adapt_delta = 0.95)
)

saveRDS(fit, here("models","stan_model.rds"))


# Analysis ----------------------------------------------------------------

pretest_idx <- long_df %>%
  filter(process == "pretest") %>%
  distinct(question_id_factor) %>%
  arrange(question_id_factor) %>%
  pull(question_id_factor)

# Stats: All posttest rows from stats classes (and not pretest)
stats_idx <- long_df %>%
  filter(process == "stats") %>%
  distinct(question_id_factor) %>%
  arrange(question_id_factor) %>%
  pull(question_id_factor) 

# AI: All posttest rows from gan classes (and not pretest)
ai_idx <- long_df %>%
  filter(process == "gan") %>%
  distinct(question_id_factor) %>%
  arrange(question_id_factor) %>%
  pull(question_id_factor)
# Convergence Diagnostics -------------------------------------------------

# 2. Convergence diagnostics
summary_fit <- summary(fit)
rhat_vals <- summary_fit$summary[, "Rhat"]
mcmc_rhat(rhat_vals) + ggtitle("Rhat Diagnostics for All Parameters")


# Posteriors of point estimates -------------------------------------------

a_mat     <- as.matrix(fit, pars = "a")
b_mat     <- as.matrix(fit, pars = "b")
theta_mat <- as.matrix(fit, pars = "theta")

# For items (columns)
a_hat     <- apply(a_mat, 2, mean)     # discrimination, 1 value per item
b_hat     <- apply(b_mat, 2, mean)     # difficulty, 1 value per item

# For persons (columns)
theta_hat <- apply(theta_mat, 2, mean) # ability, 1 value per person

a_ai_hat      <- a_hat[ai_idx]
a_stats_hat   <- a_hat[stats_idx]
a_pretest_hat <- a_hat[pretest_idx]

b_ai_hat      <- b_hat[ai_idx]
b_stats_hat   <- b_hat[stats_idx]
b_pretest_hat <- b_hat[pretest_idx]

# Discrimination
a_ai_df      <- tibble(a = a_ai_hat)
a_stats_df   <- tibble(a = a_stats_hat)
a_pretest_df <- tibble(a = a_pretest_hat)

# Difficulty
b_ai_df      <- tibble(b = b_ai_hat)
b_stats_df   <- tibble(b = b_stats_hat)
b_pretest_df <- tibble(b = b_pretest_hat)


a_df <- tibble(
  a = c(a_ai_hat, a_stats_hat),
  process = rep(c("AI", "Stats"), 
                c(length(a_ai_hat), length(a_stats_hat)))
)
ggplot(a_df, aes(x = a, fill = process, color = process)) +
  geom_density(alpha = 0.3) +
  labs(title = "Item Discrimination (a) by Process", x = "Discrimination (a)") +
  theme_minimal()

# Combine difficulty
b_df <- tibble(
  b = c(b_ai_hat, b_stats_hat),
  process = rep(c("AI", "Stats"), 
                c(length(b_ai_hat), length(b_stats_hat)))
)

ggplot(b_df, aes(x = b, fill = process, color = process)) +
  geom_density(alpha = 0.3) +
  labs(title = "Item Difficulty (b) by Process", x = "Difficulty (b)") +
  theme_minimal()


# GPT recreation ----------------------------------------------------------

student_lookup <- long_df %>%
  select(student_id, student_id_factor, process, class_id, class_id_factor) %>%
  distinct()

class_lookup <- long_df %>%
  select(class_id, class_id_factor) %>%
  distinct()


# Theta -------------------------------------------------------------------


# Big Classes -------------------------------------------------------------


ai_classes <- student_lookup %>% filter(process == "gan")
class_sizes_ai <- ai_classes %>% count(class_id, name = "n_students")
big_ai_ids <- class_sizes_ai %>% filter(n_students > 50) %>% pull(class_id)

# Each AI class: get item indices and extract draws
b_ai_df <- map_dfr(big_ai_ids, function(cid) {
  item_idx <- long_df %>%
    filter(process == "gan", class_id == cid) %>%
    distinct(question_id_factor) %>%
    pull(question_id_factor)
  df <- as.data.frame(b_mat[, item_idx, drop=FALSE]) %>%
    pivot_longer(everything(), names_to = "item", values_to = "b") %>%
    mutate(class_id = cid)
  df
}) %>%
  left_join(class_names, by = "class_id")

ggplot(b_ai_df, aes(x = b, color = name, fill = name)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ name, scales = "free_y") +
  labs(
    title = "Difficulty (b) Distributions for Large Classes\nwith AI exams",
    x     = "b",
    color = "Class",
    fill  = "Class"
  ) +
  theme_minimal()



# ICC curves - pretest ----------------------------------------------------

theta_grid <- seq(-3, 3, length.out = 100)
icc_list_pre <- lapply(pretest_idx, function(j) {
  a_j <- mean(a_mat[, j])
  b_j <- mean(b_mat[, j])
  p   <- plogis(a_j * (theta_grid - b_j))
  data.frame(item = paste0("Pretest Q", j), theta = theta_grid, p = p)
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


# TIC  --------------------------------------------------------------------

theta_grid <- seq(-3, 3, length.out = 100)
n_draws <- nrow(a_mat)
process_colors <- c(gan = "steelblue", stats = "darkorange", pretest = "forestgreen")

# 1. List of classes by process (for GAN and Stats only)
class_list <- long_df %>%
  filter(process %in% c("gan", "stats")) %>%
  distinct(class_id, process) %>%
  arrange(process, class_id) %>%
  as.data.frame()

# 2. Per-class average TICs (GAN and Stats)
tic_class_df <- map_dfr(seq_len(nrow(class_list)), function(i) {
  cid <- class_list$class_id[i]
  proc <- as.character(class_list$process[i])
  idx <- long_df %>%
    filter(class_id == cid, process == proc) %>%
    distinct(question_id_factor) %>%
    arrange(question_id_factor) %>%
    pull(question_id_factor)
  
  tic_mat <- matrix(0, nrow = n_draws, ncol = length(theta_grid))
  for (d in 1:n_draws) {
    a_d <- a_mat[d, idx]
    b_d <- b_mat[d, idx]
    tic_mat[d, ] <- sapply(theta_grid, function(th) {
      p_j <- plogis(a_d * (th - b_d))
      sum(a_d^2 * p_j * (1 - p_j))
    })
  }
  tibble(
    theta = theta_grid,
    info = colMeans(tic_mat),
    class_id = cid,
    process = proc
  )
})

# 3. For pretest, compute *one* TIC curve (shared item set)
pretest_idx <- long_df %>%
  filter(process == "pretest") %>%
  distinct(question_id_factor) %>%
  arrange(question_id_factor) %>%
  pull(question_id_factor)

tic_mat_pre <- matrix(0, nrow = n_draws, ncol = length(theta_grid))
for (d in 1:n_draws) {
  a_d <- a_mat[d, pretest_idx]
  b_d <- b_mat[d, pretest_idx]
  tic_mat_pre[d, ] <- sapply(theta_grid, function(th) {
    p_j <- plogis(a_d * (th - b_d))
    sum(a_d^2 * p_j * (1 - p_j))
  })
}

tic_pretest_df <- tibble(
  theta = theta_grid,
  mean = colMeans(tic_mat_pre),
  lower = apply(tic_mat_pre, 2, quantile, 0.025),
  upper = apply(tic_mat_pre, 2, quantile, 0.975),
  process = "pretest"
)

# 4. For GAN and Stats: summarize across classes (as before)
tic_process_df <- tic_class_df %>%
  group_by(process, theta) %>%
  summarize(
    mean = mean(info),
    lower = quantile(info, 0.025),
    upper = quantile(info, 0.975),
    .groups = "drop"
  )

# 5. Combine all
tic_all_df <- bind_rows(
  tic_process_df)

# 6. Plot
ggplot(tic_all_df, aes(x = theta, y = mean, color = process, fill = process)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  scale_color_manual(values = process_colors) +
  scale_fill_manual(values = process_colors) +
  labs(
    title = "Average Test Information Curves",
    x = expression(theta),
    y = "Information",
    color = "Process",
    fill = "Process"
  ) +
  theme_minimal()


# Reliability -------------------------------------------------------------

# Prepare item lookup
item_lookup <- long_df %>%
  select(question_id_factor, class_id, process) %>%
  distinct()

# Attach process to each student (from their class/test)
reliability_df <- student_lookup %>%
  arrange(student_id_factor) %>%
  mutate(theta_hat = theta_hat) %>%
  rowwise() %>%
  mutate(
    item_idx = list(
      item_lookup %>%
        filter(class_id == class_id, process == process) %>%
        arrange(question_id_factor) %>%
        pull(question_id_factor)
    ),
    a_items = list(a_hat[item_idx]),
    b_items = list(b_hat[item_idx]),
    info = sum(a_items^2 * plogis(a_items * (theta_hat - b_items)) * (1 - plogis(a_items * (theta_hat - b_items)))),
    reliability = info / (info + 1)
  ) %>%
  ungroup()

# Summarize by process
reliability_by_process <- reliability_df %>%
  group_by(process) %>%
  summarize(
    mean_reliability = mean(reliability, na.rm = TRUE),
    sd_reliability   = sd(reliability, na.rm = TRUE),
    min_reliability  = min(reliability, na.rm = TRUE),
    max_reliability  = max(reliability, na.rm = TRUE),
    n_students       = n()
  )

print(reliability_by_process)


# Posterior predictive checks ---------------------------------------------

# Compute predicted probabilities for each item/student/draw
# This can be memory intensive—sample or batch for large data!

# Get number of draws, items, and students
D <- nrow(a_mat)
J <- ncol(a_mat)
N <- ncol(theta_mat)

# For each process, collect item and student indices
idx_list <- list(
  gan     = list(
    items = ai_idx,
    students = long_df %>% filter(process == "gan") %>% distinct(student_id_factor) %>% pull(student_id_factor)
  ),
  stats   = list(
    items = stats_idx,
    students = long_df %>% filter(process == "stats") %>% distinct(student_id_factor) %>% pull(student_id_factor)
  ),
  pretest = list(
    items = pretest_idx,
    students = long_df %>% filter(process == "pretest") %>% distinct(student_id_factor) %>% pull(student_id_factor)
  )
)

# For each process, compute the predicted mean correct for each draw
ppc_proc <- map_dfr(names(idx_list), function(proc) {
  items <- idx_list[[proc]]$items
  students <- idx_list[[proc]]$students
  ppc_vec <- numeric(D)
  for (d in 1:D) {
    a_d <- a_mat[d, items]
    b_d <- b_mat[d, items]
    theta_d <- theta_mat[d, students]
    # Outer product: rows=students, cols=items
    prob_mat <- plogis(outer(theta_d, a_d, "-") - b_d)
    ppc_vec[d] <- mean(prob_mat, na.rm = TRUE)
  }
  tibble(
    draw = 1:D,
    process = proc,
    pred = ppc_vec
  )
})

obs_proc <- long_df %>%
  group_by(process) %>%
  summarize(observed = mean(correct, na.rm = TRUE))

# Plot with observed lines
ggplot(ppc_proc, aes(x = pred, fill = process)) +
  geom_density(alpha = 0.3) +
  geom_vline(
    data = obs_proc,
    aes(xintercept = observed, color = process),
    linetype = "dashed", size = 1
  ) +
  facet_wrap(~ process, scales = "free") +
  scale_color_manual(values = c(gan = "steelblue", stats = "darkorange", pretest = "forestgreen")) +
  scale_fill_manual(values = c(gan = "steelblue", stats = "darkorange", pretest = "forestgreen")) +
  labs(
    title = "Posterior Predictive: Overall Proportion Correct",
    x     = "Predicted proportion correct",
    y     = "Density"
  ) +
  theme_minimal()


# Posterior Predictive Big Classes ----------------------------------------



# Find top 4 largest GAN classes
big_gan_classes <- long_df %>%
  filter(process == "gan") %>%
  count(class_id) %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%
  pull(class_id)

# Loop over big classes
ppc_class <- map_dfr(big_gan_classes, function(cid) {
  items <- long_df %>%
    filter(process == "gan", class_id == cid) %>%
    distinct(question_id_factor) %>%
    arrange(question_id_factor) %>%
    pull(question_id_factor)
  
  students <- long_df %>%
    filter(process == "gan", class_id == cid) %>%
    distinct(student_id_factor) %>%
    arrange(student_id_factor) %>%
    pull(student_id_factor)
  
  # Posterior predictive for this class
  ppc_vec <- numeric(D)
  for (d in 1:D) {
    a_d <- a_mat[d, items]
    b_d <- b_mat[d, items]
    theta_d <- theta_mat[d, students]
    prob_mat <- plogis(outer(theta_d, a_d, "-") - b_d)
    ppc_vec[d] <- mean(prob_mat, na.rm = TRUE)
  }
  tibble(draw = 1:D, class_id = cid, pred = ppc_vec)
})

# Calculate observed mean for each class
obs_class <- long_df %>%
  filter(class_id %in% big_gan_classes, process == "gan") %>%
  group_by(class_id) %>%
  summarize(observed = mean(correct, na.rm = TRUE), .groups = "drop")

# Optional: Add class names
obs_class <- obs_class %>% left_join(class_names, by = "class_id")
ppc_class <- ppc_class %>% left_join(class_names, by = "class_id")

# Plot
ggplot(ppc_class, aes(x = pred, fill = name)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = obs_class, aes(xintercept = observed, color = name),
             linetype = "dashed", size = 1) +
  facet_wrap(~ name, scales = "free") +
  labs(
    title = "PPC: Proportion Correct by Class (GAN, Top 4)",
    x = "Predicted proportion correct",
    y = "Density"
  ) +
  theme_minimal()



