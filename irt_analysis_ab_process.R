# Analysis Script: Linked IRT with Process & Class×Item Shifts

library(rstan)
library(here)
library(tidyverse)
library(bayesplot)
library(posterior)
library(tidybayes)
library(patchwork)
library(gt)


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
    student_id_factor  = as.integer(factor(paste0(student_id))), # A few students are in multiple classes
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

# Make maps for each index 
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

student_map <- long_df %>%
  filter(process %in% c("gan", "stats")) %>%
  distinct(student_id_factor, process) %>%
  mutate(
    exam_type = recode(process,
                       gan   = "AI",
                       stats = "Human Made")
  )
ai_idx    <- long_df %>% filter(process=="gan")  %>% pull(question_id_factor) %>% unique()
stats_idx <- long_df %>% filter(process=="stats")%>% pull(question_id_factor) %>% unique()
pre_idx   <- long_df %>% filter(process=="pretest")%>% pull(question_id_factor) %>% unique()



# 2. Stan Fit -----------------------------------------------------------
fit <- read_rds(here("models","paper_model.rds"))




# New ---------------------------------------------------------------------



# Baseline & by‐process item parameters
a_base_mat   <- post$a_proc[ , , 1]    # draws × n_items  (baseline discrimination)
b_base_mat   <- post$b_base            # draws × n_items  (baseline difficulty)
a_proc_arr   <- post$a_proc            # draws × n_items × n_process
b_proc_arr   <- post$b_proc            # draws × n_items × n_process

# Process‐shifts on log‐a and b
delta_a_mat  <- post$delta_a           # draws × (n_process−1)
delta_b_mat  <- post$delta_b           # draws × (n_process−1)

# Person abilities
theta_mat    <- post$theta_est         # draws × n_people

n_draws      <- nrow(a_base_mat)


# ppc ---------------------------------------------------------------------


idx_list <- list(
  gan     = list(items = ai_idx,
                 students = long_df %>% filter(process=="gan")    %>% pull(student_id_factor) %>% unique()),
  stats   = list(items = stats_idx,
                 students = long_df %>% filter(process=="stats")  %>% pull(student_id_factor) %>% unique()),
  pretest = list(items = pre_idx,
                 students = long_df %>% filter(process=="pretest")%>% pull(student_id_factor) %>% unique())
)

ppc_proc <- map_dfr(names(idx_list), function(proc) {
  items    <- idx_list[[proc]]$items
  students <- idx_list[[proc]]$students
  ppc_vec  <- numeric(n_draws)
  proc_idx <- match(proc, c("pretest","gan","stats"))
  
  for (d in seq_len(n_draws)) {
    # discrimination and difficulty for these items/process
    a_d      <- a_proc_arr[d, items, proc_idx]
    shift_b  <- if (proc_idx > 1) delta_b_mat[d, proc_idx - 1] else 0
    b_d      <- b_base_mat[d, items] + shift_b
    
    # abilities for these students
    th       <- theta_mat[d, students]
    
    # predicted proportion correct
    ppc_vec[d] <- mean(plogis( outer(th, a_d, "-") - b_d ))
  }
  
  tibble(
    process = recode(proc,
                     gan     = "AI",
                     stats   = "Human Made",
                     pretest = "Pretest"),
    draw    = seq_len(n_draws),
    pred    = ppc_vec
  )
})

obs_proc <- long_df %>%
  group_by(process) %>%
  summarize(observed = mean(correct)) %>%
  mutate(process = recode(process,
                          gan     = "AI",
                          stats   = "Human Made",
                          pretest = "Pretest"))

pp <- ggplot(ppc_proc, aes(x = pred, fill = process, color = process)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = obs_proc,
             aes(xintercept = observed,
                 linetype = "Empirical Proportion Correct"),
             color = "black", linewidth = 0.8) +
  facet_wrap(~process, scales = "free") +
  theme_minimal() +
  labs(x = "Predicted Proportion Correct",
       y = "Density",
       fill = "Process",
       color = "Process",
       linetype = "") +
  scale_linetype_manual(values = c("Empirical Proportion Correct" = "dashed"))

print(pp)



# 6. Density Plots of a, b, theta ---------------------------------------
a_hat        <- colMeans(a_base_mat)
b_hat        <- colMeans(b_base_mat)
delta_a_hat  <- colMeans(delta_a_mat)    # length = n_process-1
delta_b_hat  <- colMeans(delta_b_mat)
p_gan   <- 2   # GAN process in your Stan model
p_stats <- 3   # Stats process

# pull posterior mean discrimination directly from the draws
a_ai_eff    <- colMeans( a_proc_arr[ , ai_idx,   p_gan   ] )
a_stats_eff <- colMeans( a_proc_arr[ , stats_idx, p_stats ] )

a_df <- tibble(
  a    = c(a_ai_eff, a_stats_eff),
  exam = rep(c("AI","Stats"),
             c(length(a_ai_eff), length(a_stats_eff)))
)

# pull posterior mean difficulty directly, too
b_ai_eff    <- colMeans( b_proc_arr[ , ai_idx,   p_gan   ] )
b_stats_eff <- colMeans( b_proc_arr[ , stats_idx, p_stats ] )

b_df <- tibble(
  b    = c(b_ai_eff, b_stats_eff),
  exam = rep(c("AI","Stats"),
             c(length(b_ai_eff), length(b_stats_eff)))
)

# Ability θ by exam type
theta_hat <- colMeans(theta_mat)
theta_df  <- tibble(
  student_id_factor = seq_along(theta_hat),
  theta             = theta_hat
) %>%
  left_join(
    long_df %>%
      filter(process %in% c("gan","stats")) %>%
      distinct(student_id_factor, process) %>%
      mutate(exam = recode(process,
                           gan   = "AI",
                           stats = "Human-made")),
    by = "student_id_factor"
  )

# Plot densities
p1 <- ggplot(a_df, aes(a, fill=exam, color=exam)) +
  geom_density(alpha=0.3) +
  labs(title="Discrimination (a)", fill="Exam Type", color="Exam Type") +
  theme_minimal() +
  theme(legend.position="none")

p2 <- ggplot(b_df, aes(b, fill=exam, color=exam)) +
  geom_density(alpha=0.3) +
  labs(title="Difficulty (b)", fill="Exam Type", color="Exam Type") +
  theme_minimal() +
  theme(legend.position="none")

p3 <- ggplot(theta_df, aes(theta, fill=exam, color=exam)) +
  geom_density(alpha=0.3) +
  labs(title="Student Ability (θ)", x="Ability (θ)", fill="Exam Type", color="Exam Type") +
  theme_minimal()

densities <- p1 | p2 | p3 + plot_annotation(title="Parameter Densities by Exam Type")
ggsave(here("plots","parameter_densities.png"), densities,
       dpi=300, width=12, height=4)


# 7. Summary Table of Item Parameters ----------------------------------
mean_sd <- function(x) sprintf("%.2f (%.2f)", mean(x), sd(x))

table1 <- tibble(
  `Exam Type` = c("AI","Human-made"),
  a_mean_sd   = c(mean_sd(a_ai_eff),    mean_sd(a_stats_eff)),
  a_min       = c(min(a_ai_eff),        min(a_stats_eff)),
  a_max       = c(max(a_ai_eff),        max(a_stats_eff)),
  b_mean_sd   = c(mean_sd(b_ai_eff),    mean_sd(b_stats_eff)),
  b_min       = c(min(b_ai_eff),        min(b_stats_eff)),
  b_max       = c(max(b_ai_eff),        max(b_stats_eff))
)

gt_table <- gt(table1) %>%
  tab_header(title = md("**Table 1.** Summary statistics for item parameters")) %>%
  cols_label(
    a_mean_sd = "Mean (SD)", a_min = "Min", a_max = "Max",
    b_mean_sd = "Mean (SD)", b_min = "Min", b_max = "Max"
  ) %>%
  tab_spanner(label="Discrimination (a)", columns=vars(a_mean_sd,a_min,a_max)) %>%
  tab_spanner(label="Difficulty (b)",    columns=vars(b_mean_sd,b_min,b_max))

gtsave(gt_table, filename=here("plots","table1_item_parameters_process_fx.png"))


# 8. Test Information Curves --------------------------------------------
theta_grid <- seq(-3,3,length=101)
proc_idx   <- function(p) match(p, c("pretest","gan","stats"))

tic_class_df <- map_dfr(unique(long_df$class_id_factor), function(cid) {
  proc <- unique(long_df$process[long_df$class_id_factor == cid])
  proc <- proc[proc != "pretest"]
  idx  <- long_df %>%
    filter(class_id_factor==cid, process==proc) %>%
    pull(question_id_factor) %>%
    unique()
  
  mat <- matrix(0, n_draws, length(theta_grid))
  p_i <- proc_idx(proc)
  
  for (d in 1:n_draws) {
    a_d     <- a_proc_arr[d, idx, p_i]
    shift_b <- if (p_i>1) delta_b_mat[d,p_i-1] else 0
    b_d     <- b_base_mat[d, idx] + shift_b
    
    mat[d, ] <- sapply(theta_grid, function(th) {
      pj <- plogis(a_d * (th - b_d))
      sum(a_d^2 * pj * (1-pj))
    })
  }
  
  tibble(
    theta    = theta_grid,
    info     = colMeans(mat),
    class_id = cid,
    process  = proc
  )
})

tic_proc <- tic_class_df %>%
  group_by(process,theta) %>%
  summarize(
    mean  = mean(info),
    lower = quantile(info, .025),
    upper = quantile(info, .975)
  )

ggplot(tic_proc, aes(theta,mean,color=process,fill=process)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2) +
  theme_minimal()


# 9. Reliability ---------------------------------------------------------
rel_df <- tic_proc %>%
  mutate(
    rel   = mean/(mean+1),
    lower = lower/(lower+1),
    upper = upper/(upper+1)
  ) %>%
  group_by(process) %>%
  summarize(
    avg       = mean(rel),
    rel0      = rel[theta==0],
    peak      = max(rel),
    theta_peak= theta[which.max(rel)]
  )

print(rel_df)


# 10. Delta‐contrast ------------------------------------------------------
library(posterior)
draws_da <- as_draws_df(fit, variables="^delta_a\\[")
diff_draws <- draws_da$`delta_a[1]` - draws_da$`delta_a[2]`

tibble(
  mean_diff = mean(diff_draws),
  lower95   = quantile(diff_draws, .025),
  upper95   = quantile(diff_draws, .975),
  P_gt_zero = mean(diff_draws > 0)
)





# 3. Extract Posterior Draws ---------------------------------------------
a_mat     <- post$a          # draws × items
b_mat     <- post$b          # draws × items
b_ic_arr  <- post$b_ic       # draws × items × classes
delta_mat <- post$delta_b    # draws × 2
delta_a_mat <- post$delta_a    # draws × (n_process–1)
theta_mat <- post$theta      # draws × persons

a_proc_arr  <- post$a_proc     # draws × n_items × n_process
delta_b_mat <- post$delta_b    # draws × (n_process-1)
theta_mat   <- post$theta      # draws × n_people
b_mat      <- post$b_base     # for the baseline (pretest) β_j
b_proc_arr <- post$b_proc     # for β_j by processn_draws <- nrow(a_mat)


# 4. Convergence Diagnostics -------------------------------------------
rhat_vals <- summary(fit)$summary[,"Rhat"]
mcmc_rhat(rhat_vals) + ggtitle("Rhat Diagnostics")

# 5. Posterior Predictive Checks ----------------------------------------
idx_list <- list(
  gan     = list(items=ai_idx,    students=long_df %>% filter(process=="gan")  %>% pull(student_id_factor) %>% unique()),
  stats   = list(items=stats_idx, students=long_df %>% filter(process=="stats")%>% pull(student_id_factor) %>% unique()),
  pretest = list(items=pre_idx,   students=long_df %>% filter(process=="pretest")%>% pull(student_id_factor) %>% unique())
)
ppc_proc <- map_dfr(names(idx_list), function(proc) {
  items    <- idx_list[[proc]]$items
  students <- idx_list[[proc]]$students
  ppc_vec  <- numeric(n_draws)
  
  proc_idx <- match(proc, c("pretest","gan","stats"))
  
  for (d in seq_len(n_draws)) {
    a_d <- a_proc_arr[d, items, proc_idx]
    shift_b <- if (proc_idx > 1) delta_b_mat[d, proc_idx - 1] else 0
    b_d     <- b_mat[d, items] + shift_b
    th <- theta_mat[d, students]
    ppc_vec[d] <- mean(plogis(outer(th, a_d, "-") - b_d))
  }
  
  tibble(
    process = recode(proc, gan = "AI", stats = "Human Made", pretest = "Pretest"),
    draw    = seq_len(n_draws),
    pred    = ppc_vec
  )
})

obs_proc <- long_df %>%
  group_by(process) %>%
  summarize(observed = mean(correct)) %>%
  mutate(process = recode(process, gan = "AI", stats = "Human Made", pretest = "Pretest"))

# Plot with empirical lines in legend
pp <- ggplot(ppc_proc, aes(x = pred, fill = process, color = process)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = obs_proc,
             aes(xintercept = observed, linetype = "Empirical Proportion Correct"),
             color = "black",
             linewidth = 0.8) +
  facet_wrap(~process, scales = "free") +
  theme_minimal() +
  labs(x = "Predicted Proportion Correct",
       y = "Density",
       fill = "Process",
       color = "Process",
       linetype = "") +
  scale_linetype_manual(values = c("Empirical Proportion Correct" = "dashed"))

print(pp)


# 6. Density Plots of a, b, theta ---------------------------------------
a_hat <- colMeans(a_mat)
b_hat <- colMeans(b_mat)
delta_a_hat    <- colMeans(delta_a_mat)          # length 2: GAN, Stats

a_ai_eff      <- a_hat[ai_idx]    * exp(delta_a_hat[1])
a_stats_eff   <- a_hat[stats_idx] * exp(delta_a_hat[2])

a_df <- tibble(
  a     = c(a_ai_eff, a_stats_eff),
  exam  = rep(c("AI","Stats"), c(length(a_ai_eff),length(a_stats_eff)))
)

# effective b: combine global b with process shift (mean shift)
delta_hat <- colMeans(delta_mat)
b_ai_eff    <- b_hat[ai_idx]    + delta_hat[1]
b_stats_eff <- b_hat[stats_idx] + delta_hat[2]

b_df <- tibble(
  b=c(b_ai_eff, b_stats_eff),
  exam=rep(c("AI","Stats"), c(length(b_ai_eff),length(b_stats_eff)))
)
# Ability 'theta' by exam type
theta_hat <- colMeans(theta_mat)
theta_df  <- tibble(
  student_id_factor = seq_along(theta_hat),
  theta             = theta_hat
) %>%
  left_join(
    long_df %>%
      filter(process %in% c("gan","stats")) %>%
      distinct(student_id_factor, process) %>%
      mutate(
        exam = recode(process,
                      "gan"   = "AI",
                      "stats" = "Human-made")
      ),
    by = "student_id_factor"
  )

# Plot densities
p1 <- ggplot(a_df, aes(a, fill = exam, color = exam)) +
  geom_density(alpha = 0.3) +
  labs(title = "Discrimination (a)", fill = "Exam Type", color = "Exam Type") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(b_df, aes(b, fill = exam, color = exam)) +
  geom_density(alpha = 0.3) +
  labs(title = "Difficulty (b)", fill = "Exam Type", color = "Exam Type") +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(theta_df, aes(theta, fill = exam, color = exam)) +
  geom_density(alpha = 0.3) +
  labs(title = "Student Ability (θ)", x = "Ability (θ)", fill = "Exam Type", color = "Exam Type") +
  theme_minimal()
densities<-p1|p2|p3 + plot_annotation(title="Parameter Densities by Exam Type")

ggsave(here("plots", "parameter_densities.png"), 
       densities, 
       dpi=300,
       width = 12, height = 4)
# 7. Summary Table of Item Parameters ----------------------------------
# Helper: mean (SD) formatter
mean_sd <- function(x) {
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# Compute table
table1 <- tibble(
  `Exam Type` = c("AI", "Human-made"),
  
  # Discrimination (a)
  a_mean_sd = c(mean_sd(a_ai_eff),      mean_sd(a_stats_eff)),
  a_min     = c(sprintf("%.2f", min(a_ai_eff)), 
                sprintf("%.2f", min(a_stats_eff))),
  a_max     = c(sprintf("%.2f", max(a_ai_eff)), 
                sprintf("%.2f", max(a_stats_eff))),
  
  # Difficulty (b)
  b_mean_sd = c(mean_sd(b_ai_eff),      mean_sd(b_stats_eff)),
  b_min     = c(sprintf("%.2f", min(b_ai_eff)), 
                sprintf("%.2f", min(b_stats_eff))),
  b_max     = c(sprintf("%.2f", max(b_ai_eff)), 
                sprintf("%.2f", max(b_stats_eff)))
)

# Render with gt
gt_table <- gt(table1) %>%
  tab_header(title = md("**Table 1.** Summary statistics for item parameters")) %>%
  cols_label(
    a_mean_sd = "Mean (SD)",
    a_min     = "Min",
    a_max     = "Max",
    b_mean_sd = "Mean (SD)",
    b_min     = "Min",
    b_max     = "Max"
  ) %>%
  tab_spanner(label = "Discrimination (a)", columns = vars(a_mean_sd, a_min, a_max)) %>%
  tab_spanner(label = "Difficulty (b)",    columns = vars(b_mean_sd, b_min, b_max))

# Save to file
gtsave(gt_table,
       filename = here("plots", "table1_item_parameters_process_fx.png"))


# 7. Test Information Curves --------------------------------------------
theta_grid <- seq(-3,3,length=101)
proc_idx <- function(p) match(p, c("pretest","gan","stats"))

theta_grid <- seq(-3, 3, length = 101)

tic_class_df <- map_dfr(unique(long_df$class_id_factor), function(cid) {
  proc <- unique(long_df$process[long_df$class_id_factor == cid])
  proc <- proc[proc != "pretest"]
  idx  <- long_df %>%
    filter(class_id_factor == cid, process == proc) %>%
    pull(question_id_factor) %>%
    unique()
  
  mat <- matrix(0, n_draws, length(theta_grid))
  p_i <- proc_idx(proc)           # 1, 2 or 3
  
  for (d in 1:n_draws) {
    # discrimination for this draw, items & process
    a_d    <- a_proc_arr[d, idx, p_i]
    
    # difficulty + process shift
    shift_b <- if (p_i > 1) delta_b_mat[d, p_i-1] else 0
    b_d     <- b_mat[d, idx] + shift_b
    
    # now compute info at each theta
    mat[d, ] <- sapply(theta_grid, function(th) {
      pj <- plogis(a_d * (th - b_d))
      sum(a_d^2 * pj * (1 - pj))
    })
  }
  
  tibble(
    theta    = theta_grid,
    info     = colMeans(mat),
    class_id = cid,
    process  = proc
  )
})


tic_proc <- tic_class_df %>% group_by(process,theta) %>% summarize(mean=mean(info),lower=quantile(info,.025),upper=quantile(info,.975))

ggplot(tic_proc, aes(theta,mean,color=process,fill=process)) +
  geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2) + theme_minimal()

# 8. Reliability ---------------------------------------------------------
rel_df <- tic_proc %>%
  mutate(rel=mean/(mean+1), lower=lower/(lower+1), upper=upper/(upper+1)) %>%
  group_by(process) %>%
  summarize(
    avg=mean(rel),
    rel0=rel[theta==0],
    peak=rel[which.max(rel)],
    theta_peak=theta[which.max(rel)]
  )
rel_df


# extract draws of delta_b
post <- as_draws_df(fit, variables = "^delta_b\\[")
post <- as_draws_df(fit, variables = "^delta_a\\[")

# assumes names delta_b[1], delta_b[2]
diff_draws <- post$`delta_a[1]` - post$`delta_a[2]`

# point estimate
mean_diff  <- mean(diff_draws)

# 95% credible interval
ci_diff    <- quantile(diff_draws, c(0.025, 0.975))

# posterior probability delta1 > delta2
p_gt_zero  <- mean(diff_draws > 0)

tibble(
  mean_diff = mean_diff,
  lower95   = ci_diff[1],
  upper95   = ci_diff[2],
  P_gt_zero = p_gt_zero
)
