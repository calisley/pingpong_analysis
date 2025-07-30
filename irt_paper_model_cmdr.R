# Linked IRT Analysis: Bayesian Hierarchical 2PL Data Preparation
# -------------------------------------------------------------

# 0. Load libraries
library(here)
library(ggplot2)
library(stringr)
library(bayesplot)
library(tidyverse)
library(posterior) 
library(tidybayes)
library(patchwork)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)

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
  process     = processes
) 

stan_code <- "
data {
  int<lower=1> n_obs;
  int<lower=1> n_people;
  int<lower=1> n_items;
  int<lower=1> n_classes;
  int<lower=1> n_process;

  // new array syntax
  array[n_obs] int<lower=1,upper=n_people>   person;
  array[n_obs] int<lower=1,upper=n_items>    item;
  array[n_obs] int<lower=1,upper=n_classes>  classes_obs;
  array[n_obs] int<lower=1,upper=n_process>  process;
  array[n_obs] int<lower=0,upper=1>          response;
}

parameters {
  // 1) Global ability mean & its class‐level and person‐level sds
  real      mu_theta;
  real<lower=0> sigma_class;
  real<lower=0> sigma_theta;

  // 2) Raw effects for partial pooling
  vector[n_classes] mu_class_raw;   // m_k = sigma_class * raw
  vector[n_people]  theta_raw;      // θ_i = mu_theta + sigma_theta * raw

  // 3) Item discrimination hyperpriors & shifts
  real        mu_a;
  real<lower=0> sigma_a;
  vector[n_process-1] delta_a;

  // 4) Item difficulty hyperpriors & shifts
  real        mu_b;
  real<lower=0> sigma_b;
  vector[n_process-1] delta_b;

  // 5) Non‑centered item‐level raw vars
  vector[n_items] log_a_raw;        
  vector[n_items] b_raw;            
}

transformed parameters {
  // expand to actual parameters
  vector[n_classes]      m         = sigma_class * mu_class_raw;
  vector[n_people]       theta_est = mu_theta + sigma_theta * theta_raw;

  // per‐item, per‐process matrices as arrays
  array[n_items, n_process] real a_proc;
  array[n_items, n_process] real b_proc;

  vector[n_items] log_a  = mu_a + sigma_a * log_a_raw;
  vector[n_items] b_base = mu_b + sigma_b * b_raw;

  for (j in 1:n_items) {
    // baseline process = 1
    a_proc[j,1] = exp(log_a[j]);
    b_proc[j,1] = b_base[j];
    // other processes
    for (p in 2:n_process) {
      a_proc[j,p] = exp(log_a[j] + delta_a[p-1]);
      b_proc[j,p] = b_base[j] + delta_b[p-1];
    }
  }
}

model {
  // — Priors on variance scales (half‑Cauchy)
  sigma_class ~ cauchy(0, 5);
  sigma_theta ~ cauchy(0, 5);
  sigma_a     ~ cauchy(0, 5);
  sigma_b     ~ cauchy(0, 5);

  // — Priors on means
  mu_theta ~ normal(0, 1);
  mu_a     ~ normal(0, 1);
  mu_b     ~ normal(0, 1);

  // — Non‑centered raw priors
  mu_class_raw ~ normal(0, 1);
  theta_raw    ~ normal(0, 1);
  delta_a      ~ normal(0, 1);
  delta_b      ~ normal(0, 1);
  log_a_raw    ~ normal(0, 1);
  b_raw        ~ normal(0, 1);

  // — Likelihood
  for (n in 1:n_obs) {
    int i = person[n];
    int j = item[n];
    int k = classes_obs[n];
    int p = process[n];
    real eta = a_proc[j,p] * ( (theta_est[i] + m[k]) - b_proc[j,p] );
    response[n] ~ bernoulli_logit(eta);
  }
}
"

stan_file <- write_stan_file(stan_code)  

# 3. Compile with cmdstanr
mod <- cmdstan_model(stan_file)

# 4. Sample using cmdstanr’s $sample() method
fit_cm <- mod$sample(
  data     = stan_data,
  seed     = 2025,
  chains   = 3,
  parallel_chains = 3,
  iter_warmup     = 1000,
  iter_sampling   = 1000,         # 2000 total as before
  adapt_delta     = 0.95,
  max_depth       = 12,
  refresh         = 100           # print progress every 250 iters
)

# 5. Convert to posterior::draws format (for bayesplot, tidybayes, etc.)
draws <- fit_cm$draws(format = "draws_df")
summary_df <- fit_cm$summary()

# 6. Save the fitted object
fit_cm$save_object("models/paper_model_trees_cmdstanr.rds")





