library(rstan)
library(here)
library(ggplot2)
library(stringr)
library(tidybayes)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)

fit_cm <- read_rds(here("models","final_model.rds"))

draws_prior <- fit_cm$draws(format="draws_df")

exams_long<- read_rds(here("data","clean","irt","model_data.rds"))

#–– 2. BUILD A MAPPING FROM ITEMS & STUDENTS TO PROCESSES ––
# (reuse your exams_long from data prep)

item2proc <- exams_long %>%
  distinct(question_id_factor, process) %>%
  filter(process != "pretest")      # drop pretest for item plots

student2proc <- exams_long %>%
  distinct(student_id_factor, process)

#–– 3. EXTRACT AND TIDY DRAWS ––

#TODO: Add lines for means in these plots

# discrimination a[j]
a_draws <- draws_prior %>%
  select(starts_with("a[")) %>%
  pivot_longer(everything(),
               names_to  = "param",
               values_to = "value") %>%
  mutate(j = as.integer(str_extract(param, "(?<=\\[)\\d+"))) %>%
  inner_join(item2proc, by = c("j" = "question_id_factor")) %>%
  group_by(param, process) %>%
  summarize(value = mean(value)) 
  

# difficulty b[j]
b_draws <- draws_prior %>%
  select(starts_with("b[")) %>%
  pivot_longer(everything(),
               names_to  = "param",
               values_to = "value") %>%
  mutate(j = as.integer(str_extract(param, "(?<=\\[)\\d+"))) %>%
  left_join(item2proc, by = c("j" = "question_id_factor")) %>%
  group_by(param, process) %>%
  summarize(value = mean(value)) %>%
  na.omit()

# ability θ[i]
theta_draws <- draws_prior %>%
  select(starts_with("theta[")) %>%
  pivot_longer(everything(),
               names_to  = "param",
               values_to = "value") %>%
  mutate(i = as.integer(str_extract(param, "(?<=\\[)\\d+"))) %>%
  left_join(student2proc, by = c("i" = "student_id_factor")) %>%
  group_by(param, process) %>%
  filter(process != "pretest") %>%
  summarize(value = mean(value))


p_a <- ggplot(a_draws, aes(x = value, fill = process, color = process)) +
  geom_density(alpha = 0.3, size = 0.5) +
  labs(
    title = "Posterior Mean Discrimination by Process",
    x     = expression(hat(a)),
    y     = "Density",
    fill  = "Process",
    color = "Process"
  ) +
  theme_minimal()

# 2. Difficulty means b[j]
p_b <- ggplot(b_draws, aes(x = value, fill = process, color = process)) +
  geom_density(alpha = 0.3, size = 0.5) +
  labs(
    title = "Posterior Mean Difficulty by Process",
    x     = expression(hat(b)),
    y     = "Density",
    fill  = "Process",
    color = "Process"
  ) +
  theme_minimal()

# 3. Ability means θ[i]
p_theta <- ggplot(theta_draws, aes(x = value, fill = process, color = process)) +
  geom_density(alpha = 0.3, size = 0.5) +
  labs(
    title = "Posterior Mean Ability by Process",
    x     = expression(hat(theta)),
    y     = "Density",
    fill  = "Process",
    color = "Process"
  ) +
  theme_minimal()

# 4. Stack them vertically
(p_a | p_b | p_theta) + 
  plot_layout(ncol = 3) & 
  theme(legend.position = "bottom")

## Posterior predictive checks  ------  ------  ------  ------  ------  ------


# 2. Extract posterior draws of p; this gives a draws_array [iterations × chains × n_obs]
draws_array <- fit_cm$draws("p")  

# Convert to a data frame of draws; columns will be p[1], p[2], …, p[n_obs]
draws_df <- as_draws_df(draws_array)

# 3. Pivot into long form
posterior_long <- draws_df %>%
  mutate(draw = row_number()) %>%       # label each draw
  pivot_longer(
    cols = starts_with("p["),
    names_to = "obs",
    names_pattern = "p\\[(\\d+)\\]",
    values_to = "p"
  ) %>%
  mutate(obs = as.integer(obs))

process_map <- exams_long %>% 
  mutate(obs = row_number()) %>%
  select(process, obs)

# 4. Attach process_type for each observation
posterior_long <- posterior_long %>%
  left_join(
    process_map,
    by = "obs"
  )

# 5. Compute mean p per process_type *within each draw*
posterior_by_proc <- posterior_long %>%
  group_by(draw, process) %>%
  summarize(mean_p = mean(p), .groups = "drop") %>%
  mutate(process = recode(process,
                          gan     = "AI",
                          stats   = "Human Made",
                          pretest = "Pretest"))

obs_proc <- exams_long %>%
  group_by(process) %>%
  summarise(observed = mean(correct)) %>%
  mutate(process = recode(process,
                          gan     = "AI",
                          stats   = "Human Made",
                          pretest = "Pretest"))

levels_order <- obs_proc %>% 
  pull(process) %>% 
  unique()
posterior_by_proc <- posterior_by_proc %>%
  mutate(process = factor(process, levels = levels_order))
obs_proc <- obs_proc %>%
  mutate(process = factor(process, levels = levels_order))


ggplot(posterior_by_proc, aes(x = mean_p, color = process, fill = process)) +
  geom_density(alpha = 0.3, size = 1) +
  # add dashed lines for observed proportions
  geom_vline(data = obs_proc,
             aes(xintercept = observed, color = process),
             linetype = "dashed",
             size     = 0.8) +
  labs(
    title = "Posterior Density of Mean Probability by Process Type",
    x     = "Mean Predicted Probability",
    y     = "Density",
    color = "Process Type",
    fill  = "Process Type"
  ) +
  theme_minimal() +
  facet_wrap(~ process, scales = "free") +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title   = element_text(size = 12),
    legend.title = element_text(size = 12)
  )


##### TIC Curves----------------------------------------------------------------

summ <- fit_cm$summary(c("a","b")) %>%
  as_tibble(rownames = "param") %>%
  select(variable, mean)

# split into 'a' vs 'b' and item index
item_params <- summ %>%
  extract(variable, into = c("par","item"), regex = "([ab])\\[(\\d+)\\]") %>%
  mutate(
    item = as.integer(item),
    par  = if_else(par=="a", "a_est", "b_est")
  ) %>%
  pivot_wider(names_from = par, values_from = mean)

# --- 3. Join with your item → process lookup, keep only AI items ---
# assume you have a data.frame `item_meta` with columns:
#   item (1..n_items) and process (values "gan","stats","pretest")

item_meta <- exams_long %>%
  distinct(item=question_id_factor, class=class_id, process) %>%
  mutate(
    process = recode(process,
                     gan     = "AI",
                     stats   = "Human Made",
                     pretest = "Pretest")
  )

ai_items <- item_params %>%
  left_join(item_meta, by = "item") %>%
  filter(process == "AI")

# --- 4. Compute the Test Information Curve over θ ---
theta_grid <- seq(-3, 3, length.out = 201)
classes    <- unique(ai_items$class)

tic_df <- expand_grid(
  theta = theta_grid,
  class = classes
) %>%
  rowwise() %>%
  mutate(
    # restrict to items in this class
    info = {
      cls_idx <- which(ai_items$class == class)
      a_j      <- ai_items$a_est[cls_idx]
      b_j      <- ai_items$b_est[cls_idx]
      P_ij     <- plogis(a_j * (theta - b_j))
      sum(a_j^2 * P_ij * (1 - P_ij))
    }
  ) %>%
  ungroup()

tic_df_avg <- tic_df %>% 
  group_by(class) %>%
  summarise(
    max_info = max(info)
  )

ggplot(tic_df, aes(x = theta, y = info, color = factor(class))) +
  geom_line(size = 1) +
  labs(
    title = "Test Information Curves for AI Exams, by Class",
    x     = expression(theta~"(ability)"),
    y     = "Information I(theta)",
    color = "Class ID"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title  = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) + theme(legend.position = "none")
  

