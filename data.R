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
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
        

# 1. Read data
data_dir <- here("data", "clean")

pre_test  <- read_rds(file.path(data_dir, "pre_tests.rds"))
grades    <- read_rds(file.path(data_dir, "graded_posttests.rds"))
class_names <- read_rds(here("data", "generated", "classes.rds")) %>%
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
  # Remove NA responses for students who took the 8 question pre-test
  filter(!is.na(correct)) 

#' 3. Stack AI generated (gan) vs. Stats (standardized tests) post-test data
#' 3a. Filter to only those exam types as other exams were created on an adhoc
#' basis. 

full_df <- grades %>%
  filter(process %in% c("gan", "stats")) %>%
  mutate(
    process         = as.character(process),
    question_number = as.character(question_number),
    correct         = as.numeric(correct),
  ) %>%
  select(class_id, student_id, question_number, question_id, correct, process) 

# 4. Combine pre- and post-test, then index
students <- full_df %>% 
  distinct(class_id, student_id)

# Add explicit process variable for pretest
pretest_filtered <- pre_test_long %>%
  # Filter down to students we have a pre-test and post-test for
  inner_join(students, by = c("class_id", "student_id")) %>%
  mutate(process = "pretest") %>%
  # Super-score pre-tests for people who took it twice
  group_by(class_id, student_id, question_number, process) %>%
  summarize(correct = max(correct),
            .groups = "drop") %>%
  mutate(question_id = question_number)

# stack all with explicit process column
exams_long <- bind_rows(full_df, pretest_filtered) %>%
  group_by(class_id) %>%
  filter(length(unique(student_id)) > 1) %>% #drop classes with only one student
  ungroup() %>%
  mutate(
    question_id_factor = as.integer(factor(question_id)),
    student_id_factor  = as.integer(factor(paste0(student_id))),
    class_id_factor    = as.integer(factor(class_id)),
    process            = factor(process, levels = c("pretest", "gan", "stats")),
    process_id         = as.integer(process)
  )

saveRDS(exams_long, here("data","clean","irt","model_data.rds"))

# 5. Prepare Stan data dimensions
nn_people  <- n_distinct(exams_long$student_id_factor)
nn_items   <- n_distinct(exams_long$question_id_factor)
nn_classes <- n_distinct(exams_long$class_id_factor)
nn_obs     <- nrow(exams_long)
nn_processes <- length(levels(exams_long$process))

# 6. Build index vectors and response
person_id   <- exams_long$student_id_factor
item_id     <- exams_long$question_id_factor
resp        <- exams_long$correct
classes_obs <- exams_long$class_id_factor  # one class per response

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
  response    = resp
) 

saveRDS(stan_data, here("data","clean","irt", "stan_data.rds"))

