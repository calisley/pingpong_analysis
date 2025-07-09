library(tidyverse)
library(fs)
library(lubridate)

ROOT <- here::here()

################################################################################
#' Data for Analysis
#' Merge in answer keys, textual analysis, etc to be explored
################################################################################

############################### Map ############################################



############################### Pre-Test #######################################

# 1. Grade responses for correct answers

answer_key <- list(
  q1 = "1,000",
  q2 = "32",
  q3 = "x=41",
  q4 = "2/9",
  q5 = "1/6",
  q6 = "8π",
  q7 = "3 hours",
  q8 = "A",
  q9 = "A",
  q10 = "A",
  q11 = "4"
)

readRDS(path(ROOT,"data","generated","pretest.rds")) %>%
  mutate(across(all_of(names(answer_key)), ~ .x == answer_key[[cur_column()]], .names = "correct_{.col}"))
  
