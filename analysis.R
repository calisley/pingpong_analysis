library(clubSandwich)
library(AER)
library(sandwich)
library(lmtest)
library(tidyverse)

ROOT <- here::here()
model_data <- readRDS(path("data","clean","model_data.rds"))


#ATE
m1 <- lm(posttest_score ~ treated + pretest_score , data = model_data)
clubSandwich::coef_test(m1, cluster = merged_scores$class_id, vcov = "CR2")

#ATT
m2 <- ivreg(posttest_score ~ used_pp + pretest_score | treated + pretest_score, data = model_data)
# Compute clustered (class-level) standard errors
cl_vcov <- vcovCL(m2, cluster = ~class_id)
# Coefficient test table with clustered SEs
coeftest(m2, vcov = cl_vcov)
