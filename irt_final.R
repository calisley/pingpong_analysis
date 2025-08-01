# Linked IRT Analysis: Bayesian Hierarchical 2PL Data Preparation
# -------------------------------------------------------------

# 0. Load libraries
library(here)
library(ggplot2)
library(stringr)
library(tidybayes)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)

options(mc.cores = parallel::detectCores())

stan_data<-read_rds(here("data","clean","irt", "stan_data.rds"))


# irt_model.R -------------------------------------------------------------

stan_code_basic <-
"
data {
  int<lower=1> n_obs;                   // total number of responses
  int<lower=1> n_people;                // number of respondents
  int<lower=1> n_items;                 // number of items

  array[n_obs] int<lower=1, upper=n_people> person;  // which person gave each response
  array[n_obs] int<lower=1, upper=n_items>  item;    // which item was responded to
  array[n_obs] int<lower=0, upper=1>        response; // 0/1 response
}

parameters {
  // --- item‐parameter hyperpriors
  real<lower=0> sigma_a;         // SD of log‑discrimination
  real<lower=0> sigma_b;         // SD of difficulty

  real mu_a;                     // grand mean on log‑discrimination scale
  real mu_b;                     // grand mean for difficulty

  // --- raw (unscaled) parameters for items
  vector[n_items] log_a_raw;     // raw draws for log‑discrimination
  vector[n_items] b_raw;         // raw draws for difficulty
  vector<lower=0, upper=0.35>[n_items] c;  // guessing parameter per item
  
  // --- person abilities, directly on the θ scale
  vector[n_people] theta;        // each ~ Normal(0,1)
}

transformed parameters {
  vector[n_items] a;             // discrimination parameters
  vector[n_items] b;             // difficulty parameters
  vector[n_items] c;             // fixed guessing parameter (0.25)
  
  // scale item parameters
  for (j in 1:n_items) {
    a[j] = exp(mu_a + sigma_a * log_a_raw[j]);
    b[j] = mu_b + sigma_b * b_raw[j];
    c[j] = 0.25;  // ← fixed value
  }
  
}

model {
  // Priors for item‐parameter hyperparameters
  sigma_a ~ cauchy(0, 1);
  sigma_b ~ cauchy(0, 1);

  mu_a ~ normal(0, 1);
  mu_b ~ normal(0, 1);

  // Priors on raw item parameters
  log_a_raw ~ normal(0, 1);
  b_raw     ~ normal(0, 1);

  // Prior on person abilities (no hyperprior)
  theta ~ normal(0, 1);

  // Likelihood
  {
    vector[n_obs] eta = a[item] .* (theta[person] - b[item]);
    vector[n_obs] logistic_part = inv_logit(eta);
    vector[n_obs] p = c[item] + (1 - c[item]) .* logistic_part;
    response ~ bernoulli(p);
  }
}

generated quantities {
  vector[n_obs] p;
  for (n in 1:n_obs) {
    real eta = a[item[n]] * (theta[person[n]] - b[item[n]]);
    p[n] = 0.25 + 0.75 * inv_logit(eta);  // shortcut since c = 0.25
  }
}
"

# Write stan code
stan_file <- write_stan_file(stan_code_basic)  

# 3. Compile with cmdstanr
mod <- cmdstan_model(stan_file)

# 4. Sample using cmdstanr’s $sample() method
fit_cm <- mod$sample(
  data     = stan_data,
  seed     = 2025,
  chains   = 4,
  parallel_chains = 4,
  iter_warmup     = 500,
  iter_sampling   = 1000,         
  adapt_delta     = 0.95,
  max_treedepth   = 15,
  refresh         = 20           
)

# 6. Save the fitted object
fit_cm$save_object(here("models","irt_model_3PL_constra.rds"))

