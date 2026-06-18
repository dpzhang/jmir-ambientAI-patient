library(readr)
library(readxl)
library(stringr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(tidyr)
library(purrr)
library(dplyr)
library(magrittr)
library(infer)
library(patchwork)
library(broom)
library(brms)
library(tidybayes)
library(bayesplot)

########################################
# Presets
########################################
source("utils/model_fit.R")
model_df = read.csv('data/model.csv') %>%
  mutate(site = factor(site), 
         period = factor(period, levels = c("pre", "post")))
########################################
# Prior setup
########################################
PRIOR_LIST = list(
  default = c(prior(normal(0, 2.5), class = "Intercept"),
              prior(normal(0, 1), class = "b")),
  strong = c(prior(normal(0, 2.5), class = "Intercept"),
             prior(normal(0, 0.75), class = "b")),
  weak = c(prior(normal(0, 2.5), class = "Intercept"),
           prior(normal(0, 1.5), class = "b")))

########################################
# Model specification
########################################
MODEL_FORMULAS = list(
  #----------------------------------------
  # MODEL 0: Site-adjusted Baseline
  #----------------------------------------
  ltr_base = bf(ltr_top | trials(ltr_total)  ~ site + period),
  exp_base = bf(exp_top | trials(exp_total) ~ site + period),
  #----------------------------------------
  # MODEL 1: Patient Composition
  #----------------------------------------
  ltr_comp = bf(ltr_top | trials(ltr_total)  ~ site + period + pct_old_10),
  exp_comp = bf(exp_top | trials(exp_total) ~ site + period + pct_old_10),
  #----------------------------------------
  # MODEL 2: Burnout Contextual
  #----------------------------------------
  ltr_burnout = bf(ltr_top | trials(ltr_total)  ~ site + period + pct_burnout_10),
  exp_burnout = bf(exp_top | trials(exp_total) ~ site + period + pct_burnout_10),
  #----------------------------------------
  # MODEL 3: Adoption Counterfactual
  #----------------------------------------
  ltr_adopt = bf(ltr_top | trials(ltr_total) ~ site + period + pct_heavy_10),
  exp_adopt = bf(exp_top | trials(exp_total) ~ site + period + pct_heavy_10)
)

########################################
# Fitted export
########################################
fitted_models = fit_model_grid(
  model_formulas = MODEL_FORMULAS,
  prior_list = PRIOR_LIST,
  data = model_df,
  save_list = TRUE,
  save_path = "fitted_models/fitted_models.rds"
)