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
library(glue)
library(infer)
library(patchwork)
library(broom)
library(brms)
library(tidybayes)
library(bayesplot)

TABLEAU10_SITE = c(
  "Site A" = "#4E79A7",
  "Site B" = "#E15759",
  "Site C"   = "#76B7B2"
)


source("utils/contextual_analysis.R")
source("utils/counterfactual_analysis.R")
model_df = read.csv('data/model.csv') %>%
  mutate(site = factor(site), 
         period = factor(period, levels = c("pre", "post")))
# Loading fitted model lst
model_lst = readRDS('fitted_models/fitted_models.rds')
names(model_lst$default)

################################################################
# Baseline Model (LTR)
################################################################
base_prepost_contrast_ltr_df = generate_baseline_period_contrast_df(
  model_lst$default$ltr_base,
  'ltr_total'
  )
base_prepost_contrast_ltr_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(base_prepost_contrast_ltr_df)
################################################################
# Baseline Model (EXP)
################################################################
base_prepost_contrast_exp_df = generate_baseline_period_contrast_df(
  model_lst$default$exp_base,
  'exp_total'
  )
base_prepost_contrast_exp_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(base_prepost_contrast_exp_df)

################################################################
# Patient Composition Model (LTR)
################################################################
# Adjusted pre-post contrast in LTR top-box rate
age_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_comp, 
  "ltr_total", 
  pct_old_10)
age_prepost_contrast_ltr_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(age_prepost_contrast_ltr_df) +
  labs(
    x = NULL,
    y = "LTR Top-box rate (Post - Pre)",
    title = "Posterior distribution of adjusted predicted pre/post LTR top-box contrasts",
  )

# Adjusted effect of patient age composition on LTR top-box rate
age_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_comp, 
  "ltr_total", 
  pct_old_10)
age_post_cf_contrast_ltr_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(age_post_cf_contrast_ltr_df) + 
  labs(
    x = NULL,
    y = "Post - Pre adjusted LTR rate",
    title = "Posterior distribution of adjusted case-mix contrasts in LTR top-box rate",
  )

################################################################
# Patient Composition Model (EXP)
################################################################
# Adjusted pre-post contrast in interpersonal experience composite top-box rate
age_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_comp, 
  "exp_total", 
  pct_old_10)
age_prepost_contrast_exp_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(age_prepost_contrast_exp_df) +
  labs(
    x = NULL,
    y = "COMM Top-box rate (Post - Pre)",
    title = "Posterior distribution of adjusted predicted pre/post COMM top-box contrasts",
  )

# Adjusted effect of patient age composition on LTR top-box rate
age_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_comp, 
  "exp_total", 
  pct_old_10)
age_post_cf_contrast_exp_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(age_post_cf_contrast_exp_df) + 
  labs(
    x = NULL,
    y = "COMM Adjusted Top-box rate (Post - Pre)",
    title = "Posterior distribution of adjusted case-mix contrasts in COMM top-box rate",
  )
################################################################
# Burnout Contextual Model (LTR)
################################################################
burnout_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_burnout, 
  'ltr_total', 
  pct_burnout_10)
burnout_prepost_contrast_ltr_df %>%
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(burnout_prepost_contrast_ltr_df)

burnout_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_burnout, 
  "ltr_total", 
  pct_burnout_10,
  delta = "-")
burnout_post_cf_contrast_ltr_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(burnout_post_cf_contrast_ltr_df)

################################################################
# Burnout Contextual Model (EXP)
################################################################
burnout_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_burnout, 
  'exp_total', 
  pct_burnout_10)
burnout_prepost_contrast_exp_df %>%
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(burnout_prepost_contrast_exp_df)

burnout_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_burnout, 
  "exp_total", 
  pct_burnout_10,
  delta = "-")
burnout_post_cf_contrast_exp_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(burnout_post_cf_contrast_exp_df)

################################################################
# Adoption Scenario Model (LTR)
################################################################
adoption_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_adopt, 
  'ltr_total', 
  pct_heavy_10)
adoption_prepost_contrast_ltr_df %>%
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(adoption_prepost_contrast_ltr_df)

adoption_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_adopt, 
  "ltr_total", 
  pct_heavy_10)
adoption_post_cf_contrast_ltr_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(adoption_post_cf_contrast_ltr_df)

adoption_cf_contrast_ltr_df = generate_cf_contrast(
  model_lst$default$ltr_adopt, 
  "ltr_total")
adoption_cf_contrast_ltr_df %>% 
  group_by(site, level) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_cf_contrast(adoption_cf_contrast_ltr_df) + 
  labs(
    x = NULL,
    y = "Top-box rate (Post - Pre)",
    title = "LTR Top-box rate based on counterfactual adoption rate"
  )

################################################################
# Adoption Scenario Model (EXP)
################################################################
adoption_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_adopt, 
  'exp_total', 
  pct_heavy_10)
adoption_prepost_contrast_exp_df %>%
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(adoption_prepost_contrast_exp_df)

adoption_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_adopt, 
  "exp_total", 
  pct_heavy_10)
adoption_post_cf_contrast_exp_df %>% 
  group_by(site) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_posterior_contrast(adoption_post_cf_contrast_exp_df)

adoption_cf_contrast_exp_df = generate_cf_contrast(
  model_lst$default$exp_adopt, 
  "exp_total")
adoption_cf_contrast_exp_df %>% 
  group_by(site, level) %>% 
  median_qi(diff, .width = c(0.66, 0.95))
plot_cf_contrast(adoption_cf_contrast_exp_df) 
