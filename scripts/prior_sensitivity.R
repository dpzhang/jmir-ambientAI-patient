library(readr)
library(readxl)
library(stringr)
library(psych)
library(ggplot2)
library(ggdist)
library(ggh4x)
library(ggthemes)
library(tidyr)
library(purrr)
library(dplyr)
library(magrittr)
library(purrr)
library(glue)
library(infer)
library(patchwork)
library(broom)
library(brms)
library(tidybayes)
library(bayesplot)

TABLEAU10_PRIOR = c(
  "Default" = "#4E79A7",
  "Weak"   = "#76B7B2",
  "Strong" = "#E15759"
)

source("utils/prior_sensitivity.R")
model_df = read.csv('data/model.csv') %>%
  mutate(period = as.character(ifelse(period == 0, "pre", "post"))) %>%
  mutate(site = factor(site), 
         period = factor(period, levels = c("pre", "post")))
# Loading fitted model lst
model_lst = readRDS('fitted_models/fitted_models.rds')
names(model_lst$default)

################################################################
# Extract marginal posterior
################################################################
plot_df = generate_prior_sensitivity_df(model_lst)

################################################################
# Prior sensitivity fig
################################################################
# 4 models each has 2 outcomes, 4 variables, 
base_model = plot_model_posterior_coef(plot_df, 'Baseline', FALSE) + 
  labs(title = "Model 0: Site-adjusted Baseline") 

comp_model = plot_model_posterior_coef(plot_df, 'Age Composition', FALSE) + 
  labs(title = "Model 1: Age Composition",
       y = NULL)

burnout_model = plot_model_posterior_coef(plot_df, 'Burnout Contextual',) + 
  labs(title = "Model 2: Burnout Contextual")

adopt_model = plot_model_posterior_coef(plot_df, 'Adoption Scenario') + 
  labs(title = "Model 3: Adoption Scenario",
       y = NULL)

ylab =
  patchwork::wrap_elements(
    grid::textGrob(
      "Posterior of Coefficients (log-odds)",
      rot = 90,
      gp = grid::gpar(fontsize = 11)
    )
  )

prior_sensitivity = ylab + 
  (patchwork::free(base_model, side = 'l') + comp_model) /
  (burnout_model + adopt_model) +
  plot_layout(guides = "collect") +
  patchwork::plot_layout(widths = c(0.02, 1))

ggsave('figures/prior_sensitivity.png', prior_sensitivity, 
       width = 10, height = 5, units='in', dpi = 300, bg = "transparent")