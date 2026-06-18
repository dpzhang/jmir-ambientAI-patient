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


model_df = read.csv('data/model.csv') %>%
  mutate(site = factor(site), 
         period = factor(period, levels = c("pre", "post")))
# Loading fitted model lst
model_lst = readRDS('fitted_models/fitted_models.rds')
source("utils/contextual_analysis.R")
source("utils/counterfactual_analysis.R")
source("utils/paper_figs.R")

################################################################
# Baseline Model
################################################################
baseline_prepost_contrast = 
  plot_posterior_contrast_2outcomes(base_prepost_contrast_ltr_df,
                                    base_prepost_contrast_exp_df,
                                    scale = 0.7) + 
  labs(
    x = NULL,
    y = "Posterior contrast: post - pre") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 3)) +   
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6, face = "bold"),
        strip.text.y.left = element_text(size = 6, face = "bold"),
        panel.grid.major.y = element_blank()
  )

ggsave('figures/baseline.png', baseline_prepost_contrast, 
       width = 2.6, height = 2, units='in', dpi = 300)

################################################################
# Patient Composition Model
################################################################
comp_prepost_contrast = 
  plot_posterior_contrast_2outcomes(age_prepost_contrast_ltr_df,
                                    age_prepost_contrast_exp_df,
                                    scale = 0.7) + 
  labs(
    x = NULL,
    y = "Posterior contrast: post - pre") +
  theme(plot.margin = margin(0,10,0,0),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6, face = "bold"),
        strip.text.y.left = element_text(size = 6, face = "bold"),
        panel.grid.major.y = element_blank()
        )
################################################################
comp_post_shift_contrast = 
  plot_posterior_contrast_2outcomes(age_post_cf_contrast_ltr_df,
                                    age_post_cf_contrast_exp_df,
                                    scale = 0.7) + 
  labs(
    x = NULL,
    y = "Change in predicted top-box rate from\n10pp increase in patients aged ≥65 years") +
  theme(
    plot.margin = margin(0,0,0,10),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6, face = "bold"),
    strip.text.y.left = element_text(size = 6, face = "bold"),
    panel.grid.major.y = element_blank()
    )

comp_fig = comp_prepost_contrast + comp_post_shift_contrast
ggsave('figures/com_mod1.png', comp_fig, 
       width = 5.5, height = 2, units='in', dpi = 300, bg = "transparent")
################################################################
# Burnout Contextual Model
################################################################
burnout_prepost_contrast = 
  plot_posterior_contrast_2outcomes(burnout_prepost_contrast_ltr_df,
                                  burnout_prepost_contrast_exp_df,
                                  scale = 0.7) + 
  labs(
    x = NULL,
    y = "Posterior contrast: post - pre"
  ) +
  theme(plot.margin = margin(0,10,0,0),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6, face = "bold"),
        strip.text.y.left = element_text(size = 6, face = "bold"),
        panel.grid.major.y = element_blank()
  )

################################################################
burnout_post_shift_contrast = 
  plot_posterior_contrast_2outcomes(burnout_post_cf_contrast_ltr_df,
                                  burnout_post_cf_contrast_exp_df,
                                  scale = 0.7) + 
  labs(
    x = NULL,
    y = "Change in predicted top-box rate from\n10pp decrease in burnout prevalence",
  ) + 
  theme(
    plot.margin = margin(0,0,0,10),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6, face = "bold"),
    strip.text.y.left = element_text(size = 6, face = "bold"),
    panel.grid.major.y = element_blank()
  )

burnout_fig = burnout_prepost_contrast + burnout_post_shift_contrast
ggsave('figures/burnout_mod2.png', burnout_fig, 
       width = 5.5, height = 2, units='in', dpi = 300, bg = "transparent")
################################################################
# Adoption Counterfactual Model
################################################################
adopt_prepost_contrast = 
  plot_posterior_contrast_2outcomes(adoption_prepost_contrast_ltr_df,
                                    adoption_prepost_contrast_exp_df,
                                    scale = 0.5) + 
  labs(
    x = NULL,
    y = "Posterior contrast: post - pre"
  ) +
  theme(plot.margin = margin(0,10,0,0),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6, face = "bold"),
        strip.text.y.left = element_text(size = 6, face = "bold"),
        panel.grid.major.y = element_blank()
  )
################################################################
adopt_post_shift_contrast = 
  plot_posterior_contrast_2outcomes(adoption_post_cf_contrast_ltr_df,
                                    adoption_post_cf_contrast_exp_df,
                                    scale = 0.7) + 
  labs(
    x = NULL,
    y = "Change in predicted top-box rate from\n10pp increase in adoption intensity",
  ) + 
  theme(
    plot.margin = margin(0,0,0,10),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6, face = "bold"),
    strip.text.y.left = element_text(size = 6, face = "bold"),
    panel.grid.major.y = element_blank()
  )

adopt_fig = adopt_prepost_contrast + adopt_post_shift_contrast
ggsave('figures/adopt_mod3.png', adopt_fig, 
       width = 5.5, height = 2, units='in', dpi = 300, bg = "transparent")
################################################################
adopt_scenario = plot_adoption_posterior_cf_contrast_2outcomes(
  adoption_cf_contrast_ltr_df,
  adoption_cf_contrast_exp_df,
  point_size = 5,
  scale = 0.8
  ) + 
  labs(
    x = NULL,
    y = "Posterior contrast: post - pre",
  ) +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        strip.text = element_text(size = 6, face = "bold"),
        strip.text.y.left = element_text(size = 6, face = "bold"),
        panel.grid.major.y = element_blank()
  )

ggsave('figures/adopt_scenario.png', adopt_scenario, 
       width = 4.5, height = 2, units='in', dpi = 300)
