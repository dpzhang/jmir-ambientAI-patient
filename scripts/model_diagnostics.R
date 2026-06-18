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
library(purrr)
library(glue)
library(infer)
library(patchwork)
library(broom)
library(brms)
library(tidybayes)
library(bayesplot)


source("utils/model_diagnostics.R")
model_df = read.csv('data/model.csv') %>%
  mutate(site = factor(site), 
         period = factor(period, levels = c("pre", "post")))
# Loading fitted model lst
model_lst = readRDS('fitted_models/fitted_models.rds')
names(model_lst$default)

################################################################
# Diagnoistics Table
################################################################
diag_df = imap_dfr(model_lst, function(prior_models, prior_name) {
  imap_dfr(prior_models, function(fit, model_name) {
    mcmc_convergence_check(fit) %>%
      mutate(
        prior = prior_name,
        model = model_name,
        .before = 1
      )
  })
})
diag_df = diag_df %>% 
  separate(model, c("outcome", "model")) %>%
  mutate(outcome = toupper(outcome),
         model = case_when(model == "base" ~ "Model 0",
                           model == "comp" ~ "Model 1",
                           model == "burnout" ~ "Model 2",
                           model == "adopt" ~ "Model 3"))
View(diag_df)

################################################################
# Trace Plot
################################################################
# trace plot for chain mixing
trace_theme = theme_clean(base_size = 12) +
  theme(
    panel.border = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 8, face = "bold"),
    strip.text.y.left = element_text(angle = 0),
    strip.background = element_rect(
      color = "black",
      fill = "transparent",
      linewidth = 0.5,
      linetype = 1
    )
  )

p_base = generate_trace_plot(
  model_lst$default$ltr_base,
  pars = c("b_Intercept", "b_periodpost", "b_siteSiteB", "b_siteSiteC")
) +
  trace_theme +
  labs(title = "Model 0: Baseline") +
  theme(plot.margin = margin(0, 0, 0, 0))

p_adopt = generate_trace_plot(
  model_lst$default$ltr_adopt,
  pars = c("b_Intercept", "b_periodpost", "b_pct_heavy_10", "b_siteSiteB", "b_siteSiteC")
) +
  trace_theme +
  labs(title = "Model 3: Adoption Intensity") +
  theme(plot.margin = margin(0, 0, 0, 0))

p_trace =
  p_base +
  plot_spacer() +
  p_adopt +
  plot_layout(
    widths = c(1, 0.08, 1),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0)
  )

ggsave('figures/trace.png', p_trace, 
       width = 10, height = 5, units='in', dpi = 300, bg = "transparent")

################################################################
# Trace Plot
################################################################
# PP check global stats:
plot_ppc_summary_trellis(model_lst$default$ltr_base)

# PP check conditional fit
plot_ppc_conditional_fit(model_lst$default$ltr_adopt, ltr_top, ltr_total, ndraws = 1000) +
  labs(
    x = NULL,
    y = "Top-box rate",
    title = "PP Check: LTR top-box rate by site and period",
    subtitle = "White point = observed rate; slab and interval = pp distribution"
  )


ppc_m0 = get_model_ppc_data(
  model_lst$default$ltr_base,
  model_lst$default$exp_base
)

ppc_m1 = get_model_ppc_data(
  model_lst$default$ltr_comp,
  model_lst$default$exp_comp
)

ppc_m2 = get_model_ppc_data(
  model_lst$default$ltr_burnout,
  model_lst$default$exp_burnout
)

ppc_m3 = get_model_ppc_data(
  model_lst$default$ltr_adopt,
  model_lst$default$exp_adopt
)

facet_theme = theme(
  plot.margin = margin(0,0,0,10),
  plot.title = element_text(face = "bold", 
                            size = 10, hjust = 0.5),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 6),
  strip.text = element_text(size = 6, face = "bold"),
  strip.text.y.left = element_text(size = 6, face = "bold"),
)

p_m0 = plot_model_ppc(ppc_m0, F) + 
  labs(x = NULL, y = NULL,
       title = "Model 0: Site-adjusted Baseline") + 
  facet_theme 
p_m1 = plot_model_ppc(ppc_m1, F) + 
  labs(x = NULL, y = NULL,
       title = "Model 1: Age Composition") + 
  facet_theme 
p_m2 = plot_model_ppc(ppc_m2) + 
  labs(x = NULL, y = NULL,
       title = "Model 2: Burnout Contextual") + 
  facet_theme 
p_m3 = plot_model_ppc(ppc_m3) + 
  labs(x = NULL, y = NULL,
       title = "Model 3: Adoption Intensity") + 
  facet_theme 


ylab =
  patchwork::wrap_elements(
    full = grid::textGrob(
      "Predicted Top-box Rate",
      rot = 90,
      gp = grid::gpar(fontsize = 8)
    )
  )

plot_grid =
  (
    patchwork::free(p_m0, side = "l") + p_m1
  ) /
  patchwork::plot_spacer() /
  (
    p_m2 + p_m3
  ) +
  patchwork::plot_layout(
    heights = c(1, 0.04, 1),
    guides = "collect"
  ) &
  theme(plot.margin = margin(0, 0, 0, 0))

pp_check_figure =
  ylab + plot_grid +
  patchwork::plot_layout(widths = c(0.015, 1)) &
  theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "white", color = NA)
  )

pp_check_figure

ggsave('figures/pp_check.png', pp_check_figure, 
       width = 10, height = 5, units='in', dpi = 300, bg = "transparent")
