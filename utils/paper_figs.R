################################################################
# Baseline Model
################################################################
base_prepost_contrast_ltr_df = generate_baseline_period_contrast_df(
  model_lst$default$ltr_base,
  'ltr_total') 
base_prepost_contrast_exp_df = generate_baseline_period_contrast_df(
  model_lst$default$exp_base,
  'exp_total')

################################################################
# Patient Composition Model: site-adjusted pre/post
################################################################
age_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_comp, 
  "ltr_total", 
  pct_old_10)
age_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_comp, 
  "exp_total", 
  pct_old_10)

################################################################
# Patient Composition Model: post counterfactual
################################################################
age_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_comp, 
  "ltr_total", 
  pct_old_10)

age_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_comp, 
  "exp_total", 
  pct_old_10)

################################################################
# Burnout Contextual Model: site-adjusted pre/post
################################################################
burnout_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_burnout, 
  'ltr_total', 
  pct_burnout_10)

burnout_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_burnout, 
  'exp_total', 
  pct_burnout_10)

################################################################
# Burnout Contextual Model: post counterfactual
################################################################
burnout_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_burnout, 
  "ltr_total", 
  pct_burnout_10,
  delta = "-")

burnout_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_burnout, 
  "exp_total", 
  pct_burnout_10,
  delta = "-")

################################################################
# Adoption Counterfactual Model: site-adjust pre/post
################################################################
adoption_prepost_contrast_ltr_df = generate_adjusted_period_contrast_df(
  model_lst$default$ltr_adopt, 
  'ltr_total', 
  pct_heavy_10)

adoption_prepost_contrast_exp_df = generate_adjusted_period_contrast_df(
  model_lst$default$exp_adopt, 
  'exp_total', 
  pct_heavy_10)

################################################################
# Adoption Counterfactual Model: post counterfactual
################################################################
adoption_post_cf_contrast_ltr_df = generate_post_cf_contrast_df(
  model_lst$default$ltr_adopt, 
  "ltr_total", 
  pct_heavy_10)

adoption_post_cf_contrast_exp_df = generate_post_cf_contrast_df(
  model_lst$default$exp_adopt, 
  "exp_total", 
  pct_heavy_10)

################################################################
# Adoption Counterfactual Model: use scenario
################################################################
adoption_cf_contrast_ltr_df = generate_cf_contrast(
  model_lst$default$ltr_adopt, 
  "ltr_total") %>%
  mutate(
    level = forcats::fct_recode(
      level,
      "Low\n(40%)" = "Low (40%)",
      "Medium\n(60%)" = "Medium (60%)",
      "High\n(80%)" = "High (80%)"
    )
  )

adoption_cf_contrast_exp_df = generate_cf_contrast(
  model_lst$default$exp_adopt, 
  "exp_total") %>%
  mutate(
    level = forcats::fct_recode(
      level,
      "Low\n(40%)" = "Low (40%)",
      "Medium\n(60%)" = "Medium (60%)",
      "High\n(80%)" = "High (80%)"
    )
  )

################################################################################
build_annot_contrast_2outcomes = function(labels){
  annot_df = tibble(
    Outcome = rep(c("LTR", "EXP"), each = 2),
    facet = rep(c("Overall", "Site-specific"), times = 2),
    label = c(labels)
  ) %>%
    mutate(
      Outcome = factor(Outcome, levels = c("LTR", "EXP")),
      facet = factor(facet, levels = c("Overall", "Site-specific"))
    )
}

build_annot_scenario_2outcomes = function(labels){
  annot_df = tibble(
    Outcome = rep(c("LTR", "EXP"), each = 3),
    site = rep(c("Site A", "Site B", "Site C"), times = 2),
    label = c(labels)
  ) %>%
    mutate(
      Outcome = factor(Outcome, levels = c("LTR", "EXP")),
      site = factor(site, levels = c("Site A", "Site B", "Site C"))
    )
}

add_gg_facet_labels = function(annot_df){
  gg_facet_labels = geom_text(
    data = annot_df,
    aes(x = -Inf, y = -Inf, label = label),
    hjust = -0.15,
    vjust = -0.6,
    inherit.aes = FALSE,
    size = 1.5,
    fontface = "bold"
  )
}
################################################################################
plot_posterior_contrast_2outcomes = function(ltr_df, exp_df,
                                             scale = 0.7){
  ltr_df = ltr_df %>% mutate(Outcome = "LTR")
  exp_df = exp_df %>% mutate(Outcome = "EXP")
  
  plot_df = bind_rows(ltr_df, exp_df) %>%
    mutate(
      Outcome = factor(Outcome, levels = c("LTR", "EXP")),
      facet = factor(facet, levels = c("Overall", "Site-specific")),
      site = factor(site, levels = c("Pooled", "Site A", "Site B", "Site C"))
    )
  
  ggplot(
    plot_df,
    aes(x = site, y = diff, fill = site)
  ) +
    stat_slabinterval(
      aes(
        slab_fill = site,
        slab_color = site,
        point_fill = site,
        point_color = site
      ),
      shape = 95,
      stroke = 0.8,
      point_size = 5,
      interval_color = "black",
      .width = c(0.66, 0.95),
      point_interval = median_qi,
      slab_alpha = 0.30,
      slab_linewidth = 0.2,
      scale = scale
    ) +
    facet_grid(
      Outcome ~ facet,
      scales = "free_x",
      space = "free_x",
      axes = "all_x",
      axis.labels = "margins",
      switch = "y"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = scales::breaks_pretty(n = 5)) +
    scale_color_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("slab_color", "interval_color", "point_color")
    ) +
    scale_fill_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("point_fill", "slab_fill")
    ) +
    theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0,0,0,0),
      
      strip.placement = "outside",
      strip.text = element_text(
        size = 8, 
        face = "bold",
        margin = margin(1, 2, 1, 2),
        lineheight = 0.9
      ),
      strip.text.y.left = element_text(
        size = 8, 
        face = "bold",
        margin = margin(1, 1, 1, 1),
        lineheight = 0.9
        ),
      
      strip.background.x = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      ),
      strip.background.y = element_blank(),
      
      strip.switch.pad.grid = unit(1, "pt"),
      strip.switch.pad.wrap = unit(1, "pt")
    )
}


plot_adoption_posterior_cf_contrast_2outcomes = function(ltr_df, exp_df,
                                                         point_size = 2,
                                                         scale = 0.7){
  ltr_df = ltr_df %>% mutate(Outcome = "LTR")
  exp_df = exp_df %>% mutate(Outcome = "EXP")
  
  plot_df = bind_rows(ltr_df, exp_df) %>%
    mutate(
      Outcome = factor(Outcome, levels = c("LTR", "EXP")),
      site = factor(site, levels = c("Site A", "Site B", "Site C"))
    )
  
  ggplot(
    plot_df,
    aes(x = level, y = diff, fill = site)
  ) +
    stat_slabinterval(
      aes(
        slab_fill = site,
        slab_color = site,
        point_fill = site,
        point_color = site
      ),
      shape = 95,
      stroke = 0.8,
      point_size = point_size,
      interval_color = "black",
      .width = c(0.66, 0.95),
      point_interval = median_qi,
      slab_alpha = 0.30,
      slab_linewidth = 0.2,
      scale = scale
    ) +
    facet_grid(
      Outcome ~ site,
      scales = "free_x",
      space = "free_x",
      axes = "all_x",
      axis.labels = "margins",
      switch = "y"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = scales::breaks_pretty(n = 5)) +
    scale_color_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("slab_color", "interval_color", "point_color")
    ) +
    scale_fill_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("point_fill", "slab_fill")
    ) +
    theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0,0,0,0),
      
      strip.placement = "outside",
      strip.text = element_text(
        size = 8, 
        face = "bold",
        margin = margin(1, 2, 1, 2),
        lineheight = 0.9
      ),
      strip.text.y.left = element_text(
        size = 8, 
        face = "bold",
        margin = margin(1, 1, 1, 1),
        lineheight = 0.9
      ),
      
      strip.background.x = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      ),
      strip.background.y = element_blank(),
      
      strip.switch.pad.grid = unit(1, "pt"),
      strip.switch.pad.wrap = unit(1, "pt")
    )
}
