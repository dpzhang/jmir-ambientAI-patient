
########################################
# Convergence diagnostics
########################################
mcmc_convergence_check = function(fit, 
                                  rhat_threshold = RHAT_THRESHOLD, 
                                  max_treedepth = MAX_TREEDEPTH[["binom"]]){
  draws = fit$draws
  n_post = posterior::ndraws(draws)
  
  draw_sum = posterior::summarise_draws(fit$draws) %>%
    as_tibble() %>%
    filter(
      !variable %in% c("lp__", "lprior"),
      !startsWith(variable, "prior_")
    ) %>%
    mutate(
      ess_bulk_ratio = ess_bulk / n_post,
      ess_tail_ratio = ess_tail / n_post
    ) 
  
  np = fit$sampler_diagnostics
  
  max_rhat = max(draw_sum$rhat, na.rm = TRUE)
  n_rhat_gt_threshold = sum(draw_sum$rhat > rhat_threshold, na.rm = TRUE)
  min_ess_bulk = min(draw_sum$ess_bulk, na.rm = TRUE)
  min_ess_tail = min(draw_sum$ess_tail, na.rm = TRUE)
  min_bulk = min(draw_sum$ess_bulk_ratio, na.rm = TRUE)
  min_tail = min(draw_sum$ess_tail_ratio, na.rm = TRUE)  
  n_divergent = np %>%
    filter(Parameter == "divergent__") %>%
    summarise(n = sum(Value == 1)) %>%
    pull(n)
  n_treedepth = np %>%
    filter(Parameter == "treedepth__") %>%
    summarise(n = sum(Value >= max_treedepth)) %>%
    pull(n)
  
  tibble(
    max_rhat = max_rhat,
    !!paste0("n_rhat>", rhat_threshold) := n_rhat_gt_threshold,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    min_neff_ratio_bulk = min_bulk,
    min_neff_ratio_tail = min_tail,
    n_divergent = n_divergent,
    `n_reached_max_treedepth` = n_treedepth
  )
}

build_model_diagnostics_table = function(model_output) {
  imap_dfr(model_output, function(outcomes, likelihood) {
    imap_dfr(outcomes, function(priors, outcome) {
      imap_dfr(priors, function(models, prior) {
        imap_dfr(models, function(entry, model) {
          mcmc_convergence_check(entry, max_treedepth = MAX_TREEDEPTH[[likelihood]]) %>%
            mutate(likelihood = LIKELIHOOD_LABELS[[likelihood]],
                   prior = PRIOR_LABELS[[.env$likelihood]][[.env$prior]],
                   outcome = OUTCOME_LABELS[[outcome]],
                   model = paste("Model", sub("m", "", model)), .before = 1)
        })
      })
    })
  })
}

########################################
# Trace plots
########################################
plot_binom_trace_block = function(plot_df, model_label) {
  ggplot(
    plot_df %>% filter(model == model_label),
    aes(x = .iteration, y = value, color = .chain, group = .chain)
  ) +
    geom_line(linewidth = 0.18, alpha = 0.80) +
    facet_grid(
      parameter ~ outcome,
      scales = "free_y",
      switch = "y"
    ) +
    scale_color_manual(values = trace_chain_colors) +
    labs(
      x = "Post-warmup Iteration",
      y = NULL,
      color = "Chain",
      title = model_label
    ) +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.width = unit(0.35, "cm"),
      legend.key.height = unit(0.20, "cm"),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 7, face = "bold"),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(0, 2, 0, 2),
      strip.placement = "outside",
      strip.text = element_text(size = 7, face = "bold"),
      strip.text.y.left = element_text(size = 7, face = "bold", angle = 90),
      strip.background.x = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      ),
      strip.background.y = element_blank()
    )
}
plot_bbinom_trace = function(bbinom_trace_df) {
  ggplot(
    bbinom_trace_df,
    aes(x = .iteration, y = value, color = .chain, group = .chain)
  ) +
  geom_line(linewidth = 0.18, alpha = 0.80) +
  ggh4x::facet_nested(
    parameter ~ prior + Outcome,
    scales = "free_y",
    switch = "y",
    labeller = labeller(parameter = label_parsed),
    nest_line = element_line(color = "grey45", linewidth = 0.35)
  ) +
  scale_color_manual(
    values = trace_chain_colors
  ) +
  labs(x = "Post-warmup Iteration", y = NULL, color = "Chain") +
  ggthemes::theme_clean(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.35, "cm"),
    legend.key.height = unit(0.20, "cm"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7, face = "bold"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    strip.placement = "outside",
    strip.text = element_text(size = 7, face = "bold"),
    strip.text.y.left = element_text(size = 7, face = "bold", angle = 90),
    strip.background.x = element_rect(
      color = "black",
      fill = "white",
      linewidth = 0.4,
      linetype = "solid"
    ),
    strip.background.y = element_blank()
  )

}

########################################
# Predictive distributions
########################################
build_observed_rates = function(data) {
  map_dfr(names(OUTCOME_LABELS), function(outcome) {
    tibble(
      site = as.character(data$site),
      period = as.character(data$period),
      Outcome = OUTCOME_LABELS[[outcome]],
      obs_rate = data[[paste0(outcome, "_top")]] / data[[paste0(outcome, "_total")]]
    )
  })
}

plot_binom_predictive = function(ppc_data) {
  observed = distinct(ppc_data, Outcome, model, period, site, obs_rate)
  ggplot(ppc_data, aes(x = site, y = pred_rate)) +
    stat_slabinterval(
      aes(
        slab_fill = site,
        slab_color = site,
        point_fill = site
      ),
      shape = 22,
      stroke = 0.8,
      point_color = "black",
      point_size = 2,
      interval_color = "black",
      .width = PREDICTIVE_INTERVALS,
      point_interval = median_qi,
      slab_alpha = 0.30,
      slab_linewidth = 0.2,
      scale = 0.7,
      show.legend = FALSE
    ) +
    geom_point(
      data = observed,
      aes(x = site, y = obs_rate),
      inherit.aes = FALSE,
      shape = 21,
      fill = "white",
      color = "black",
      size = 1.1,
      stroke = 0.5
    ) +
    ggh4x::facet_nested(
      Outcome ~ model + period,
      switch = "y",
      scales = "free_y",
      nest_line = element_line(color = "grey45", linewidth = 0.4)
    ) +
    ggh4x::facetted_pos_scales(
      y = list(
        Outcome == "LTR" ~ scale_y_continuous(
          limits = BINOM_PREDICTIVE_LIMITS$LTR,
          breaks = BINOM_PREDICTIVE_BREAKS$LTR,
          labels = scales::percent
        ),
        Outcome == "EXP" ~ scale_y_continuous(
          limits = BINOM_PREDICTIVE_LIMITS$EXP,
          breaks = BINOM_PREDICTIVE_BREAKS$EXP,
          labels = scales::percent
        )
      )
    ) +
    scale_color_manual(
      values = TABLEAU10_SITE,
      labels = SITE_DISPLAY_LABELS,
      aesthetics = c("slab_color")
    ) +
    scale_fill_manual(
      values = TABLEAU10_SITE,
      labels = SITE_DISPLAY_LABELS,
      aesthetics = c("point_fill", "slab_fill", "fill")
    ) +
    labs(x = NULL, y = NULL) +
    theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 7),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_text(size = 7),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      
      strip.placement = "outside",
      strip.text = element_text(
        size = 7,
        face = "bold",
        margin = margin(1, 2, 1, 2),
        lineheight = 0.9
      ),
      strip.text.y.left = element_text(size = 8, face = "bold", angle = 90),
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

plot_bbinom_predictive = function(ppc_data, y_limits = BBINOM_PREDICTIVE_LIMITS) {
  observed = distinct(ppc_data, Outcome, prior, period, site, obs_rate)
  ggplot(ppc_data, aes(x = prior, y = pred_rate)) +
    stat_slabinterval(
      aes(
        group = interaction(prior, site),
        slab_fill = site,
        slab_color = site,
        point_fill = site
      ),
      position = position_dodge(width = 0.60),
      shape = 22,
      fill = NA,
      stroke = 0.8,
      point_color = "black",
      point_size = 2,
      interval_color = "black",
      .width = PREDICTIVE_INTERVALS,
      point_interval = median_qi,
      slab_alpha = 0.30,
      slab_linewidth = 0.2,
      scale = 0.7,
      show.legend = FALSE
    ) +
    geom_point(
      data = observed,
      aes(x = prior, y = obs_rate, group = site),
      inherit.aes = FALSE,
      position = position_dodge(width = 0.60),
      shape = 21,
      fill = "white",
      color = "black",
      size = 1.2,
      stroke = 0.5
    ) +
    facet_grid(
      Outcome ~ period,
      switch = "y"
    ) +
    scale_y_continuous(labels = scales::percent) +
    coord_cartesian(ylim = y_limits) +
    scale_color_manual(
      values = TABLEAU10_SITE,
      labels = SITE_DISPLAY_LABELS,
      aesthetics = c("slab_color")
    ) +
    scale_fill_manual(
      values = TABLEAU10_SITE,
      labels = SITE_DISPLAY_LABELS,
      aesthetics = c("point_fill", "slab_fill", "fill")
    ) +
    scale_interval_size_continuous(guide = "none") +
    scale_interval_alpha_continuous(guide = "none") +
    scale_thickness_shared(guide = "none") +
    guides(
      fill = guide_legend(
        title = "Site",
        order = 1,
        override.aes = list(alpha = 1, shape = 22, size = 2.5)
      ),
      slab_fill = "none",
      slab_color = "none",
      point_fill = "none",
      .width = "none"
    ) +
    labs(x = NULL, y = NULL) +
    theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.width = unit(0.35, "cm"),
      legend.key.height = unit(0.20, "cm"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      
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


plot_site_legend = function() {
  site_key = tibble(
    x = c(1.05, 2.20, 3.35),
    y = 1,
    site = factor(names(TABLEAU10_SITE), levels = names(TABLEAU10_SITE)),
    site_label = SITE_DISPLAY_LABELS[as.character(site)]
  )

  ggplot() +
    annotate(
      "text",
      x = 0.25,
      y = 1,
      label = "Site",
      fontface = "bold",
      size = 3.2,
      hjust = 0,
      vjust = 0.5
    ) +
    geom_point(
      data = site_key,
      aes(x = x, y = y, fill = site),
      shape = 22,
      color = "black",
      size = 3.0,
      stroke = 0.5
    ) +
    geom_text(
      data = site_key,
      aes(x = x + 0.18, y = y, label = site_label),
      hjust = 0,
      vjust = 0.5,
      size = 3.2
    ) +
    annotate(
      "segment",
      x = 4.40,
      xend = 4.40,
      y = 0.78,
      yend = 1.22,
      linewidth = 0.3,
      color = "grey45"
    ) +
    annotate(
      "point",
      x = 4.85,
      y = 1,
      shape = 21,
      fill = "white",
      color = "black",
      size = 3.0,
      stroke = 0.6
    ) +
    annotate(
      "text",
      x = 5.10,
      y = 1,
      label = "Observed data",
      hjust = 0,
      vjust = 0.5,
      size = 3.2
    ) +
    scale_fill_manual(values = TABLEAU10_SITE, labels = SITE_DISPLAY_LABELS) +
    coord_cartesian(xlim = c(0.15, 6.65), ylim = c(0.70, 1.30), clip = "off") +
    theme_void(base_size = 12) +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
}

combine_predictive_figure = function(plot, legend_height = 0.10) {
  label = patchwork::wrap_elements(full = grid::textGrob(
    "Predicted Top-box Rate", rot = 90, gp = grid::gpar(fontsize = 10)))
  label + (plot / plot_site_legend() + plot_layout(heights = c(1, legend_height))) +
    plot_layout(widths = c(0.035, 1)) &
    theme(plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "white", color = NA))
}

########################################
# Contextual prior-to-posterior learning
########################################
generate_contextual_learning_df = function(fit,
                                           coefficient,
                                           contextual_label,
                                           outcome) {
  draws = fit$draws
  
  bind_rows(
    tibble(
      value = draws$prior_b,
      distribution = "Prior"
    ),
    tibble(
      value = draws[[coefficient]],
      distribution = "Posterior"
    )
  ) %>%
    mutate(
      contextual = contextual_label,
      outcome = outcome,
      coefficient = coefficient
    )
}

plot_contextual_prior_posterior = function(learning_df) {
  plot_df = learning_df %>%
    mutate(
      contextual = factor(
        contextual,
        levels = unname(CONTEXTUAL_LABELS)
      ),
      outcome = factor(outcome, levels = unname(OUTCOME_LABELS)),
      distribution = factor(
        distribution,
        levels = c("Prior", "Posterior")
      )
    )
  
  ggplot(
    plot_df,
    aes(
      x = distribution,
      y = value
    )
  ) +
    stat_slabinterval(
      aes(
        slab_fill = distribution,
        slab_color = distribution,
        point_fill = distribution
      ),
      shape = 22,
      point_color = "black",
      interval_color = "black",
      point_size = 1.8,
      stroke = 0.5,
      .width = PREDICTIVE_INTERVALS,
      point_interval = median_hdi,
      slab_alpha = 0.30,
      slab_linewidth = 0.25,
      scale = 0.70,
      show.legend = FALSE
    ) +
    facet_grid(
      outcome ~ contextual,
      switch = "y"
    ) +
    scale_color_manual(
      values = DISTRIBUTION_COLORS,
      aesthetics = "slab_color"
    ) +
    scale_fill_manual(
      values = DISTRIBUTION_COLORS,
      aesthetics = c("point_fill", "slab_fill")
    ) +
    labs(
      x = NULL,
      y = "Contextual coefficient (log-odds scale)"
    ) +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      strip.placement = "outside",
      strip.text = element_text(size = 7, face = "bold"),
      strip.text.y.left = element_text(size = 8, face = "bold", angle = 90),
      strip.background.x = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      ),
      strip.background.y = element_blank()
    )
}


########################################
# Figure export
########################################

save_diagnostic_figure = function(plot, filename, size = SUMMARY_SIZE) {
  ggsave(file.path(FIGURE_DIR, filename), plot, width = size[["width"]],
         height = size[["height"]], units = "in", dpi = FIGURE_DPI, bg = "white")
}
