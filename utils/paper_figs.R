########################################
# Contrast data
########################################
prepare_model_implied_contrasts = function(model_output, model_key) {
  imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
    model_output$binom[[outcome_key]]$default[[model_key]]$expected_contrasts %>%
      mutate(Outcome = outcome_label)
  }) %>%
    mutate(
      Outcome = factor(Outcome, levels = unname(OUTCOME_LABELS)),
      facet = factor(facet, levels = PAPER_FIGURE_FACET_ORDER),
      site = factor(site, levels = PAPER_FIGURE_SITE_ORDER)
    )
}

########################################
# Facet labels
########################################
build_contrast_facet_labels = function() {
  tibble(
    Outcome = rep(unname(OUTCOME_LABELS), each = length(PAPER_FIGURE_FACET_ORDER)),
    facet = rep(PAPER_FIGURE_FACET_ORDER, times = length(OUTCOME_LABELS)),
    label = PAPER_FIGURE_FACET_LABELS
  ) %>%
    mutate(
      Outcome = factor(Outcome, levels = unname(OUTCOME_LABELS)),
      facet = factor(facet, levels = PAPER_FIGURE_FACET_ORDER)
    )
}

########################################
# Model-implied contrast figure
########################################
plot_model_implied_contrast = function(contrast_draws) {
  contrast_summary = contrast_draws %>%
    group_by(Outcome, facet, site) %>%
    median_hdi(diff, .width = PAPER_FIGURE_INTERVAL) %>%
    ungroup()

  ggplot(contrast_draws, aes(x = site, y = diff, fill = site)) +
    stat_slab(
      aes(slab_fill = site, slab_color = site),
      slab_alpha = 0.30,
      slab_linewidth = 1,
      scale = PAPER_FIGURE_SLAB_SCALE
    ) +
    geom_linerange(
      data = contrast_summary,
      aes(x = site, ymin = .lower, ymax = .upper),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.65,
      lineend = "round"
    ) +
    geom_point(
      data = contrast_summary,
      aes(x = site, y = diff, fill = site),
      inherit.aes = FALSE,
      shape = 22,
      color = "black",
      stroke = 0.8,
      size = 3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
    ggh4x::facet_nested(
      . ~ Outcome + facet,
      scales = "free_x",
      space = "free_x",
      axes = "x",
      nest_line = element_line(color = "grey45", linewidth = 0.4)
    ) +
    geom_text(
      data = build_contrast_facet_labels(),
      aes(x = -Inf, y = -Inf, label = label),
      inherit.aes = FALSE,
      hjust = -0.15,
      vjust = -0.6,
      size = 2.4,
      fontface = "bold"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = 5)
    ) +
    scale_x_discrete(labels = PAPER_FIGURE_SITE_LABELS) +
    scale_color_manual(
      values = PAPER_FIGURE_SITE_COLORS,
      aesthetics = "slab_color"
    ) +
    scale_fill_manual(
      values = PAPER_FIGURE_SITE_COLORS,
      aesthetics = c("point_fill", "slab_fill", "fill")
    ) +
    labs(x = NULL, y = "Posterior contrast: post - pre") +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      strip.placement = "outside",
      strip.text = element_text(
        size = 8,
        face = "bold",
        margin = margin(1, 2, 1, 2),
        lineheight = 0.9
      ),
      strip.background.x = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      ),
      strip.switch.pad.grid = grid::unit(1, "pt"),
      strip.switch.pad.wrap = grid::unit(1, "pt")
    )
}
