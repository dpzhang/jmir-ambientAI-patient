########################################
# Prior-sensitivity data
########################################
generate_prior_sensitivity_df = function(model_output) {
  imap_dfr(model_output$binom, function(priors, outcome_key) {
    imap_dfr(priors, function(models, prior_key) {
      imap_dfr(models, function(entry, model_key) {
        as_tibble(entry$draws) %>%
          select(.draw, starts_with("b_")) %>%
          pivot_longer(
            cols = starts_with("b_"),
            names_to = "term",
            values_to = "value"
          ) %>%
          mutate(
            prior = PRIOR_LABELS$binom[[prior_key]],
            model = model_key,
            outcome = OUTCOME_LABELS[[outcome_key]],
            term = stringr::str_remove(term, "^b_")
          )
      })
    })
  }) %>%
    filter(
      term != "Intercept",
      !stringr::str_starts(term, "site")
    ) %>%
    mutate(
      prior = factor(prior, levels = unname(PRIOR_LABELS$binom[PRIOR_SENSITIVITY_PRIOR_ORDER])),
      model_group = factor(
        unname(PRIOR_SENSITIVITY_MODEL_GROUPS[model]),
        levels = unname(PRIOR_SENSITIVITY_MODEL_GROUPS)
      ),
      outcome = factor(outcome, levels = unname(OUTCOME_LABELS)),
      term_label = factor(
        unname(PRIOR_SENSITIVITY_TERM_LABELS[term]),
        levels = unname(PRIOR_SENSITIVITY_TERM_LABELS)
      )
    )
}

########################################
# Coefficient panels
########################################
plot_model_posterior_coef = function(plot_df, model_name) {
  plot_df %>%
    filter(model_group == model_name) %>%
    ggplot(aes(x = prior, y = value, fill = prior)) +
    stat_pointinterval(
      aes(interval_color = prior),
      position = position_dodge(width = 0.45),
      shape = 22,
      stroke = 0.7,
      point_color = "black",
      point_fill = "white",
      point_size = 2.3,
      .width = PRIOR_SENSITIVITY_INTERVALS,
      point_interval = median_hdi
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    ggh4x::facet_grid2(
      rows = vars(term_label),
      cols = vars(outcome),
      scales = "free",
      independent = "y",
      axes = "margins",
      switch = "y"
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    scale_fill_manual(values = TABLEAU10_PRIOR, aesthetics = "point_fill") +
    scale_color_manual(values = TABLEAU10_PRIOR, aesthetics = "interval_color") +
    labs(x = NULL, y = NULL, fill = "Prior") +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      panel.border = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_blank(),
      legend.position = "none",
      strip.placement = "outside",
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      strip.text = element_text(size = 8, face = "bold"),
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_rect(
        color = "black",
        fill = "transparent",
        linewidth = 0.5,
        linetype = 1
      )
    ) +
    guides(
      color = "none",
      point_fill = "none",
      fill = guide_legend(title = "Prior")
    )
}

########################################
# Combined figure
########################################
plot_prior_sensitivity = function(plot_df) {
  model_panels = imap(PRIOR_SENSITIVITY_MODEL_GROUPS, function(model_group, model_key) {
    plot_model_posterior_coef(plot_df, model_group) +
      labs(title = MODEL_LABELS[[model_key]])
  })

  ylab = patchwork::wrap_elements(
    grid::textGrob(
      "Posterior of Coefficients (log-odds)",
      rot = 90,
      gp = grid::gpar(fontsize = 11)
    )
  )

  panels =
    (patchwork::free(model_panels$m0, side = "l") +
       model_panels$m1 +
       model_panels$m2) +
    patchwork::plot_layout(widths = c(1.25, 1, 1))

  ylab + panels +
    patchwork::plot_layout(widths = c(0.035, 1), guides = "collect")
}
