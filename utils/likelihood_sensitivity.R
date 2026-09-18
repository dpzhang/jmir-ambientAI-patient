########################################
# Model reports
########################################
build_likelihood_reports = function(model_output, compile_model_reporting) {
  bbinom_reports = imap_dfr(PRIOR_LABELS$bbinom, function(prior_label, prior_key) {
    imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
      compile_model_reporting(
        model_output$bbinom[[outcome_key]][[prior_key]]$m0$expected_contrasts
      ) %>%
        mutate(
          model = 0L,
          outcome = outcome_key,
          likelihood = "bbinom",
          prior = prior_key,
          .before = 1
        )
    })
  })

  binom_reports = imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
    compile_model_reporting(
      model_output$binom[[outcome_key]]$default$m0$expected_contrasts
    ) %>%
      mutate(
        model = 0L,
        outcome = outcome_key,
        likelihood = "binom",
        prior = "default",
        .before = 1
      )
  })

  list(bbinom = bbinom_reports, binom = binom_reports)
}

########################################
# Likelihood comparison
########################################
compare_likelihood_reports = function(bbinom_reports, binom_reports) {
  bbinom_reports %>%
    transmute(
      model,
      outcome,
      likelihood,
      prior,
      site,
      bbinom_median = diff,
      bbinom_width = .upper - .lower
    ) %>%
    inner_join(
      binom_reports %>%
        transmute(
          outcome,
          site,
          binom_median = diff,
          binom_width = .upper - .lower
        ),
      by = c("outcome", "site")
    ) %>%
    mutate(
      abs_median_shift = abs(bbinom_median - binom_median),
      width_ratio = bbinom_width / binom_width
    )
}

########################################
# Phi prior-to-posterior draws
########################################
generate_phi_learning_df = function(draws,
                                     prior_median,
                                     prior,
                                     outcome,
                                     prior_sdlog = PHI_PRIOR_SDLOG,
                                     seed = PHI_LEARNING_SEED) {
  phi_post = draws$phi
  set.seed(seed)
  phi_prior = rlnorm(
    n = length(phi_post),
    meanlog = log(prior_median),
    sdlog = prior_sdlog
  )

  bind_rows(
    tibble(
      phi = phi_prior,
      log_phi = log(phi_prior),
      distribution = "Prior"
    ),
    tibble(
      phi = phi_post,
      log_phi = log(phi_post),
      distribution = "Posterior"
    )
  ) %>%
    mutate(
      model = 0L,
      outcome = outcome,
      likelihood = "bbinom",
      prior = prior,
      .before = 1
    )
}

########################################
# Phi learning summaries
########################################
summarize_phi_learning = function(draws,
                                   prior_median,
                                   prior_sdlog = PHI_PRIOR_SDLOG,
                                   width = PHI_LEARNING_INTERVAL) {
  log_phi_post = log(draws$phi)
  log_phi_hdi = tibble(log_phi = log_phi_post) %>%
    median_hdi(log_phi, .width = width)

  tibble(
    prior_median_phi = prior_median,
    posterior_median_phi = exp(log_phi_hdi$log_phi),
    posterior_lower_phi = exp(log_phi_hdi$.lower),
    posterior_upper_phi = exp(log_phi_hdi$.upper),
    posterior_log_sd = sd(log_phi_post),
    posterior_prior_sd_ratio = sd(log_phi_post) / prior_sdlog,
    sd_reduction = 1 - sd(log_phi_post) / prior_sdlog
  )
}

build_phi_learning = function(model_output) {
  draws = imap_dfr(PHI_PRIOR_MEDIANS, function(prior_median, prior_key) {
    imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
      generate_phi_learning_df(
        model_output$bbinom[[outcome_key]][[prior_key]]$m0$draws,
        prior_median = prior_median,
        prior = prior_key,
        outcome = outcome_key
      )
    })
  }) %>%
    mutate(
      outcome_label = factor(OUTCOME_LABELS[outcome], levels = unname(OUTCOME_LABELS)),
      prior_label = factor(
        bbinom_prior_labels[prior],
        levels = bbinom_prior_labels[bbinom_prior_order]
      ),
      distribution = factor(distribution, levels = c("Posterior", "Prior"))
    )

  summary = imap_dfr(PHI_PRIOR_MEDIANS, function(prior_median, prior_key) {
    imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
      summarize_phi_learning(
        model_output$bbinom[[outcome_key]][[prior_key]]$m0$draws,
        prior_median = prior_median
      ) %>%
        mutate(
          model = 0L,
          outcome = outcome_key,
          likelihood = "bbinom",
          prior = prior_key,
          .before = 1
        )
    })
  })

  list(draws = draws, summary = summary)
}

########################################
# Phi learning figure
########################################
plot_phi_learning = function(phi_learning_df) {
  ggplot(phi_learning_df, aes(x = log_phi, y = distribution)) +
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
      .width = PHI_LEARNING_PLOT_INTERVALS,
      point_interval = median_hdi,
      slab_alpha = 0.30,
      slab_linewidth = 0.25,
      scale = 0.90,
      show.legend = FALSE
    ) +
    facet_grid(outcome_label ~ prior_label, switch = "y") +
    scale_color_manual(values = DISTRIBUTION_COLORS, aesthetics = "slab_color") +
    scale_fill_manual(
      values = DISTRIBUTION_COLORS,
      aesthetics = c("point_fill", "slab_fill")
    ) +
    labs(x = expression(log(phi)), y = NULL) +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
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
# Pooled contrast data
########################################
build_pooled_likelihood_contrasts = function(model_output) {
  bind_rows(
    imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
      model_output$binom[[outcome_key]]$default$m0$expected_contrasts %>%
        filter(site == "Pooled") %>%
        mutate(
          outcome_label = outcome_label,
          specification = LIKELIHOOD_SPECIFICATION_LABELS[["binom"]]
        )
    }),
    imap_dfr(PRIOR_LABELS$bbinom, function(prior_label, prior_key) {
      imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
        model_output$bbinom[[outcome_key]][[prior_key]]$m0$expected_contrasts %>%
          filter(site == "Pooled") %>%
          mutate(
            outcome_label = outcome_label,
            specification = LIKELIHOOD_SPECIFICATION_LABELS[[prior_key]]
          )
      })
    })
  ) %>%
    mutate(
      outcome_label = factor(outcome_label, levels = unname(OUTCOME_LABELS)),
      specification = factor(
        specification,
        levels = unname(LIKELIHOOD_SPECIFICATION_LABELS)
      )
    )
}

########################################
# Pooled likelihood-sensitivity figure
########################################
plot_likelihood_sensitivity_pooled = function(contrast_draws) {
  contrast_summary = contrast_draws %>%
    group_by(outcome_label, specification) %>%
    median_hdi(diff, .width = ANALYSIS_INTERVAL) %>%
    ungroup()

  facet_labels = tibble(
    outcome_label = factor(unname(OUTCOME_LABELS), levels = unname(OUTCOME_LABELS)),
    label = LIKELIHOOD_FACET_LABELS
  )

  ggplot(contrast_draws, aes(x = specification, y = diff, fill = specification)) +
    stat_slab(
      aes(slab_fill = specification, slab_color = specification),
      slab_alpha = 0.30,
      slab_linewidth = 1,
      scale = LIKELIHOOD_SLAB_SCALE
    ) +
    geom_linerange(
      data = contrast_summary,
      aes(x = specification, ymin = .lower, ymax = .upper),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.65,
      lineend = "round"
    ) +
    geom_point(
      data = contrast_summary,
      aes(x = specification, y = diff, fill = specification),
      inherit.aes = FALSE,
      shape = 22,
      color = "black",
      stroke = 0.8,
      size = 3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~outcome_label, nrow = 1) +
    geom_text(
      data = facet_labels,
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
    scale_color_manual(
      values = LIKELIHOOD_SENSITIVITY_COLORS,
      aesthetics = "slab_color"
    ) +
    scale_fill_manual(
      values = LIKELIHOOD_SENSITIVITY_COLORS,
      aesthetics = c("point_fill", "slab_fill", "fill")
    ) +
    labs(x = NULL, y = "Posterior contrast: post - pre") +
    ggthemes::theme_clean(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      strip.text = element_text(
        size = 8,
        face = "bold",
        margin = margin(1, 2, 1, 2),
        lineheight = 0.9
      ),
      strip.background = element_rect(
        color = "black",
        fill = "white",
        linewidth = 0.4,
        linetype = "solid"
      )
    )
}
