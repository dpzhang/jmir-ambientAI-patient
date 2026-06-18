generate_prior_sensitivity_df = function(model_lst){
  # Extract posterior coefficients
  plot_df = imap_dfr(model_lst, function(prior_models, prior_name) {
    imap_dfr(prior_models, function(fit, model_name) {
      
      posterior::as_draws_df(fit) %>%
        as_tibble %>%
        select(starts_with("b_")) %>% 
        mutate(draw = row_number()) %>%
        pivot_longer(
          cols = starts_with("b_"),
          names_to = "term",
          values_to = "value"
        ) %>%
        mutate(
          prior = prior_name,
          model = model_name,
          term = str_remove(term, "^b_")
        )
    })
  })
 
  # plot_df formatting
  plot_df = plot_df %>%
    filter(term != 'Intercept', 
           !str_starts(term, 'site')) %>%
    mutate(
      prior = prior %>% 
        str_to_title %>% 
        factor(levels = c("Strong", "Default", "Weak")),
      model_group = case_when(
        str_detect(model, "base")    ~ "Baseline",
        str_detect(model, "comp")    ~ "Age Composition",
        str_detect(model, "burnout") ~ "Burnout Contextual",
        str_detect(model, "adopt")   ~ "Adoption Scenario"
      ) %>%
        factor(
          levels = c(
            "Baseline",
            "Age Composition",
            "Burnout Contextual",
            "Adoption Scenario"
          )),
      outcome = case_when(
        str_detect(model, "^ltr") ~ "LTR",
        str_detect(model, "^exp") ~ "EXP"
      ) %>% factor(levels = c("LTR", "EXP")),
      term_label = case_when(
        term == "periodpost"     ~ "Period\n(Post)",
        term == "pct_old_10"     ~ "Age >65\n(+10pp)",
        term == "pct_burnout_10" ~ "Burnout\n(+10pp)",
        term == "pct_heavy_10"   ~ "Adoption\n(+10pp)",
        TRUE ~ term
      ) %>%
        factor(
          levels = c('Period\n(Post)', 
                     'Age >65\n(+10pp)',
                     'Burnout\n(+10pp)',
                     'Adoption\n(+10pp)')
        )
    ) 
  
  return(plot_df)
}

plot_model_posterior_coef = function(plot_df, 
                                     model_name = c("Baseline", "Age Composition", 
                                                    "Burnout Contextual", 
                                                    "Adoption Scenario"),
                                     show_x_axis = TRUE){
  model_name = match.arg(model_name)
  
  plot_df %>%
    filter(model_group == model_name) %>%
    ggplot(aes(x = prior, y = value, fill = prior)) +
    stat_pointinterval(
      aes(interval_color = prior),
      position = position_dodge(width = 0.45),
      shape = 22,
      stroke = 0.7,
      point_color = "black",
      point_fill = 'white',
      point_size = 2.3,
      .width = c(0.66, 0.95),
      point_interval = median_qi
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    facet_grid2(
      rows = vars(term_label),
      cols = vars(outcome),
      scales = "free",
      independent = "y",
      axes = "margins",
      switch = "y"
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 3)
    ) +
    scale_fill_manual(
      values = TABLEAU10_PRIOR,
      aesthetics = "point_fill"
    ) +
    scale_color_manual(
      values = TABLEAU10_PRIOR,
      aesthetics = "interval_color"
    ) +
    labs(
      x = NULL,
      #y = "Posterior coefficient (log-odds)",
      y = NULL,
      fill = "Prior",
    ) +
    theme_clean(base_size = 12) +
    theme(
      panel.border = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.text.x = if (show_x_axis) {
        element_text(size = 7)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x_axis) {
        element_line()
      } else {
        element_blank()
      },
      axis.title.x = element_blank(),
      legend.position = 'none',
      strip.placement = 'outside',
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
