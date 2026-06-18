generate_cf_contrast = function(fit, 
                                var_total = c("ltr_total", 
                                              "exp_total"),
                                cf = c(0.4, 0.6, 0.8),
                                ndraws = 1000,
                                draw_seed = 42){
  var_total = match.arg(var_total)
  # prep the new data
  new_data = expand.grid(
    site = levels(fit$data$site),
    period = levels(fit$data$period),
    pct_heavy_10 = cf * 10) %>%
    as_tibble() %>%
    mutate(!!rlang::sym(var_total) := 1) %>%
    mutate(pct_heavy_10 = ifelse(period == 'pre', 0, pct_heavy_10)) %>% 
    distinct %>%
    mutate(
      level = case_when(
        period == "pre" ~ "Pre: No Ambient AI",
        TRUE ~ paste0("Post: ", pct_heavy_10 * 10, "%")))
  print(new_data)
  # posterior predicted top-box rate from linear predictors
  draws = new_data %>% 
    add_linpred_draws(fit, ndraws = ndraws, value = '.prob', 
                      transform = TRUE, # transform logit to prob
                      re_formula = NA,
                      draw_seed = 42) 
  
  # Marginalize across levels, site, period within each posterior draw
  avg_draws = draws %>%
    group_by(.draw, site, period, level) %>%
    summarise(
      prob = mean(.prob),
      .groups = "drop"
    )
  
  # Compute post - pre contrast within each draw
  contrast_draws = avg_draws %>%
    select(.draw, site, level, prob) %>%
    pivot_wider(names_from = level, values_from = prob) %>%
    transmute(
      .draw = .draw,
      site = site,
      diff_40 = `Post: 40%` - `Pre: No Ambient AI`,
      diff_60 = `Post: 60%` - `Pre: No Ambient AI`,
      diff_80 = `Post: 80%` - `Pre: No Ambient AI`
    ) %>%
    pivot_longer(
      cols = starts_with("diff_"),
      names_to = "contrast",
      values_to = "diff"
    ) %>%
    mutate(level = case_when(
      contrast == 'diff_40' ~ 'Low (40%)',
      contrast == 'diff_60' ~ 'Medium (60%)',
      contrast == 'diff_80' ~ 'High (80%)'
    )) %>%
    mutate(
      level = factor(
        level, 
        levels = c('Low (40%)', 
                   'Medium (60%)', 
                   'High (80%)')
      )
    ) %>%
    select(site, level, diff)
  
  return(contrast_draws)
}


plot_cf_contrast = function(plot_df){
  ggplot(
    plot_df,
    aes(x = level, y = diff, fill = site)
  ) +
    stat_slabinterval(
      aes(
        slab_fill = site,
        slab_color = site,
        point_fill = site
      ),
      shape = 22,
      stroke = 0.8,
      point_color = "black",
      point_size = 3,
      interval_color = "black",
      .width = c(0.66, 0.95),
      point_interval = median_qi,
      slab_alpha = 0.30,
      scale = 0.7
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~site) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(
      values = unname(TABLEAU10_SITE),
      aesthetics = c("slab_color", "interval_color", "point_color")
    ) +
    scale_fill_manual(
      values = unname(TABLEAU10_SITE),
      aesthetics = c("point_fill", "slab_fill")
    ) + 
    theme_clean(base_size = 12) +
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      strip.text = element_text(size = 8, face="bold"),
      plot.background = element_blank(),
      strip.background = element_rect(color="black", 
                                      fill='transparent', 
                                      linewidth=1, 
                                      linetype="solid"))
}
