generate_baseline_period_contrast_df = function(fit,
                                                var_total = c("ltr_total", 
                                                              "exp_total"),
                                                ndraws = 1000,
                                                draw_seed = 42) {
  var_total = match.arg(var_total)
  
  # Prepare new data: all sites x pre/post
  new_data = expand.grid(
    site = levels(fit$data$site),
    period = levels(fit$data$period)
  ) %>%
    as_tibble() %>%
    mutate(!!var_total := 1)
  
  print(new_data)
  
  # Posterior expected top-box probability
  draws = new_data %>%
    add_linpred_draws(fit, ndraws = ndraws, value = ".prob",
                      transform = TRUE, # transform logit to prob
                      re_formula = NA,
                      seed = draw_seed
                      )
  
  # Pooled site-average prediction
  avg_draws_pooled = draws %>%
    group_by(.draw, period) %>%
    summarise(
      prob = mean(.prob),
      .groups = "drop"
    ) %>%
    mutate(site = "Pooled", .before = 1)
  
  # Site-specific prediction
  avg_draws_site = draws %>%
    ungroup() %>%
    select(site, .draw, period, .prob) %>%
    rename(prob = .prob)
  
  # Combine and compute post - pre
  contrast_draws = bind_rows(avg_draws_pooled, avg_draws_site) %>%
    mutate(
      site = factor(site, levels = c("Pooled", levels(fit$data$site)))
    ) %>%
    pivot_wider(names_from = period, values_from = prob) %>%
    mutate(
      diff = post - pre,
      facet = ifelse(site == "Pooled", "Overall", "Site-specific")
    )
  
  return(contrast_draws)
}

generate_adjusted_period_contrast_df = function(fit, 
                                                var_total = c("ltr_total", 
                                                              "exp_total"),
                                                covariate,
                                                ndraws = 1000,
                                                draw_seed = 42){
  
  var_total = match.arg(var_total)
  # prep the new data
  new_data = expand.grid(
    site = levels(fit$data$site),
    period = levels(fit$data$period)
  ) %>%
    as_tibble() %>%
    mutate(!!var_total := 1) %>%
    left_join(fit$data %>% select(site, period, {{covariate}}),
              by = c('site', 'period'))
  print(new_data)
  # posterior predicted top-box rate from linear predictors
  draws = new_data %>% 
    add_linpred_draws(fit, ndraws = ndraws, value = '.prob', 
                      transform = TRUE, # transform logit to prob
                      re_formula = NA,
                      seed = draw_seed) 
  
  # Pool draws 
  avg_draws_pooled = draws %>%
    group_by(.draw, period) %>%
    summarise(
      prob = mean(.prob),
      .groups = "drop"
    ) %>% 
    mutate(site = "Pooled", .before = 1)
  
  # Marginalize across site within each posterior draw
  avg_draws_site = draws %>%
    ungroup() %>%
    select(site, .draw, period, .prob) %>%
    rename(prob = .prob)
  
  # combine
  avg_draws = bind_rows(avg_draws_pooled, avg_draws_site) %>%
    mutate(site = factor(site, levels = c("Pooled", "Site A", "Site B", "Site C")))
  
  # Compute post - pre contrast within each draw
  contrast_draws = avg_draws %>%
    pivot_wider(names_from = period, values_from = prob) %>%
    mutate(diff = post - pre) %>%
    mutate(facet = ifelse(site == 'Pooled', 'Overall', 'Site-specific'))
  
  return(contrast_draws)
}

generate_post_cf_contrast_df = function(fit,
                                        var_total = c("ltr_total", 
                                                      "exp_total"), 
                                        covariate,
                                        ndraws = 1000,
                                        delta = c("+", "-"),
                                        draw_seed = 42){
  var_total = match.arg(var_total)
  delta = match.arg(delta)
  cf_label = ifelse(delta == "+", "+10pp", "-10pp")
  
  # prep the new data
  new_data_observed = fit$data %>% 
    select(site, period, {{covariate}}) %>%
    filter(period == 'post') %>% 
    mutate(scenario = 'Observed')
  
  if (delta == "+"){
    new_data_cf = new_data_observed %>%
      mutate( {{covariate}} := {{covariate}} + 1,
              scenario = cf_label)
  }else{
    new_data_cf = new_data_observed %>%
      mutate( {{covariate}} := {{covariate}} - 1,
              scenario = cf_label)
  }
  
  
  new_data = bind_rows(new_data_observed, new_data_cf) %>%
    mutate(scenario = factor(scenario, levels = c("Observed", cf_label)),
           !!var_total := 1)
  print(new_data)
  
  # posterior predicted top-box rate from linear predictors
  draws = new_data %>% 
    add_linpred_draws(fit, ndraws = ndraws, value = '.prob', 
                      transform = TRUE, # transform logit to prob
                      re_formula = NA,
                      seed = draw_seed) 
  
  # Overall contrast grouped across sites
  avg_draws_pooled = draws %>%
    group_by(.draw, scenario) %>%
    summarise(
      prob = mean(.prob),
      .groups = "drop"
    ) %>%
    mutate(site = "Pooled", .before = 1)
  
  # Site-specific contrasts grouped by sites
  avg_draws_site = draws %>%
    ungroup() %>%
    select(site, .draw, scenario, .prob) %>%
    rename(prob = .prob)
  
  # combine
  avg_draws = bind_rows(avg_draws_pooled, avg_draws_site) %>%
    mutate(
      site = factor(site, 
                    levels = c("Pooled", "Site A", "Site B", "Site C"))
    )
  
  # Compute counterfactual contrasts within each draw
  contrast_draws = avg_draws %>%
    pivot_wider(names_from = scenario, values_from = prob) %>%
    mutate(diff = .data[[cf_label]] - Observed,
           facet = ifelse(site == "Pooled", "Overall", "Site-specific"))
  
  return(contrast_draws)
}


plot_posterior_contrast = function(plot_df){
  ggplot(
    plot_df,
    aes(x = site, y = diff, fill = site)
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
    facet_wrap(~facet, scales = 'free_x') + 
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
    scale_y_continuous(labels = scales::percent) +
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
