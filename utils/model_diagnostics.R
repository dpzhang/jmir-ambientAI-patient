TABLEAU10_SITE = c(
  "Site A" = "#4E79A7",
  "Site B" = "#E15759",
  "Site C"   = "#76B7B2"
)

OUTCOME_SPECS = list(
  ltr = list(
    top = "ltr_top",
    total = "ltr_total",
    label = "LTR top-box rate"
  ),
  comm = list(
    top = "comm_top",
    total = "comm_total",
    label = "Communication quality top-box rate"
  )
)

mcmc_convergence_check = function(fit, 
                                  rhat_threshold = 1.01, 
                                  max_treedepth = 10){
  # Total number of post-warmup posterior draws across all chains.
  n_post = posterior::ndraws(as_draws_df(fit))
  
  # Summarize posterior draws for all params
  draw_sum = posterior::summarise_draws(as_draws_df(fit)) %>%
    as_tibble() %>%
    filter(!variable %in% c("lp__", "lprior")) %>%
    # neff ratio for central and tail
    mutate(
      ess_bulk_ratio = ess_bulk / n_post,
      ess_tail_ratio = ess_tail / n_post
    ) 
  
  # NUTS sampler diagnostics
  np = nuts_params(fit)
  
  ########################################################
  # Rhat
  max_rhat = max(draw_sum$rhat, na.rm = T)
  n_rhat_gt_threshold = sum(draw_sum$rhat > rhat_threshold, na.rm = TRUE)
  ########################################################
  # neff ratio
  min_bulk = min(draw_sum$ess_bulk_ratio, na.rm = T)
  min_tail = min(draw_sum$ess_tail_ratio, na.rm = T)  
  ########################################################
  # divergent transitions 
  n_divergent = np %>%
    filter(Parameter == "divergent__") %>%
    summarise(n = sum(Value == 1)) %>%
    pull(n)
  ########################################################
  # treedepth
  n_treedepth = np %>%
    filter(Parameter == "treedepth__") %>%
    summarise(n = sum(Value >= max_treedepth)) %>%
    pull(n)
  
  # summary output
  tibble(
    max_rhat = max_rhat,
    `n_rhat>1.01` = n_rhat_gt_threshold,
    min_neff_ratio_bulk = min_bulk,
    min_neff_ratio_tail = min_tail,
    n_divergent = n_divergent,
    `n_reached_max_treedepth` = n_treedepth
  )
}

generate_trace_plot = function(fit, pars){
  mcmc_trace(as.array(fit), pars = pars) + 
    theme_clean() + 
    theme(legend.position = "bottom", plot.margin = margin(10, 10, 10, 40))
}

plot_pp_stat = function(fit, stat_name, ndraws = 1000, binwidth = 1) {
  pp_check(fit, type = "stat", stat = stat_name, ndraws = ndraws, binwidth = binwidth) +
    labs(
      title = glue("Posterior predictive check: {tools::toTitleCase(stat_name)}"),
      x = glue("Replicated {stat_name}"),
      y = "Frequency"
    ) +
    theme_clean() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.line = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()
          )
}

plot_ppc_summary_trellis = function(fit){
  pp_plot_mean = plot_pp_stat(fit, 'mean')
  pp_plot_sd = plot_pp_stat(fit, 'sd')
  pp_plot_min = plot_pp_stat(fit, 'min')
  pp_plot_max = plot_pp_stat(fit, 'max')
  
  pp_trellis =
    (pp_plot_mean | pp_plot_sd) /
    (pp_plot_min  | pp_plot_max)
  
  return(pp_trellis)
}

plot_ppc_conditional_fit = function(fit, var_top, var_total, ndraws = 1000){
  # observed proportions
  obs_rate = fit$data %>%
    mutate(
      obs_rate = {{var_top}} / {{var_total}}
    ) %>%
    select(site, period, {{var_top}}, {{var_total}}, obs_rate)
  
  # get posterior predictive draws
  ppc_pred_rate = fit$data %>% 
    add_predicted_draws(fit, ndraws = ndraws) %>% 
    mutate(pred_rate = .prediction / {{var_total}})
  
  # compile plot_df
  plot_df = obs_rate %>% 
    left_join(ppc_pred_rate, by = c("site", "period"))

  # plotting function
  output = ggplot(plot_df) +
    aes(x = period, y = pred_rate) +
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
    geom_point(
      data = obs_rate,
      aes(x = period, y = obs_rate),
      inherit.aes = FALSE,
      shape = 21, fill = "white", color = "black", size = 2, stroke = 0.5
    ) +
    facet_wrap(~ site) +
    theme_clean() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      strip.text = element_text(size = 8, face="bold"),
      plot.background = element_blank(),
      strip.background = element_rect(color="black", 
                                      fill='transparent', 
                                      linewidth=1, 
                                      linetype="solid")
    ) +
    scale_y_continuous(labels = scales::percent) + 
    scale_color_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("slab_color", "interval_color", "point_color")
    ) +
    scale_fill_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("point_fill", "slab_fill")
    )
  
  return(output)
}


get_ppc_rate_data = function(fit, outcome, top_var, total_var, ndraws = 1000) {
  
  top_var = enquo(top_var)
  total_var = enquo(total_var)
  
  obs = fit$data %>%
    mutate(
      Outcome = outcome,
      top = !!top_var,
      total = !!total_var,
      obs_rate = top / total
    ) %>%
    select(Outcome, site, period, top, total, obs_rate)
  
  pred = fit$data %>%
    add_predicted_draws(fit, ndraws = ndraws) %>%
    mutate(
      Outcome = outcome,
      top = !!top_var,
      total = !!total_var,
      pred_rate = .prediction / total
    )
  
  list(obs = obs, pred = pred)
}

get_model_ppc_data = function(ltr_fit, exp_fit, ndraws = 1000) {
  
  ltr = get_ppc_rate_data(
    fit = ltr_fit,
    outcome = "LTR",
    top_var = ltr_top,
    total_var = ltr_total,
    ndraws = ndraws
  )
  
  exp = get_ppc_rate_data(
    fit = exp_fit,
    outcome = "EXP",
    top_var = exp_top,
    total_var = exp_total,
    ndraws = ndraws
  )
  
  list(
    obs = bind_rows(ltr$obs, exp$obs) %>% 
      mutate(Outcome = factor(Outcome, levels = c("LTR", "EXP"))),
    pred = bind_rows(ltr$pred, exp$pred) %>% 
      mutate(Outcome = factor(Outcome, levels = c("LTR", "EXP")))
  )
}

plot_model_ppc = function(ppc_data, show_x_axis = TRUE) {
  ggplot(ppc_data$pred, aes(x = period, y = pred_rate)) +
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
      .width = c(0.66, 0.95),
      point_interval = median_qi,
      slab_alpha = 0.30,
      slab_linewidth = 0.2,
      scale = 0.7
    ) +
    geom_point(
      data = ppc_data$obs,
      aes(x = period, y = obs_rate),
      inherit.aes = FALSE,
      shape = 21,
      fill = "white",
      color = "black",
      size = 1,
      stroke = 0.5
    ) +
    facet_grid(
      Outcome ~ site,
      scales = "free_x",
      space = "free_x",
      axes = "all_x",
      axis.labels = "margins",
      switch = "y"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(
      values = TABLEAU10_SITE,
      aesthetics = c("slab_color")
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
      plot.margin = margin(0, 0, 0, 0),
      
      strip.placement = "outside",
      
      axis.text.x = if (show_x_axis) {
        element_text(size = 7)
      } else {
        element_blank()
      },
      
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
