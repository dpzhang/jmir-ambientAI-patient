########################################
# Practical-effect probabilities
########################################
summarize_practical_regions = function(draws_df,
                                          threshold = PRACTICAL_THRESHOLD) {
  
  draws_df %>%
    group_by(site) %>%
    summarise(
      p_decline_1pp = mean(diff <= -threshold) %>% round(3),
      p_small_decline = mean(diff > -threshold & diff < 0) %>% round(3),
      p_small_improve = mean(diff >= 0 & diff < threshold) %>% round(3),
      p_improve_1pp = 1 - p_decline_1pp - p_small_decline - p_small_improve,
      n_draws = n(),
      .groups = "drop"
    ) 
}

########################################
# Contrast summaries
########################################
compile_model_reporting = function(draws_df){
  outcome_hdi = draws_df %>% 
    group_by(site) %>% 
    median_hdi(diff, .width = ANALYSIS_INTERVAL)
  
  outcome_regions = draws_df %>%
    summarize_practical_regions %>% 
    select(-n_draws)
  
  report_df = outcome_hdi %>%
    select(site, diff, .lower, .upper) %>% 
    inner_join(outcome_regions, by = c('site'))
  
  return(report_df)
}

########################################
# Prior-to-posterior learning
########################################
summarize_prior_posterior_learning = function(draws,
                                              coef,
                                              prior_mean = CONTEXTUAL_PRIOR_MEAN,
                                              prior_sd = CONTEXTUAL_PRIOR_SD,
                                              width = ANALYSIS_INTERVAL) {
  
  
  post = draws[[coef]]
  
  hdi = tibble(value = post) %>%
    median_hdi(value, .width = width)
  
  output_df = tibble(
    coefficient = coef,
    prior_mean = prior_mean,
    prior_sd = prior_sd,
    posterior_median = hdi$value,
    posterior_lower = hdi$.lower,
    posterior_upper = hdi$.upper,
    posterior_mean = mean(post),
    posterior_sd = sd(post),
    sd_ratio = sd(post) / prior_sd,
    variance_ratio = var(post) / prior_sd^2,
    sd_reduction = 1 - sd(post) / prior_sd
  )
  
  return(output_df)
}

########################################
# Coefficient correlations
########################################
posterior_coef_correlation = function(draws, coef1, coef2, method = "pearson") {
  
  output_df = tibble(
    coefficient_1 = coef1,
    coefficient_2 = coef2,
    correlation = cor(
      draws[[coef1]],
      draws[[coef2]],
      method = method
    )
  )
  
  return(output_df)
}

########################################
# Contextual identification
########################################
summarize_contextual_identification = function(draws, contextual_coef,
                                               period_coef = PERIOD_COEFFICIENT,
                                               prior_mean = CONTEXTUAL_PRIOR_MEAN, prior_sd = CONTEXTUAL_PRIOR_SD) {
  learning = summarize_prior_posterior_learning(
    draws,
    coef = contextual_coef,
    prior_mean = prior_mean,
    prior_sd = prior_sd
  )
  
  correlation = posterior_coef_correlation(
    draws,
    coef1 = period_coef,
    coef2 = contextual_coef
  )
  
  output_df = learning %>%
    mutate(
      period_correlation = correlation$correlation
    ) %>%
    pivot_longer(cols = -coefficient, names_to = "metric", values_to = "value")
  
  return(output_df)
}
