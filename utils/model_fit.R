########################################
# Main model fit function
########################################
fit_model = function(formula_obj, # model specification
                     data, # model data
                     prior_obj, # prior choice
                     prior_label, # prior type: weak, strong, default
                     model_name,
                     family_obj = binomial(link = "logit"),
                     likelihood_label = "binom",
                     chains = 4,
                     iter = 4000,
                     warmup = 2000,
                     cores = 4,
                     seed = 42,
                     control_lst = NULL,
                     file_refit = "on_change") {
  
  brm(
    formula = formula_obj,
    data = data,
    family = family_obj,
    prior = prior_obj,
    chains = chains,
    iter = iter,
    warmup = warmup,
    sample_prior = "yes",
    cores = cores,
    seed = seed,
    control = control_lst,
    file_refit = file_refit,
    file = file.path("fitted_models", likelihood_label, prior_label, model_name)
  )
}

########################################
# Nested loop function for model fitting
########################################
fit_model_grid = function(model_formulas,
                          prior_list,
                          data,
                          family_obj = binomial(link = "logit"),
                          likelihood_label = "binom",
                          control_lst = NULL,
                          file_refit = "on_change",
                          save_list = TRUE,
                          save_path = "fitted_models/fitted_models.rds") {
  
  fitted_models = list()
  
  for (prior_label in names(prior_list)) {
    cat("========================================================\n")
    cat("Fitting prior setting:", prior_label, "\n")
    cat("========================================================\n")
    
    fitted_models[[prior_label]] = list()
    
    for (model_name in names(model_formulas)) {
      cat("========================================================\n")
      cat("Fitting model:", model_name, "\n")
      cat("========================================================\n")
      fitted_models[[prior_label]][[model_name]] = fit_model(
        formula_obj = model_formulas[[model_name]],
        data = data,
        prior_obj = prior_list[[prior_label]],
        prior_label = prior_label,
        model_name = model_name,
        family_obj = family_obj,
        likelihood_label = likelihood_label,
        control_lst = control_lst,
        file_refit = file_refit
      )
    }
  }
  
  if (save_list) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fitted_models, save_path)
  }
  
  return(fitted_models)
}