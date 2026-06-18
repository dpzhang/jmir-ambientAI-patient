########################################
# Main model fit function
########################################
fit_model = function(formula_obj, # model specification
                     data, # model data
                     prior_obj, # prior choice
                     prior_label, # prior type: weak, strong, default
                     model_name,
                     chains = 4,
                     iter = 4000,
                     warmup = 2000,
                     cores = 4,
                     seed = 42) {
  
  dir.create(file.path("fitted_models", prior_label),
             recursive = TRUE, showWarnings = FALSE)
  
  brm(
    formula = formula_obj,
    data = data,
    family = binomial(link = "logit"),
    prior = prior_obj,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    #control = control_lst,
    file_refit = "on_change",
    file = file.path("fitted_models", prior_label, model_name)
  )
}

########################################
# Nested loop function for model fitting
########################################
fit_model_grid = function(model_formulas,
                          prior_list,
                          data,
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
        model_name = model_name
      )
    }
  }
  
  if (save_list) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fitted_models, save_path)
  }
  
  return(fitted_models)
}
