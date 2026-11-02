library(brms)
library(dplyr)

########################################
# Presets
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "model_fit.R"))
dir.create(FIT_DIR, recursive = TRUE, showWarnings = FALSE)
model_df = read_model_data(file.path(REPO_ROOT, "data", "data.csv"))

########################################
# Model fit
########################################
FITTED_MODELS = list()
FITTED_MODELS$binom = fit_model_grid(
  model_formulas = MODEL_FORMULAS,
  prior_list = PRIOR_LIST_BINOM,
  data = model_df,
  family_obj = binomial(link = "logit"),
  likelihood_label = "binom",
  save_list = FALSE
)

FITTED_MODELS$bbinom = fit_model_grid(
  model_formulas = MODEL_FORMULAS_BASELINE,
  prior_list = PRIOR_LIST_BBINOM,
  data = model_df,
  family_obj = beta_binomial(link = "logit"),
  likelihood_label = "bbinom",
  control_lst = BBINOM_CONTROL,
  file_refit = "on_change",
  save_list = FALSE
)
