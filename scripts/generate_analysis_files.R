########################################
# Setup
########################################
library(brms)
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "model_fit.R"))
source(file.path(REPO_ROOT, "utils", "generate_analysis_files.R"))

########################################
# Fitted model objects -> analysis inputs
########################################
model_df = read_model_data(file.path(REPO_ROOT, "data", "data.csv"))
fitted_models = read_fitted_models()
export_model_outputs(fitted_models, model_df)
