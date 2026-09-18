########################################
# Packages
########################################
library(dplyr)
library(purrr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(tidybayes)

########################################
# Setup
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "model_analysis.R"))
source(file.path(REPO_ROOT, "utils", "likelihood_sensitivity.R"))
model_output = readRDS(DATA_FILE)

########################################
# Model reports
########################################
likelihood_reports = build_likelihood_reports(
  model_output,
  compile_model_reporting = compile_model_reporting
)

bbinom_model_report = likelihood_reports$bbinom
write.csv(
  bbinom_model_report,
  file.path(TABLE_DIR, "bbinom_model_report.csv"),
  row.names = FALSE
)

########################################
# Likelihood comparison
########################################
likelihood_sensitivity = compare_likelihood_reports(
  likelihood_reports$bbinom,
  likelihood_reports$binom
)
write.csv(
  likelihood_sensitivity,
  file.path(TABLE_DIR, "likelihood_sensitivity.csv"),
  row.names = FALSE
)

########################################
# Phi prior-to-posterior learning
########################################
phi_learning = build_phi_learning(model_output)
phi_learning_df = phi_learning$draws
phi_learning_summary = phi_learning$summary

write.csv(
  phi_learning_df,
  file.path(TABLE_DIR, "phi_learning_draws.csv"),
  row.names = FALSE
)
write.csv(
  phi_learning_summary,
  file.path(TABLE_DIR, "phi_learning_summary.csv"),
  row.names = FALSE
)

phi_learning_plot = plot_phi_learning(phi_learning_df)
ggsave(
  file.path(FIGURE_DIR, "phi_learning.png"),
  phi_learning_plot,
  width = PHI_LEARNING_SIZE[["width"]],
  height = PHI_LEARNING_SIZE[["height"]],
  units = "in",
  dpi = PHI_LEARNING_DPI,
  bg = "white"
)

########################################
# Pooled likelihood-sensitivity contrast
########################################
pooled_likelihood_contrasts = build_pooled_likelihood_contrasts(model_output)
pooled_likelihood_plot = plot_likelihood_sensitivity_pooled(
  pooled_likelihood_contrasts
)
ggsave(
  file.path(FIGURE_DIR, "m0_likelihood_sensitivity_pooled.png"),
  pooled_likelihood_plot,
  width = LIKELIHOOD_SENSITIVITY_SIZE[["width"]],
  height = LIKELIHOOD_SENSITIVITY_SIZE[["height"]],
  units = "in",
  dpi = LIKELIHOOD_SENSITIVITY_DPI,
  bg = "white"
)
