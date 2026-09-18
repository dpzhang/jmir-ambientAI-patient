########################################
# Packages
########################################
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggdist)
library(ggh4x)
library(ggthemes)
library(patchwork)
library(tidybayes)

########################################
# Setup
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "prior_sensitivity.R"))
model_output = readRDS(DATA_FILE)

########################################
# Prior-sensitivity analysis
########################################
prior_sensitivity_df = generate_prior_sensitivity_df(model_output)
prior_sensitivity = plot_prior_sensitivity(prior_sensitivity_df)

ggsave(
  file.path(FIGURE_DIR, "prior_sensitivity.png"),
  prior_sensitivity,
  width = PRIOR_SENSITIVITY_SIZE[["width"]],
  height = PRIOR_SENSITIVITY_SIZE[["height"]],
  units = "in",
  dpi = PRIOR_SENSITIVITY_DPI,
  bg = "transparent"
)
