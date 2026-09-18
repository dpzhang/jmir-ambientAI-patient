########################################
# Packages
########################################
library(dplyr)
library(purrr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(ggh4x)
library(tidybayes)

########################################
# Setup
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "paper_figs.R"))
model_output = readRDS(DATA_FILE)

########################################
# Model-implied contrast figures
########################################
paper_figures = imap(PAPER_FIGURE_FILENAMES, function(filename, model_key) {
  contrast_draws = prepare_model_implied_contrasts(model_output, model_key)
  figure = plot_model_implied_contrast(contrast_draws)

  ggsave(
    file.path(FIGURE_DIR, filename),
    figure,
    width = PAPER_FIGURE_SIZE[["width"]],
    height = PAPER_FIGURE_SIZE[["height"]],
    units = "in",
    dpi = PAPER_FIGURE_DPI,
    bg = "white"
  )

  figure
})
