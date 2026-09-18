########################################
# Packages
########################################
library(dplyr)
library(tidyr)
library(purrr)
library(tidybayes)

########################################
# Setup
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "model_analysis.R"))
model_output = readRDS(DATA_FILE)

########################################
# Model 0–2: Pre/post contrast summaries
########################################
model_reporting = map_dfr(names(MODEL_LABELS), function(model_key) {
  map_dfr(names(OUTCOME_LABELS), function(outcome_key) {
    contrasts = model_output$binom[[outcome_key]]$default[[model_key]]$expected_contrasts
    compile_model_reporting(contrasts) %>%
      mutate(model = as.integer(sub("m", "", model_key)),
             outcome = outcome_key, likelihood = "binom", prior = "default", .before = 1)
  })
})

write.csv(model_reporting, file.path(TABLE_DIR, "binom_default_model_report.csv"), row.names = FALSE)

########################################
# Model 1–2: Contextual identification
########################################
contextual_identification = map_dfr(names(CONTEXTUAL_COEFFICIENTS), function(model_key) {
  map_dfr(names(OUTCOME_LABELS), function(outcome_key) {
    draws = model_output$binom[[outcome_key]]$default[[model_key]]$draws
    summarize_contextual_identification(
      draws, contextual_coef = CONTEXTUAL_COEFFICIENTS[[model_key]]) %>%
      mutate(model = as.integer(sub("m", "", model_key)),
             outcome = outcome_key, likelihood = "binom", prior = "default", .before = 1)
  })
})

write.csv(contextual_identification, file.path(TABLE_DIR, "contextual_identification.csv"), row.names = FALSE)
