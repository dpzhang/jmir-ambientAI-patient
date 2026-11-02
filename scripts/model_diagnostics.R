########################################
# Packages
########################################
library(this.path)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(ggh4x)
library(patchwork)
library(tidybayes)

########################################
# Setup
########################################
source(file.path(this.path::this.dir(), "..", "utils", "global.R"))
source(file.path(REPO_ROOT, "utils", "model_diagnostics.R"))
model_output = readRDS(DATA_FILE)
source(file.path(REPO_ROOT, "utils", "model_fit.R"))
observed_rates = build_observed_rates(
  read_model_data(file.path(REPO_ROOT, "data", "data.csv"))
)

########################################
# Model diagnostics table
########################################
diag_df = build_model_diagnostics_table(model_output)
write.csv(diag_df, file.path(TABLE_DIR, "model_diagnostics.csv"), row.names = FALSE)

########################################
# Binomial trace plots
########################################
binom_trace_df = imap_dfr(MODEL_LABELS, function(model_label, model_key) {
  imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
    draws = model_output$binom[[outcome_key]]$default[[model_key]]$draws
    pars = intersect(binom_trace_pars, posterior::variables(draws))
    as_tibble(draws) %>%
      select(.chain, .iteration, all_of(pars)) %>%
      pivot_longer(all_of(pars), names_to = "parameter", values_to = "value") %>%
      mutate(model = model_label, outcome = outcome_label)
  })
}) %>%
  mutate(model = factor(model, levels = unname(MODEL_LABELS)),
         outcome = factor(outcome, levels = unname(OUTCOME_LABELS)),
         parameter = factor(parameter, levels = binom_trace_pars,
                            labels = binom_trace_parameter_labels[binom_trace_pars]),
         .chain = factor(.chain))
p_trace_binom = wrap_plots(lapply(MODEL_LABELS, function(label) {
  plot_binom_trace_block(binom_trace_df, label)
}), widths = c(4, 5, 5), guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom", plot.background = element_rect(fill = "white", color = NA))
save_diagnostic_figure(p_trace_binom, "trace_binom.png", TRACE_SIZE)

########################################
# Beta-binomial trace plots
########################################
bbinom_trace_df = imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
  map_dfr(bbinom_prior_order, function(prior_key) {
    draws = model_output$bbinom[[outcome_key]][[prior_key]]$m0$draws
    as_tibble(draws) %>%
      mutate(log_phi = log(phi)) %>%
      select(.chain, .iteration, all_of(trace_pars_base)) %>%
      pivot_longer(all_of(trace_pars_base), names_to = "parameter", values_to = "value") %>%
      mutate(prior = bbinom_prior_labels[[prior_key]], Outcome = outcome_label)
  })
}) %>%
  mutate(prior = factor(prior, levels = bbinom_prior_labels[bbinom_prior_order]),
         Outcome = factor(Outcome, levels = unname(OUTCOME_LABELS)),
         parameter = factor(parameter, levels = trace_pars_base,
                            labels = bbinom_trace_parameter_labels[trace_pars_base]),
         .chain = factor(.chain))
p_trace_bbinom = plot_bbinom_trace(bbinom_trace_df)
save_diagnostic_figure(p_trace_bbinom, "trace_bbinom.png", TRACE_SIZE)

########################################
# Binomial predictive distributions
########################################
binom_predictive_df = imap_dfr(MODEL_LABELS, function(model_label, model_key) {
  imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
    model_output$binom[[outcome_key]]$default[[model_key]]$posterior_predictive %>%
      mutate(model = model_label, Outcome = outcome_label)
  })
}) %>%
  left_join(observed_rates, by = c("site", "period", "Outcome"), relationship = "many-to-one") %>%
  mutate(model = factor(model, levels = unname(MODEL_LABELS)),
         Outcome = factor(Outcome, levels = unname(OUTCOME_LABELS)),
         site = factor(site, levels = names(TABLEAU10_SITE)),
         period = factor(period, levels = c("pre", "post"), labels = c("Pre", "Post")))
p_predictive_binom = combine_predictive_figure(plot_binom_predictive(binom_predictive_df))
save_diagnostic_figure(p_predictive_binom, "ppc_binom.png")

########################################
# Beta-binomial predictive distributions
########################################
bbinom_predictive_df = imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
  map_dfr(bbinom_prior_order, function(prior_key) {
    model_output$bbinom[[outcome_key]][[prior_key]]$m0$posterior_predictive %>%
      mutate(prior = PRIOR_LABELS$bbinom[[prior_key]], Outcome = outcome_label)
  })
}) %>%
  left_join(observed_rates, by = c("site", "period", "Outcome"), relationship = "many-to-one") %>%
  mutate(prior = factor(prior, levels = PRIOR_LABELS$bbinom[bbinom_prior_order]),
         Outcome = factor(Outcome, levels = unname(OUTCOME_LABELS)),
         site = factor(site, levels = names(TABLEAU10_SITE)),
         period = factor(period, levels = c("pre", "post"),
                         labels = c("Pre-Implementation", "Post-Implementation")))
p_predictive_bbinom = combine_predictive_figure(
  plot_bbinom_predictive(bbinom_predictive_df), legend_height = 0.08
)
save_diagnostic_figure(p_predictive_bbinom, "ppc_bbinom.png")

########################################
# Contextual prior-to-posterior learning
########################################
contextual_learning_df = imap_dfr(CONTEXTUAL_LABELS, function(label, model_key) {
  imap_dfr(OUTCOME_LABELS, function(outcome_label, outcome_key) {
    generate_contextual_learning_df(
      model_output$binom[[outcome_key]]$default[[model_key]],
      coefficient = CONTEXTUAL_COEFFICIENTS[[model_key]],
      contextual_label = label, outcome = outcome_label)
  })
})
p_contextual_learning = plot_contextual_prior_posterior(contextual_learning_df)
save_diagnostic_figure(p_contextual_learning, "contextual_coef_learning.png")
