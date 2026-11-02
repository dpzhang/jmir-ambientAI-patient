# Read the individual model caches saved by scripts/model_fit.R.
read_fitted_models = function() {
  grids = list(
    binom = list(priors = names(PRIOR_LIST_BINOM), models = names(MODEL_FORMULAS)),
    bbinom = list(priors = names(PRIOR_LIST_BBINOM), models = names(MODEL_FORMULAS_BASELINE))
  )
  paths = unlist(lapply(names(grids), function(likelihood) {
    grid = grids[[likelihood]]
    unlist(lapply(grid$priors, function(prior) {
      file.path(FIT_DIR, likelihood, prior, paste0(grid$models, ".rds"))
    }))
  }))
  missing = paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing ", length(missing), " fitted model file(s). Run scripts/model_fit.R first.\n",
         "First missing file: ", missing[[1]], call. = FALSE)
  }
  fits = list()
  for (likelihood in names(grids)) {
    for (prior in grids[[likelihood]]$priors) {
      for (model in grids[[likelihood]]$models) {
        path = file.path(FIT_DIR, likelihood, prior, paste0(model, ".rds"))
        fit = readRDS(path)
        if (!inherits(fit, "brmsfit")) stop("Not a brms fit: ", path, call. = FALSE)
        fits[[likelihood]][[prior]][[model]] = fit
      }
    }
  }
  fits
}

# Export the same nested interface consumed by all downstream scripts.
# Contrasts compare the observed site-period covariates; Pooled weights sites equally.
export_model_outputs = function(fitted_models, data, seed = 42) {
  output = list()
  model_keys = c(base = "m0", comp = "m1", adopt = "m2")
  for (likelihood in names(fitted_models)) {
    for (prior in names(fitted_models[[likelihood]])) {
      for (name in names(fitted_models[[likelihood]][[prior]])) {
        parts = strsplit(name, "_", fixed = TRUE)[[1]]
        outcome = parts[1]
        model = unname(model_keys[parts[2]])
        fit = fitted_models[[likelihood]][[prior]][[name]]
        draws = posterior::as_draws_df(fit)
        keep = grepl("^(b_|prior_|Intercept$|phi$)", names(draws)) |
          names(draws) %in% c(".chain", ".iteration", ".draw")
        item = list(draws = posterior::as_draws_df(as.data.frame(draws)[, keep]),
                    sampler_diagnostics = subset(brms::nuts_params(fit),
                      Parameter %in% c("treedepth__", "divergent__")))
        if (likelihood == "bbinom" || prior == "default") {
          rates = brms::posterior_epred(fit, newdata = data)
          rates = sweep(rates, 2, data[[paste0(outcome, "_total")]], "/")
          by_site = lapply(levels(data$site), function(site) {
            pre = rates[, which(data$site == site & data$period == "pre")]
            post = rates[, which(data$site == site & data$period == "post")]
            data.frame(site = site, .draw = seq_along(pre), pre = pre, post = post,
                       diff = post - pre, facet = "Site-specific")
          })
          pooled = by_site[[1]]
          pooled$site = "Pooled"
          pooled$facet = "Overall"
          for (column in c("pre", "post", "diff")) {
            pooled[[column]] = Reduce(`+`, lapply(by_site, `[[`, column)) / length(by_site)
          }
          item$expected_contrasts = dplyr::bind_rows(c(list(pooled), by_site))
          item$expected_contrasts$site = factor(item$expected_contrasts$site,
                                               levels = c("Pooled", levels(data$site)))
          set.seed(seed)
          ids = sort(sample.int(nrow(rates), min(1000L, nrow(rates))))
          predicted = brms::posterior_predict(fit, newdata = data, draw_ids = ids)
          item$posterior_predictive = dplyr::bind_rows(lapply(seq_len(nrow(data)), function(i) {
            data.frame(site = data$site[i], period = data$period[i], .draw = ids,
                       pred_rate = predicted[, i] / data[[paste0(outcome, "_total")]][i])
          }))
        }
        output[[likelihood]][[outcome]][[prior]][[model]] = item
        for (type in names(item)) {
          dir.create(file.path(MODEL_OUTPUT_DIR, type), recursive = TRUE, showWarnings = FALSE)
          write.csv(as.data.frame(item[[type]]), file.path(MODEL_OUTPUT_DIR, type,
                    paste0(paste(model, outcome, likelihood, prior, sep = "_"), ".csv")),
                    row.names = FALSE)
        }
      }
    }
  }
  saveRDS(output, DATA_FILE)
  invisible(output)
}
