# Supplemental Material for "Ambient AI Scribe Implementation and Patient Experience Across Three Health Systems: A Descriptive Multi-site Observational Study"

Authors: Dongping Zhang, Mukund Nakhate, Cheryl D. Stults, Heather L. Himelhoch, Urmila Ravichandran, Prsni Patel, Anthony Solomonides, Sien Deng, Joseph Wilcox, Naveed Rabbani, Sachin D. Shah, Veena G. Jones, Nirav S. Shah

## About this repository

This repository contains the R analysis code and the released six-row aggregate dataset for the manuscript. The workflow fits 24 Bayesian models: 18 binomial models (2 outcomes × 3 specifications × 3 coefficient-prior settings) and 6 beta-binomial models (2 outcomes × 1 specification × 3 precision-prior settings).

All fitted models and analysis outputs are generated locally. _The `output/` directory is created automatically and is excluded from version control_.

The structure and contents of the repository are detailed below.

## Contents

| Directory               | Description                                                         |
| ----------------------- | ------------------------------------------------------------------- |
| `data/`                 | Released six-row aggregate dataset (`data.csv`).                    |
| `scripts/`              | Entry points for fitting, export generation, analyses, and figures. |
| `utils/`                | Shared settings and helper functions.                               |
| `output/model_fits/`    | Store individual fitted `brms` objects.                             |
| `output/model_outputs/` | Store `model_output.rds` and 72 CSV exports.                        |
| `output/tables/`        | Store seven generated analysis tables.                              |
| `output/figures/`       | Store eleven generated figures.                                     |

Each row of the aggregated data represents one site (`Endeavor`, `UCM`, or `Sutter`) and period (`pre` or `post`). `ltr_top` / `ltr_total` and `exp_top` / `exp_total` are outcome numerator / denominator counts. `pct_old_10` and `pct_heavy_10` are age-composition and adoption covariates, scaled in 10-percentage-point units. No patient-level records are included.

## Reproducing the analysis

Run these commands from the repository root, in order:

```sh
Rscript scripts/model_fit.R
Rscript scripts/generate_analysis_files.R
Rscript scripts/model_diagnostics.R
Rscript scripts/model_analysis.R
Rscript scripts/prior_sensitivity.R
Rscript scripts/likelihood_sensitivity.R
Rscript scripts/paper_figs.R
```

1. `model_fit.R` validates `data/data.csv` and saves all 24 fitted model objects in `output/model_fits/`. It does not generate analysis exports, tables, or figures.
2. `generate_analysis_files.R` loads fitted model objects and exports a nested RDS and four types of CSV files to `output/model_outputs/`.
3. The remaining five scripts independently read `model_output.rds` and generate corresponding tables and figures.

### Software and fitting settings

Model fitting requires the listed R packages and a working C++ toolchain for RStan. The workflow was validated with R 4.6.0 and the following package versions:

| Package     | Version |
| ----------- | ------: |
| `this.path` |   2.8.0 |
| `brms`      |  2.23.0 |
| `posterior` |   1.7.0 |
| `dplyr`     |   1.2.1 |
| `tidyr`     |   1.3.2 |
| `purrr`     |   1.2.2 |
| `ggplot2`   |   4.0.3 |
| `ggdist`    |   3.3.3 |
| `ggthemes`  |   5.2.0 |
| `ggh4x`     |   0.3.1 |
| `patchwork` |   1.3.2 |
| `tidybayes` |   3.0.7 |

## Generated model outputs

The RDS preserves R types and numerical precision and is the direct input to the analysis scripts. CSV exports provide portable, inspectable copies. Both are generated from the full fitted model objects.

```r
model_output = readRDS("output/model_outputs/model_output.rds")
# model_output[[likelihood]][[outcome]][[prior]][[model]][[data_type]]
model_output$binom$ltr$default$m0$draws
model_output$binom$exp$default$m1$expected_contrasts
model_output$bbinom$exp$lower$m0$posterior_predictive
```

The likelihood keys are `binom` and `bbinom`; outcomes are `ltr` and `exp`. Binomial priors are `weak`, `default`, and `strong`; beta-binomial priors are `lower`, `moderate`, and `higher`. Model keys are `m0`, `m1`, and `m2`, with only `m0` used for beta-binomial models.

| Data type              | CSV files | Contents                                                                                         |
| ---------------------- | --------: | ------------------------------------------------------------------------------------------------ |
| `draws`                |        24 | Parameter and prior draws, including chain and iteration identifiers.                            |
| `sampler_diagnostics`  |        24 | Divergence and tree-depth records for post-warmup iterations.                                    |
| `posterior_predictive` |        12 | Simulated outcome rates for predictive-distribution figures.                                     |
| `expected_contrasts`   |        12 | Expected pre/post probabilities and differences for primary and likelihood-sensitivity analyses. |
