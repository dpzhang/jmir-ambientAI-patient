########################################
# Prior setup
########################################
PRIOR_LIST_BINOM = list(
  default = c(brms::prior(normal(0, 2.5), class = "Intercept"),
              brms::prior(normal(0, 1), class = "b")),
  strong = c(brms::prior(normal(0, 2.5), class = "Intercept"),
             brms::prior(normal(0, 0.75), class = "b")),
  weak = c(brms::prior(normal(0, 2.5), class = "Intercept"),
           brms::prior(normal(0, 1.5), class = "b"))
)

PRIOR_LIST_BBINOM = list(
  moderate = c(brms::prior(lognormal(log(100), 1), class = "phi"),
               PRIOR_LIST_BINOM$default),
  lower = c(brms::prior(lognormal(log(50), 1), class = "phi"),
          PRIOR_LIST_BINOM$default),
  higher = c(brms::prior(lognormal(log(200), 1), class = "phi"),
           PRIOR_LIST_BINOM$default)
)

BBINOM_CONTROL = list(
  adapt_delta = 0.99,
  max_treedepth = 15
)

########################################
# Model specification
########################################
MODEL_FORMULAS = list(
  #----------------------------------------
  # MODEL 0: Site-adjusted Baseline
  #----------------------------------------
  ltr_base = brms::bf(ltr_top | trials(ltr_total)  ~ site + period),
  exp_base = brms::bf(exp_top | trials(exp_total) ~ site + period),
  #----------------------------------------
  # MODEL 1: Patient Composition Sensitivity
  #----------------------------------------
  ltr_comp = brms::bf(ltr_top | trials(ltr_total)  ~ site + period + pct_old_10),
  exp_comp = brms::bf(exp_top | trials(exp_total) ~ site + period + pct_old_10),
  #----------------------------------------
  # MODEL 2: Adoption Intensity Exploratory
  #----------------------------------------
  ltr_adopt = brms::bf(ltr_top | trials(ltr_total) ~ site + period + pct_heavy_10),
  exp_adopt = brms::bf(exp_top | trials(exp_total) ~ site + period + pct_heavy_10)
)

MODEL_FORMULAS_BASELINE = MODEL_FORMULAS[c("ltr_base", "exp_base")]
########################################
# Repository paths
########################################
REPO_ROOT = dirname(this.path::this.dir())
OUTPUT_DIR = Sys.getenv("OUTPUT_DIR", file.path(REPO_ROOT, "output"))
MODEL_OUTPUT_DIR = file.path(OUTPUT_DIR, "model_outputs")
DATA_FILE = file.path(MODEL_OUTPUT_DIR, "model_output.rds")
TABLE_DIR = file.path(OUTPUT_DIR, "tables")
FIGURE_DIR = file.path(OUTPUT_DIR, "figures")
FIT_DIR = file.path(OUTPUT_DIR, "model_fits")
for (path in c(MODEL_OUTPUT_DIR, TABLE_DIR, FIGURE_DIR)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

########################################
# Diagnostic settings
########################################
RHAT_THRESHOLD = 1.01
MAX_TREEDEPTH = c(binom = 10L, bbinom = 15L)
OUTCOME_LABELS = c(ltr = "LTR", exp = "EXP")
MODEL_LABELS = c(m0 = "M0: Site-Adjusted Baseline",
                 m1 = "M1: Age-Composition Sensitivity",
                 m2 = "M2: Adoption-Intensity Exploratory")
PRIOR_LABELS = list(binom = c(weak = "Weaker", default = "Default", strong = "Stronger"),
                    bbinom = c(lower = "Lower", moderate = "Moderate", higher = "Higher"))
LIKELIHOOD_LABELS = c(binom = "Binomial", bbinom = "Beta-binomial")
CONTEXTUAL_LABELS = c(m1 = "M1: Age Composition", m2 = "M2: Adoption Intensity")
CONTEXTUAL_COEFFICIENTS = c(m1 = "b_pct_old_10", m2 = "b_pct_heavy_10")

########################################
# Figure settings
########################################
FIGURE_DPI = 600
TRACE_SIZE = c(width = 12, height = 6.5)
SUMMARY_SIZE = c(width = 6.5, height = 3.4)
PREDICTIVE_INTERVALS = c(0.66, 0.95)
BBINOM_PREDICTIVE_LIMITS = c(0.75, 1)
BINOM_PREDICTIVE_LIMITS = list(LTR = c(0.84, 0.92), EXP = c(0.84, 0.96))
BINOM_PREDICTIVE_BREAKS = list(LTR = c(0.84, 0.86, 0.88, 0.90, 0.92),
                               EXP = c(0.84, 0.87, 0.90, 0.93, 0.96))
TABLEAU10_SITE = c(Endeavor = "#4E79A7", UCM = "#E15759", Sutter = "#76B7B2")
SITE_DISPLAY_LABELS = setNames(names(TABLEAU10_SITE), names(TABLEAU10_SITE))
DISTRIBUTION_COLORS = c(Prior = "#4E79A7", Posterior = "#F28E2B")

binom_trace_pars = c(
  "b_Intercept",
  "b_periodpost",
  "b_siteUCM",
  "b_siteSutter",
  "b_pct_old_10",
  "b_pct_heavy_10"
)

binom_trace_parameter_labels = c(
  b_Intercept = "Intercept",
  b_periodpost = "Post period",
  b_siteUCM = "Site UCM",
  b_siteSutter = "Site Sutter",
  b_pct_old_10 = "Age",
  b_pct_heavy_10 = "Adoption"
)

trace_chain_colors = c(
  "1" = "#011f4b",
  "2" = "#005b96",
  "3" = "#6497b1",
  "4" = "#b3cde0"
)

bbinom_trace_parameter_labels = c(
  b_Intercept = "bold('Intercept')",
  b_periodpost = "bold('Post period')",
  b_siteUCM = "bold('Site UCM')",
  b_siteSutter = "bold('Site Sutter')",
  log_phi = "bold(log(phi))"
)

trace_pars_base = c(
  "b_Intercept",
  "b_periodpost",
  "b_siteUCM",
  "b_siteSutter",
  "log_phi"
)

########################################
# Beta-binomial figure labels
########################################
bbinom_prior_order = c("lower", "moderate", "higher")
bbinom_prior_labels = c(lower = "Lower Precision", moderate = "Moderate Precision", higher = "Higher Precision")

########################################
# Model analysis settings
########################################
ANALYSIS_INTERVAL = 0.95
PRACTICAL_THRESHOLD = 0.01
CONTEXTUAL_PRIOR_MEAN = 0
CONTEXTUAL_PRIOR_SD = 1
PERIOD_COEFFICIENT = "b_periodpost"

########################################
# Prior-sensitivity settings
########################################
PRIOR_SENSITIVITY_PRIOR_ORDER = c("weak", "default", "strong")
PRIOR_SENSITIVITY_MODEL_GROUPS = c(
  m0 = "Baseline",
  m1 = "Age Composition",
  m2 = "Adoption Scenario"
)
PRIOR_SENSITIVITY_TERM_LABELS = c(
  periodpost = "Period\n(Post)",
  pct_old_10 = "Age",
  pct_heavy_10 = "Adoption"
)
PRIOR_SENSITIVITY_INTERVALS = c(0.66, 0.95)
PRIOR_SENSITIVITY_SIZE = c(width = 12, height = 4)
PRIOR_SENSITIVITY_DPI = 300
TABLEAU10_PRIOR = c(
  Weaker = "#F28E2B",
  Default = "#4E79A7",
  Stronger = "#E15759"
)

########################################
# Likelihood-sensitivity settings
########################################
PHI_PRIOR_MEDIANS = c(lower = 50, moderate = 100, higher = 200)
PHI_PRIOR_SDLOG = 1
PHI_LEARNING_SEED = 42
PHI_LEARNING_INTERVAL = 0.95
PHI_LEARNING_PLOT_INTERVALS = c(0.66, 0.95)
PHI_LEARNING_SIZE = c(width = 6.5, height = 3.4)
PHI_LEARNING_DPI = 300

LIKELIHOOD_SPECIFICATION_LABELS = c(
  binom = "Binomial\nDefault",
  lower = "Beta-Binomial\nLower",
  moderate = "Beta-Binomial\nModerate",
  higher = "Beta-Binomial\nHigher"
)
LIKELIHOOD_SENSITIVITY_COLORS = c(
  "Binomial\nDefault" = "grey70",
  "Beta-Binomial\nLower" = "#F28E2B",
  "Beta-Binomial\nModerate" = "#4E79A7",
  "Beta-Binomial\nHigher" = "#E15759"
)
LIKELIHOOD_FACET_LABELS = c("(A)", "(B)")
LIKELIHOOD_SLAB_SCALE = 0.9
LIKELIHOOD_SENSITIVITY_SIZE = c(width = 6.5, height = 3.4)
LIKELIHOOD_SENSITIVITY_DPI = 600

########################################
# Paper-figure settings
########################################
PAPER_FIGURE_SITE_ORDER = c("Pooled", "Endeavor", "UCM", "Sutter")
PAPER_FIGURE_SITE_LABELS = c(
  Pooled = "Pooled",
  Endeavor = "Endeavor",
  UCM = "UCM",
  Sutter = "Sutter"
)
PAPER_FIGURE_SITE_COLORS = c(
  Pooled = "grey70",
  Endeavor = "#4E79A7",
  UCM = "#E15759",
  Sutter = "#76B7B2"
)
PAPER_FIGURE_FACET_ORDER = c("Overall", "Site-specific")
PAPER_FIGURE_FACET_LABELS = c("(A)", "(B)", "(C)", "(D)")
PAPER_FIGURE_INTERVAL = 0.95
PAPER_FIGURE_SLAB_SCALE = 0.9
PAPER_FIGURE_SIZE = c(width = 6.5, height = 3.4)
PAPER_FIGURE_DPI = 600
PAPER_FIGURE_FILENAMES = c(
  m0 = "paper_m0_contrast.png",
  m1 = "paper_m1_contrast.png",
  m2 = "paper_m2_contrast.png"
)
