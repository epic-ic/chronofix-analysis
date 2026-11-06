# Shared setup for workflow_local.R and workflow_cluster.R: scenario lists and
# helpers.
#
# The simulation parameters for each scenario are defined in
# src/sim_params/sim_params.R, and the scenarios shown in each figure are
# listed in src/sim_comparison/support.R. If you add a scenario, update those
# files and this one.

# Number of datasets simulated per scenario. sim_collate expects 100.
nsims <- 100

# Scenarios for which data are simulated (sim_data)
data_scenarios <- c(
  "baseline",
  # missing dates and errors
  "low_missingness",
  "no_missing",
  "no_error",
  "no_error_no_missing",
  "low_error",
  "high_error",
  # sample size
  "very_small_sample",
  "small_sample",
  "moderate_sample",
  "very_large_sample",
  # delays
  "long_delays",
  "short_delays",
  "high_variability",
  "low_variability",
  "lognormal_delays",
  "same_means",
  # only used as data for misspecify_independent
  "dependent_delays"
)

# Scenarios in which the fitted model differs from the data-generating model.
# The value is the simulated data each scenario is fitted to, e.g. for
# misspecify_gamma, delays are simulated as log-normal but fitted as gamma.
misspecification_data <- c(
  misspecify_gamma = "lognormal_delays",
  misspecify_lognormal = "baseline",
  misspecify_independent = "dependent_delays"
)

# Scenarios that are fitted (sim_estim) and collated (sim_collate)
fit_scenarios <- c(setdiff(data_scenarios, "dependent_delays"),
                   names(misspecification_data))

# Comparisons drawn by sim_comparison
comparisons <- c("main_scenarios", "misspecification_scenarios")

# The simulated data that a scenario needs
data_scenario_for <- function(scenario) {
  ifelse(scenario %in% names(misspecification_data),
         misspecification_data[scenario],
         scenario)
}

# The scenarios that a sim_comparison run needs collated results for
comparison_needs <- function(comparison) {
  env <- new.env()
  sys.source(file.path("src", "sim_comparison", "support.R"), envir = env)
  s <- env$comparison_scenarios[[comparison]]
  s[!grepl("^gap", s)]
}
