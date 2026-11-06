# Run the full chronofix analysis on the Imperial DIDE cluster using hipercow.
#
# This is how the results in the paper were produced. It needs access to the
# cluster (only accessible to Imperial DIDE staff/students). For everyone 
# else, please use workflow_local.R to reproduce results instead.
#
# Run from the root of the repository on a network share the cluster can see.
# Run each task in turn: each section submits jobs, and the next
# section needs those jobs to have finished first.

library(orderly)
library(hipercow)
source("workflow_setup.R")

# Setup ------------------------------------------------------------------------

# First time only, in this directory:
# hipercow_init(driver = "dide-windows")

# Install the packages in pkgdepends.txt on the cluster
hipercow_provision(method = "pkgdepends")

# Simulation parameters (very quick - run locally) -----------------------------
orderly_run("sim_params")

## main simulation scenarios
scenarios <- c("baseline",
               "low_missingness",
               "no_missing",
               "no_error",
               "no_error_no_missing",
               "low_error",
               "high_error",
               "very_small_sample",
               "small_sample",
               "moderate_sample",
               "very_large_sample",
               "long_delays",
               "short_delays",
               "high_variability",
               "low_variability",
               "lognormal_delays",
               "same_means",
               "dependent_delays")

# Simulate data for all scenarios ----------------------------------------------

sim_data <- task_create_bulk_expr(
  orderly::orderly_run("sim_data",
                       parameters = list(scenario = scenario, nsims = nsims)),
  data.frame(scenario = data_scenarios),
  resources = hipercow_resources(cores = 1))

hipercow_bundle_wait(sim_data)
hipercow_bundle_result(sim_data)

# Fit the model ----------------------------------------------------------------
# One task per scenario and dataset (21 scenarios x 100 datasets). Each task
# runs 4 MCMC chains in parallel. To re-run a subset, replace fit_scenarios
# with the scenarios you want.

fits <- expand.grid(scenario = fit_scenarios, dataset = seq_len(nsims),
                    stringsAsFactors = FALSE)

sim_estim <- task_create_bulk_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = scenario,
                                         dataset = dataset)),
  fits,
  resources = hipercow_resources(cores = 4))

hipercow_bundle_wait(sim_estim)
hipercow_bundle_result(sim_estim)

# Collate fits for each scenario -----------------------------------------------

sim_collate <- task_create_bulk_expr(
  orderly::orderly_run("sim_collate", parameters = list(scenario = scenario)),
  data.frame(scenario = fit_scenarios),
  resources = hipercow_resources(cores = 4))

hipercow_bundle_wait(sim_collate)
hipercow_bundle_result(sim_collate)

# Visualisations and results quoted in text ------------------------------------

main_scenarios <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "main_scenarios")),
  resources = hipercow_resources(cores = 1)
)
task_result(main_scenarios)

misspecification_scenarios <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "misspecification_scenarios")),
  resources = hipercow_resources(cores = 1)
)
task_result(misspecification_scenarios)
