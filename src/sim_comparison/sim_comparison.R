library(orderly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(glue)
library(stringr)
library(ggrastr)
library(forcats)

pars <- orderly_parameters(comparison = "sanity")

orderly_resource("support.R")
orderly_resource("plot.R")
source("support.R")
source("plot.R")

scenarios <- comparison_scenarios[[pars$comparison]]

# Filter out gaps used for legend when plotting all scenarios
actual_scenarios <- scenarios[!grepl("^gap", scenarios)]

# Loop through scenarios and fetch the individual summaries
for (s in actual_scenarios) {
  remote_files <- c(
    "outputs/pars_summary.rds",
    "outputs/errors_summary.rds"
  )
  
  local_names <- 
    glue("inputs/{str_remove(basename(remote_files), '\\\\.rds')}_{s}.rds")
  deps_mapping <- setNames(remote_files, local_names)
  
  orderly_dependency(
    "sim_collate", 
    "latest(parameter:scenario == environment:s)",
    deps_mapping
  )
}

orderly_artefact(files = c("figures/coverage_and_bias_mean.pdf",
                          "figures/coverage_and_bias_cv.pdf",
                          "figures/coverage_and_bias_q95.pdf",
                          "figures/sensitivity_50.pdf",
                          "figures/sensitivity_95.pdf",
                          "figures/specificity_50.pdf",
                          "figures/specificity_95.pdf",
                          "figures/specificity_50_and_95.pdf"),
                 description = "Publication figures")

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# Bind all the individual scenarios together -----------------------------------
pars_summary <- combine_df(scenarios, "pars_summary")
errors_summary <- combine_df(scenarios, "errors_summary")

# Plots ----------------------------------------------------------------------

if (pars$comparison == "misspecification_scenarios") {
  w_perf <- 10
  h_perf <- 7.5
  w_sens <- 10
  h_sens <- 9
  h_sens_both <- 16
} else {
  # Defaults for "all_scenarios"
  w_perf <- 13
  h_perf <- 7.5
  w_sens <- 13
  h_sens <- 9
  h_sens_both <- 16
}

# Coverage and relative bias plot for Mean
ggsave("figures/coverage_and_bias_mean.pdf",
       plot_performance_figure(pars_summary, target_role = "Mean"), 
       width = w_perf, height = h_perf)

# Coverage and relative bias plot for CV
ggsave("figures/coverage_and_bias_cv.pdf",
       plot_performance_figure(pars_summary, target_role = "CV"), 
       width = w_perf, height = h_perf)

# Coverage and relative bias plot for 95th quantile
ggsave("figures/coverage_and_bias_q95.pdf",
       plot_performance_figure(pars_summary, target_role = "95th Quantile"), 
       width = w_perf, height = h_perf)

# Sensitivity - 50% threshold
ggsave("figures/sensitivity_50.pdf",
       plot_sensitivity_figure(errors_summary, target_threshold = 0.5),
       width = w_sens, height = h_sens)

# Sensitivity - 95% threshold
ggsave("figures/sensitivity_95.pdf",
       plot_sensitivity_figure(errors_summary, target_threshold = 0.95),
       width = w_sens, height = h_sens)

# Specificity - 50% threshold
ggsave("figures/specificity_50.pdf",
       plot_specificity_figure(errors_summary, target_threshold = 0.5),
       width = w_sens, height = h_sens)

# Specificity - 95% threshold
ggsave("figures/specificity_95.pdf",
       plot_specificity_figure(errors_summary, target_threshold = 0.95),
       width = w_sens, height = h_sens)

# Specificity - both thresholds in the same figure
ggsave("figures/specificity_50_and_95.pdf",
       plot_all_specificity(errors_summary),
       width = w_sens, height = h_sens_both)
