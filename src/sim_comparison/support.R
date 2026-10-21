comparison_scenarios <- list(
  all_scenarios = c(
    "baseline", 
    "gap1", "low_missingness", "no_missing", "no_error", "no_error_no_missing",
    "gap2", "low_error", "high_error",
    "gap3", "very_small_sample", "small_sample", "moderate_sample", "very_large_sample",
    "gap4", "long_delays", "short_delays",
    "gap5", "high_variability", "low_variability",
    "gap6", "lognormal_delays", "same_means"
  ),
  misspecification_scenarios = c(
    "baseline", "misspecify_lognormal",
    "gap1",
    "lognormal_delays", "misspecify_gamma",
    "gap7",
    "misspecify_independent"
  )
)


scenario_labels <- c(
  "baseline" = "Baseline",
  "misspecify_lognormal" = "Misspecify lognormal",
  
  # workaround to add some whitespace between scenario groups in the legend
  "gap1" = " ",
  
  "no_missing" = "Errors only (0.05)",
  "no_error" = "Missing dates only (0.2)",
  "no_error_no_missing" = "No errors or missing dates",
  "low_missingness" = "Low missingness (0.05)",
  
  "gap2" = "  ",
  
  "low_error" = "Low error (0.02)",
  "high_error" = "High error (0.2)",
  
  "gap3" = "   ",
  
  "very_small_sample" = "Very small groups (n = 10)",
  "small_sample" = "Small groups (n = 20)",
  "moderate_sample" = "Moderate groups (n = 50)",
  "very_large_sample" = "Very large groups (n = 250)",
  
  "gap4" = "    ",
  
  "short_delays" = "Short delays (0.5x baseline)",
  "long_delays" = "Long delays (2x baseline)",
  
  "gap5" = "     ",
  
  "low_variability" = "Low variability (0.5x baseline cv)",
  "high_variability" = "High variability (2x baseline cv)",
  
  "gap6" = "      ",
  
  "lognormal_delays" = "Lognormal delays",
  "misspecify_gamma" = "Misspecify gamma",
  "same_means" = "Same mean delay (7 days)",
  
  "gap7" = "      ",
  
  "misspecify_independent" = "Misspecify independent"
)

scenario_colours <- c(
  "Baseline" = "#000000",
  "Misspecify lognormal" = "#00A0B0",
  
  # blank for the gaps in legend
  " " = "transparent",
  "  " = "transparent",
  "   " = "transparent",
  "    " = "transparent",
  "     " = "transparent",
  "      " = "transparent",
  
  "Errors only (0.05)" = "#6BAED6",
  "Missing dates only (0.2)" = "#3182BD",
  "No errors or missing dates" = "#08519C",
  "Low missingness (0.05)" = "#08306B",
  
  "Low error (0.02)" = "#E0A21F",
  "High error (0.2)" = "#7F4F0A",
  
  "Very small groups (n = 10)" = "#FB7B5B",
  "Small groups (n = 20)" = "#EF3B2C",
  "Moderate groups (n = 50)" = "#CB181D",
  "Very large groups (n = 250)" = "#67000D",
  
  "Short delays (0.5x baseline)" = "#74C476",
  "Long delays (2x baseline)" = "#00441B",
  
  "Low variability (0.5x baseline cv)" = "#9E9AC8",
  "High variability (2x baseline cv)" = "#3F007D",
  
  "Lognormal delays" = "#F768A1",
  "Misspecify gamma" = "#F2A900",
  "Same mean delay (7 days)" = "#7A0177",
  
  ## Temporary colour
  "Misspecify independent" = "#74C476"
)

global_delay_levels <- c("onset to report", "onset to hospitalisation",
                         "hospitalisation to discharge", "hospitalisation to death",
                         "onset to death")
global_delay_labels <- tools::toTitleCase(global_delay_levels)

global_group_levels <- c("community-alive", "community-dead",
                         "hospitalised-alive", "hospitalised-dead")
global_group_labels <- tools::toTitleCase(gsub("-", " ", global_group_levels))

global_event_levels <- c("onset", "report", "hospitalisation", "discharge", "death")
global_event_labels <- tools::toTitleCase(global_event_levels)


combine_df <- function(scenarios, type) {
  scenario_labels <- 
    scenario_labels[intersect(names(scenario_labels), scenarios)]
  scenario_labels <- factor(scenario_labels, levels = scenario_labels)
  actual_scenarios <- scenarios[!grepl("^gap", scenarios)]
  names(actual_scenarios) <- actual_scenarios
  df <- purrr::map_dfr(actual_scenarios, ~readRDS(glue("inputs/{type}_{.x}.rds")), 
                       .id = "scenario")
  apply_factor_levels(df, scenario_labels)
}


# Re-apply correct factor orderings after the bind_rows
apply_factor_levels <- function(df, scenario_labels) {
  if("scenario" %in% names(df)) {
    df$scenario <- factor(df$scenario, levels = names(scenario_labels),
                          labels = unname(scenario_labels))
  }
  
  if("group" %in% names(df)) {
    df$group <- factor(df$group, 
                       levels = global_group_levels, 
                       labels = global_group_labels)
  }
  
  if ("par_label" %in% names(df)) {
    levels <- unique(df$par_label)
    df$par_label <- factor(df$par_label, levels = unique(df$par_label))
  }
  df
}


# Combine gamma and lognormal into same panels
add_par_roles <- function(df) {
  roles <- c("Mean", "CV", "95th Quantile")
  df %>%
    filter(par != "probability of error") %>%
    mutate(role = factor(case_when(
      par == "mean" ~ roles[1],
      par == "cv" ~ roles[2],
      par == "q95" ~ roles[3],
      TRUE ~ NA
    ), levels = roles))
}
