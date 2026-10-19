# Plot for coverage and relative bias
plot_performance_figure <- function(pars_summary, target_role = "Mean") {

  plot_data_raw <- pars_summary %>%
    add_par_roles() %>%
    filter(!is.na(true_value), !is.na(role), role == target_role) %>%
    mutate(sim_rel_bias = (median - true_value) / true_value) %>%
    group_by(scenario, group, delay, role) %>%
    summarise(
      n_sims = sum(!is.na(q2.5) & !is.na(q97.5)),
      cov_50_n = sum(q25 <= true_value & true_value <= q75, na.rm = TRUE),
      cov_95_n = sum(q2.5 <= true_value & true_value <= q97.5, na.rm = TRUE),
      bias_median = median(sim_rel_bias, na.rm = TRUE),
      bias_low = quantile(sim_rel_bias, 0.025, na.rm = TRUE),
      bias_high = quantile(sim_rel_bias, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_sims > 0) %>%
    rowwise() %>%
    mutate(
      cov_50 = cov_50_n / n_sims,
      cov_95 = cov_95_n / n_sims,
      cov_50_lower = binom.test(cov_50_n, n_sims)$conf.int[1],
      cov_50_upper = binom.test(cov_50_n, n_sims)$conf.int[2],
      cov_95_lower = binom.test(cov_95_n, n_sims)$conf.int[1],
      cov_95_upper = binom.test(cov_95_n, n_sims)$conf.int[2]
    ) %>%
    ungroup()

  # maximum number of delays in any single group
  delay_counts <- plot_data_raw %>% distinct(group, delay) %>% count(group)
  max_delays <- max(delay_counts$n, na.rm = TRUE)

  # groups that need padding to match max_delays
  dummy_groups <- delay_counts %>% filter(n < max_delays)

  if (nrow(dummy_groups) > 0) {
    dummy_rows <- dummy_groups %>%
      rowwise() %>%
      mutate(delay = list(strrep(" ", 1:(max_delays - n)))) %>%
      unnest(delay) %>%
      mutate(scenario = plot_data_raw$scenario[1]) %>%
      select(group, delay, scenario) %>%
      ungroup()

    plot_data <- bind_rows(plot_data_raw, dummy_rows)
  } else {
    plot_data <- plot_data_raw
  }

  dummy_levels <- strrep(" ", 1:max_delays)

  plot_data <- plot_data %>%
    mutate(
      delay = factor(as.character(delay),
                     levels = c(global_delay_levels, dummy_levels),
                     labels = c(global_delay_labels, dummy_levels))
    )

  cov_data <- plot_data %>%
    select(scenario, group, delay, cov_50, cov_95,
           cov_50_lower, cov_50_upper, cov_95_lower, cov_95_upper) %>%
    pivot_longer(c(cov_50, cov_95),
                 names_to = "interval",
                 values_to = "coverage") %>%
    mutate(
      ci_lower = ifelse(interval == "cov_50", cov_50_lower, cov_95_lower),
      ci_upper = ifelse(interval == "cov_50", cov_50_upper, cov_95_upper),
      interval = factor(ifelse(interval == "cov_50", "50% CrI", "95% CrI"),
                        levels = c("50% CrI", "95% CrI"))
    )

  # coverage
  p_cov <- ggplot(cov_data, aes(x = delay, y = coverage, colour = scenario)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = 0.85),
                    size = 0.25, fatten = 1.8, alpha = 0.85, na.rm = TRUE) +
    facet_wrap(~ group, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_discrete(labels = scales::label_wrap(18),
                     expand = expansion(add = 0.4)) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = sprintf("Coverage of the %s", target_role),
         x = NULL, y = "Coverage Probability", colour = "Scenario") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10, face = "bold", hjust = 0,
                                margin = margin(t = 5, b = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.spacing = unit(1.5, "lines"),
      panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 1))

  # relative bias
  p_bias <- ggplot(plot_data, aes(x = delay, y = bias_median, colour = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black", alpha = 0.5) +
    geom_pointrange(aes(ymin = bias_low, ymax = bias_high),
                    position = position_dodge(width = 0.85),
                    size = 0.25, fatten = 1.8, alpha = 0.85, na.rm = TRUE) +
    facet_wrap(~ group, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = scales::percent, limits = c(-1, 1)) +
    scale_x_discrete(labels = scales::label_wrap(18), expand = expansion(add = 0.4)) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = sprintf("Relative Bias of the %s", target_role),
         x = NULL, y = "Relative Bias (Median vs True)", colour = "Scenario") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10, face = "bold", colour = "transparent",
                                margin = margin(t = 5, b = 10)),
      axis.title.y = element_text(margin = margin(r = 10, l = 10)),
      panel.spacing = unit(1.5, "lines"),
      panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 1))

  combined_plot <- p_cov + p_bias +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "right",
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 11, hjust = 0.5))

  return(combined_plot)
}

# Individual and Event-level sensitivity
plot_sensitivity_figure <- function(errors_summary, target_threshold = 0.5) {
  
  sens_data <- errors_summary %>%
    filter(threshold == target_threshold) %>%
    filter(!scenario %in% c("Missing dates only (0.2)",
                            "No errors or missing dates")) %>%
    filter(trimws(scenario) != "") %>%
    mutate(scenario = factor(scenario,
                             levels = setdiff(levels(scenario),
                                              c("Missing dates only (0.2)",
                                                "No errors or missing dates"))))
  
  indiv_data <- sens_data %>% 
    filter(event == "individual") %>%
    mutate(accuracy = n_true_errors_flagged / n_true_errors) %>%
    filter(!is.na(accuracy))
  
  event_data <- sens_data %>% 
    filter(event != "individual") %>%
    group_by(scenario, group, event) %>%
    summarise(pct_accuracy = sum(n_true_errors_flagged) / sum(n_true_errors),
              .groups = "drop") %>%
    filter(!is.na(pct_accuracy)) %>%
    mutate(event = factor(event,
                          levels = global_event_levels,
                          labels = global_event_labels))
  
  # individual plot
  p_indiv <- ggplot(indiv_data, aes(x = scenario, y = accuracy,
                                    fill = scenario, colour = scenario)) +
    geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1,
                 position = position_dodge2(reverse = TRUE, padding = 0.1)) +
    facet_grid(group ~ .) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = "Individual-Level Sensitivity",
         y = sprintf("Sensitivity (%.0f%% Threshold)", target_threshold * 100),
         x = "") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 10, l = 10)),
          plot.title = element_text(size = 10),
          legend.position = "none")
  
  # event plot grid
  p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                    fill = pct_accuracy)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f", pct_accuracy * 100)),
              size = 2.8, colour = "grey20") +
    facet_grid(group ~ .) +
    scale_fill_gradient2(midpoint = 0.5,
                         low = "firebrick", mid = "white", high = "steelblue", 
                         limits = c(0, 1), labels = scales::percent,
                         na.value = "grey95") +
    scale_y_discrete(drop = FALSE, limits = rev) +
    labs(title = "Event-Level Sensitivity",
         y = "",
         x = "",
         fill = sprintf("Sensitivity\n(%.0f%% Threshold)",
                        target_threshold * 100)) +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          legend.title = element_text(margin = margin(b = 15)))
  
  combined_plot <- p_indiv + p_event + 
    plot_layout(ncol = 2, widths = c(1, 1))
  
  return(combined_plot)
}

# Individual and Event-level specificity
plot_specificity_figure <- function(errors_summary, target_threshold = 0.5) {

  spec_data <- errors_summary %>%
    filter(threshold == target_threshold) %>%
    filter(trimws(scenario) != "")

  indiv_data <- spec_data %>%
    filter(event == "individual") %>%
    filter(n_true_non_errors > 0) %>%
    mutate(specificity =
             (n_true_non_errors - n_false_positives) / n_true_non_errors) %>%
    filter(!is.na(specificity))

  event_data <- spec_data %>%
    filter(event != "individual") %>%
    group_by(scenario, group, event) %>%
    summarise(pct_specificity =
                (sum(n_true_non_errors) - sum(n_false_positives)) /
                sum(n_true_non_errors),
              .groups = "drop") %>%
    filter(!is.na(pct_specificity)) %>%
    mutate(event = factor(event,
                          levels = global_event_levels,
                          labels = global_event_labels))

  p_indiv <- ggplot(indiv_data, aes(x = scenario, y = specificity,
                                    fill = scenario, colour = scenario)) +
    geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1,
                 position = position_dodge2(reverse = TRUE, padding = 0.1)) +
    facet_grid(group ~ .) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = "Individual-Level Specificity",
         y = sprintf("Specificity (%.0f%% Threshold)", target_threshold * 100),
         x = "") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 10, l = 10)),
          plot.title = element_text(size = 10),
          legend.position = "none")

  p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                    fill = pct_specificity)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f", pct_specificity * 100)),
              size = 2.8, colour = "grey20") +
    facet_grid(group ~ .) +
    scale_fill_gradient2(midpoint = 0.5,
                         low = "firebrick", mid = "white", high = "steelblue",
                         limits = c(0, 1), labels = scales::percent,
                         na.value = "grey95") +
    scale_y_discrete(drop = FALSE, limits = rev) +
    labs(title = "Event-Level Specificity",
         y = "",
         x = "",
         fill = sprintf("Specificity\n(%.0f%% Threshold)",
                        target_threshold * 100)) +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          legend.title = element_text(margin = margin(b = 15)))

  combined_plot <- p_indiv + p_event +
    plot_layout(ncol = 2, widths = c(1, 1))

  return(combined_plot)
}

# both specificity error thresholds together
plot_all_specificity <- function(errors_summary) {

  target_thresholds <- sort(unique(errors_summary$threshold))
  plot_list <- list()
  
  for (i in seq_along(target_thresholds)) {
    thr <- target_thresholds[i]
    is_bottom_row <- (i == length(target_thresholds))
    
    spec_data <- errors_summary %>%
      filter(threshold == thr) %>%
      filter(trimws(scenario) != "")
    
    indiv_data <- spec_data %>% 
      filter(event == "individual") %>%
      filter(n_true_non_errors > 0) %>%
      mutate(specificity = (n_true_non_errors - n_false_positives) /
               n_true_non_errors) %>%
      filter(!is.na(specificity))
    
    event_data <- spec_data %>% 
      filter(event != "individual") %>%
      group_by(scenario, group, event) %>%
      summarise(
        pct_specificity = (sum(n_true_non_errors) - sum(n_false_positives)) /
          sum(n_true_non_errors), 
        .groups = "drop"
      ) %>%
      filter(!is.na(pct_specificity)) %>%
      mutate(event = factor(event,
                            levels = global_event_levels,
                            labels = global_event_labels))
    
    p_indiv <- ggplot(indiv_data, aes(x = scenario, y = specificity,
                                      fill = scenario, colour = scenario)) +
      geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1) +
      facet_grid(group ~ .) +
      scale_x_discrete(drop = TRUE) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_fill_manual(values = scenario_colours, drop = FALSE) +
      scale_colour_manual(values = scenario_colours, drop = FALSE) +
      labs(title = sprintf("Individual-Level Specificity (%.0f%% Threshold)", thr * 100),
           y = "Specificity",
           x = "") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "darkgrey",
                                        fill = NA, linewidth = 1),
            strip.text.y = element_text(size = 9, face = "bold", angle = 270),
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            plot.title = element_text(size = 10),
            legend.position = "none")
    
    p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                      fill = pct_specificity)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f", pct_specificity * 100)),
                size = 2.8, colour = "grey20") +
      facet_grid(group ~ .) +
      scale_fill_gradient2(midpoint = 0.5,
                           low = "firebrick", mid = "white", high = "steelblue", 
                           limits = c(0, 1), labels = scales::percent,
                           na.value = "grey95") +
      scale_y_discrete(drop = FALSE, limits = rev, expand = c(0, 0)) +
      labs(title = sprintf("Event-Level Specificity (%.0f%% Threshold)", thr * 100),
           y = "",
           x = "",
           fill = "Specificity") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "darkgrey",
                                        fill = NA, linewidth = 1),
            strip.text.y = element_text(size = 9, face = "bold", angle = 270),
            panel.grid = element_blank(),
            plot.title = element_text(size = 10),
            legend.title = element_text(margin = margin(b = 15)))
    
    if (is_bottom_row) {
      p_indiv <- p_indiv +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      p_event <- p_event +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p_indiv <- p_indiv +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      p_event <- p_event +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }

    plot_list[[length(plot_list) + 1]] <- p_indiv
    plot_list[[length(plot_list) + 1]] <- p_event
  }
  
  combined_plot <- wrap_plots(plot_list, ncol = 2, widths = c(1, 1)) + 
    plot_layout(guides = "collect")
  
  return(combined_plot)
}
