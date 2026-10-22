global_group_levels <- c("community-alive", "community-dead",
                         "hospitalised-alive", "hospitalised-dead")
global_group_labels <- tools::toTitleCase(gsub("-", " ", global_group_levels))

global_event_levels <- 
  c("onset", "case_report", "hospitalisation", "outcome_report")
global_event_labels <- tools::toTitleCase(gsub("_", " ", global_event_levels))

error_type_cols <- c("correct" = "#2E6F40",
                     "missing" =  "#00008B",
                     "possible error" = "#F7BD03",
                     "likely error" = "#C76E00",
                     "highly likely error" = "#7C0A02")


plot_events <- function(samples) {
  p_error_type <- plot_error_type(samples)
  p_est_date_diff <- plot_est_date_diff(samples)
  
  combined_plot <- p_error_type + p_est_date_diff + 
    plot_layout(ncol = 2, widths = c(1, 1), guides = "collect")
}


plot_error_type <- function(samples) {
  group <- attr(samples$data, "group")
  
  df <- summarise_errors(samples) %>%
    select(!total) %>%
    pivot_longer(!all_of(c(group, "event")), names_to = "type", 
                 values_to = "value") %>%
    mutate(type = gsub("_", " ", type))
  df$type <- factor(df$type, levels = c("correct", "possible error", 
                                        "likely error", "highly likely error",
                                        "missing"))
  df$group <- factor(df$group, levels =  global_group_levels,
                     labels = global_group_labels)
  df$event <- factor(df$event, levels =  global_event_levels,
                     labels = global_event_labels)
  
  ggplot(df, aes(fill = type, y = event, x = value)) + 
    geom_bar(position = position_fill(reverse = TRUE),
             stat = "identity", show.legend = TRUE) +
    scale_fill_manual(values = error_type_cols) +
    scale_y_discrete(drop = FALSE, limits = rev) +
    facet_grid(rows = vars(group)) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Date classification",
         y = "",
         x = "") +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1))
}


summarise_errors <- function(samples) {
  id <- attr(samples$data, "id")
  group <- attr(samples$data, "group")
  
  error_indicators <- samples$augmented_data$error_indicators
  n_samples <- dim(error_indicators)[3]
  
  events <- colnames(error_indicators)
  
  ## n_individuals by n_dates array of proportion of samples where the date is
  ## an error 
  prop_is_error <- apply(error_indicators, c(1, 2), sum) / 
    n_samples
  
  df <- cbind(samples$data[, c(id, group)], prop_is_error) %>%
    pivot_longer(!all_of(c(id, group)), names_to = "event", 
                 values_to = "prob_error")
  
  delay_map <- samples$delay_map
  groups <- unique(unlist(samples$delay_map$group))
  
  for (g in groups) {
    is_delay_in_group <- 
      vapply(seq_len(nrow(delay_map)),
             function(i) g %in% unlist(delay_map$group[i]),
             logical(1L))
    group_events <- unique(c(delay_map$from[is_delay_in_group],
                             delay_map$to[is_delay_in_group]))
    to_remove <- df$group == g & !(df$event %in% group_events)
    df <- df[!to_remove, ]
  }
  
  df[[group]] <- factor(df[[group]], levels = groups)
  df$event <- factor(df$event, levels = events)
  
  df_summary <- df %>%
    group_by(.data[[group]], event) %>%
    summarise(
      total = n(),
      missing = sum(is.na(prob_error)),
      correct = sum(prob_error < 0.05, na.rm = TRUE),
      possible_error = sum(prob_error >= 0.05 & prob_error < 0.5, na.rm = TRUE),
      likely_error = sum(prob_error >= 0.5 & prob_error < 0.95, na.rm = TRUE),
      highly_likely_error = sum(prob_error >= 0.95, na.rm = TRUE))
  
  df_summary
}


plot_est_date_diff <- function(samples, absolute = FALSE){
  id <- attr(samples$data, "id")
  group <- attr(samples$data, "group")
  
  error_indicators <- samples$augmented_data$error_indicators
  estimated_dates <- samples$augmented_data$estimated_dates
  n_samples <- dim(error_indicators)[3]
  
  events <- colnames(error_indicators)
  
  ## n_individuals by n_dates array of proportion of samples where the date is
  ## an error 
  prop_is_error <- apply(error_indicators, c(1, 2), sum) / 
    n_samples
  
  df_prob_error <- cbind(samples$data[, c(id, group)], prop_is_error) %>%
    pivot_longer(!all_of(c(id, group)), names_to = "event", 
                 values_to = "prob_error")
  
  observed_dates <- date_to_int(data_frame_to_array(samples$data[, events]))
  
  est_date_diff <- floor(estimated_dates) - 
    replicate(n_samples, observed_dates, simplify = "array")
  
  if (absolute) {
    mean_est_date_diff <- apply(abs(est_date_diff), c(1, 2), mean)
  } else {
    mean_est_date_diff <- apply(est_date_diff, c(1, 2), mean)
  }
  
  delay_map <- samples$delay_map
  groups <- unique(unlist(samples$delay_map$group))
  
  df_est_date_diff <- cbind(samples$data[, c(id, group)], 
                            mean_est_date_diff) %>%
    pivot_longer(!all_of(c(id, group)), names_to = "event", 
                 values_to = "mean_date_diff")
  
  df <- df_prob_error %>% 
    right_join(df_est_date_diff) %>%
    filter(!is.na(prob_error) & prob_error > 0.05) %>%
    mutate(type = case_when(
      prob_error >= 0.05 & prob_error < 0.5 ~ "possible error",
      prob_error >= 0.5 & prob_error < 0.95 ~ "likely error",
      prob_error >= 0.95 ~ "highly likely error"))
  df$group <- factor(df$group, levels =  global_group_levels,
                     labels = global_group_labels)
  df$event <- factor(df$event, levels =  global_event_levels,
                     labels = global_event_labels)
  
  ggplot(df, aes(colour = type, y = event, x = mean_date_diff)) + 
    geom_jitter(width = 0, height = 0.3, show.legend = FALSE) +
    scale_colour_manual(values = error_type_cols) +
    scale_y_discrete(drop = FALSE, limits = rev) +
    facet_grid(rows = vars(group)) +
    theme_bw() +
    labs(title = "Mean difference between estimated and observed date",
         y = "",
         x = "") +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1))
}


date_to_int <- function(date, origin = "1970-01-01") {
  int <- as.integer(as.Date(date) - as.Date(origin))
  
  d <- dim(date)
  if (!is.null(d)) {
    int <- array(int, d)
  }
  
  int
}


data_frame_to_array <- function(df) {
  array(unlist(df), dim(df))
}
