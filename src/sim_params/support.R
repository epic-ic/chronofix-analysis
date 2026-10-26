# Function to create sim_params for all scenarios

build_date_params <- function(
    n_per_group = rep(100, 4),
    group_names = c("community-alive", "community-dead",
                    "hospitalised-alive", "hospitalised-dead"),
    mean_scale = 1,
    cv_scale = 1,
    delay_distribution = "gamma",
    same_means = FALSE,
    dependent_delays = FALSE
) {
  date_range <- as.integer(as.Date(c("2014-01-01", "2015-01-01")))
  
  if (same_means) {
    base_means <- rep(7, 9)
  } else {
    base_means <- c(5, 6, 8, 11, 7, 9, 10, 12, 13)
  }
  
  delay_info <- data.frame(
    from = c("onset", "onset", "onset", "onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "report", "report", "report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = c("community-alive", "community-dead", "hospitalised-alive",
              "hospitalised-dead", "community-dead", "hospitalised-alive",
              "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
    distribution = delay_distribution,
    mean = base_means,
    cv = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )
  
  delay_info$mean <- delay_info$mean * mean_scale
  delay_info$cv <- delay_info$cv * cv_scale
  
  if (dependent_delays) {
    onset_to_discharge <- delay_info$group == "hospitalised-alive" &
      delay_info$to %in% c("hospitalisation", "discharge")
    mean_onset_to_discharge <- sum(delay_info$mean[onset_to_discharge])
    onset_to_death <- delay_info$group == "hospitalised-dead" &
      delay_info$to %in% c("hospitalisation", "death")
    mean_onset_to_death <- sum(delay_info$mean[onset_to_death])
    delay_special <- data.frame(
      from = "onset",
      to = c("discharge", "death"),
      group = c("hospitalised-alive", "hospitalised-dead"),
      distribution = delay_distribution,
      mean = c(mean_onset_to_discharge, mean_onset_to_death),
      cv = 0.5)
    delay_info <- rbind(delay_info, delay_special)
    delay_info <- delay_info[delay_info$from != "hospitalisation", ]
  }
  
  list(
    n_per_group = n_per_group,
    group_names = group_names,
    delay_info = delay_info,
    date_range = date_range
  )
}
