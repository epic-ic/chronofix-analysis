simulate_true_data <- function(date_params, nsims, scenario) {
  sim1 <- function() {
    dat <- chronofix::chronofix_simulate_true_data(
      date_params$n_per_group,
      date_params$group_names,
      date_params$delay_info,
      date_params$date_range
    )
    
    if (scenario == "dependent_delays") {
      resample <- function(df, group, outcome_event) {
        to_resample <- which(df$group == group &
                               df$hospitalisation > df[[outcome_event]])
        if (length(to_resample) == 0) {
          return(df)
        } else {
          n_per_group <- rep(0, 4)
          n_per_group[date_params$group_names == group] <- 
            length(to_resample)
          df_resamp <- chronofix::chronofix_simulate_true_data(
            n_per_group,
            date_params$group_names,
            date_params$delay_info,
            date_params$date_range
          )
          df_resamp$id <- df$id[to_resample]
          df[to_resample, ] <- df_resamp
          resample(df, group, outcome_event)
        }
      }
      
      dat <- resample(dat, "hospitalised-alive", "discharge")
      dat <- resample(dat, "hospitalised-dead", "death")
    }
    
    dat
  }
  
  replicate(nsims, sim1(), simplify = FALSE)
}


summarise_params <- function(delay_info, scenario) {
  
  delay_info <- delay_info %>%
    mutate(shape = case_when(distribution == "gamma" ~ 1 / cv^2),
           precisionlog = case_when(
             distribution == "log-normal" ~ 1 / log(cv^2 + 1)),
           meanlog = case_when(
             distribution == "log-normal" ~ log(mean) - 1 / (2 * precisionlog)),
           q95 = case_when(
             distribution == "gamma" ~ 
               qgamma(0.95, shape = shape, scale = mean / shape),
             distribution == "log-normal" ~ 
               qlnorm(0.95, meanlog = meanlog, sdlog = 1 / sqrt(precisionlog)))
    )
  
  if (scenario == "dependent_delays") {
    delay_info <- summarise_dependent(delay_info, "hospitalised-alive",
                                      "discharge")
    delay_info <- summarise_dependent(delay_info, "hospitalised-dead",
                                      "death")
  }
  
  delay_info
}


summarise_dependent <- function(delay_info, group, outcome_event) {
  
  delay1 <- which(delay_info$to == "hospitalisation" & 
    delay_info$group == group)
  delay2 <- which(delay_info$to == outcome_event & 
    delay_info$group == group)
  
  
  n_samp <- 1e6
  samp <- 
    replicate(n_samp,
              r_dep_gamma(delay_info$shape[delay1], delay_info$mean[delay1],
                          delay_info$shape[delay2], delay_info$mean[delay2]))
  ## row 1 is onset to hospitalisation, row 2 is onset to outcome_event
  ## so convert row 2 to hospitalisation to outcome_event
  samp[2, ] <- samp[2, ] - samp[1, ]
  
  dep_delay_info <- data.frame(
    from = c("onset", "hospitalisation"),
    to = c("hospitalisation", outcome_event),
    group = group,
    distribution = NA,
    mean = rowMeans(samp),
    cv = apply(samp, 1, function(x) sqrt(var(x)) / mean(x)),
    shape = NA,
    precisionlog = NA,
    meanlog = NA,
    q95 = apply(samp, 1, function(x) quantile(x, 0.95))
  )

  delay_info <- delay_info[-c(delay1, delay2), ]
  
  delay_info <- rbind(delay_info, dep_delay_info)
  delay_info
}


r_dep_gamma <- function(shape1, mean1, shape2, mean2) {
  x <- rgamma(2, c(shape1, shape2), scale = c(mean1 / shape1, mean2 / shape2))
  if (x[1] <= x[2]) {
    return(x)
  } else {
    r_dep_gamma(shape1, mean1, shape2, mean2)
  }
}
