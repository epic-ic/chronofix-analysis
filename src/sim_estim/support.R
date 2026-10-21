summarise_pars <- function(samples, delay_info, true_prob_error, 
                           true_delay_info) {
  
  samples_df <- posterior::as_draws_df(samples)
  
  # calculate mean and CV and 95th quantile for every individual posterior sample
  for (i in seq_len(nrow(delay_info))) {
    dist <- delay_info$distribution[i]
    
    if (dist == "gamma") {
      mean_col <- paste0("delay", i, "_mean")
      shape_col <- paste0("delay", i, "_shape")
      cv_col <- paste0("delay", i, "_cv")
      q95_col <- paste0("delay", i, "_q95")
      
      # gamma CV = 1 / sqrt(shape)
      samples_df[[cv_col]] <- 1 / sqrt(samples_df[[shape_col]])
      
      # gamma 95th quantile
      scale_vec <- samples_df[[mean_col]] / samples_df[[shape_col]]
      samples_df[[q95_col]] <- stats::qgamma(0.95, 
                                             shape = samples_df[[shape_col]], 
                                             scale = scale_vec)
      
    } else if (dist == "log-normal") {
      ml_col <- paste0("delay", i, "_meanlog")
      pr_col <- paste0("delay", i, "_precisionlog")
      mean_col <- paste0("delay", i, "_mean")
      cv_col <- paste0("delay", i, "_cv")
      q95_col <- paste0("delay", i, "_q95")
      
      mu <- samples_df[[ml_col]]
      tau <- samples_df[[pr_col]]
      
      # lognormal mean = exp(mu + 1 / (2 * tau))
      samples_df[[mean_col]] <- exp(mu + 1 / (2 * tau))
      # lognormal CV = sqrt(exp(1 / tau) - 1)
      samples_df[[cv_col]]   <- sqrt(exp(1 / tau) - 1)
      # lognormal 95th quantile
      sdlog_vec <- sqrt(1 / tau)
      samples_df[[q95_col]] <- stats::qlnorm(0.95, 
                                             meanlog = mu, 
                                             sdlog = sdlog_vec)
    }
  }
  
  p_quantile <- c(0.025, 0.25, 0.75, 0.975)
  summary <- 
    posterior::summarise_draws(samples_df, mean, median, sd, 
                               ~posterior::quantile2(.x, p_quantile),
                               posterior::default_convergence_measures())
  
  prob_error <- data.frame(par = "probability of error",
                           delay = NA,
                           group = NA,
                           variable = "prob_error",
                           true_value = true_prob_error)
  
  delays <- true_delay_info
  delays$delay_number <- seq_len(nrow(true_delay_info))
  delays <- delays %>%
    mutate(delay = paste0(from, " to ", to)) %>%
    pivot_longer(c(mean, cv, shape, precisionlog, meanlog, q95),
                 names_to = "par", values_to = "true_value") %>%
    mutate(variable = paste0("delay", delay_number, "_", par)) %>%
    select(par, delay, group, variable, true_value)
  
  rbind(prob_error, delays) %>%
    mutate(
      par_label = dplyr::case_when(par == "probability of error" ~ par,
                                   .default = paste0(delay, " ", par,
                                                     " (", group, ")"))) %>%
    right_join(summary)
}


summarise_errors <- function(samples, data) {
  sample_errors <- samples$augmented_data$error_indicators
  data_errors <- data$error_indicators
  n_samples <- dim(sample_errors)[3]
  
  date_cols <- c("onset", "hospitalisation", "report", "death", "discharge")
  ## we will only include individuals with at least two non-missing dates
  eligible <- rowSums(!is.na(data_errors[, date_cols])) > 1
  data_errors <- data_errors[eligible, , drop = FALSE]
  sample_errors <- sample_errors[eligible, date_cols, , drop = FALSE]
  
  ## vector indicating whether or not the individual has at least one true error
  data_errors$individual <- 
    apply(data_errors[, date_cols], 1, function(x) sum(x, na.rm = TRUE)) > 0
  
  
  ## n_individuals by n_dates array of proportion of samples where the date is
  ## an  error 
  prop_is_sample_errors <- apply(sample_errors, c(1, 2), sum) / 
    n_samples
  
  ## n_individuals by n_samples array of whether there is at least one error
  individual_has_sample_errors <- 
    apply(sample_errors, c(1, 3), function(x) sum(x, na.rm = TRUE)) > 0
  ## n_individuals vector of proportion of samples where the individual has
  ## as least one error
  prop_individual_has_sample_errors <- 
    apply(individual_has_sample_errors, 1, sum) / n_samples
  prop_is_sample_errors <- cbind(prop_is_sample_errors,
                                 prop_individual_has_sample_errors)
  colnames(prop_is_sample_errors) <- c(date_cols, "individual")
  
  calc_error_summary <- function(threshold) {
    df_sample_errors <- 
      data.frame(id = data_errors$id,
                 prop_is_sample_errors > threshold)
    
    df_sample_errors <- df_sample_errors %>%
      pivot_longer(!id, names_to = "event", values_to = "sample_error")
    
    data_errors %>%
      pivot_longer(!c(id, group), names_to = "event", 
                   values_to = "data_error") %>%
      right_join(df_sample_errors) %>%
      group_by(group, event) %>%
      summarise(
        n_true_errors_flagged = sum(sample_error & data_error, na.rm = TRUE),
        n_true_errors = sum(data_error, na.rm = TRUE),
        n_false_positives = sum(sample_error & !data_error, na.rm = TRUE),
        n_true_non_errors = sum(!data_error, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(threshold = threshold) %>%
      relocate(threshold, .after = event)
  }
  
  rbind(calc_error_summary(0.5), calc_error_summary(0.95))
  
}
