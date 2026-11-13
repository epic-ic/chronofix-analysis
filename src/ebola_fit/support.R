filter_data <- function(data, onset_inferred = FALSE) {
  data$row_id <- seq_len(nrow(data))
  ret <- data %>% 
    filter(EpiCaseDef == "confirmed") %>%
    filter(!is.na(FinalStatus)) %>%
    filter(!retrospective) %>%
    filter(!is.na(HospitalizedEver)) %>%
    mutate(group = case_when(
      FinalStatus == "Alive" & HospitalizedEver == "Yes" ~ "hospitalised-alive",
      FinalStatus == "Dead" & HospitalizedEver == "Yes" ~ "hospitalised-dead",
      FinalStatus == "Alive" & HospitalizedEver == "No" ~ "community-alive",
      FinalStatus == "Dead" & HospitalizedEver == "No" ~ "community-dead")) %>%
    mutate(onset = DateOnset,
           onset_inferred = DateOnsetInferred,
           case_report = DateReport,
           hospitalisation = DateHospitalCurrentAdmit,
           outcome_report = case_when(
             group != "community-alive" ~ DateOutcomeComp,
             group == "community-alive" ~ NA)) %>%
    filter(!(is.na(onset) & is.na(case_report) & is.na(hospitalisation) & 
               is.na(outcome_report))) %>%
    select(row_id, group, onset, onset_inferred, case_report, 
           hospitalisation, outcome_report)
  
  if (!onset_inferred) {
    ret <- ret %>% select(!onset_inferred)
  }
  
  ret
}


summarise_pars <- function(samples, delay_info) {
  samples_df <- posterior::as_draws_df(samples)
  
  for (i in seq_len(nrow(delay_info))) {
    dist <- delay_info$distribution[i]
    
    if (dist == "gamma") {
      mean_col <- paste0("delay", i, "_mean")
      shape_col <- paste0("delay", i, "_shape")
      rate_col <- paste0("delay", i, "_rate")
      cv_col <- paste0("delay", i, "_cv")
      q95_col <- paste0("delay", i, "_q95")
      
      # gamma CV = 1 / sqrt(shape)
      samples_df[[cv_col]] <- 1 / sqrt(samples_df[[shape_col]])
      
      # gamma rate = shape / mean
      samples_df[[rate_col]] <- samples_df[[shape_col]] / samples_df[[mean_col]]
      
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
                           variable = "prob_error")
  
  get_delay_info_i <- function(i) {
    if (delay_info$distribution[i] == "gamma") {
      par <- c("mean", "shape", "rate", "cv", "q95")
    } else if (delay_info$distribution[i] == "log-normal") {
      par <- c("meanlog", "precisionlog", "mean", "cv", "q95")
    } 
    delay <- paste0(delay_info$from[i], " to ", delay_info$to[i])
    group <- delay_info$group[i]
    
    data.frame(par = par,
               delay = delay,
               group = group,
               variable = paste0("delay", i, "_", par))
  }
  
  delays <- lapply(seq_len(nrow(delay_info)),
                   get_delay_info_i) %>%
    dplyr::bind_rows() %>% 
    arrange(group)
  
  rbind(prob_error, delays) %>%
    mutate(
      par_label = dplyr::case_when(par == "probability of error" ~ par,
                                   .default = paste0(delay, " ", par,
                                                     " (", group, ")"))) %>%
    right_join(summary)
}
