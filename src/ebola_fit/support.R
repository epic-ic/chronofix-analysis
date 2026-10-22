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
    true_mean <- delay_info$mean[i]
    true_cv <- delay_info$cv[i]
    if (delay_info$distribution[i] == "gamma") {
      par <- c("mean", "shape")
    } else if (delay_info$distribution[i] == "log-normal") {
      par <- c("meanlog", "precisionlog")
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
