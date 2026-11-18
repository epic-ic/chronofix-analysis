calc_credible_set <- function(samples, i, j, cred_level) {
  estimated_dates <- 
    as.Date(floor(samples$augmented_data$estimated_dates[i, j, ]))
  freq <- sort(table(estimated_dates), decreasing = TRUE)
  prob <- freq / sum(freq)
  cum_prob <- cumsum(prob)
  k <- which(cum_prob >= cred_level)[1]
  names(cum_prob[seq_len(k)])
}

summarise_observed <- function(samples) {
  delay_map <- samples$delay_map
  data <- samples$data
  
  summarise1 <- function(i) {
    from <- delay_map$from[i]
    to <- delay_map$to[i]
    group <- delay_map$group[i]
    delay <- gsub("_", " ", paste0(from, " to ",  to, " (", group, ")"))
    
    mean <- mean(data[[to]][data$group == group] - 
                   data[[from]][data$group == group], na.rm = TRUE)
    median <- median(data[[to]][data$group == group] - 
                       data[[from]][data$group == group], na.rm = TRUE)
    stdev <- sqrt(var(data[[to]][data$group == group] - 
                        data[[from]][data$group == group], na.rm = TRUE))
    
    data.frame(Delay = delay,
               Observed_Mean = as.numeric(mean),
               Observed_Median = as.numeric(median),
               Observed_CV = as.numeric(stdev) / as.numeric(mean))
  }
  
  lapply(seq_len(nrow(delay_map)), summarise1) %>%
    dplyr::bind_rows()
}
