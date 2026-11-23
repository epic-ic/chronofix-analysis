traceplots <- function(samples, burnin, pars_summary) {
  full_chains <- samples$full_chains
  pars_summary <- pars_summary %>%
    filter(variable %in% rownames(full_chains$pars))
  full_chains$pars <- full_chains$pars[pars_summary$variable, , ]
  rownames(full_chains$pars) <- pars_summary$par_label
  
  which_ess_min <- which.min(pars_summary$ess_bulk)[1]
  ess_min_par <- pars_summary$par_label[which_ess_min]
  ess_min_val <- round(pars_summary$ess_bulk[which_ess_min])
  which_rhat_max <- which.max(pars_summary$rhat)[1]
  rhat_max_par <- pars_summary$par_label[which_rhat_max]
  rhat_max_val <- round(pars_summary$rhat[which_rhat_max], 3)
  
  
  full_chains$pars <- abind::abind(full_chains$pars, full_chains$density, 
                                   along = 1)
  rownames(full_chains$pars)[nrow(full_chains$pars)] <- 
    "log posterior density"
  
  samples_df <- posterior::as_draws_df(full_chains)
  
  color_scheme <- unname(unlist(rev(bayesplot::color_scheme_get("viridis"))))
  color_scheme <- gsub("*FF", "", color_scheme)
  
  bayesplot::color_scheme_set(color_scheme)
  p <- bayesplot::mcmc_trace(samples_df,
                             n_warmup = burnin,
                             facet_args = list(nrow = 5)) +
    bayesplot::theme_default(base_size = 16) +
    theme(legend.position = "none") +
    labs(subtitle = 
           glue("min ESS: {ess_min_val} ({ess_min_par})
                \nmax Rhat: {rhat_max_val} ({rhat_max_par})"),
         )
  p
}


rankplots <- function(samples, burnin, pars_summary) {
  full_chains <- samples$full_chains
  pars_summary <- pars_summary %>%
    filter(variable %in% rownames(full_chains$pars))
  full_chains$pars <- full_chains$pars[pars_summary$variable, , ]
  full_chains$pars <- full_chains$pars[, -seq_len(burnin), ]
  rownames(full_chains$pars) <- pars_summary$par_label
  
  samples_df <- posterior::as_draws_df(full_chains)
  
  color_scheme <- unname(unlist(rev(bayesplot::color_scheme_get("viridis"))))
  color_scheme <- gsub("*FF", "", color_scheme)
  
  bayesplot::color_scheme_set(color_scheme)
  p <- bayesplot::mcmc_rank_overlay(samples_df,
                                    facet_args = list(nrow = 5)) +
    bayesplot::theme_default(base_size = 16) +
    theme(legend.position = "none")
  p
}
