library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(monty)
library(chronofix)

orderly::orderly_resource("support.R")
source("support.R")
orderly::orderly_resource("plot.R")
source("plot.R")
orderly::orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.13")

pars <- orderly::orderly_parameters(include_retrospective = FALSE)

orderly::orderly_artefact(description = "MCMC outputs", 
                          files = c("outputs/samples.rds",
                                    "outputs/data_with_onset_inferred.rds",
                                    "outputs/pars_summary.rds"))

orderly::orderly_artefact(description = "MCMC plots", 
                          files = c("figures/traceplots.pdf",
                                    "figures/rankplots.pdf"))


n_steps <- 3000
burnin <- 1000
thinning_factor <- 8

raw_data <- read.csv("rstb20160308supp1.csv")

data <- filter_data(raw_data,
                    include_retrospective = pars$include_retrospective)
data <- chronofix_prepare_data(data, id = "row_id")
data_with_onset_inferred <- 
  filter_data(raw_data, onset_inferred = TRUE,
              include_retrospective = pars$include_retrospective)
data_with_onset_inferred <- 
  chronofix_prepare_data(data_with_onset_inferred, id = "row_id")

dir.create("outputs", showWarnings = FALSE)
saveRDS(data_with_onset_inferred, "outputs/data_with_onset_inferred.rds")

delay_map <- delay_info <- data.frame(
  from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("case_report", "case_report", "case_report", "case_report",
         "outcome_report", "hospitalisation",
         "outcome_report", "hospitalisation", "outcome_report"),
  group = c("community-alive", "community-dead", "hospitalised-alive",
            "hospitalised-dead", "community-dead", "hospitalised-alive",
            "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
  distribution = "gamma"
)

# MCMC settings ---------------------------------------------------------------

control <- chronofix_mcmc_control(n_steps = n_steps,
                                  burnin = burnin,
                                  thinning_factor = thinning_factor,
                                  n_chains = 4,
                                  parallel = TRUE,
                                  n_workers = 4,
                                  date_buffer = 15)

hyperparameters <- chronofix_hyperparameters(
  gamma_shape_prior_shape = 1,
  gamma_shape_prior_rate = 0.1,
  gamma_mean_prior_shape = 2,
  gamma_mean_prior_scale = 10
)

# Run MCMC -------------------------------------------------------------------

samples <- chronofix_mcmc(data, delay_map, hyperparameters, control = control)
saveRDS(samples, "outputs/samples.rds")

pars_summary <- summarise_pars(samples, delay_map)

dir.create("figures", showWarnings = FALSE)

ggsave("figures/traceplots.pdf", traceplots(samples, burnin, pars_summary),
       width = 20, height = 12)

ggsave("figures/rankplots.pdf", rankplots(samples, burnin, pars_summary),
       width = 20, height = 12)

pars_summary <- pars_summary %>% select(!variable)
saveRDS(pars_summary, "outputs/pars_summary.rds")
