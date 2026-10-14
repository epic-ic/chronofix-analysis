library(chronofix)
library(orderly)
library(ggplot2)
library(rmarkdown)

orderly::orderly_shared_resource("util.R")
source("util.R")
orderly::orderly_resource("support.R")
source("support.R")

orderly::orderly_resource("paper_numbers.Rmd")

version_check("chronofix", "0.0.15")

orderly_dependency("ebola_fit", "latest", 
                   files = c("inputs/samples.rds" = "outputs/samples.rds",
                             "inputs/pars_summary.rds" = 
                               "outputs/pars_summary.rds",
                             "inputs/data_with_onset_inferred.rds" = 
                               "outputs/data_with_onset_inferred.rds"))

orderly_artefact(description = "chronofix linelist",
                 files = "outputs/chronofix_linelist.xlsx")

orderly_artefact(description = "paper numbers",
                 files = "outputs/paper_numbers.html")

orderly_artefact(description = "figures",
                 files = c("figures/ebola_delays_ribbon.pdf",
                           "figures/ebola_delays_spaghetti.pdf"))

samples <- readRDS("inputs/samples.rds")
data_with_onset_inferred <- readRDS("inputs/data_with_onset_inferred.rds")
pars_summary <- readRDS("inputs/pars_summary.rds")

dir.create("outputs", showWarnings = FALSE)
chronofix::chronofix_linelist(samples, 
                              filename = "outputs/chronofix_linelist.xlsx")

dir.create("figures", showWarnings = FALSE)
ggsave("figures/ebola_delays_ribbon.pdf", 
       chronofix::chronofix_plot_delays(samples, plot_style = "ribbon"),
       width = 20, height = 12)
ggsave("figures/ebola_delays_spaghetti.pdf", 
       chronofix::chronofix_plot_delays(samples, plot_style = "spaghetti"),
       width = 20, height = 12)

## Render rmd
rmarkdown::render("paper_numbers.Rmd", output_dir = "outputs")
