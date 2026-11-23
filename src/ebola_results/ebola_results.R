library(chronofix)
library(orderly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(rmarkdown)
library(kableExtra)

orderly::orderly_shared_resource("util.R")
source("util.R")
orderly::orderly_resource("plot.R")
source("plot.R")
orderly::orderly_resource("support.R")
source("support.R")

orderly::orderly_resource("paper_numbers.Rmd")

version_check("chronofix", "0.0.18")

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
                 files = c("figures_pdf/ebola_delays_ribbon.pdf",
                           "figures_pdf/ebola_delays_spaghetti.pdf",
                           "figures_pdf/ebola_dates_and_errors.pdf",
                           "figures_pdf/ebola_individual1.pdf",
                           "figures_pdf/ebola_individual2.pdf",
                           "figures_pdf/ebola_individual3.pdf",
                           "figures_tiff/ebola_delays_ribbon.tiff",
                           "figures_tiff/ebola_delays_spaghetti.tiff",
                           "figures_tiff/ebola_dates_and_errors.tiff",
                           "figures_tiff/ebola_individual1.tiff",
                           "figures_tiff/ebola_individual2.tiff",
                           "figures_tiff/ebola_individual3.tiff"))

samples <- readRDS("inputs/samples.rds")
data_with_onset_inferred <- readRDS("inputs/data_with_onset_inferred.rds")
pars_summary <- readRDS("inputs/pars_summary.rds")

dir.create("outputs", showWarnings = FALSE)
chronofix::chronofix_linelist(samples, 
                              filename = "outputs/chronofix_linelist.xlsx",
                              show_p_error = TRUE,
                              observed_dates_tab = TRUE)

dir.create("figures_pdf", showWarnings = FALSE)
dir.create("figures_tiff", showWarnings = FALSE)
ggsave("figures_pdf/ebola_delays_ribbon.pdf", 
       chronofix::chronofix_plot_delays(samples, plot_style = "ribbon",
                                        show_title = FALSE,
                                        show_legend = FALSE),
       width = 20, height = 12)
ggsave("figures_tiff/ebola_delays_ribbon.tiff", 
       chronofix::chronofix_plot_delays(samples, plot_style = "ribbon",
                                        show_title = FALSE,
                                        show_legend = FALSE),
       width = 20, height = 12)

ggsave("figures_pdf/ebola_delays_spaghetti.pdf", 
       chronofix::chronofix_plot_delays(samples, plot_style = "spaghetti",
                                        show_title = FALSE,
                                        show_legend = FALSE),
       width = 20, height = 12)
ggsave("figures_tiff/ebola_delays_spaghetti.tiff", 
       chronofix::chronofix_plot_delays(samples, plot_style = "spaghetti",
                                        show_title = FALSE,
                                        show_legend = FALSE),
       width = 20, height = 12)

ggsave("figures_pdf/ebola_dates_and_errors.pdf", 
       plot_events(samples),
       width = 12, height = 7.2)
ggsave("figures_tiff/ebola_dates_and_errors.tiff", 
       plot_events(samples),
       width = 12, height = 7.2)


ggsave("figures_pdf/ebola_individual1.pdf", 
       chronofix::chronofix_plot_individual(samples, id = 76),
       width = 12, height = 9)
ggsave("figures_tiff/ebola_individual1.tiff", 
       chronofix::chronofix_plot_individual(samples, id = 76),
       width = 12, height = 9)

ggsave("figures_pdf/ebola_individual2.pdf", 
       chronofix::chronofix_plot_individual(samples, id = 1637),
       width = 12, height = 9)
ggsave("figures_tiff/ebola_individual2.tiff", 
       chronofix::chronofix_plot_individual(samples, id = 1637),
       width = 12, height = 9)

ggsave("figures_pdf/ebola_individual3.pdf", 
       chronofix::chronofix_plot_individual(samples, id = 3703),
       width = 12, height = 9)
ggsave("figures_tiff/ebola_individual3.tiff", 
       chronofix::chronofix_plot_individual(samples, id = 3703),
       width = 12, height = 9)

## Render rmd
rmarkdown::render("paper_numbers.Rmd", output_dir = "outputs")
