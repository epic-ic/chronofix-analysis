# Chronofix simulation study

Analysis code for the chronofix simulation study and the application to West
African Ebola line list data. We use the [chronofix](https://github.com/epic-ic/chronofix)
R package to estimate delay distributions from line lists that contain errors
and missing dates.

The analysis is an [orderly](https://mrc-ide.github.io/orderly/) project, and
each step in the workflow can be found under `src/`. We ran the full study on a
high-performance computing cluster with `workflow_cluster.R`. You can run it on
your local machine by following `workflow_local.R`.

## Requirements

```r
install.packages("pak")
pak::pkg_install(c("epic-ic/chronofix", "mrc-ide/monty@mrc-6769"))
pak::pkg_install(c("orderly", "tibble", "dplyr", "tidyr", "purrr",
                   "patchwork", "ggplot2", "glue", "posterior", "bayesplot",
                   "stringr", "scales", "ggrastr", "png", "furrr", "future",
                   "forcats", "globals", "abind", "rmarkdown"))
```

## Run locally (`workflow_local.R`)

TODO
