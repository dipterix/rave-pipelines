#' File whose name starts with `shared-` will be automatically
#' loaded by the pipeline. Hence you can put global static objects
#' here, for example,
#'   - define functions that may be used multiple times,
#'   - declare variables that can be re-used
#'   - import packages that will be used via `library()`, or `targets::tar_option_set` (see below)
NULL


library(dipsaus)
library(raveio)
loadNamespace("filearray")
loadNamespace("ravetools")

# plot option
graphics_matplot_max_points <- 200000

# options(shiny.reactlog = TRUE)
