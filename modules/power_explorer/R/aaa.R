library(ravedash)
# global variables for the module

# Stores global variables
pipeline_name <- "power_explorer"
module_id <- "power_explorer"
debug <- TRUE

baseline_choices <- c("% Change Power", "% Change Amplitude", "z-score Power", "z-score Amplitude", "Decibel")
baseline_along_choices <- c("Per frequency, trial, and electrode", "Across electrode", "Across trial", "Across trial and electrode")
analysis_lock_choices <- c("Unlocked", "Lock frequency", "Lock time")
max_analysis_ranges <- 2
gray_label_color <- "#c8c9ca"

auto_recalculate_onchange <- c(
  "merge_hemisphere_labels",
  "analysis_lock",
  "baseline_windows",
  "global_baseline_choice",
  "unit_of_analysis",
  "analysis_ranges",
  "electrode_text",
  "condition_groups"
)

#' Function to check whether data is loaded.
#' @param first_time whether this function is run for the first time
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_ui_main} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{module_ui_loader}.
#' @return Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){
  re <- tryCatch({
    repo <- raveio::pipeline_read('repository', pipe_dir = pipeline_path)
    short_msg <- sprintf("%s [%s, %s]", repo$subject$subject_id, repo$epoch_name, repo$reference_name)
    ravedash::fire_rave_event('loader_message', short_msg)
    TRUE
  }, error = function(e){
    ravedash::fire_rave_event('loader_message', NULL)
    FALSE
  })
  # if(first_time){
  #   ravedash::fire_rave_event('loader_message', NULL)
  #   re <- FALSE
  # }
  re
}

# ----------- Some Utility functions for modules -----------


if(exists('debug') && isTRUE(get('debug'))){
  assign(".module_debug", environment(), envir = globalenv())
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}

# Get pipeline
tryCatch({
  if(exists('pipeline_name')){
    if(system.file(package = 'raveio') != ""){
      if(dir.exists("./_pipelines")) {
        raveio::pipeline_root(c("./modules", "./_pipelines", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
      } else {
        raveio::pipeline_root(c("./modules", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
      }
    }
    pipeline_name <- get('pipeline_name')

    if(length(pipeline_name) == 1 && !is.na(pipeline_name)) {
      pipeline_path <- raveio::pipeline_find(pipeline_name)
      pipeline_settings_path <- file.path(pipeline_path, "settings.yaml")
      pipeline_settings <- local({
        settings <- raveio::load_yaml(pipeline_settings_path)
        list(
          set = function(...){
            args <- list(...)
            argnames <- names(args)
            if(!length(argnames)){
              return(as.list(settings))
            }
            args <- args[argnames != ""]
            argnames <- names(args)
            if(!length(argnames)){
              return(as.list(settings))
            }
            for(nm in argnames){
              settings[[nm]] <<- args[[nm]]
            }
            raveio::save_yaml(x = settings, file = pipeline_settings_path)
            return(as.list(settings))
          },
          get = function(key, default = NA){
            if(missing(key)){
              return(as.list(settings))
            }
            if(!settings$`@has`(key)){
              return(default)
            }
            settings[[key]]
          }
        )
      })
      pipeline_set <- pipeline_settings$set
      pipeline_get <- function(key, missing = NULL, constraint){
        re <- pipeline_settings$get(key, missing)
        if(!missing(constraint)){
          re <- re %OF% constraint
        }
        re
      }

      component_container <- ravedash:::RAVEShinyComponentContainer$new(
        module_id = module_id, pipeline_name = pipeline_name,
        settings_file = "settings.yaml"
      )
    }

  }

}, error = function(e){
  ravedash::logger(e$message, level = "warning")
})



