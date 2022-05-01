library(ravedash)
# global variables for the module

# Stores global variables. These are required
pipeline_name <- "electrode_localization"
pipeline_settings_file <- "settings.yaml"
module_id <- "electrode_localization"
debug <- TRUE

options(shiny.maxRequestSize = 300 * 1024 ^ 2)

#' Function to check whether data is loaded.
#' @param first_time whether this function is run for the first time
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_html} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{loader_html}.
#' @return Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){
  brain <- raveio::pipeline_read('brain', pipe_dir = pipeline_path)
  if(first_time || is.null(brain)) {
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  } else {
    ct_exists <- raveio::pipeline_read('ct_exists', pipe_dir = pipeline_path)
    subject <- raveio::pipeline_read('subject', pipe_dir = pipeline_path)
    if(isTRUE(ct_exists)) {
      ravedash::fire_rave_event('loader_message', sprintf("Localizing [%s] with CT", subject$subject_id))
    } else {
      ravedash::fire_rave_event('loader_message', sprintf("Localizing [%s] without CT", subject$subject_id))
    }
    return(TRUE)
  }
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}

# Register RAVE pipeline, this will give you:
# `pipeline_set`: set variables in the pipeline
# `pipeline_get`: get variables in the pipeline
# `pipeline_settings_path`: where the settings file locate
# `pipeline_path`: parent directory path of the pipeline
register_pipeline(pipeline_name = pipeline_name,
                  settings_file = pipeline_settings_file,
                  env = environment())


