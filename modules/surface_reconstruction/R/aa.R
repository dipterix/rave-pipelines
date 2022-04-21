library(ravedash)
# global variables for the module

# Stores global variables. These are required
pipeline_name <- "surface_reconstruction"
pipeline_settings_file <- "settings.yaml"
module_id <- "surface_reconstruction"
debug <- TRUE

# Variables that could be accessed within shiny module (not include pipeline)
global_data <- dipsaus::fastmap2()

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
  FALSE
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


# Check if dcm2niix is missing
global_data$flirt_path <- raveio::raveio_getopt("flirt_path", default = Sys.which("flirt"))

normalize_dcm2niix_path <- function(path) {
  if(length(path) != 1 || trimws(path) == ""){
    return("")
  }
  if(!file.exists(path)) {
    return("")
  }
  path <- normalizePath(path)
  try({
    suppressWarnings({
      res <- system2(path, args = "--version", wait = TRUE, stdout = TRUE, stderr = TRUE)
    })
    if(any(grepl("dcm2niix", res, ignore.case = TRUE))) {
      return(path)
    }
  }, silent = TRUE)
  return("")

}

global_data$dcm2niix_path <- normalize_dcm2niix_path(
  raveio::raveio_getopt("dcm2niix_path", default = Sys.which("dcm2niix")))

normalize_freesurfer_path <- function(path) {
  if(length(path) != 1 || trimws(path) == ""){
    return("")
  }
  if(!dir.exists(path)) {
    return("")
  }
  path <- normalizePath(path)
  recon_all <- file.path(path, "bin", "recon-all")

  if(file.exists(recon_all)){
    res <- system2(
      command = recon_all,
      args = "--version",
      env = c(sprintf(
        "FREESURFER_HOME=%s", shQuote(path, type = "cmd")
      )),
      wait = TRUE, stdout = TRUE, stderr = TRUE
    )
    if(grepl("freesurder", res, ignore.case = TRUE)) {
      return(path)
    }
  }

  return("")
}

global_data$freesurfer_path <- normalize_freesurfer_path(
  raveio::raveio_getopt("freesurfer_path", default = {
    Sys.getenv("FREESURFER_HOME", unset = local({
      fs <- c(
        "/Applications/freesurfer",
        "/usr/local/freesurfer"
      )
      fs <- fs[dir.exists(fs)]
      if(length(fs)) { fs[[1]] } else { "" }
    }))
  })
)

