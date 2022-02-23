library(ravedash)
# global variables for the module

# Stores global variables
pipeline_name <- "jupyterlab"
module_id <- "jupyterlab"
debug <- TRUE


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

  if(isTRUE(raveio::raveio_getopt("jupyter_disabled"))){
    return(FALSE)
  }
  port <- raveio::raveio_getopt("jupyter_port", default = 17284)

  tryCatch({
    jupyter_list <- jupyter_server_status(port = port, force = FALSE, verbose = FALSE)
    return(jupyter_list$alive)
  }, error = function(e){
    ravedash::logger("Encountered the following error while checking Jupyter servers: {e$message}", level = "error")
    FALSE
  })
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
        raveio::pipeline_root(c("./_pipelines", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
      } else {
        raveio::pipeline_root(c(".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
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


local_ips <- dipsaus::get_ip(get_public = FALSE)
local_ips <- local_ips$available[-2]

local_data <- dipsaus::fastmap2()
dipsaus::list_to_fastmap2(list(
  host = pipeline_get("host", missing = local_ips),
  port = pipeline_get("port", missing = 17284, constraint = c(17284, 1000:65535)),
  token = pipeline_get("token", missing = '')
), map = local_data)


jupyter_server_status <- function(port, force = FALSE, verbose = TRUE){
  jupyter_alive <- FALSE
  jupyter_instances <- NULL
  tryCatch({
    jupyter_instances <- rpymat::jupyter_server_list()
    sel <- jupyter_instances$port %in% port
    if(any(sel)){
      if( verbose ){
        ravedash::logger("Jupyter is running at port: {port}")
      }

      if(force){
        if( verbose ){
          ravedash::logger("Force stopping this instance and restart a new one")
        }
        rpymat::jupyter_server_stop(port)
      } else {
        jupyter_alive <- TRUE
      }
    }
  }, error = function(e){
    if( verbose ){
      ravedash::logger("No jupyter server found. Will start a new one")
    }
  })
  list(
    alive = jupyter_alive,
    instances = jupyter_instances
  )
}
