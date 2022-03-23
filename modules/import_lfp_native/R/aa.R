library(ravedash)
# global variables for the module

# Stores global variables
pipeline_name <- "import_lfp_native"
module_id <- "import_lfp_native"
debug <- TRUE

check_data_loaded <- function(first_time = FALSE){
  FALSE
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
          set = function(..., .list = NULL){
            args <- c(list(...), as.list(.list))
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
            raveio::save_yaml(x = settings, file = pipeline_settings_path, sorted = TRUE)
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

    }

  }

}, error = function(e){
  ravedash::logger(e$message, level = "warning")
})



