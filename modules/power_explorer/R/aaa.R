# global variables for the module

# Stores global variables
local_reactives <- shiny::reactiveValues()
pipeline_name <- "power_explorer"
debug <- TRUE

#' Function to check whether data is loaded.
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_ui_main} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{module_ui_loader}.
#' @return Logical variable of length one.
check_data_loaded <- function(){
  reg <- ravedash::register_rave_session(session = session)
  return(isTRUE(reg$rave_event$data_loaded))
}

# ----------- Some Utility functions for modules -----------

`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}

card <- shidashi::card

get_projects <- local({
  re <- NULL
  function(refresh = FALSE){
    if(refresh || !length(re)){
      re <<- rave::get_projects()
    }
    re
  }
})

column_md <- function(width, ..., class = ""){
  if (!is.numeric(width) || (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  colClass <- paste0("col-md-", width)
  colClass <- shidashi:::combine_class(colClass, class)
  shiny::div(class = colClass, ...)
}

# Get pipeline
tryCatch({
  if(exists('pipeline_name')){
    if(system.file(package = 'raveio') != ""){
      if(dir.exists("./_pipelines")) {
        raveio::pipeline_root(c("./_pipelines", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
        print(raveio::pipeline_root())
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
    }

  }

}, error = function(e){
  logger::log_warn("[{pipeline_name}] ", e$message)
})


if(exists('debug') && isTRUE(get('debug'))){
  assign(".module_debug", environment(), envir = globalenv())
}


logger <- function(...){
  logger::log_info("[{pipeline_name}] ", ...)
}

