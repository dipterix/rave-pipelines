library(shiny)

# Debug
if(FALSE){
  template_settings$set(
    'root_path' = "inst/template/"
  )
}


if(system.file(package = 'raveio') != ""){
  if(dir.exists("./_pipelines")) {
    raveio::pipeline_root(c("./_pipelines", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
    print(raveio::pipeline_root())
  } else {
    raveio::pipeline_root(c(".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
  }
}

server <- function(input, output, session){

  # Sync input ID
  shared_data <- shidashi::register_session_id(session)
  shared_data$enable_broadcast()
  shared_data$enable_sync()

  tools <- ravedash::register_rave_session(session = session)

  timer <- shiny::reactiveTimer(intervalMs = 1000)
  shiny::observe({
    nevents <- length(tools$loop_event)

    if(nevents){
      lapply(seq_len(min(3, nevents)), function(i){
        callback <- tools$loop_event$remove(missing = NULL)
        if(is.function(callback)){

          tryCatch({
            callback()
          }, error = function(e){

            try({
              logger::log_warn("Error `{e$message}` while trying to run callback function:\n",
                               deparse(callback))
            }, silent = TRUE)

          })

        }
      })
    }
  }) |>
    shiny::bindEvent(timer())



  # Fixed usage, call modules
  shiny::observe({
    try({
      req <- list(QUERY_STRING = session$clientData$url_search)
      resource <- shidashi::load_module(request = req)
      if(resource$has_module){

        module_table <- shidashi::module_info()
        module_table <- module_table[module_table$id %in% resource$module$id, ]
        if(nrow(module_table)){
          group_name <- as.character(module_table$group[[1]])
          if(is.na(group_name)){
            group_name <- "<no group>"
          }
          if(system.file(package = "logger") != ''){
            logger::log_info("Loading - { module_table$label[1] } ({group_name}/{ module_table$id })")
          }
          shiny::moduleServer(resource$module$id, resource$module$server, session = session)
        }
      }
    })
  }) |>
    shiny::bindEvent(session$clientData$url_search, ignoreNULL = TRUE)
}
