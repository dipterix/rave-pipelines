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
  } else {
    raveio::pipeline_root(c(".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
  }
}

server <- function(input, output, session){

  # Sync input ID
  shared_data <- shidashi::register_session_id(session)
  shared_data$enable_broadcast()
  shared_data$enable_sync()

  # tools <- ravedash::register_rave_session(session = session)

  # Fixed usage, call modules
  shiny::observe({
    try({
      req <- list(QUERY_STRING = session$clientData$url_search)
      ravedash::logger("GET request: /{req$QUERY_STRING}", level = "trace", use_glue = TRUE)

      parse_env <- new.env(parent = globalenv())

      resource <- shidashi::load_module(request = req, env = parse_env)
      if(resource$has_module){

        module_table <- shidashi::module_info()
        module_table <- module_table[module_table$id %in% resource$module$id, ]
        if(nrow(module_table)){
          group_name <- as.character(module_table$group[[1]])
          if(is.na(group_name)){
            group_name <- "<no group>"
          }
          if(system.file(package = "logger") != ''){
            ravedash::logger(
              level = "info",
              "Loading - { module_table$label[1] } ({group_name}/{ module_table$id })",
              use_glue = TRUE
            )
          }
          rave_action <- list(
            type = "active_module",
            id = module_table$id,
            label = module_table$label[1]
          )
          # ravedash::fire_rave_event(key = rave_action$type, value = rave_action)
          # ravedash::logger("[{rave_action$type}] (rave-action).", level = "trace", use_glue = TRUE)
          shiny::moduleServer(resource$module$id, function(input, output, session, ...){

            # ravedash::register_rave_session(session = session)

            # Register a common screen
            ravedash::module_server_common(
              resource$module$id,
              check_data_loaded = parse_env$check_data_loaded,
              session = session,
              parse_env = parse_env,
              ...
            )

            resource$module$server(input, output, session, ...)

          }, session = session)
        }
      }
    })
  }) |>
    shiny::bindEvent(session$clientData$url_search, ignoreNULL = TRUE)

  # session$allowReconnect("force")


}
