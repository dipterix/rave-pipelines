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
  # shared_data$enable_broadcast()
  # shared_data$enable_sync()

  # Set max upload file size to be 300MB by default
  if(!isTRUE(getOption('shiny.maxRequestSize', 0) > 0)) {
    options(shiny.maxRequestSize = 300 * 1024 ^ 2)
  }
  is_single_session <- isTRUE(getOption("ravedash.single.session", default = FALSE))

  # # Register bindings for compound input
  # dipsaus::registerInputBinding('textOutput', 'shiny', 'shiny.textOutput', update_function = NULL)

  # tools <- ravedash::register_rave_session(session = session)

  # Fixed usage, call modules
  shiny::bindEvent(
    ravedash::safe_observe({
      query_string <- session$clientData$url_search
      if(length(query_string) != 1) {
        query_string <- "/"
      }

      ravedash::logger("GET request: /{query_string}", level = "trace", use_glue = TRUE)

      # query_string <- "/?type=widget&output_id=aaaa&rave_id=NAXzMcGKxoqwFeCjswfX"
      query_list <- httr::parse_url(query_string)

      parse_env <- new.env(parent = globalenv())
      resource <- shidashi::load_module(request = list(QUERY_STRING = query_string),
                                        env = parse_env)
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
      } else {
        # No module, render rave_options
        if(!isTRUE(raveio::raveio_getopt(key = "secure_mode", default = FALSE))) {
          source("./R/rave-options.R", local = parse_env)
          shiny::moduleServer("._raveoptions_.", parse_env$rave_option_server)


          shiny::bindEvent(
            ravedash::safe_observe({
              ravedash::shutdown_session(session = session)
            }),
            input$ravedash_shutdown,
            ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE
          )

          shiny::bindEvent(
            ravedash::safe_observe({
              shiny::showModal(
                shiny::modalDialog(
                  title = "Power-off RAVE",
                  easyClose = TRUE, size = "m",
                  footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    dipsaus::actionButtonStyled(
                      inputId = "ravedash_shutdown",
                      label = "Shutdown",
                      icon = ravedash::shiny_icons[["power-off"]]
                    )
                  ),
                  "This will shutdown the RAVE server. Please press the [Shutdown] button to proceed."
                )
              )
            }),
            input$ravedash_shutdown_btn,
            ignoreNULL = TRUE, ignoreInit = TRUE
          )

          if( is_single_session ) {
            ravedash::logger("Single-session mode is ON: closing the website tab will shutdown the RAVE application.", level = "info")
            session$onEnded(function() {
              shiny::stopApp()
            })
          } else {
            ravedash::logger("Single-session mode is OFF: closing the website tab will NOT shutdown the RAVE application. Please use the builtin shutdown button.", level = "info")
          }
        }
      }

    }),
    session$clientData$url_search, ignoreNULL = TRUE
  )

}
