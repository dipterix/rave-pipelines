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

  tools <- ravedash::register_rave_session(session = session)


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
            ravedash::logger(
              level = "info",
              "Loading - { module_table$label[1] } ({group_name}/{ module_table$id })"
            )
          }
          tools$rave_event$active_module <- list(
            id = module_table$id,
            label = module_table$label[1]
          )
          shiny::moduleServer(resource$module$id, resource$module$server, session = session)
        }
      }
    })
  }) |>
    shiny::bindEvent(session$clientData$url_search, ignoreNULL = TRUE)

  shiny::observe({
    tab_info <- input[["@shidashi_page@"]]
    id <- sub("^tab-module-", "", tab_info$id)
    id <- sub("-shared_id-[a-zA-Z0-9]+$", "", id)
    tools$rave_event$active_module <- list(
      id = id,
      label = tab_info$text
    )
  }) |>
    shiny::bindEvent(input[["@shidashi_page@"]], ignoreNULL = TRUE, ignoreInit = FALSE)

}
