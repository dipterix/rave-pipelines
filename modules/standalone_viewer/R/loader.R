# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::uiOutput(ns("viewer"), container = function(...) {
    shiny::div(
      class = "no-padding no-margin",
      style = "width:100vw; height:100vh",
      ...
    )
  })
}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  local_data <- shiny::reactiveValues(
    module_id = NULL,
    output_id = NULL,
    ui_function = NULL,
    renderer = NULL
  )



  output$viewer <- shiny::renderUI({
    # query_string <- "/?type=widget&output_id=aaaa&rave_id=NAXzMcGKxoqwFeCjswfX&module=standalone_viewer&ui=threeBrain%3A%3AthreejsBrainOutput&renderer=threeBrain%3A%3ArenderBrain"
    query_string <- session$clientData$url_search

    query_list <- httr::parse_url(query_string)
    stopifnot(identical(query_list$query$type, "widget"))

    # This is a widget that should belongs to some module

    output_id <- query_list$query$output_id
    if(length(output_id) != 1 || is.na(output_id) || !nchar(output_id)) {
      stop("The output ID is blank or invalid.")
    }

    ui_function <- query_list$query$ui
    if(!length(ui_function)) {
      stop("No UI function is specified")
    }
    if(!grepl("::", ui_function)) {
      stop("No package namespace specified in UI function")
    }
    ui_function <- strsplit(ui_function, "::")[[1]]
    ui_function <- asNamespace(ui_function[[1]])[[ui_function[[2]]]]

    renderer <- query_list$query$renderer
    if(!length(renderer)) {
      stop("No renderer is specified")
    }
    if(!grepl("::", renderer)) {
      stop("No package namespace specified in renderer")
    }
    renderer <- strsplit(renderer, "::")[[1]]
    renderer <- asNamespace(renderer[[1]])[[renderer[[2]]]]

    # get parent session
    parent_session <- ravedash::get_session_by_rave_id(query_list$query$rave_id)

    if(is.null(parent_session)) {
      stop("There is no shiny-session with provided `rave_id`.")
    }
    module_info <- ravedash::get_active_module_info(session = parent_session)
    if(is.null(module_info)) {
      stop("There is a shiny-session with provided `rave_id`. However, the module information is broken")
    }
    module_id <- module_info$id

    ravedash::logger("Preparing standalone viewer for output `{output_id}` (module: module_id)", level = "trace", use_glue = TRUE)

    local_data$module_id <- module_id
    local_data$output_id <- output_id
    local_data$ui_function <- ui_function
    local_data$renderer <- renderer

    tryCatch({
      ui_function(shiny::NS(module_id, output_id), width = "100vw", height = "100vh")
    }, error = function(e) {
      ui_function(shiny::NS(module_id, output_id))
    })
  })



}
