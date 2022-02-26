library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  # Register a loader screen
  ravedash::module_server_common(input, output, session, check_data_loaded = check_data_loaded, ...)

  server_loader(input, output, session, ...)
  module_server(input, output, session, ...)



  # shiny::observe({
  #   print(shiny::reactiveValuesToList(tools$theme_event))
  # })


  get_inputs <- shiny::debounce(shiny::reactive({
    if(!isTRUE(input$auto_recalculate)){ return() }
    shiny::reactiveValuesToList(input)
  }), millis = 100, priority = 100)

  shiny::observe({
    ravedash::fire_rave_event(key = 'run_analysis', value = Sys.time())
  }) |>
    shiny::bindEvent(
      get_inputs(),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

}
