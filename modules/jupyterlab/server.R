library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  server_loader(input, output, session, ...)
  module_server(input, output, session, ...)



  # shiny::observe({
  #   print(shiny::reactiveValuesToList(tools$theme_event))
  # })




  # shiny::observe({
  #   ravedash::get_rave_event("run_analysis")
  #   ravedash::logger("Run Analysis", level = "info")
  # })

  # output[['_module_ui_']] <- shiny::renderUI({
  #
  #   shiny::validate(
  #     shiny::need(
  #       length(local_reactives$open_loader),
  #       message = "Initializing..."
  #     )
  #   )
  #   module_ui_main()
  #   if(isFALSE(local_reactives$open_loader)){
  #     return(module_ui_main())
  #   } else {
  #     return(module_ui_loader())
  #   }
  #
  # })

}
