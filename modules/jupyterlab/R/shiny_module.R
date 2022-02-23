
module_ui_main <- function(){
  shiny::uiOutput(ns("jupyter_iframe"), style = "width:100%; height:100vh")
}

module_server <- function(input, output, session, ...){

  output$jupyter_iframe <- shiny::renderUI({
    data_loaded <- ravedash::watch_data_loaded()
    if(data_loaded){

      port <- raveio::raveio_getopt("jupyter_port", default = 17284)
      jupyter_list <- jupyter_server_status(port = port, force = FALSE, verbose = FALSE)
      instance <- jupyter_list$instances[
        jupyter_list$instances$port == port
      ]

      jupyter_url <- sprintf("http://%s:%s/jupyter/lab?token=%s",
                             instance$host, instance$port, instance$token)

      shiny::tags$iframe(
        src = jupyter_url,
        width = "100%",
        height = "100%"
      )

    }
  })

}
