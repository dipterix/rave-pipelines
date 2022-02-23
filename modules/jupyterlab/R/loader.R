# .module_id  <-  "jupyterlab"
# if(interactive() && !dipsaus::shiny_is_running()){
#   setwd(rstudioapi::getActiveProject())
#   source('./modules/jupyterlab/R/aaa.R')
# }


module_ui_loader <- function(session = shiny::getDefaultReactiveDomain()){

  ravedash::simple_layout(
    {
      shiny::div(
        shiny::p("Jupyter has been disabled. Please check RAVE settings, or ask your system administrator for help.")
      )
    }, {}, input_width = 10L
  )


}

server_loader <- function(input, output, session, ...){
  # shiny::observe({
  #   # TODO: validate inputs
  #
  #   host = input[['loader_host']]
  #   port = input[['loader_port']]
  #   force = input$loader_force
  #   token = input$loader_token
  #
  #   # gather information
  #   pipeline_set(
  #     host = host,
  #     port = port,
  #     force = force,
  #     token = token
  #   )
  #
  #
  #   dipsaus::shiny_alert2(
  #     title = "Launching Jupyter-lab in progress",
  #     text = paste(
  #       "Everything takes time. This should take less time than others."
  #     ), icon = "info", auto_close = FALSE, buttons = FALSE
  #   )
  #
  #   tryCatch({
  #
  #     jupyter_list <- jupyter_server_status(port = port, force = force, verbose = TRUE)
  #     if(!isTRUE(jupyter_list$alive)){
  #       rpymat::jupyter_launch(
  #         workdir = raveio::raveio_getopt("data_dir"),
  #         host = host, port = port, token = token,
  #         open_browser = TRUE, async = TRUE, use_rs = TRUE
  #       )
  #       # wait the server to launch
  #       Sys.sleep(3)
  #       for(i in 1:10){
  #         jupyter_list <- jupyter_server_status(port = port, force = FALSE, verbose = FALSE)
  #         if(jupyter_list$alive){
  #           break
  #         }
  #         Sys.sleep(1)
  #       }
  #     }
  #     instance <- jupyter_list$instances[
  #       jupyter_list$instances$port == port
  #     ]
  #     if(length(instance)){
  #       local_data$host <- instance$host
  #       local_data$port <- instance$port
  #       local_data$token <- instance$token
  #     } else {
  #       stop("Cannot find Jupyterlab instance. Please check your firewall settings.")
  #     }
  #
  #     # raveio::pipeline_run(pipe_dir = pipeline_path, type = "basic")
  #     ravedash::fire_rave_event('data_changed', Sys.time())
  #     ravedash::logger("Jupyterlab has been launched successfully")
  #     dipsaus::close_alert2()
  #   }, error = function(e){
  #     dipsaus::close_alert2()
  #     dipsaus::shiny_alert2(
  #       title = "Errors",
  #       text = paste(
  #         "Found an error while creating Jupyterlab server instance:\n\n",
  #         paste(e, collapse = "\n")
  #       ),
  #       icon = "error",
  #       danger_mode = TRUE,
  #       auto_close = FALSE
  #     )
  #   })
  #
  # }) |>
  #   shiny::bindEvent(input$loader_run, ignoreNULL = TRUE, ignoreInit = TRUE)



}
