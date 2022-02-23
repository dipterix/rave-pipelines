jupyter_server_status <- function(port, force = FALSE){
  jupyter_alive <- FALSE
  jupyter_instances <- NULL
  tryCatch({
    jupyter_instances <- rpymat::jupyter_server_list()
    sel <- jupyter_instances$port %in% port
    if(any(sel)){
      ravedash::logger("Jupyter is running at port: {port}")

      if(force){
        ravedash::logger("Force stopping this instance and restart a new one")
        rpymat::jupyter_server_stop(port)
      } else {
        jupyter_alive <- TRUE
      }
    }
  }, error = function(e){
    ravedash::logger("No jupyter server found. Will start a new one")
  })
  list(
    alive = jupyter_alive,
    instances = jupyter_instances
  )
}