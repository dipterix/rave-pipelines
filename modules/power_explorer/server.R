library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  ravedash::register_rave_session(session = session)

  server_loader(input, output, session, ...)
  module_server(input, output, session, ...)


  local_reactives <- shiny::reactiveValues(
    first_time = TRUE
  )


  handler_on_data_changed <- shiny::observe({
    # Check whether is there any missing data for this module
    check_results <- FALSE
    tryCatch({

      shiny::withLogErrors({
        if(length(formals(check_data_loaded)) == 0){
          check_results <- isTRUE(check_data_loaded())
        } else {
          check_results <- isTRUE(check_data_loaded(shiny::isolate(local_reactives$first_time)))
        }

        local_reactives$first_time <- FALSE
        if( check_results ){
          ravedash::logger("Checking whether data has been loaded: YES")
        } else {
          ravedash::logger("Checking whether data has been loaded: NO")
        }
      })
    }, error = function(e){
      ravedash::logger("Found an error while checking data...", level = "warning")
      ravedash::logger(paste(e$message, sep = "\n", collapse = "\n"), level = "error")
      msg <- paste(utils::capture.output({
        if(length(e$call)){
          cat("Error in ", deparse1(e$call), ": ", sep = "")
        } else {
          cat("Error: ")
        }
        cat(e$message, "\nTraceback:\n")
        traceback(e)
      }), collapse = "\n")
      shidashi::show_notification(
        title = "Error found!", autohide = FALSE, close = TRUE, type = 'danger',
        message = shiny::div(
          "Found an error while trying to check this module. The debug message is displayed below.",
          shiny::hr(),
          shiny::pre(msg)
        )
      )
    })

    # print(check_results)
    local_reactives$check_results <- check_results
    if(isTRUE(check_results)){
      shidashi::clear_notifications()
      ravedash::logger("Skip loader interface", level = "debug")
      ravedash::close_loader()
      ravedash::fire_rave_event('data_loaded', Sys.time())
    } else {
      ravedash::logger("Opening loader interface")
      ravedash::open_loader()
      ravedash::fire_rave_event('data_loaded', FALSE)
    }
  }) |>
    shiny::bindEvent(
      ravedash::get_rave_event("data_changed"),
      ignoreInit = FALSE, ignoreNULL = FALSE
    )

  shiny::observe({

    if(!ravedash::watch_loader_opened()){
      ravedash::open_loader()
    } else if(isTRUE(local_reactives$check_results)){
      ravedash::close_loader()
    }
  }) |>
    shiny::bindEvent(input$loader_show, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$loader_short_message <- shiny::renderText({
    msg <- paste(ravedash::get_rave_event('loader_message'), collapse = "")
    if(msg == ""){
      msg <- "Toggle data loader"
    }
    msg
  })

  add_html_class <- function(selector, class,
                             session = shiny::getDefaultReactiveDomain()){
    session$sendCustomMessage("shidashi.add_class", list(
      selector = selector,
      class = class
    ))
  }
  remove_html_class <- function(selector, class,
                             session = shiny::getDefaultReactiveDomain()){
    session$sendCustomMessage("shidashi.remove_class", list(
      selector = selector,
      class = class
    ))
  }
  handler_open_loader <- shiny::observe({
    # Listen to a global event on whether data has changed
    loader_open <- ravedash::watch_loader_opened()
    if(loader_open){
      local_reactives$open_loader <- Sys.time()
      add_html_class(".module_main_ui", "soft-hidden")
      remove_html_class(".module_loader_ui", "soft-hidden")
    } else {
      local_reactives$open_loader <- FALSE
      add_html_class(".module_loader_ui", "soft-hidden")
      remove_html_class(".module_main_ui", "soft-hidden")
    }
  }) |>
    shiny::bindEvent(
      ravedash::watch_loader_opened(),
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

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
