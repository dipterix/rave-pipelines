library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  local_reactives <- shiny::reactiveValues(
    results = NULL
  )
  # Register a loader screen
  ravedash::module_server_common(input, output, session, check_data_loaded = check_data_loaded, ...)

  server_loader(input, output, session, ...)
  module_server(input, output, session, ...)



  # shiny::observe({
  #   print(shiny::reactiveValuesToList(tools$theme_event))
  # })


  get_inputs <- shiny::debounce(shiny::reactive({
    shiny::reactiveValuesToList(input)
  }), millis = 200, priority = 100)

  shiny::observe({
    if(!isTRUE(input$auto_recalculate)){ return() }
    get_inputs()
    ravedash::fire_rave_event(key = 'run_analysis', value = Sys.time())
  })



  shiny::observe({

    if(length(local_reactives$results)) {
      try({
        old_results <- shiny::isolate(local_reactives$results)
        old_results$valid <- FALSE
        if(old_results$process$is_alive()){
          ravedash::logger("Previous process invalidated", level = 'trace')
          old_results$process$kill_tree()
        }
      }, silent = TRUE)
    }

    # ravedash::logger("Run Analysis - Power Explorer", level = "info")

    # flush inputs to settings.yaml
    electrode_category_selector <- input$electrode_category_selector
    unit_of_analysis <- input$unit_of_analysis
    analysis_electrodes <- input$electrode_text
    condition_groups <- input$condition_groups
    global_baseline_choice <- input$global_baseline_choice
    baseline_windows <- unname(dipsaus::drop_nulls(lapply(input$baseline_windows, function(x){
      if(length(x$window_interval) == 2){ unname(unlist(x)) } else {NULL}
    })))
    analysis_lock <- input$analysis_lock
    analysis_ranges <- unname(lapply(input$analysis_ranges, function(x){
      list(
        frequency = unlist(x$frequency),
        time = unlist(x$time)
      )
    }))
    pipeline_set(
      electrode_category_selector = electrode_category_selector,
      unit_of_analysis = unit_of_analysis,
      analysis_electrodes = analysis_electrodes,
      condition_groups = condition_groups,
      global_baseline_choice = global_baseline_choice,
      baseline_windows = baseline_windows,
      analysis_lock = analysis_lock,
      analysis_ranges = analysis_ranges
    )


    results <- raveio::pipeline_run_async(
      pipe_dir = pipeline_path, type = "basic", progress_title = "Calculating in progress",
      use_future = TRUE, progress_max = 12, check_interval = 0.1, shortcut = TRUE, names = c(
        "settings", "baseline_windows", "unit_of_analysis",
        "analysis_electrodes", "electrode_category_selector", "analysis_ranges",
        "global_baseline_choice", "condition_groups", "analysis_lock",
        "requested_electrodes", "analysis_ranges_index", "cond_groups",
        "bl_power", "collapsed_data"
      ))


    local_reactives$results <- results
    ravedash::logger("Scheduled: Power Explorer", level = 'debug')

  }) |>
    shiny::bindEvent(
      ravedash::get_rave_event("run_analysis"),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  shiny::observe({
    results <- local_reactives$results
    if(!isTRUE(results$valid)){ return() }
    results$promise <- results$promise$then(
      onFulfilled = function(...){
        if(results$valid){
          ravedash::logger("Fulfilled: Power Explorer", level = 'debug')
          shidashi::clear_notifications(class = "pipeline-error")
          return(TRUE)
        } else {
          return("Pipeline invalidated.")
        }
      },
      onRejected = function(msg, ...){
        if(!results$valid){
          ravedash::logger("Abort previous results whatsoever because inputs have been changed", level = 'trace')
          return()
        }
        msg <- paste(msg, collapse = "\n")
        ravedash::logger(msg, level = 'error')
        shidashi::show_notification(
          message = msg,
          title = "Error while running pipeline", type = "danger",
          autohide = FALSE, close = TRUE, class = "pipeline-error"
        )
        return(msg)
      }
    )
  }, priority = 10) |>
    shiny::bindEvent(
      local_reactives$results,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  output$collapse_over_trial <- shiny::renderPlot({

    local_reactives$results$promise |>
      promises::then(
        function(...){
          collapsed_data <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "collapsed_data")
          repository <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "repository")

          time_points <- repository$time_points
          frequencies <- repository$frequency

          data <- collapsed_data[[1]]$collasped$range_1
          image(t(data$freq_time), x = time_points[data$cube_index$Time], y = frequencies[data$cube_index$Frequency])
        },
        function(...){}
      )
    # shiny::validate(shiny::need(isFALSE(local_reactives$pipeline_errored), message = "One or more errors while executing pipeline. Please check the notification."))

  }) |>
    shiny::bindEvent(
      local_reactives$results,
      ignoreNULL = TRUE, ignoreInit = TRUE
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
