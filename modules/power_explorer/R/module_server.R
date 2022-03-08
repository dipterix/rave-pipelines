
module_server <- function(input, output, session, ...){

  repository <- list()
  local_reactives <- shiny::reactiveValues(
    refresh = NULL,
    results = NULL,
    update_outputs = NULL
  )

  server_tools <- get_default_handlers(session = session)
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text", "baseline_choices",
      "analysis_ranges", "condition_groups"
    ))
  )

  ravedash::safe_observe({

    if(length(shiny::isolate(local_reactives$results))) {
      local_reactives$results$invalidate()
      ravedash::logger("Invalidating previous run", level = "trace")
    }

    # ravedash::logger("Run Analysis - Power Explorer", level = "info")

    # flush inputs to settings.yaml
    preset_settings <- component_container$collect_settings(ids = c(
      "electrode_text", "baseline_choices", "condition_groups", "analysis_ranges"
    ))

    pipeline_set(.list = preset_settings)

    # shidashi::card_operate(title = "Select Electrodes", method = "collapse")
    results <- raveio::pipeline_run(
      pipe_dir = pipeline_path,
      scheduler = "none",
      type = "smart",
      callr_function = NULL,
      progress_title = "Calculating in progress",
      async = TRUE,
      check_interval = 0.1,
      shortcut = TRUE,
      names = c(
        "settings",
        # "baseline_windows", "unit_of_analysis", "analysis_electrodes", "electrode_category_selector", "analysis_ranges", "global_baseline_choice", "condition_groups", "analysis_lock",
        names(preset_settings),
        "requested_electrodes", "analysis_ranges_index", "cond_groups",
        "bl_power", "collapsed_data"
      )
    )
    # results <- raveio::pipeline_run_async(
    #   pipe_dir = pipeline_path, type = "basic", progress_title = "Calculating in progress",
    #   use_future = TRUE, progress_max = 12, check_interval = 0.1, shortcut = TRUE, )


    local_reactives$results <- results
    ravedash::logger("Scheduled: Power Explorer", level = 'debug', reset_timer = TRUE)

    results$promise$then(
      onFulfilled = function(...){
        ravedash::logger("Fulfilled: Power Explorer", level = 'debug')
        shidashi::clear_notifications(class = "pipeline-error")
        local_reactives$update_outputs <- Sys.time()
        return(TRUE)
      },
      onRejected = function(e, ...){
        msg <- paste(e$message, collapse = "\n")
        if(inherits(e, "error")){
          ravedash::logger(msg, level = 'error')
          ravedash::logger(traceback(e), level = 'error', .sep = "\n")
          shidashi::show_notification(
            message = msg,
            title = "Error while running pipeline", type = "danger",
            autohide = FALSE, close = TRUE, class = "pipeline-error"
          )
          # } else {
          #   ravedash::logger(msg, level = 'debug')
        }
        return(msg)
      }
    )

    return()

  }) |>
    shiny::bindEvent(
      server_tools$run_analysis_flag(),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  # Preset UI server functions
  # electrode_selector$server_func(input = input, output = output, session = session)



  ravedash::safe_observe({
    # local_reactives$update_outputs <- NULL
    shidashi::reset_output("collapse_over_trial")
  }) |>
    shiny::bindEvent(
      local_reactives$refresh,
      ignoreNULL = TRUE
    )

  output$collapse_over_trial <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
          isTRUE(shiny::isolate(local_reactives$results$valid)),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )

    collapsed_data <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "collapsed_data")
    repository <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "repository")

    time_points <- repository$time_points
    frequencies <- repository$frequency

    data <- collapsed_data[[1]]$collasped$range_1
    image(t(data$freq_time), x = time_points[data$cube_index$Time], y = frequencies[data$cube_index$Frequency])

  })


  ravedash::safe_observe({
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }
    new_repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
    if(!inherits(new_repository, "rave_prepare_power")){
      ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
      return()
    }
    ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

    # check if the repository has the same subject as current one
    if(inherits(repository, "rave_prepare_power")){

      if( !attr(loaded_flag, "force") &&
          identical(repository$signature, new_repository$signature) ){
        ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
        return()
      }
    }

    # TODO: Make a function to reset UIs to default
    repository <<- new_repository
    component_container$reset_data()
    component_container$data$repository <- new_repository
    component_container$initialize_with_new_data()
    local_reactives$refresh <- Sys.time()

  }, priority = 1001) |>
    shiny::bindEvent(
      ravedash::watch_data_loaded(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )




}
