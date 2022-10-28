
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_data$runtime_env <- new.env()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # # Run analysis once the following input IDs are changed
  # # This is used by auto-recalculation feature
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text", "baseline_choices", "condition_groups", "analysis_ranges"
      ))
      settings <- list(
        analysis_electrodes = settings$analysis_electrodes,
        condition_groups = settings$condition_groups,
        downsample = input$downsample,
        analysis_event = input$analysis_event,
        baseline_settings = list(
          method = input$baseline_method,
          source = NULL,
          global = FALSE,
          window = input$baseline_window
        ),
        interpolate = list(
          method = input$interpolate_method,
          from = input$interpolate_from,
          to = input$interpolate_to
        )
      )
      pipeline$set_settings(.list = settings)

      results <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        progress_title = "Calculating in progress",
        async = TRUE,
        check_interval = 0.1,
        shortcut = TRUE,
        names = c(
          "settings_path", "settings", names(settings),
          "subject", "analysis_electrodes2", "baseline_settings2",
          "baseline_data", "interpolate_settings", "interpolated_data",
          "shifted_downsampled_data", "condition_groups2"
        )
      )

      ravedash::logger("Scheduled: ", pipeline$pipeline_name,
                       level = 'debug', reset_timer = TRUE)

      on_error <- function(e, ...){
        msg <- paste(e$message, collapse = "\n")
        if(inherits(e, "error")){
          ravedash::logger(msg, level = 'error')
          ravedash::logger(traceback(e), level = 'error', .sep = "\n")
          shidashi::show_notification(
            message = msg,
            title = "Error while running pipeline", type = "danger",
            autohide = FALSE, close = TRUE, class = "pipeline-error"
          )
        }
        return(msg)
      }

      results$promise$then(
        onFulfilled = function(...){

          # continue the rest of the pipeline
          tryCatch({
            shidashi::clear_notifications(class = "pipeline-error")
            list2env(pipeline$read(), local_data$runtime_env)

            if(!length(local_data$runtime_env$condition_groups2)) {
              stop("Please apecify at least one valid condition group.")
            }

            pipeline$eval(c(
              'plot_data_trial_by_time'
            ), env = local_data$runtime_env, clean = FALSE)

            ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
                             level = 'debug')

            local_reactives$update_outputs <- Sys.time()
          }, error = on_error)

          return(TRUE)
        },
        onRejected = on_error
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_subject_voltage_with_epoch")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_subject_voltage_with_epoch`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_subject_voltage_with_epoch")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # TODO: reset UIs to default

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      # DIPSAUS DEBUG START
      # new_repository <- pipeline$read("repository")

      # baseline_window
      time_windows <- sort(unlist(new_repository$time_windows)[1:2])
      baseline_settings <- pipeline$get_settings("baseline_settings")
      baseline_window <- c(time_windows[1], 0)
      if(is.list(baseline_settings) && length(baseline_settings$window) == 2) {
        baseline_window <- sort(unlist(baseline_settings$window))
      }
      shiny::updateSliderInput(
        session = session, inputId = "baseline_window",
        min = time_windows[[1]], max = time_windows[[2]],
        value = baseline_window)

      interpolate_from <- shiny::isolate(input$interpolate_from)
      interpolate_to <- shiny::isolate(input$interpolate_to)
      analysis_event <- shiny::isolate(input$analysis_event)
      event_names <- names(new_repository$epoch_table)
      event_names <- event_names[grepl("^Event_", event_names)]
      event_names <- gsub("^Event_", "", event_names)
      event_names <- unique(c("Onset", event_names))
      if(!isTRUE(interpolate_from %in% event_names)) {
        interpolate_from <- event_names[[1]]
      }
      if(!isTRUE(interpolate_to %in% event_names)) {
        interpolate_to <- event_names[[1]]
      }
      if(!isTRUE(analysis_event %in% event_names)) {
        analysis_event <- event_names[[1]]
      }
      shiny::updateSelectInput(session = session, inputId = "interpolate_from",
                               choices = event_names, selected = interpolate_from)
      shiny::updateSelectInput(session = session, inputId = "interpolate_to",
                               choices = event_names, selected = interpolate_to)
      shiny::updateSelectInput(session = session, inputId = "analysis_event",
                               choices = event_names, selected = analysis_event)

      downsample <- shiny::isolate(input$downsample)
      if(length(downsample) != 1 || is.na(downsample) || downsample < 1 || new_repository$sample_rate / downsample <= 1) {
        downsample <- 1
      } else if(downsample != round(downsample)) {
        downsample <- round(downsample)
      }
      shiny::updateNumericInput(session = session, inputId = "downsample", value = downsample, max = floor(new_repository$sample_rate / 2), label = sprintf("Down-sample signals (from %.0fHz)", new_repository$sample_rate))

      shiny::updateNumericInput(
        session = session,
        inputId = "collapse_over_trial_value_range",
        value = 0
      )
      shiny::updateNumericInput(
        session = session,
        inputId = "collapse_over_trial_time_range_min",
        value = time_windows[[1]]
      )
      shiny::updateNumericInput(
        session = session,
        inputId = "collapse_over_trial_time_range_max",
        value = time_windows[[2]]
      )


      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")


    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )





  # Register outputs
  ravedash::safe_observe({
    style <- input$collapse_over_trial_style %OF% c("Shared canvas", "Stack vertically")
    if(style == "Shared canvas") {
      style <- "shared"
    } else {
      style <- "vstack"
    }
    local_reactives$collapse_over_trial_style <- style

    vrange <- input$collapse_over_trial_value_range
    if(length(vrange) == 1 && !is.na(vrange) && vrange > 0) {
      vrange <- c(-1, 1) * vrange
      local_reactives$collapse_over_trial_value_range <- vrange
    } else {
      local_reactives$collapse_over_trial_value_range <- NULL
    }
    tmin <- input$collapse_over_trial_time_range_min
    tmax <- input$collapse_over_trial_time_range_max

    if(length(tmin) == 1 && length(tmax) == 1 &&
       !is.na(tmin) && !is.na(tmax) && tmin < tmax) {
      local_reactives$collapse_over_trial_time_range <- c(tmin, tmax)
    }
  })
  output$collapse_over_trial <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    plot_data <- local_data$runtime_env$plot_data_trial_by_time

    plot_trial_by_time_per_condtion(
      plot_data = plot_data,
      style = local_reactives$collapse_over_trial_style,
      time_range = local_reactives$collapse_over_trial_time_range,
      value_range = local_reactives$collapse_over_trial_value_range)

  })




}
