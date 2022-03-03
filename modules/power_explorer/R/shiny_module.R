
module_ui_main <- function(){

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll fancy-scroll-y",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              toggle_advanced = TRUE,
              class_header = "shidashi-anchor",
              title = shiny::tagList(
                "Select Electrodes ",
                shiny::textOutput(ns("card_electrode_selector"), inline = TRUE, shiny::tags$small)
              ),
              shiny::div(
                class = "rave-optional",
                shiny::selectInput(
                  inputId = ns("electrode_category_selector"),
                  label = "Electrode categories",
                  choices = ""
                ),
                shiny::selectInput(
                  inputId = ns("electrode_category_selector_choices"),
                  label = "Select electrode by category (multi-select)",
                  choices = "",
                  multiple = TRUE
                ),
                shiny::checkboxInput(
                  inputId = ns("merge_hemisphere_labels"),
                  label = "Merge LH/RH categories"
                )
              ),
              shiny::textInput(
                inputId = ns("electrode_text"),
                label = "Select electrode by number",
                value = "",
                placeholder = "E.g. 1-30,55-60,88"
              ),
              footer = shiny::div(
                class = "rave-optional",
                shiny::div(
                  class = "form-group",
                  shiny::actionLink(
                    inputId = ns("electrode_selectors_reset"),
                    label = "Reset electrode selectors"
                  )
                ),
                shiny::div(
                  class = "form-group",
                  shiny::downloadLink(
                    outputId = ns("electrode_csv_download"),
                    label = "Download copy of meta data for all electrodes"
                  )
                )
              )
            ),


            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = "Create condition contrasts",
              dipsaus::compoundInput2(
                inputId = ns("condition_groups"),
                label = "Group",
                initial_ncomp = 1L,
                min_ncomp = 1L,
                max_ncomp = 40L,
                label_color = gray_label_color,
                components = shiny::div(
                  shiny::textInput(inputId = "group_name", label = "Name"),
                  # shiny::selectInput(inputId = "group_conditions", label = NULL, choices = "", multiple = TRUE)
                  shinyWidgets::pickerInput(
                    inputId = "group_conditions", label = NULL,
                    choices = "", multiple = TRUE,
                    options = list(
                      "live-search" = TRUE,
                      "actions-box" = TRUE,
                      "size" = 4
                    )
                  )
                )
              )
            ),


            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = "Configure analysis",
              footer = list(
                ravedash::run_analysis_button(width = "100%",
                                              class = "margin-bottom-7"),
                shinyWidgets::prettyCheckbox(
                  inputId = ns("auto_recalculate"),
                  label = "Automatically re-calculate",
                  value = FALSE,
                  animation = "jelly",
                  width = "100%"
                )
              ),

              ravedash::flex_group_box(
                title = "Baseline settings",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("unit_of_analysis"),
                    label = "Electrode unit of analysis",
                    choices = baseline_choices,
                    selected = "Decibel"
                  ),
                  shiny::selectInput(
                    inputId = ns("global_baseline_choice"),
                    label = "Baseline method",
                    choices = baseline_along_choices
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::tags$label("Baseline windows"),
                  dipsaus::compoundInput2(
                    inputId = ns("baseline_windows"),
                    label = NULL,
                    initial_ncomp = 1L, min_ncomp = 1L,
                    label_color = gray_label_color,
                    components = {
                      shiny::sliderInput(
                        inputId = "window_interval",
                        label = NULL, width = "100%",
                        min = 0, max = 1, value = c(0, 0),
                        round = -2, post = " s", step = 0.01
                      )
                    }
                  )
                )
              ),


              ravedash::flex_group_box(
                title = "Analysis ranges",

                shidashi::flex_item(
                  # shiny::radioButtons(
                  #   inputId = ns("analysis_lock"),
                  #   label = "Lock selection",
                  #   choices = c("Unlocked", "Lock frequency", "Lock time"),
                  #   inline = TRUE
                  # )
                  shinyWidgets::radioGroupButtons(
                    inputId = ns("analysis_lock"),
                    label = "Lock range selector",
                    choices = analysis_lock_choices,
                    justified = TRUE,
                    checkIcon = list(
                      yes = shiny::icon("check")
                    ),
                    width = "100%",
                    size = "sm"
                  )

                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  dipsaus::compoundInput2(
                    inputId = ns("analysis_ranges"),
                    label = "Analysis range",
                    initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = max_analysis_ranges,
                    label_color = gray_label_color,
                    components = shiny::div(
                      shiny::sliderInput(inputId = "frequency", label = "Frequency",
                                         min = 0, max = 200, value = c(75, 150),
                                         step = 0.5, round = -1, post = " Hz"),
                      shiny::sliderInput(inputId = "time", label = "Time range",
                                         min = 0, max = 1, value = c(0, 1),
                                         step = 0.01, round = -2, post = " s")
                    )
                  )
                )

              )



            )




          )
        )
      ),


      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll fancy-scroll-y",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              'Collapsed over frequency',
              class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
              shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
            )
          )
        )
      )
    )
  )
}

module_server <- function(input, output, session, ...){

  repository <- list()
  local_reactives <- shiny::reactiveValues(
    refresh = NULL,
    results = NULL,
    update_outputs = NULL
  )

  server_tools <- get_default_handlers(session = session)

  # Set auto-recalculate
  server_tools$run_analysis_onchange(auto_recalculate_onchange)
  shiny::observe({
    server_tools$auto_recalculate(isTRUE(input$auto_recalculate))
  })

  shiny::observe({

    if(length(shiny::isolate(local_reactives$results))) {
      local_reactives$results$invalidate()
      ravedash::logger("Invalidating previous run", level = "trace")
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
        "settings", "baseline_windows", "unit_of_analysis",
        "analysis_electrodes", "electrode_category_selector", "analysis_ranges",
        "global_baseline_choice", "condition_groups", "analysis_lock",
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

  shiny::observe({
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

  # get pipeline's default, or subject's default, or program default
  reset_electrode_selectors <- function(repo, from_pipeline = TRUE){
    electrodes <- repo$electrode_list
    electrode_table <- repo$electrode_table
    electrode_table <- electrode_table[electrode_table$Electrode %in% electrodes, ]
    electrode_table_names <- names(electrode_table)


    if(from_pipeline){
      electrode_text <- pipeline_get("analysis_electrodes", dipsaus::deparse_svec(electrodes))
      electrode_category_selector <-
        pipeline_get(
          key = "electrode_category_selector",
          missing = c('freesurferlabel', "FSLabel",
                      input$electrode_category_selector),
          constraint = electrode_table_names
        )
    } else {
      electrode_text <- dipsaus::deparse_svec(electrodes)
      electrode_category_selector <- repo$subject$get_default("electrode_category_selector", default_if_missing = c('freesurferlabel', "FSLabel")) %OF% electrode_table_names
    }

    ravedash::logger("Initializing `electrode_category_selector`: {electrode_category_selector}", level = "debug", use_glue = TRUE)
    shiny::updateSelectInput(
      session = session, inputId = "electrode_category_selector",
      choices = electrode_table_names,
      selected = electrode_category_selector
    )
    ravedash::logger("Initializing `electrode_text`: {electrode_text}", level = "debug", use_glue = TRUE)
    shiny::updateTextInput(
      session = session, inputId = "electrode_text",
      label = sprintf("Select electrode by number (currently loaded: %s)", dipsaus::deparse_svec(repo$electrode_list)),
      value = electrode_text
    )

  }

  reset_condition_groups <- function(repo, from_pipeline = TRUE){
    cond_cont <- table(repo$epoch$table$Condition)
    cond_cont <- cond_cont[order(names(cond_cont))]
    conditions <- names(cond_cont)
    default <- list(list(
      group_name = "All Conditions",
      group_conditions = conditions
    ))

    value <- repo$subject$get_default("condition_groups", default_if_missing = default)
    if(from_pipeline){
      value <- pipeline_get("condition_groups", value)
    }

    if(!length(value) || !is.list(value) || !all(value$group_conditions %in% conditions)){
      value <- default
    }
    dipsaus::updateCompoundInput2(
      session = session,
      inputId = "condition_groups",
      initialization = list(
        group_conditions = list(
          choices = conditions,
          choicesOpt = list(
            subtext = sprintf("(n = %d)", cond_cont)
          )
        )
      ),
      value = value,
      ncomp = length(value)
    )
    # shinyWidgets::updatePickerInput(
    #   session = session, selected =
    #   inputId = "condition_groups_group_conditions_1")
  }

  reset_baselines <- function(repo, from_pipeline = TRUE){
    time_range <- range(repo$time_points)
    if(time_range[1] <= 0){
      default_baseline_windows <- c(time_range[1], 0)
    } else {
      default_baseline_windows <- c(time_range[1], time_range[1])
    }


    if( from_pipeline ) {
      unit_of_analysis <-
        pipeline_get(
          key = "unit_of_analysis",
          missing = repo$subject$get_default("unit_of_analysis", default_if_missing = "Decibel"),
          constraint = baseline_choices
        )
      global_baseline_choice <-
        pipeline_get(
          key = "global_baseline_choice",
          missing = repo$subject$get_default("global_baseline_choice", default_if_missing = baseline_along_choices[[1]]),
          constraint = baseline_along_choices
        )
      baseline_windows <-
        pipeline_get(
          key = "baseline_windows",
          missing = repo$subject$get_default(
            "baseline_windows",
            default_if_missing = list(default_baseline_windows)
          )
        )
    } else {
      unit_of_analysis <- repo$subject$get_default("unit_of_analysis", default_if_missing = "Decibel") %OF% baseline_choices
      global_baseline_choice <- repo$subject$get_default("global_baseline_choice", default_if_missing = baseline_along_choices[[1]]) %OF% baseline_along_choices
      baseline_windows <- repo$subject$get_default("baseline_windows", default_if_missing = list(default_baseline_windows))
    }

    baseline_windows <- tryCatch({
      raveio::validate_time_window(baseline_windows)
    }, error = function(e){
      list(default_baseline_windows)
    })

    # update
    updateSelectInput(session = session, inputId = "unit_of_analysis",
                      selected = unit_of_analysis)
    updateSelectInput(session = session, inputId = "global_baseline_choice",
                      selected = global_baseline_choice)
    dipsaus::updateCompoundInput2(
      session = session, inputId = 'baseline_windows',
      ncomp = length(baseline_windows),
      initialization = list(
        window_interval = list(
          min = time_range[[1]],
          max = time_range[[2]]
        )
      ),
      value = lapply(baseline_windows, function(x){
        list(window_interval = x)
      })
    )

  }

  reset_analysis_ranges <- function(repo, from_pipeline = TRUE){
    time_range <- range(repo$time_points)
    freq_range <- range(repo$frequency)
    default_analysis_ranges <- list(
      list(
        frequency = c(75, 150),
        time = c(max(0, time_range[[1]]), time_range[[2]])
      )
    )

    if( from_pipeline ){
      analysis_lock <- pipeline_get(key = "analysis_lock", missing = repo$subject$get_default("analysis_lock", default_if_missing = analysis_lock_choices[[1]]), constraint = analysis_lock_choices)
      analysis_ranges <- pipeline_get(key = "analysis_ranges", missing = repo$subject$get_default("analysis_ranges", default_if_missing = default_analysis_ranges))
    } else {
      analysis_lock <- repo$subject$get_default("analysis_lock", default_if_missing = analysis_lock_choices[[1]]) %OF% analysis_lock_choices
      analysis_ranges <- repo$subject$get_default("analysis_ranges", default_if_missing = default_analysis_ranges)
    }


    if(!length(analysis_ranges)){
      analysis_ranges <- default_analysis_ranges
    } else if(length(analysis_ranges) > max_analysis_ranges){
      analysis_ranges <- analysis_ranges[seq_len(max_analysis_ranges)]
    }

    shinyWidgets::updateRadioGroupButtons(
      session = session, inputId = "analysis_lock",
      selected = analysis_lock
    )
    dipsaus::updateCompoundInput2(
      session = session, inputId = "analysis_ranges",
      ncomp = length(analysis_ranges),
      initialization = list(
        frequency = list(min = freq_range[[1]], max = freq_range[[2]]),
        time = list(min = time_range[[1]], max = time_range[[2]])
      ),
      value = analysis_ranges
    )
  }

  shiny::observe({
    try({
      repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
      subject_id <- repository$subject$subject_id
      if(length(subject_id)){
        shidashi::show_notification(
          title = "Saving pipeline",
          type = "info",
          message = as.character(
            shiny::tagList(shiny::p(
              raveio::glue("Saving pipeline [{pipeline_name}] to subject [{subject_id}]. Please name your pipeline below:")
            ),
            shidashi::flex_container(
              direction = "column",
              shiny::textInput(
                ns("save_pipeline_name"), label = "Pipeline name",
                value = sprintf("%s-%s", pipeline_name, strftime(Sys.time(), "%y_%m_%d-%H_%M_%S"))),
              shiny::actionButton(
                ns("save_pipeline_name_btn"), label = "Confirm", width = "100%"
              )
            ))

          ),
          close = TRUE, autohide = FALSE, fixed = TRUE, class = "rave-notification-save-pipeline"
        )
      }

    })

    # # Save current pipeline
    # # load current subject information
    # repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
    # file.path(repository$subject$pipeline_path, pipeline_name, )
    # raveio::pipeline_fork(src = pipeline_path, dest = )
  }) |>
    shiny::bindEvent(
      ravedash::get_rave_event("save_pipeline"),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )


  local({
    sv <- shinyvalidate::InputValidator$new(session = session)
    sv$add_rule("save_pipeline_name", function(name){
      if(grepl("[^a-zA-Z0-9_-]", x = name)){
        return("Pipeline name can only contain letters, digits, '_', and '-'")
      }
      return(NULL)
    })
  })

  shiny::observe({
    try({
      repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
      if(!inherits(repository, "rave_prepare_subject")){ return() }
      name <- input$save_pipeline_name
      name <- gsub("[^a-zA-Z0-9_-]", "_", name)
      name <- gsub("[_]+", "_", name)
      name <- gsub("[\\-]+", "-", name)
      dest <- file.path(repository$subject$pipeline_path, pipeline_name, name)
      if(dir.exists(dest)){
        shidashi::show_notification(
          title = "Saving pipeline",
          type = "danger",
          message = "A pipeline with this name has already existed. Please choose another name.",
          close = TRUE, autohide = TRUE, fixed = TRUE, class = "rave-notification-save-pipeline"
        )
        return()
      }

      shidashi::clear_notifications(class = "rave-notification-save-pipeline")
      dipsaus::shiny_alert2(
        title = "Saving in progress",
        icon = "info",
        text = sprintf("Saving pipeline %s", name),
        auto_close = FALSE,
        buttons = FALSE
      )

      tryCatch({
        raveio::pipeline_fork(src = pipeline_path, dest = dest, activate = FALSE)
        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Saved!",
          icon = "success",
          auto_close = TRUE,
          buttons = list("Dismiss" = TRUE)
        )
      }, error = function(e){
        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Error!",
          icon = "error",
          auto_close = TRUE,
          buttons = list("Dismiss" = TRUE),
          danger_mode = TRUE,
          text = raveio::glue("An error found while saving the pipeline. Reason: \n  {e$message}")
        )
      })

    })

  }) |>
    shiny::bindEvent(
      input$save_pipeline_name_btn,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

  shiny::observe({
    try({

      if(inherits(repository, "rave_prepare_subject")){
        subject <- repository$subject
      } else {
        project_name <- pipeline_get("project_name")
        subject_code <- pipeline_get("subject_code")
        if(!length(project_name) || !length(subject_code)){ return() }
        subject <- raveio::as_rave_subject(sprintf("%s/%s", project_name, subject_code))
      }

      saved_path <- file.path(subject$pipeline_path, pipeline_name)
      dirs <- list.dirs(saved_path, full.names = FALSE, recursive = FALSE)

      if(!length(dirs)){ return() }

      dirs <- sort(dirs, decreasing = TRUE)

      shidashi::show_notification(
        title = "Load pipeline",
        type = "info",
        message = as.character(shiny::fluidRow(
          shiny::column(
            width = 12L,
            shiny::p("Please choose a pipeline to load from the list below:"),
            # shinyWidgets::pickerInput(ns("load_pipeline"), label = NULL, choices = dirs,
            #                           options = list(
            #                             "live-search" = TRUE
            #                           )),
            shiny::selectInput(ns("load_pipeline"), label = NULL, choices = dirs, selectize = FALSE, width = "100%", size = 10),
            shiny::actionButton(ns("load_pipeline_btn"), label = "Confirm", width = "100%")
          )
        )),
        autohide = FALSE, close = TRUE, class = "rave-notification-load-pipeline"
      )

    })
  }) |>
    shiny::bindEvent(
      ravedash::get_rave_event("load_pipeline"),
      ignoreNULL = TRUE,
      ignoreInit = FALSE
    )

  shiny::observe({
    on.exit({
      shidashi::clear_notifications(class = "rave-notification-load-pipeline")
    })
    try({
      name <- input$load_pipeline
      if(!length(name)){ return() }
      if(inherits(repository, "rave_prepare_subject")){
        subject <- repository$subject
      } else {
        project_name <- pipeline_get("project_name")
        subject_code <- pipeline_get("subject_code")
        if(!length(project_name) || !length(subject_code)){ return() }
        subject <- raveio::as_rave_subject(sprintf("%s/%s", project_name, subject_code))
      }
      src <- file.path(subject$pipeline_path, pipeline_name, name, 'settings.yaml')
      if(!file.exists(src)){ return() }
      file.copy(src, file.path(pipeline_path, 'settings.yaml'), recursive = FALSE, overwrite = TRUE)
      new_repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
      if(!inherits(new_repository, "rave_prepare_power")){
        return()
      }

      repository <<- new_repository
      reset_electrode_selectors(new_repository, from_pipeline = TRUE)
      reset_condition_groups(new_repository, from_pipeline = TRUE)
      reset_baselines(new_repository, from_pipeline = TRUE)
      reset_analysis_ranges(new_repository, from_pipeline = TRUE)
      local_reactives$refresh <- Sys.time()
    })

  }) |>
    shiny::bindEvent(
      input$load_pipeline_btn,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )


  shiny::observe({
    if(!ravedash::watch_data_loaded()){ return() }
    new_repository <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
    if(!inherits(new_repository, "rave_prepare_power")){
      ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
      return()
    }
    ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

    # check if the repository has the same subject as current one
    if(inherits(repository, "rave_prepare_power")){
      if( identical(repository$signature, new_repository$signature) ){
        ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
        return()
      }
    }

    # TODO: Make a function to reset UIs to default
    repository <<- new_repository
    reset_electrode_selectors(new_repository, from_pipeline = FALSE)
    reset_condition_groups(new_repository, from_pipeline = FALSE)
    reset_baselines(new_repository, from_pipeline = FALSE)
    reset_analysis_ranges(new_repository, from_pipeline = FALSE)
    local_reactives$refresh <- Sys.time()

  }, priority = 1001) |>
    shiny::bindEvent(
      ravedash::watch_data_loaded(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

  dipsaus::sync_shiny_inputs(
    input, session,
    c(
      'electrode_text',
      'electrode_category_selector_choices'
    ),
    uniform = list(
      function(electrode_text) {
        electrodes <- dipsaus::parse_svec(electrode_text)
        electrodes <- electrodes[electrodes %in% repository$electrode_list]
        if(!length(electrodes)){ electrodes <- NULL }
        electrodes
      },
      function(category_selected) {
        category_name <- input$electrode_category_selector
        current_electrodes <- dipsaus::parse_svec(input$electrode_text)
        current_electrodes <- current_electrodes[current_electrodes %in% repository$electrode_list]
        if(!length(current_electrodes)){ current_electrodes <- NULL }

        if(
          length(category_name) &&
          category_name %in% names(repository$electrode_table) &&
          length(repository$electrode_table[[category_name]])
        ) {
          choices <- repository$electrode_table[[category_name]]
          all_electrodes <- repository$electrode_table$Electrode
          expected_category <- choices[all_electrodes %in% current_electrodes]
          if(!setequal(expected_category, category_selected)){

            electrodes <- all_electrodes[
              choices %in% category_selected &
                repository$electrode_table$isLoaded
            ]
            return(electrodes)
          }
        }

        return(current_electrodes)

      }
    ),
    updates = list(
      function(electrodes) {
        if(length(electrodes)){
          new_value <- dipsaus::deparse_svec(electrodes)
          if(!identical(new_value, input$electrode_text)){
            ravedash::logger("Updating `electrode_text`", level = "trace")
            shiny::updateTextInput(
              session = session, inputId = "electrode_text",
              value = dipsaus::deparse_svec(electrodes)
            )
          }
        }
      },
      function(electrodes) {
        if(length(electrodes)){
          category_name <- input$electrode_category_selector
          if(
            length(category_name) &&
            category_name %in% names(repository$electrode_table) &&
            length(repository$electrode_table[[category_name]])
          ) {
            choices <- repository$electrode_table[[category_name]]
            all_electrodes <- repository$electrode_table$Electrode
            expected_category <- choices[all_electrodes %in% electrodes]
            category_selected <- input$electrode_category_selector_choices
            if(!setequal(expected_category, category_selected)){

              if(!length(expected_category)){
                expected_category <- character(0L)
              }

              ravedash::logger("Updating `electrode_category_selector_choices`", level = "trace")

              shiny::updateSelectInput(
                session = session,
                inputId = "electrode_category_selector_choices",
                selected = expected_category
              )
            }
          }

        }
      }
    )
  )
  shiny::observe({
    electrodes <- dipsaus::parse_svec(input$electrode_text)
    electrodes <- electrodes[electrodes %in% repository$electrode_list]
    category <- input$electrode_category_selector

    choices <- character(0L)
    if(category %in% names(repository$electrode_table)) {
      choices <- repository$electrode_table[[category]]
      if(!length(choices)){ choices <- character(0L) }
    }

    # selected <- choices[repository$electrode_table$Electrode %in% electrodes]
    # ravedash::logger("Updating `electrode_category_selector_choices`", level = "trace")
    shiny::updateSelectInput(
      session = session,
      inputId = "electrode_category_selector_choices",
      choices = unique(choices),
      selected = character(0L)
    )
    shiny::updateTextInput(
      session = session,
      inputId = "electrode_text",
      value = sprintf("%s ", input$electrode_text)
    )

  }) |>
    shiny::bindEvent(
      input$electrode_category_selector,
      local_reactives$refresh,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

  shiny::observe({
    reset_electrode_selectors(repository)
  }) |>
    shiny::bindEvent(
      input$electrode_selectors_reset,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  # sync analysis ranges
  shiny::observe({
    analysis_lock <- which(analysis_lock_choices %in% input$analysis_lock)
    if(!length(analysis_lock) || analysis_lock == 1){ return() }
    if(analysis_lock == 2){
      value <- input$analysis_ranges[[1]]$frequency
      lapply(seq_len(max_analysis_ranges), function(ii){
        shiny::updateSliderInput(
          session = session, inputId = sprintf("analysis_ranges_frequency_%d", ii),
          value = value
        )
      })
    } else if(analysis_lock == 3){
      value <- input$analysis_ranges[[1]]$time
      lapply(seq_len(max_analysis_ranges), function(ii){
        shiny::updateSliderInput(
          session = session, inputId = sprintf("analysis_ranges_time_%d", ii),
          value = value
        )
      })
    }
  }) |> shiny::bindEvent(
    ravedash::watch_data_loaded(),
    input$analysis_lock,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  dipsaus::sync_shiny_inputs(
    input = input, session = session,
    inputIds = sprintf("analysis_ranges_frequency_%d", seq_len(max_analysis_ranges)),
    uniform = as.list(rep("I", max_analysis_ranges)),
    updates = lapply(seq_len(max_analysis_ranges), function(ii){
      function(value){
        if(isTRUE(input$analysis_lock == analysis_lock_choices[[2]])){
          shiny::updateSliderInput(
            session = session, inputId = sprintf("analysis_ranges_frequency_%d", ii),
            value = value
          )
        }
      }
    })
  )
  dipsaus::sync_shiny_inputs(
    input = input, session = session,
    inputIds = sprintf("analysis_ranges_time_%d", seq_len(max_analysis_ranges)),
    uniform = as.list(rep("I", max_analysis_ranges)),
    updates = lapply(seq_len(max_analysis_ranges), function(ii){
      function(value){
        if(isTRUE(input$analysis_lock == analysis_lock_choices[[3]])){
          shiny::updateSliderInput(
            session = session, inputId = sprintf("analysis_ranges_time_%d", ii),
            value = value
          )
        }
      }
    })
  )

  output$card_electrode_selector <- shiny::renderText({
    dipsaus::deparse_svec(dipsaus::parse_svec(input$electrode_text))
  })
}
