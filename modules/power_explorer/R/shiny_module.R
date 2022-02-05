
module_ui_main <- function(){

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row",
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
                ravedash::run_analysis_button(width = "100%")
              ),

              ravedash::flex_group_box(
                title = "Baseline settings",
                ravedash::flex_item2(
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
                ravedash::flex_new_line(),
                ravedash::flex_item2(
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

                ravedash::flex_item2(
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
                ravedash::flex_new_line(),
                ravedash::flex_item2(
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
      )
    )
  )
}

module_server <- function(input, output, session, ...){

  repository <- list()
  local_reactives <- shiny::reactiveValues(
    refresh = NULL
  )

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

    ravedash::logger("Initializing `electrode_category_selector`: {electrode_category_selector}", level = "debug")
    shiny::updateSelectInput(
      session = session, inputId = "electrode_category_selector",
      choices = electrode_table_names,
      selected = electrode_category_selector
    )
    ravedash::logger("Initializing `electrode_text`: {electrode_text}", level = "debug")
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
        ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug")
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

  shiny::observe({
    electrodes <- dipsaus::parse_svec(input$electrode_text)
    electrodes <- electrodes[electrodes %in% repository$electrode_list]
    if(!length(electrodes)){ return() }

    category <- input$electrode_category_selector
    if(!length(category)){ return() }
    choices <- repository$electrode_table[[category]]
    if(!length(choices)){ choices <- character(0L) }

    selected <- choices[repository$electrode_table$Electrode %in% electrodes]
    ravedash::logger("Updating `electrode_category_selector_choices`", level = "trace")
    shiny::updateSelectInput(
      session = session, inputId = "electrode_category_selector_choices",
      choices = unique(choices), selected = selected
    )

  }) |>
    shiny::bindEvent(
      input$electrode_text,
      input$electrode_category_selector,
      local_reactives$refresh,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

  shiny::observe({
    category_name <- input$electrode_category_selector
    if(!length(category_name)){ return() }
    choices <- repository$electrode_table[[category_name]]
    if(!length(choices)){ return() }
    category_selected <- input$electrode_category_selector_choices
    current_electrodes <- dipsaus::parse_svec(input$electrode_text)

    all_electrodes <- repository$electrode_table$Electrode
    expected_category <- choices[all_electrodes %in% current_electrodes]

    if(setequal(expected_category, category_selected)){ return() }

    electrodes <- dipsaus::deparse_svec(all_electrodes[
      choices %in% category_selected &
        repository$electrode_table$isLoaded
    ])

    ravedash::logger("Updating `electrode_text`", level = "trace")
    shiny::updateTextInput(
      session = session, inputId = "electrode_text",
      value = electrodes
    )

  }) |>
    shiny::bindEvent(
      input$electrode_category_selector,
      input$electrode_category_selector_choices,
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
