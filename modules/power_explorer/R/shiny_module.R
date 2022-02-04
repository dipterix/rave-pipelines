
module_ui_main <- function(){
  theme <- ravedash::current_shiny_theme()
  print(theme)

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
              class_header = "shidashi-anchor",
              title = shiny::tagList(
                "Select Electrodes ",
                shiny::textOutput(ns("card_electrode_selector"), inline = TRUE, shiny::tags$small)
              ),
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
    print(value)
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
