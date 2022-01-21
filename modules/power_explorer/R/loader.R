
module_ui_loader <- function(){

  remembered_project_name <- pipeline_get("project_name", constraint = get_projects(refresh = TRUE))

  shiny::div(
    class = "container-fluid",
    style = 'max-width:1444px;',
    shiny::fluidRow(
      column_md(
        width = 4L,

        # project & subject
        card(
          title = "Project and Subject",
          class_body = "padding-10",
          shiny::fluidRow(
            column_md(
              width = 6L,
              shiny::selectInput(
                inputId = ns("loader_project_name"),
                label = "Project",
                choices = get_projects(),
                selected = remembered_project_name,
                multiple = FALSE
              )
            ),
            column_md(
              width = 6L,
              shiny::selectInput(
                inputId = ns("loader_subject_code"),
                label = "Subject",
                choices = "",
                selected = NULL,
                multiple = FALSE
              )
            )
          )
        ),


        card(
          title = "Epoch and Trial Duration",
          class_body = "padding-5",
          shidashi::flex_container(
            wrap = "wrap",
            direction = 'row',
            shidashi::flex_item(
              flex = "2",
              class = "fill-width padding-5",
              shiny::selectInput(
                inputId = ns("loader_epoch_name"),
                label = "Epoch name",
                choices = '',
                selected = NULL,
                multiple = FALSE
              )
            ),
            shidashi::flex_item(
              class = "fill-width padding-5",
              shiny::numericInput(
                inputId = ns("loader_epoch_pre"),
                label = "Pre",
                min = 0,
                value = 1
              )
            ),
            shidashi::flex_item(
              class = "fill-width padding-5",
              shiny::numericInput(
                inputId = ns("loader_epoch_post"),
                label = "Post",
                min = 0,
                value = 2
              )
            )
          ),
          shiny::div(
            class = "padding-left-5",
            shinyWidgets::prettyCheckbox(
              inputId = ns("loader_epoch_name_default"),
              label = "Set this epoch as the default",
              status = "success",
              shape = "square",
              animation = "smooth"
            )
          )
        ),


        # Electrode and reference
        card(
          title = "Electrode and Reference",
          class_body = "padding-5",
          shidashi::flex_container(
            wrap = "wrap",
            direction = 'row',
            shidashi::flex_item(
              class = "fill-width padding-5",
              shiny::selectInput(
                inputId = ns("loader_reference_name"),
                label = "Reference name",
                choices = '',
                selected = NULL,
                multiple = FALSE
              ),
              shinyWidgets::prettyCheckbox(
                inputId = ns("loader_reference_name_default"),
                label = "Set this reference as the default",
                status = "success",
                shape = "square",
                animation = "smooth"
              )
            )
          ),
          shidashi::flex_container(
            wrap = "wrap",
            shidashi::flex_item(
              class = "fill-width padding-5",
              shiny::textInput(
                inputId = ns("loader_electrode_text"),
                label = "Electrodes",
                placeholder = "E.g. 1-84,90-100",
                value = ""
              )
            ),
            shidashi::flex_item(
              class = "fill-width padding-5",
              shiny::fileInput(
                inputId = ns("loader_mask_file"),
                label = "or Mask file"
              )
            )
          ),
          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
              type = "primary",
              width = "100%"
            )
          )
        )

      ),
      column_md(
        width = 8L,
        card(
          title = "3D Viewer",
          class_body = "no-padding",
          threeBrain::threejsBrainOutput(
            outputId = ns("loader_3d_viewer"),
            height = "570px", reportSize = FALSE
          )
        )
      )
    )

  )

}

server_loader <- function(input, output, session, tools, ...){

  event_data <- register_session_events(session = session)

  # Add validator
  # session <- shiny::MockShinySession$new()
  loader_validator_project_name <- shinyvalidate::InputValidator$new(session = session)
  loader_validator_project_name$add_rule("loader_project_name", function(project_name){
    if(length(project_name) != 1 || is.na(project_name)){
      return("Missing project name. Please choose one.")
    }
    project <- raveio::as_rave_project(project_name, strict = FALSE)
    if(!dir.exists(project$path)){
      return(raveio::glue("Cannot find path to project `{project_name}`"))
    }
    return(NULL)
  })
  loader_validator_subject_code <- shinyvalidate::InputValidator$new(session = session)
  loader_validator_subject_code$add_validator(loader_validator_project_name)
  loader_validator_subject_code$add_rule("loader_subject_code", function(subject_code){
    if(length(subject_code) != 1 || is.na(subject_code) ||
       startsWith(subject_code, "_")){
      return("Invalid subject code.")
    }
    project_name <- input$loader_project_name
    subject <- raveio::as_rave_subject(
      subject_id = sprintf("%s/%s", project_name, subject_code),
      strict = FALSE)
    if(!dir.exists(subject$path)){
      return("Subject directory is broken or missing.")
    }
    if(!any(subject$preprocess_settings$has_wavelet)) {
      return("Please run wavelet on this subject first.")
    }
    return(NULL)
  })
  loader_validator_project_name$enable()
  loader_validator_subject_code$enable()


  # Loader update subject code
  shiny::observe({
    if(!loader_validator_project_name$is_valid()){ return() }
    project_name <- input$loader_project_name
    project <- raveio::as_rave_project(project_name)
    all_subjects <- project$subjects()
    selected <- NULL
    if(isTRUE(pipeline_get("project_name", NA) == project_name)){
      selected <- pipeline_get("subject_code", missing = NULL)
    }

    shiny::updateSelectInput(session = session, inputId = 'loader_subject_code', choices = all_subjects, selected = selected)
  }) |>
    shiny::bindEvent(
      input$loader_project_name,
      ignoreNULL = TRUE
    )

  # Loader update
  # epoch name, reference name, and electrode text
  shiny::observe({
    if(!loader_validator_subject_code$is_valid()){ return() }
    project_name <- input$loader_project_name
    subject_code <- input$loader_subject_code
    subject <- raveio::as_rave_subject(
      subject_id = sprintf("%s/%s", project_name, subject_code),
      strict = TRUE)

    epoch_choices <- subject$epoch_names
    epoch_name <- subject$get_default("epoch_name", default_if_missing = pipeline_get("epoch_name")) %OF% epoch_choices
    shiny::updateSelectInput(
      session = session,
      inputId = "loader_epoch_name",
      choices = epoch_choices,
      selected = epoch_name
    )

    ref_choices <- subject$reference_names
    if(!length(ref_choices)){
      ref_choices <- c(ref_choices, "[no reference]")
    }
    reference_name <- subject$get_default("reference_name", default_if_missing = pipeline_get("epoch_name")) %OF% ref_choices
    shiny::updateSelectInput(
      session = session,
      inputId = "loader_reference_name",
      choices = ref_choices,
      selected = reference_name
    )

    # electrodes
    # check if subject is last input
    electrode_text <- dipsaus::deparse_svec(subject$electrodes)
    if(isTRUE(pipeline_get("subject_code", missing = NULL) == subject$subject_code)) {
      electrode_text <- pipeline_get("loaded_electrodes", missing = electrode_text)
    }
    shiny::updateTextInput(
      session = session,
      inputId = "loader_electrode_text",
      value = electrode_text
    )

  }) |>
    shiny::bindEvent(
      input$loader_project_name,
      input$loader_subject_code,
      ignoreNULL = TRUE
    )

  # Loader 3D viewer
  brain_proxy <- threeBrain::brain_proxy('loader_3d_viewer', session = session)
  output$loader_3d_viewer <- threeBrain::renderBrain({
    shiny::validate(
      shiny::need(loader_validator_subject_code$is_valid(),
                  message = "Initializing...")
    )
    # shiny::validate(
    #   shiny::need(loader_validator_subject_code$is_valid(),
    #               message = "   Initializing...")
    # )
    #
    project_name <- input$loader_project_name
    subject_code <- input$loader_subject_code
    print(subject_code)
    reference_name <- input$loader_reference_name
    subject <- raveio::as_rave_subject(
      subject_id = sprintf("%s/%s", project_name, subject_code),
      strict = FALSE)
    brain <- raveio::rave_brain(subject, surfaces = 'pial')
    # TODO: brain is null

    electrodes <- dipsaus::parse_svec(input$loader_electrode_text)
    all_electrodes <- subject$electrodes
    valid_electrodes <- subject$valid_electrodes(reference_name = reference_name)
    val <- rep("Not Loading", length(all_electrodes))
    val[all_electrodes %in% electrodes] <- "Excluded"
    val[all_electrodes %in% electrodes & all_electrodes %in% valid_electrodes] <- "Loading"
    val <- factor(val, levels = c("Loading", "Excluded", "Not Loading"))
    tbl <- data.frame(
      Subject = subject$subject_code,
      Electrode = subject$electrodes,
      Value = val
    )

    brain$set_electrode_values(tbl)

    theme <- shidashi::get_theme(event_data)

    brain$plot(
      volumes = FALSE,
      start_zoom = 1,
      atlases = FALSE,
      side_canvas = FALSE,
      control_display = FALSE,
      # show_modal = TRUE,
      controllers = list("Background Color" = theme$background),
      background = theme$background,
      palettes = list(Value = c("navy", "red", "gray80"))
    )
  }) |>
    shiny::bindEvent(
      input$loader_project_name,
      input$loader_subject_code,
      input$loader_reference_name,
      input$loader_electrode_text,
      shidashi::get_theme(event_data),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )


  shiny::observe({
    # gather information
    pipeline_set(
      project_name = input$loader_project_name,
      subject_code = input$loader_subject_code,
      epoch_name = input$loader_epoch_name,
      reference_name = input$loader_reference_name,
      trial_starts = -input$loader_epoch_pre,
      trial_ends = input$loader_epoch_post,
      loaded_electrodes = input$loader_electrode_text
    )

    default_epoch <- isTRUE(input$loader_epoch_name_default)
    default_reference <- isTRUE(input$loader_reference_name_default)

    # Run the pipeline!
    tarnames <- raveio::pipeline_target_names(pipe_dir = pipeline_path)
    count <- length(tarnames) + dipsaus::parse_svec(input$loader_electrode_text) + 4

    dipsaus::shiny_alert2(
      title = "Loading in progress",
      text = paste(
        "Everything takes time. You can still browse other modules though."
      ), icon = "info", auto_close = FALSE, buttons = FALSE
    )

    tryCatch({
      raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "repository",
        type = "basic",
        use_future = FALSE
      )
      # load subject
      if(default_epoch || default_reference){
        repo <- raveio::pipeline_read('repository', pipe_dir = pipeline_path)
        if(default_epoch){
          repo$subject$set_default("epoch_name", repo$epoch_name)
        }
        if(default_reference) {
          repo$subject$set_default("reference_name", repo$reference_name)
        }

      }

      tools$rave_event$data_changed <- Sys.time()
      tools$rave_event$data_loaded <- TRUE
      logger("Data has been loaded loaded")
      dipsaus::close_alert2()
    }, error = function(e){
      dipsaus::close_alert2()
      dipsaus::shiny_alert2(
        title = "Errors",
        text = paste(
          "Found an error while loading the power data:\n\n",
          paste(e, collapse = "\n")
        ),
        icon = "error",
        danger_mode = TRUE,
        auto_close = FALSE
      )
    })
    # raveio::pipeline_run_async(
    #   pipe_dir = pipeline_path, names = "repository",
    #   use_future = FALSE,
    #   progress_quiet = FALSE,
    #   progress_max = count
    # ) |>
    #   promises::then(
    #     onFulfilled = function(...){
    #       tools$rave_event$data_changed <- Sys.time()
    #       tools$rave_event$data_loaded <- TRUE
    #       logger("Data has been loaded loaded")
    #       dipsaus::close_alert2()
    #     },
    #     onRejected = function(e){
    #       dipsaus::close_alert2()
    #       dipsaus::shiny_alert2(
    #         title = "Errors",
    #         text = paste(
    #           "Found an error while loading the data:\n\n",
    #           paste(e, collapse = "\n"),
    #           "\n\nPlease check RAVE console for details."
    #         ), icon = "error", danger_mode = TRUE, auto_close = FALSE
    #       )
    #     }
    #   )


  }) |>
    shiny::bindEvent(input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE)



}
