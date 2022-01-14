
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
    if(isTRUE(pipeline_get("loader_project_name", NA) == project_name)){
      selected <- pipeline_get("loader_subject_code", missing = NULL)
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
    shiny::updateSelectInput(
      session = session,
      inputId = "loader_epoch_name",
      choices = epoch_choices,
      selected = pipeline_get("epoch_name", constraint = epoch_choices)
    )

    ref_choices <- subject$reference_names
    if(!length(ref_choices)){
      ref_choices <- c(ref_choices, "[no reference]")
    }
    shiny::updateSelectInput(
      session = session,
      inputId = "loader_reference_name",
      choices = ref_choices,
      selected = pipeline_get("reference_name", constraint = ref_choices)
    )

    # electrodes
    electrode_text <- dipsaus::deparse_svec(subject$electrodes)
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
      ignoreNULL = TRUE
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

    # Run the pipeline!
    tryCatch({
      # names <- c("subject_id", "power_dimnames",
      #   "epoch_name", "reference_name", "loaded_electrodes", "trial_ends",
      #   "subject_code", "trial_starts", "project_name", "subject", "frequency_table",
      #   "reference_table", "epoch", "epoch_table", "electrode_table",
      #   "power_list")
      # raveio::pipeline_progress(pipeline_path, method = 'summary')
      raveio::pipeline_run(
        type = "basic", pipe_dir = pipeline_path,
        names = c(
          "power_list", "power_dimnames", "epoch_table", "epoch",
          "reference_table", "electrode_table", "frequency_table"
        )
      )
      # raveio::pipeline_run_interactive(
      #   names = c(
      #     "power_list",
      #     "power_dimnames"
      #   ),
      #   skip_names = c(
      #     "settings_path", "settings", "reference_name", "subject_code",
      #     "project_name", "trial_starts", "trial_ends", "epoch_name", "loaded_electrodes",
      #     "subject",
      #     "epoch_table",
      #     "reference_table",
      #     "electrode_table",
      #     "frequency_table",
      #     "epoch"
      #   ),
      #   pipe_dir = pipeline_path,
      #   env = new.env(parent = globalenv())
      # )

      tools$rave_event$data_changed <- Sys.time()
      tools$rave_event$data_loaded <- TRUE
      logger("Data has been loaded loaded")
    }, error = function(e){
      cat("The error occurs in the following pipeline expression:\n")
      print(e$call)
      dipsaus::shiny_alert2(
        title = "Errors",
        text = paste(
          "Found an error while loading the data:\n\n",
          paste(e$message, collapse = "\n"),
          "\n\nPlease check RAVE console for details."
        ), icon = "error", danger_mode = TRUE, auto_close = FALSE
      )
    })


  }) |>
    shiny::bindEvent(input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE)



}
