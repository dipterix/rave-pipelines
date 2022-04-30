# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 8L,
        ravedash::input_card(
          title = "Data settings",
          class_header = "",

          shiny::p(
            shiny::tags$small(
              "This module imports raw MR & CT images into Nifti format using ",
              "dcm2niix, re-constructs ",
              "surface using FreeSurfer, and co-registers CT to MRI via ",
              "FSL-FLIRT. Please note that this module is experimental and ",
              "simply wraps bundles of command-lines. ",
              "Some of these commands (e.g. FreeSurfer) might not work properly under Windows.",
              shiny::br(),
              "Although this module can invoke system command, it is Highly Recommended that you run ",
              "these command by yourself. The module will provide dry-run code. ",
              "Please use bash terminal to run the code on Linux or MacOS. ",
              "It has not been tested on Windows. If you do want to use Window, ",
              "please install its Linux sub-system first."
            )
          ),

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 6L,
              ravedash::flex_group_box(
                title = "MRI",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("mri_path"),
                    label = "Raw MRI (DICOM folder or Nifti file)",
                    choices = character(0L)
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::checkboxInput(
                    inputId = ns("skip_recon"),
                    label = "Skip the FreeSurfer reconstruction",
                    value = FALSE
                  )
                )
              )
            ),
            shiny::column(
              width = 6L,
              ravedash::flex_group_box(
                title = "CT",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("ct_path"),
                    label = "Raw CT DICOM folder",
                    choices = character(0L)
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::checkboxInput(
                    inputId = ns("skip_coregistration"),
                    label = "Skip the CT co-registration",
                    value = FALSE
                  )
                )
              )
            )
          ),


          footer = shiny::tagList(
            shiny::actionLink(
              inputId = ns("loader_sync_btn"),
              label = "Sync project & subject from [Import LFP] module"
            ),
          )

        )
      ),

      shiny::column(
        width = 4L,

        ravedash::input_card(
          title = "Command-line settings",
          class_header = "",

          ravedash::flex_group_box(
            title = "Command-line path",
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_dcm2niix_path"),
                label = "Dcm2niix path (needed to convert DICOM images to Nifti format)",
                value = raveio::cmd_dcm2niix(error_on_missing = FALSE, unset = "")
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_fs_path"),
                label = "FreeSurfer home (`FREESURFER_HOME`, needed for surface reconstruction)",
                value = raveio::cmd_freesurfer_home(error_on_missing = FALSE, unset = "")
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_fsl_path"),
                label = "FSL home (`FSLDIR`, needed for co-registration)",
                value = raveio::cmd_fsl_home(error_on_missing = FALSE, unset = "")
              )
            )
          ),

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Check data and command-line tools",
              type = "primary",
              width = "100%"
            )
          )
        )
      )

    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline_set(
        path_mri = input$mri_path,
        path_ct = input$ct_path,
        skip_recon = input$skip_recon,
        skip_coregistration = input$skip_coregistration,
        dcm2niix_path = input$cmd_dcm2niix_path,
        freesurfer_path = input$cmd_fs_path,
        fsl_path = input$cmd_fsl_path,
        dryrun = FALSE,
        .list = settings
      )

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "check_result",
        type = "vanilla", scheduler = "none"
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          check_result <- raveio::pipeline_read("check_result", pipe_dir = pipeline_path)
          cmd_tools <- raveio::pipeline_read("cmd_tools", pipe_dir = pipeline_path)

          msg_ui <- NULL
          warn_ui <- NULL
          if(length(check_result$messages)) {
            msg_ui <- shiny::tagList(
              shiny::p("The following pipeline will run:"),
              shiny::tags$ul(
                lapply(check_result$messages, shiny::tags$li)
              )
            )
          }

          if(length(check_result$warnings)) {
            warn_ui <- shiny::tagList(
              shiny::p("Please check the following warnings:"),
              shiny::tags$ul(
                lapply(check_result$warnings, shiny::tags$li)
              )
            )
          }
          cmd_ui <- shiny::tagList(
            shiny::p("The following external commands will be used. "),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("dcm2niix"), ": ", paste(cmd_tools$dcm2niix, collapse = "")),
              shiny::tags$li(shiny::strong("FreeSurfer"), ": ", paste(cmd_tools$freesurfer, collapse = "")),
              shiny::tags$li(shiny::strong("FSL-flirt"), ": ", paste(cmd_tools$flirt, collapse = ""))
            ),
            local({
              if(cmd_tools$dry_run) {
                shiny::p(shiny::tags$small(
                  "* Security-mode is ON, ",
                  "no system command will be invoked in this module. ",
                  "You will have to enter the command-lines by yourself."
                ))
              } else { NULL }
            })
          )

          dipsaus::close_alert2()

          shiny::showModal(shiny::modalDialog(
            title = "Confirmation",
            size = "l",
            shiny::p("Please confirm the to-do list and potential warnings. ",
                     "The warnings can be ignored ",
                     "if you are going to enter the commands ",
                     "in the terminals by yourself."),
            msg_ui,
            warn_ui,
            cmd_ui,

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              dipsaus::actionButtonStyled(ns("loader_ready_btn2"), "Proceed")
            )
          ))

        },


        # this is what should happen when pipeline fails
        onRejected = function(e){

          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the power data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "default_paths",
        type = "vanilla",
        scheduler = "none"
      )

      # local({
      #   raveio:::activate_pipeline(pipeline_path)
      #   targets::tar_make(names = "default_paths",
      #                     shortcut = TRUE)
      # })


      shiny::removeModal()
      # Let the module know the data has been changed
      ravedash::fire_rave_event('data_changed', Sys.time())
      ravedash::logger("Data has been loaded loaded")
    }),
    input$loader_ready_btn2,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      pipe2 <- raveio::pipeline_find("import_lfp_native")
      data <- structure(raveio::pipeline_read(c(
        "import_setup__project_name",
        "import_setup__subject_code"
      ), pipe_dir = pipe2),
      names = c("project_name", "subject_code"))

      if(length(data$project_name)) {
        shiny::updateSelectInput(
          session = session,
          inputId = loader_project$get_sub_element_id(),
          selected = data$project_name
        )
      }

      if(length(data$subject_code)) {
        shiny::updateSelectInput(
          session = session,
          inputId = loader_subject$get_sub_element_id(),
          selected = data$subject_code
        )
      }


    }),
    input$loader_sync_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  local_cache <- dipsaus::fastmap2()

  shiny::bindEvent(
    ravedash::safe_observe({
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      if(!length(project_name) || !length(subject_code)) { return() }

      if(is.null(local_cache$subject) || !isTRUE(local_cache$subject$subject_id == sprintf("%s/%s", project_name, subject_code))) {
        local_cache$subject <- raveio::RAVESubject$new(
          project_name = project_name,
          subject_code = subject_code,
          strict = FALSE)
      }
      subject <- local_cache$subject

      if(!length(subject)) { return() }

      # paths <- list.dirs(subject$preprocess_settings$raw_path, full.names = FALSE, recursive = TRUE)
      paths <- list.files(subject$preprocess_settings$raw_path, all.files = TRUE, full.names = FALSE, include.dirs = TRUE, no.. = TRUE, recursive = TRUE)
      paths <- paths[dir.exists(file.path(subject$preprocess_settings$raw_path, paths)) | grepl("nii(^|\\.gz$)", x = paths, ignore.case = TRUE)]
      paths <- paths[!paths %in% c("", ".", "..", "/")]

      selected <- NULL
      if(length(paths)) {
        selected <- paths[grepl("MR", paths)]
        if(length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_mri_path", namespace = pipeline_name),
        pipeline_get("path_mri"), selected
      ) %OF% paths
      shiny::updateSelectInput(session = session, inputId = "mri_path", choices = paths, selected = selected)

      if(length(paths)) {
        selected <- paths[grepl("CT", paths)]
        if(length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_ct_path", namespace = pipeline_name),
        pipeline_get("path_ct"), selected
      ) %OF% paths
      shiny::updateSelectInput(session = session, inputId = "ct_path", choices = paths, selected = selected)

      fs_reconstructed <- FALSE
      fs_path <- subject$freesurfer_path
      if(length(fs_path) == 1 && !is.na(fs_path) && isTRUE(dir.exists(fs_path))) {
        fs_reconstructed <- threeBrain::check_freesurfer_path(
          fs_path,
          autoinstall_template = FALSE,
          check_volume = TRUE,
          check_surface = FALSE
        )
      }
      shiny::updateCheckboxInput(
        session = session,
        inputId = "skip_recon",
        value = fs_reconstructed
      )

    }),
    loader_subject$get_sub_element_input(),
    loader_project$get_sub_element_input(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


}
