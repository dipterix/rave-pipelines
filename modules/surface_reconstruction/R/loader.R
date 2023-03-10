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
              "This module provides terminal scripts to ",
              "imports the raw MR & CT images into RAVE, reconstruct ",
              "surface using FreeSurfer, and co-register CT to MRI via ",
              "AFNI-3dAllineate or FSL-FLIRT. ",
              shiny::br(),
              "This module requires installation of `dcm2niix`, `FreeSurfer`, `AFNI` (or `FSL`). ",
              "Some of these commands (e.g. FreeSurfer) might not work properly under Windows. ",
              "It is Highly Recommended that you run ",
              "these command in the terminal by yourself once the bash scripts are generated. ",
              shiny::br(),
              "Please use bash terminal to run the code on Linux or MacOS. ",
              "The script has not been fully tested on Windows yet."
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
                )
                # shidashi::flex_break(),
                # shidashi::flex_item(
                #   shiny::checkboxInput(
                #     inputId = ns("skip_recon"),
                #     label = "Skip the FreeSurfer reconstruction",
                #     value = FALSE
                #   )
                # )
              )
            ),
            shiny::column(
              width = 6L,
              ravedash::flex_group_box(
                title = "CT",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("ct_path"),
                    label = "Raw CT (DICOM folder or Nifti file)",
                    choices = character(0L)
                  )
                )
                # shidashi::flex_break(),
                # shidashi::flex_item(
                #   shiny::checkboxInput(
                #     inputId = ns("skip_coregistration"),
                #     label = "Skip the CT co-registration",
                #     value = FALSE
                #   )
                # )
              )
            )
          ),


          footer = shiny::tagList(
            loader_sync1$ui_func(),
            shiny::br(),
            loader_sync2$ui_func()
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
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_afni_path"),
                label = "AFNI home (needed for co-registration if `FSL` is not installed)",
                value = raveio::cmd_afni_home(error_on_missing = FALSE, unset = "")
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
      pipeline$set_settings(
        path_mri = input$mri_path,
        path_ct = input$ct_path,
        skip_recon = FALSE,
        skip_coregistration = FALSE,
        dcm2niix_path = input$cmd_dcm2niix_path,
        freesurfer_path = input$cmd_fs_path,
        fsl_path = input$cmd_fsl_path,
        afni_path = input$cmd_afni_path,
        .list = settings
      )

      res <- pipeline$run(
        as_promise = TRUE,
        names = "check_result",
        type = "vanilla",
        scheduler = "none"
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          check_result <- pipeline$read("check_result")
          cmd_tools <- pipeline$read("cmd_tools")

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
            shiny::p("External commandline paths. "),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("dcm2niix"), ": ", paste(cmd_tools$dcm2niix, collapse = "")),
              shiny::tags$li(shiny::strong("FreeSurfer"), ": ", paste(cmd_tools$freesurfer, collapse = "")),
              shiny::tags$li(shiny::strong("FSL-flirt"), ": ", paste(cmd_tools$flirt, collapse = "")),
              shiny::tags$li(shiny::strong("AFNI"), ": ", paste(cmd_tools$afni, collapse = ""))
            )
          )

          dipsaus::close_alert2()

          shiny::showModal(shiny::modalDialog(
            title = "Confirmation",
            size = "l",
            shiny::p("Please confirm the to-do list and carefully read warnings. ",
                     "Please do NOT ignore warnings ",
                     "that have statements such as ",
                     '"the script will fail/error".'),
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
              "Found an error while checking the path/data:\n\n",
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
      shiny::removeModal()
      # Let the module know the data has been changed
      ravedash::fire_rave_event('data_changed', Sys.time())
      ravedash::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = pipeline$read("project_name"),
        subject_code = pipeline$read("subject_code")
      )
    }),
    input$loader_ready_btn2,
    ignoreNULL = TRUE, ignoreInit = TRUE
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
      paths <- paths[dir.exists(file.path(subject$preprocess_settings$raw_path, paths)) | grepl("nii($|\\.gz$)", x = paths, ignore.case = TRUE)]
      paths <- paths[!paths %in% c("", ".", "..", "/")]
      paths <- paths[!startsWith(paths, "rave-imaging")]

      selected <- NULL
      if(length(paths)) {
        selected <- paths[grepl("MR", paths)]
        if(length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_mri_path", namespace = pipeline$pipeline_name),
        pipeline$get_settings("path_mri"), selected
      ) %OF% paths
      shiny::updateSelectInput(session = session, inputId = "mri_path", choices = paths, selected = selected)

      if(length(paths)) {
        selected <- paths[grepl("CT", paths)]
        if(length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_ct_path", namespace = pipeline$pipeline_name),
        pipeline$get_settings("path_ct"), selected
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
