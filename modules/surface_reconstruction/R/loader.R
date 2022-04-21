# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(width = 2L),
      shiny::column(
        width = 8L,
        ravedash::input_card(
          title = "Configurations",
          class_header = "",

          shiny::p(
            shiny::tags$small(
              "This module imports raw MR & CT images into Nifti format using ",
              "dcm2niix, re-constructs ",
              "surface using FreeSurfer, and co-registers CT to MRI via ",
              "FSL-FLIRT. Please note that this module is experimental and ",
              "simply wraps bundles of command-lines. ",
              "Some of these commands (e.g. FreeSurfer) might not work properly under Windows."
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

          ravedash::flex_group_box(
            title = "MRI",
            shidashi::flex_item(
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] !== true", ns("has_freesurfer")),
                shiny::selectInput(
                  inputId = ns("mri_path"),
                  label = "Raw MRI DICOM folder",
                  choices = character(0L)
                )
              )
            ),
            shidashi::flex_item(
              shiny::checkboxInput(
                inputId = ns("has_freesurfer"),
                label = "The FreeSurfer reconstruction has been imported to the subject folder (FreeSurfer reconstruction will be skipped)",
                value = FALSE
              )
            )
          ),

          ravedash::flex_group_box(
            title = "CT",
            shidashi::flex_item(
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] !== true", ns("has_coregistration")),
                shiny::selectInput(
                  inputId = ns("ct_path"),
                  label = "Raw CT DICOM folder",
                  choices = character(0L)
                )
              )
            ),
            shidashi::flex_item(
              shiny::checkboxInput(
                inputId = ns("has_coregistration"),
                label = "The CT image is absent or co-registration has been done before (Co-registration will be skipped)",
                value = FALSE
              )
            )
          ),

          ravedash::flex_group_box(
            title = "Command-line path",
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_dcm2niix_path"),
                label = "Dcm2niix path (needed to convert DICOM images to Nifti format)",
                value = global_data$dcm2niix_path
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_fs_path"),
                label = "FreeSurfer home (needed for surface reconstruction)",
                value = global_data$freesurfer_path
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::textInput(
                inputId = ns("cmd_flirt_path"),
                label = "FLIRT path (needed for co-registration)",
                value = global_data$flirt_path
              )
            )
          ),

          footer = shiny::tagList(
            shiny::actionLink(
              inputId = ns("loader_sync_btn"),
              label = "Sync project & subject from [Import LFP] module"
            ),
            shiny::hr(),
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
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
      pipeline_set(.list = settings)

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "imported_electrodes",
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          dipsaus::close_alert2()

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravedash::logger("Data has been loaded loaded")

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
      pipe2 <- raveio::pipeline_find("import_lfp_native")
      data <- structure(raveio::pipeline_read(c(
        "import_setup__project_name",
        "import_setup__subject_code"
      ), pipe_dir = pipe2),
      names = c("project_name", "subject_code"))

      shiny::updateSelectInput(
        session = session,
        inputId = loader_project$get_sub_element_id(),
        selected = data$project_name
      )

      shiny::updateSelectInput(
        session = session,
        inputId = loader_subject$get_sub_element_id(),
        selected = data$subject_code
      )

    }),
    input$loader_sync_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )


}
