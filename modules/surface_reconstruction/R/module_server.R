
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)


  # check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      # There is not too many interaction, so update everytime
      check_result <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "check_result")
      cmd_tools <- raveio::pipeline_read(pipe_dir = pipeline_path, var_names = "cmd_tools")
      local_reactives$project_name <- check_result$project_name
      local_reactives$subject_code <- check_result$subject_code
      local_reactives$path_mri <- check_result$path_mri
      local_reactives$path_ct <- check_result$path_ct
      local_reactives$subject_fspath <- check_result$fs_path
      local_reactives$actions <- list(
        fs_reconstructed = check_result$fs_reconstructed,
        skip_recon = check_result$skip_recon,
        skip_coregistration = check_result$skip_coregistration
      )
      local_reactives$tools <- cmd_tools

      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration", method = "collapse")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "expand")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration", method = "collapse")
    }),
    input$jump_import_dicom,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "expand")
      shidashi::card_operate(title = "Co-registration", method = "collapse")
    }),
    input$jump_recon,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration", method = "expand")
    }),
    input$jump_coregistration,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )

  output$basic_info <- shiny::renderUI({
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }

    project_name <- local_reactives$project_name
    subject_code <- local_reactives$subject_code
    actions <- local_reactives$actions
    tools <- local_reactives$tools
    if(!length(project_name)) { return() }

    shiny::div(
      "Project: ", shiny::strong(project_name),
      shiny::br(),
      "Subject: ", shiny::strong(subject_code),
      shiny::br(),
      "Actions:",
      shiny::tags$ul(
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_import_dicom"),
            label = shiny::tagList(
              "Convert .dcm images to .nii ", local({
                if(isTRUE(file.exists(tools$dcm2niix))) {
                  shiny::span(type = "primary", ravedash::shiny_icons$arrow_right)
                } else {
                  shiny::icon("times")
                }
              })
            )
          )
        ),
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_recon"),
            label = shiny::tagList(
              "Surface reconstruction ", local({
                if(actions$skip_recon) {
                  shiny::icon("times")
                } else {
                  ravedash::shiny_icons$arrow_right
                }
              })
            )
          )
        ),
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_coregistration"),
            label = shiny::tagList(
              "CT co-registration ", local({
                if(actions$skip_coregistration) {
                  shiny::icon("times")
                } else {
                  ravedash::shiny_icons$arrow_right
                }
              })
            )
          )
        )
      )
    )


  })

  get_dcm2niix_cmd <- function(type = c("MRI", "CT")){

    type <- match.arg(type)

    subject_fspath <- local_reactives$subject_fspath
    dcm2niix <- local_reactives$tools$dcm2niix
    if(!file.exists(dcm2niix)) {
      dcm2niix <- "dcm2niix"
    }

    if(type == "MRI") {
      indir <- local_reactives$path_mri
    }


    outdir <- normalizePath(file.path(subject_fspath, "RAVE_raw", type), mustWork = FALSE)

    cmd <- c(
      sprintf("mkdir -p %s", shQuote(outdir)),
      sprintf("%s -o %s %s", dcm2niix, shQuote(outdir, type = "cmd"), shQuote(indir, type = "cmd"))
    )

    cmd
  }



  output$panel_import_dicom <- shiny::renderUI({

    cmd <- get_dcm2niix_cmd()

    dry_run <- raveio::is_dry_run()

    shiny::div(
      shiny::p(
        "The following script uses ", shiny::pre(class="pre-compact no-padding display-inline", "dcm2niix"),
        " external library to convert DICOM images to Nifti format for later purposes. ",
        "To run the script, please open your console terminal and copy-paste the following commands into ",
        "the terminal, and then hit ", shiny::pre(class="pre-compact no-padding", "Return/Enter"),
        " button. ", ifelse(dry_run, "", "Alternatively, you could let RAVE run these system command for you.")
      ),
      shiny::div(
        class = "clipboard-btn shidashi-clipboard-output",
        shiny::pre(
          class='pre-compact no-padding bg-gray-90',
          paste(c("/bin/bash", cmd), collapse = "\n")
        ),
        `data-clipboard-text` = paste(c("/bin/bash", cmd), collapse = "\n"),
        title='Click to copy!',
        role = "button"
      )


    )

  })



}
