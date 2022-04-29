
module_server <- function(input, output, session, ...){

  error_notification <- function(e) {
    if(!inherits(e, "condition")) {
      e <- simpleError(message = e$message)
    }
    ravedash::logger_error_condition(e)
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE, delay = 30000,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }
  info_notification <- function(...) {
    ravedash::logger(..., level = "info")
    shidashi::show_notification(
      message = paste(..., sep = ""),
      title = "Notification",
      type = "success",
      close = TRUE,
      autohide = TRUE,
      class = ns("info_notif"),
      collapse = "\n"
    )
  }


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
      paths <- raveio::rave_directories(subject_code = check_result$subject_code, project_name = check_result$project_name)
      local_reactives$temp_dir <- check_result$path_temp
      local_reactives$path_mri <- check_result$path_mri
      local_reactives$path_ct <- check_result$path_ct
      local_reactives$subject_fspath <- check_result$fs_path
      local_reactives$actions <- list(
        fs_reconstructed = check_result$fs_reconstructed,
        skip_recon = check_result$skip_recon,
        skip_coregistration = check_result$skip_coregistration
      )
      local_reactives$tools <- cmd_tools
      local_reactives$build_command <- Sys.time()

      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT to T1", method = "collapse")

      component_container$reset_data()
      component_container$data$subject <- raveio::RAVESubject$new(
        project_name = check_result$project_name,
        subject_code = check_result$subject_code,
        strict = FALSE
      )

      update_nii_t1()
      update_nii_ct()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "expand")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT to T1", method = "collapse")
    }),
    input$jump_import_dicom,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "expand")
      shidashi::card_operate(title = "Co-registration CT to T1", method = "collapse")
    }),
    input$jump_recon,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Images", method = "collapse")
      shidashi::card_operate(title = "Surface Reconstruction", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT to T1", method = "expand")
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


  update_nii_t1 <- function(){
    mri_path <- shiny::isolate(file.path(local_reactives$temp_dir, "inputs", "MRI"))
    choices <- character(0L)
    if(length(mri_path) == 1 && !is.na(mri_path) && dir.exists(mri_path)) {
      choices <- list.files(mri_path, pattern = "nii($|\\.gz$)", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
    }
    if(!length(choices)) {
      choices <- character(0L)
    }

    selected <- tryCatch({
      subject <- component_container$data$subject
      subject$get_default("nii_t1", default_if_missing = input$param_fs_infile,
                          namespace = "surface_reconstruction")
    }, error = function(e){
      input$param_fs_infile
    })

    selected <- selected %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_fs_infile",
      choices = choices, selected = selected
    )
  }

  update_nii_ct <- function(){
    ct_path <- shiny::isolate(file.path(local_reactives$temp_dir, "inputs", "CT"))
    choices <- character(0L)
    if(length(ct_path) == 1 && !is.na(ct_path) && dir.exists(ct_path)) {
      choices <- list.files(ct_path, pattern = "nii($|\\.gz$)", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
    }
    if(!length(choices)) {
      choices <- character(0L)
    }

    selected <- tryCatch({
      subject <- component_container$data$subject
      subject$get_default("nii_ct", default_if_missing = input$param_coreg_ct,
                          namespace = "surface_reconstruction")
    }, error = function(e){
      input$param_coreg_ct
    })

    selected <- selected %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_coreg_ct",
      choices = choices, selected = selected
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      update_nii_t1()
    }),
    input$param_fs_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      update_nii_ct()
    }),
    input$param_coreg_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  watch_log <- function(){
    path <- local_data$log_file
    if(length(path) != 1 || is.na(path) || !file.exists(path) || path == '') {
      msg <- NULL
    } else {
      suppressWarnings({
        msg <- readLines(path)
      })
      if(!length(msg) || isTRUE(msg == "")) {
        msg <- "Waiting for outputs..."
      }
    }
    msg <- paste(msg, collapse = "\n")

    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("pre#%s", ns("verbatim_log")),
        content = paste0(
          '<code class="hljs-literal" style="word-wrap:break-word;width: 100%;white-space: pre-wrap;">',
          msg,
          '</code>'
        )
      )
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      local_data$log_file <- NULL
      shidashi::clear_notifications(class = "dismissible")
      shiny::removeModal()
    }),
    input$dismiss_modal,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  run_command_pipeline <- function(
    script_name = "script_dcm2nii",
    execute_name = "conversion_mri",
    title = "Running dcm2niix",
    dryrun = TRUE, ...
  ) {
    dryrun <- raveio::is_dry_run() || dryrun
    pipeline_set(
      dryrun = dryrun
    )

    res <- raveio::pipeline_run(
      pipe_dir = pipeline_path,
      names = script_name,
      scheduler = "none",
      type = "vanilla",
      async = FALSE
    )
    script_details <- raveio::pipeline_read(script_name, pipe_dir = pipeline_path)
    local_data$log_file <- script_details$log_file

    if(dryrun) {
      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = execute_name,
        scheduler = "none",
        type = "vanilla"
      )

    } else {

      ravedash::logger("Running system command with log file: ", local_data$log_file, level = "info")

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = execute_name,
        scheduler = "none",
        type = "vanilla",
        async = TRUE,
        check_interval = 1,
        progress_quiet = TRUE
      )
      res$async_callback <- watch_log
      shiny::showModal(shiny::modalDialog(
        title = title,
        size = "l", easyClose = FALSE,
        shidashi::flex_container(
          class = "fill-width max-height-500 overflow-y-auto",
          style = "flex-direction: column-reverse",
          shidashi::flex_item(
            shiny::verbatimTextOutput(ns("verbatim_log"), placeholder = TRUE)
          )
        ),
        footer = dipsaus::actionButtonStyled(ns("dismiss_modal"), "Running...", disabled = "")
      ))
    }

    res$promise$then(
      onFulfilled = function(...) {

        watch_log()
        dipsaus::updateActionButtonStyled(session = session, inputId = 'dismiss_modal', disabled = FALSE, label = "Finished")

        conv <- raveio::pipeline_read(execute_name, pipe_dir = pipeline_path)
        if(conv$dry_run) {
          shidashi::show_notification(
            message = shiny::div(
              paste0("Shell script saved to [", conv$script_path,
                     "]! Please open your terminal, ",
                     "paste the command below:"),
              shiny::hr(),
              shiny::pre(
                class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
                `data-dismiss`="toast",
                type = "button",
                `aria-label`="Close",
                `data-clipboard-text` = conv$command,
                shiny::code( conv$command )
              )
            ),
            title = "Saved!", autohide = FALSE, close = TRUE,
            class = "dismissible",
            icon = ravedash::shiny_icons$terminal
          )
        } else {
          dipsaus::updateActionButtonStyled(session = session, inputId = 'dismiss_modal', disabled = FALSE, label = "Finished")
          shidashi::show_notification(
            message = c(
              "Done executing conmmand. ",
              "The shell command has been saved to: [", conv$script_path,
              "], and log file has been saved to: [", script_details$log_file,
              "]. RAVE does know if any error occurs in the command script. ",
              "Please read the log file by yourself."
            ),
            title = "Success!",
            autohide = FALSE, close = TRUE, type = "success",
            icon = ravedash::shiny_icons$terminal,
            class = "dismissible",
            collapse = ""
          )
        }
      },
      onRejected = function(e){
        try({
          if( length(script_details$log_file ) && file.exists(script_details$log_file)) {
            writeLines(c(
              readLines(script_details$log_file),
              "Found Errors... Please check the log file: ",
              e$message
            ), con = script_details$log_file)
          }
          watch_log()
        }, silent = TRUE)
        error_notification(e)
      }
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      run_command_pipeline(dryrun = TRUE, title = "Running dcm2niix",
                           script_name = "script_dcm2nii", execute_name = "conversion_mri")
    }),
    input$btn_dcm2niix_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger("Running dcm2niix from console", level = "info")
      run_command_pipeline(dryrun = FALSE, title = "Running dcm2niix",
                           script_name = "script_dcm2nii", execute_name = "conversion_mri")
    }),
    input$btn_dcm2niix_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      run_command_pipeline(dryrun = TRUE, title = "Running FreeSurfer `recon-all`",
                           script_name = "script_recon", execute_name = "fs_recon")
    }),
    input$btn_recon_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger("Running fs recon-all from console", level = "info")
      run_command_pipeline(dryrun = FALSE, title = "Running FreeSurfer `recon-all`",
                           script_name = "script_recon", execute_name = "fs_recon")
    }),
    input$btn_recon_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      run_command_pipeline(dryrun = TRUE, title = "Running FSL-flirt to co-registrate CT to T1",
                           script_name = "script_coreg", execute_name = "coreg_results")
    }),
    input$btn_coreg_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      run_command_pipeline(dryrun = FALSE, title = "Running FSL-flirt to co-registrate CT to T1",
                           script_name = "script_coreg", execute_name = "coreg_results")
    }),
    input$btn_coreg_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  input_params <- shiny::debounce(shiny::reactive({

    list(
      nii_t1 = input$param_fs_infile,
      nii_ct = input$param_coreg_ct,
      dcm2niix = list(
        merge = input$param_dcm2niix_merge,
        float = input$param_dcm2niix_float,
        crop = input$param_dcm2niix_crop
      ),
      freesurfer = list(
        steps = input$param_fs_steps,
        fresh_start = isTRUE(input$param_fs_fresh_start)
      )
    )
  }), priority = 1L, millis = 300)



  shiny::bindEvent(
    shiny::observe({
      params <- input_params()
      print(params)
      # save parameters
      pipeline_set(
        params = params
      )

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = c("settings", "params", "script_dcm2nii"),
        type = "vanilla",
        scheduler = "none",
        shortcut = TRUE
      )

      res$promise$then(
        onFulfilled = function(...){
          script_dcm2nii <- raveio::pipeline_read(var_names = "script_dcm2nii", pipe_dir = pipeline_path)
          local_reactives$script_dcm2nii <- script_dcm2nii
        },
        onRejected = function(e) {
          local_reactives$script_dcm2nii <- list(
            error = TRUE,
            reason = e
          )
        }
      )

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "script_recon",
        type = "vanilla",
        scheduler = "none",
        shortcut = TRUE
      )

      res$promise$then(
        onFulfilled = function(...){
          script_recon <- raveio::pipeline_read(var_names = "script_recon", pipe_dir = pipeline_path)
          local_reactives$script_recon <- script_recon
        },
        onRejected = function(e) {
          local_reactives$script_recon <- list(
            error = TRUE,
            reason = e
          )
        }
      )


      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "script_coreg",
        type = "vanilla",
        scheduler = "none",
        shortcut = TRUE
      )

      res$promise$then(
        onFulfilled = function(...){
          script_coreg <- raveio::pipeline_read(var_names = "script_coreg", pipe_dir = pipeline_path)
          local_reactives$script_coreg <- script_coreg
        },
        onRejected = function(e) {
          local_reactives$script_coreg <- list(
            error = TRUE,
            reason = e
          )
        }
      )


    }),
    local_reactives$build_command,
    input_params(),
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  render_shell <- function(cmd){
    shiny::div(
      # class = "clipboard-btn shidashi-clipboard-output",
      shiny::pre(
        class='pre-compact bg-gray-90',
        lapply(cmd, function(s){
          if(startsWith(trimws(s), "#")) {
            shiny::tags$code(class = "hljs-comment", s)
          } else {
            shiny::tags$code(class = "hljs-literal", s)
          }
        })
      ),
      `data-clipboard-text` = paste(c(
        "/bin/sh",
        unlist(lapply(cmd, function(s){
          if(startsWith(trimws(s), "#")) { s <- NULL }
          s
        })),
        "\n"
      ), collapse = "\n"),
      # title='Click to copy!',
      # role = "button"
    )
  }

  output$panel_import_dicom <- shiny::renderUI({

    script_dcm2nii <- as.list(local_reactives$script_dcm2nii)

    shiny::validate(shiny::need(
      isFALSE(script_dcm2nii$error),
      message = local({
        if(length(script_dcm2nii$reason)) {
          paste(script_dcm2nii$reason$message, collapse = " ")
        } else {
          "Cannot find valid MRI/CT"
        }
      })
    ))

    cmd <- script_dcm2nii$script

    render_shell(cmd)
  })

  output$panel_fs_recon <- shiny::renderUI({

    script_recon <- as.list(local_reactives$script_recon)

    shiny::validate(shiny::need(
      isFALSE(script_recon$error),
      message = local({
        if(length(script_recon$reason)) {
          paste(script_recon$reason$message, collapse = " ")
        } else {
          "Cannot run FreeSurfer recon-all"
        }
      })
    ))

    cmd <- script_recon$script
    render_shell(cmd)
  })

  output$panel_coreg <- shiny::renderUI({

    script_coreg <- as.list(local_reactives$script_coreg)

    shiny::validate(shiny::need(
      isFALSE(script_coreg$error),
      message = local({
        if(length(script_coreg$reason)) {
          paste(script_coreg$reason$message, collapse = " ")
        } else {
          "Cannot run FreeSurfer recon-all"
        }
      })
    ))

    cmd <- script_coreg$script
    render_shell(cmd)
  })



}
