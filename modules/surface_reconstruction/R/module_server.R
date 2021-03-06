
module_server <- function(input, output, session, ...){

  error_notification <- function(e) {
    shidashi::clear_notifications(class = ns("error_notif"))
    if(!inherits(e, "condition")) {
      e <- simpleError(message = e$message)
    }
    ravedash::logger_error_condition(e)
    Sys.sleep(0.1)
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
      check_result <- pipeline$read(var_names = "check_result")
      cmd_tools <- pipeline$read(var_names = "cmd_tools")
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
                if(length(tools$dcm2niix) == 1 &&
                   !is.na(tools$dcm2niix) &&
                   isTRUE(file.exists(tools$dcm2niix))) {
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
      later::later(watch_log, delay = 0.5)
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

  run_command_pipeline <- function(cmd, wait = TRUE, title = "Running Terminal Command") {

    shidashi::clear_notifications(class = ns("error_notif"))

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

    cmd$execute(dry_run = TRUE, backup = FALSE)

    renderMsg <- function(msg) {
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
    check <- dipsaus::rs_exec(bquote({
      script <- .(cmd$script)
      script_path <- .(cmd$script_path)
      log_path <- .(cmd$log_file)
      writeLines(c(
        "=================== Start: shell script ===================",
        script,
        paste("=================== Log:", Sys.time(), "===================")
      ), con = log_path)
      if(.(!wait)) {
        cat(script)
      }
      Sys.sleep(0.5)
      raveio::cmd_execute(script = script, script_path = script_path, stderr = log_path)
    }), wait = wait, quoted = TRUE, name = title)

    final <- function(has_error = FALSE) {
      dipsaus::updateActionButtonStyled(
        session = session, inputId = 'dismiss_modal', disabled = FALSE,
        label = ifelse(has_error, "Gotcha", "Done"))
      shidashi::show_notification(
        message = shiny::div(
          "Done executing conmmand. ",
          "Please check the ",
          shiny::a("log file", target="_blank",
                   href=sprintf("file://%s", cmd$log_file)),
          "(this link only works when you run RAVE locally. Try right-click and open) ",
          "to see ", ifelse(has_error, "the error information.", "the full log")
        ),
        title = ifelse(has_error, "Error!", "Finished!"),
        autohide = FALSE, close = TRUE, type = "dark",
        icon = ravedash::shiny_icons$terminal,
        class = "dismissible"
      )
    }

    promise <- promises::promise(function(resolve, reject) {
      listener <- function() {
        if(is.function(check)) {
          code <- check()
        } else {
          code <- check
        }
        path <- cmd$log_file
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

        if(code == 0) {
          renderMsg(c(msg, "Finished."))
          resolve(attr(code, "rs_exec_result"))
        } else if(code < 0) {
          renderMsg(c(msg, "An error is detected."))
          reject(1)
        } else {
          renderMsg(msg)
          later::later(listener, delay = 0.5)
        }
      }
      listener()
    })

    promises::then(
      promise,
      onFulfilled = function(...) {
        final(has_error = FALSE)
      },
      onRejected = function(...) {
        final(has_error = TRUE)
      }
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      # cmd2 <- res$import_CT
      cmd1 <- local_reactives$bash_scripts$import_T1
      cmd2 <- local_reactives$bash_scripts$import_CT
      if(isTRUE(cmd1$error)) {
        error_notification(cmd1$condition)
        return()
      }
      if(isTRUE(cmd2$error)) {
        error_notification(cmd2$condition)
        return()
      }

      script1 <- cmd1$execute(dry_run = TRUE, backup = FALSE)
      script2 <- cmd2$execute(dry_run = TRUE, backup = FALSE)

      script <- paste(script1, script2, sep = "\n")

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", dirname(cmd1$script_path),
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }),
    input$btn_dcm2niix_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      cmd <- local_reactives$bash_scripts$import_T1
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravedash::logger("Running dcm2niix from console - Importing T1 MRI", level = "info")

      run_command_pipeline(cmd = cmd, wait = FALSE, title = "Importing T1")
    }),
    input$btn_dcm2niix_run_t1,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      cmd <- local_reactives$bash_scripts$import_CT
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravedash::logger("Running dcm2niix from console - Importing CT", level = "info")

      run_command_pipeline(cmd = cmd, wait = FALSE, title = "Importing CT")
    }),
    input$btn_dcm2niix_run_ct,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd <- res$fs_recon
      # cmd2 <- res$import_CT
      cmd <- local_reactives$bash_scripts$fs_recon
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }

      script <- cmd$execute(dry_run = TRUE, backup = FALSE)

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", cmd$script_path,
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }),
    input$btn_recon_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      cmd <- local_reactives$bash_scripts$fs_recon
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravedash::logger("Running FreeSurfer recon-all from console", level = "info")
      run_command_pipeline(cmd = cmd, wait = FALSE, title = "FreeSurfer recon-all")
    }),
    input$btn_recon_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd <- res$coreg_flirt
      if(identical(input$coreg_ct_program, "AFNI")) {
        cmd <- local_reactives$bash_scripts$coreg_3dallineate
      } else {
        cmd <- local_reactives$bash_scripts$coreg_flirt
      }
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }

      script <- cmd$execute(dry_run = TRUE, backup = FALSE)

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", cmd$script_path,
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }),
    input$btn_coreg_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd <- res$coreg_flirt
      if(identical(input$coreg_ct_program, "AFNI")) {
        bin <- "AFNI/3dAllineate"
        cmd <- local_reactives$bash_scripts$coreg_3dallineate
      } else {
        bin <- "FSL/flirt"
        cmd <- local_reactives$bash_scripts$coreg_flirt
      }
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravedash::logger("Running {bin} from console", level = "info", use_glue = TRUE)
      run_command_pipeline(cmd = cmd, wait = FALSE, title = bin)
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
        flag = input$param_fs_steps,
        fresh_start = isTRUE(input$param_fs_fresh_start)
      )
    )
  }), priority = 1L, millis = 300)



  shiny::bindEvent(
    shiny::observe({
      params <- input_params()
      # save parameters
      pipeline$set_settings(
        params = params
      )
      shidashi::clear_notifications(class = ns("error_notif"))

      tryCatch({
        res <- pipeline$run(
          as_promise = FALSE,
          names = c("settings", 'subject', "params", "import_T1", "import_CT",
                    "fs_recon", "coreg_flirt", "coreg_3dallineate"),
          type = "vanilla",
          scheduler = "none",
          shortcut = TRUE
        )
        local_reactives$bash_scripts <- res
      }, error = function(e) {
        error_notification(e)
      })

    }),
    local_reactives$build_command,
    input_params(),
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  render_shell <- function(cmd){
    cmd <- paste(cmd, collapse = "\n")
    cmd <- unlist(strsplit(cmd, "\n"))
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

  output$panel_import_T1 <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$import_T1
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_import_CT <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$import_CT
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_fs_recon <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$fs_recon
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_coreg <- shiny::renderUI({
    if(identical(input$coreg_ct_program, "AFNI")) {
      cmd <- local_reactives$bash_scripts$coreg_3dallineate
    } else {
      cmd <- local_reactives$bash_scripts$coreg_flirt
    }

    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })



}
