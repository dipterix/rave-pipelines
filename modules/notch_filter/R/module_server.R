
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    refresh = NULL,
    update_outputs = NULL
  )

  server_tools <- ravedash::get_default_handlers()

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  # local_data$subject
  # local_data$imported_electrodes


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      data <- raveio::pipeline_read(c("subject", "imported_electrodes"), pipe_dir = pipeline_path)

      subject <- data$subject
      blocks <- subject$preprocess_settings$blocks
      imported_electrodes <- data$imported_electrodes
      sample_rates <- subject$preprocess_settings$sample_rates

      local_data$subject <- subject
      local_data$imported_electrodes <- imported_electrodes

      notch_filtered <- subject$preprocess_settings$notch_filtered
      notch_filtered <- sapply(imported_electrodes, function(e){
        notch_filtered[subject$electrodes == e]
      })

      if(all(notch_filtered)) {
        shidashi::card_operate(title = "Filter settings", method = "collapse")
      } else {
        shidashi::card_operate(title = "Filter settings", method = "expand")
      }


      shiny::updateSelectInput(
        session = session,
        inputId = "block",
        choices = blocks,
        selected = input$block %OF% blocks
      )

      selected_electrode <- as.integer(input$electrode)
      selected_electrode <- selected_electrode %OF% imported_electrodes
      shiny::updateSelectInput(
        session = session,
        inputId = "electrode",
        choices = imported_electrodes,
        selected = selected_electrode
      )
      current_srate <- sample_rates[imported_electrodes == selected_electrode]
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_winlen",
        max = floor(current_srate * 2),
        value = floor(current_srate * 2)
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_freqlim",
        max = floor(current_srate / 2)
      )


      local_reactives$refresh <- Sys.time()
      # Reset outputs
      # shidashi::reset_output("signal_plot", message = "The plot has been reset. Please wait...")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  get_electrode <- shiny::bindEvent(
    shiny::reactive({
      if(!length(local_data$subject)) { return(integer()) }
      electrode <- input$electrode
      if(!length(electrode)){ return(integer()) }
      electrode <- as.integer(electrode)
      if(!electrode %in% local_data$imported_electrodes){ return(integer()) }

      subject <- local_data$subject
      sample_rates <- subject$preprocess_settings$sample_rates
      sample_rate <- sample_rates[local_data$imported_electrodes == electrode]

      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_winlen",
        max = floor(sample_rate * 2)
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_freqlim",
        max = floor(sample_rate / 2)
      )

      ravedash::logger("Switch to electrode ", electrode, level = "trace")

      return(electrode)
    }),
    input$electrode,
    local_reactives$refresh,
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      electrode <- get_electrode()
      if(!length(electrode)){ return() }
      imported_electrodes <- local_data$imported_electrodes
      idx <- which(imported_electrodes == electrode) + 1
      if(idx > length(imported_electrodes)) {
        idx <- 1
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "electrode",
        selected = imported_electrodes[[idx]]
      )
    }),
    input$next_electrode,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      electrode <- get_electrode()
      if(!length(electrode)){ return() }
      imported_electrodes <- local_data$imported_electrodes
      idx <- which(imported_electrodes == electrode) - 1
      if(idx <= 0) {
        idx <- length(imported_electrodes)
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "electrode",
        selected = imported_electrodes[[idx]]
      )
    }),
    input$previous_electrode,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )


  # check notch parameters
  parameter_validator <- shinyvalidate::InputValidator$new(session = session)
  parameter_validator$add_rule(
    inputId = "notch_filter_base_freq",
    rule = function(value){
      if(is.na(value) || value <= 0) {
        return("Line noise frequency must be positive")
      }
    }
  )
  parameter_validator$add_rule(
    inputId = "notch_filter_times",
    rule = function(mult_times){
      mult_times <- strsplit(mult_times, "[, ]+")[[1]]
      if(!length(mult_times)){ return() }
      mult_times <- as.numeric(mult_times)

      if(any(is.na(mult_times))){
        return("Contains invalid value, must be numerical values separated by `,`")
      }
      return()
    }
  )
  parameter_validator$add_rule(
    inputId = "notch_filter_bandwidth",
    rule = function(bandwidths){
      bandwidths <- strsplit(bandwidths, "[, ]+")[[1]]
      if(!length(bandwidths)){ return() }
      mult_times <- as.numeric(bandwidths)
      if(any(is.na(bandwidths))){
        return("Contains invalid value, must be numerical values separated by comma (,)")
      }
      return()
    }
  )
  parameter_validator$add_rule(
    inputId = "notch_filter_bandwidth",
    rule = function(bandwidths){
      mult_times <- input$notch_filter_times
      nbands1 <- length(strsplit(mult_times, "[, ]+")[[1]])
      nbands2 <- length(strsplit(bandwidths, "[, ]+")[[1]])

      if(nbands1 != nbands2) {
        return("Length mismatch")
      }
      return()
    }
  )
  parameter_validator$enable()

  output$notch_filter_preview <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        parameter_validator$is_valid(),
        message = "Please fix the errors above"
      )
    )
    base_freq <- input$notch_filter_base_freq
    mult_times <- input$notch_filter_times
    bandwidths <- input$notch_filter_bandwidth

    mult_times <- as.numeric(strsplit(mult_times, "[, ]+")[[1]])
    bandwidths <- as.numeric(strsplit(bandwidths, "[, ]+")[[1]])

    if(!length(mult_times)) {
      return(shiny::p("No notch filter will be applied. Signal will be copied 'as-is'."))
    }
    shiny::tags$ul(
      style = 'font-family: "Lucida Sans Typewriter", "Lucida Typewriter", "monospace", "Courier New", "Courier',
      lapply(seq_along(mult_times), function(i){
        center <- base_freq * mult_times[[i]]
        shiny::tags$li(
          shiny::strong(sprintf("Filter %d: ", i)),
          sprintf("%.1fHz - %.1fHz",
                  center - bandwidths[[i]],
                  center + bandwidths[[i]])
        )
      })
    )

  })

  error_notification <- function(e) {
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      tryCatch({
        if(!parameter_validator$is_valid()) {
          stop("The notch filter parameters are invalid. Please correct them")
        }
        # collect inputs and flush to pipeline settings.yaml

        base_freq <- input$notch_filter_base_freq
        mult_times <- input$notch_filter_times
        bandwidths <- input$notch_filter_bandwidth

        mult_times <- as.numeric(strsplit(mult_times, "[, ]+")[[1]])
        bandwidths <- as.numeric(strsplit(bandwidths, "[, ]+")[[1]])

        center_frequencies <- base_freq * mult_times
        notch_filter_lowerbound <- center_frequencies - bandwidths
        notch_filter_upperbound <- center_frequencies + bandwidths

        pipeline_set(
          notch_filter_lowerbound = notch_filter_lowerbound,
          notch_filter_upperbound = notch_filter_upperbound
        )

        res <- pipeline_run(pipe_dir = pipeline_path, scheduler = 'none', type = "vanilla", callr_function = NULL, names = "filter_settings", async = FALSE)

        res$promise$then(
          onFulfilled = function(...){

            subject <- raveio::pipeline_read("subject",
                                             pipe_dir = pipeline_path)
            filter_settings <- raveio::pipeline_read("filter_settings",
                                                     pipe_dir = pipeline_path)

            shiny::showModal(shiny::modalDialog(
              title = "Confirmation",
              shiny::p("It is always a good idea to check before running Notch filters!"),
              shiny::tags$ul(
                shiny::tags$li(
                  shiny::strong("Subject: "),
                  subject$subject_id
                ),
                shiny::tags$li(
                  shiny::strong("Electrodes: "),
                  dipsaus::deparse_svec(local_data$imported_electrodes)
                ),
                shiny::tags$li(
                  shiny::strong("Frequencies to remove: "),
                  local({
                    if(length(filter_settings$lb)) {
                      shiny::tags$ul(
                        lapply(
                          sprintf("%.1fHz-%.1fHz", filter_settings$lb, filter_settings$ub),
                          shiny::tags$li
                        )
                      )
                    } else {
                      "none (the signals will be copied as-is)"
                    }
                  })
                )
              ),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                dipsaus::actionButtonStyled(ns("notch_confirm"), "Confirm")
              ),
              size = "m", easyClose = FALSE
            ))
          },
          onRejected = function(e){
            error_notification(e)
          }
        )

      }, error = function(e) {
        error_notification(e)
      })


    }),
    input$notch_filter_btn,
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      dipsaus::shiny_alert2(
        title = "Applying Notch filters",
        text = ravedash::be_patient_text(),
        buttons = FALSE, auto_close = FALSE
      )

      res <-
        pipeline_run(
          names = "apply_notch",
          pipe_dir = pipeline_path,
          scheduler = 'none',
          type = "smart",
          callr_function = NULL,
          async = FALSE
        )

      res$promise$then(
        onFulfilled = function(...) {
          dipsaus::close_alert2()
          shiny::removeModal()
          shidashi::card_operate(title = "Filter settings", method = "collapse")
          dipsaus::shiny_alert2(
            title = "Finished!",
            icon = "success",
            text = ravedash::finished_text(),
            auto_close = TRUE, buttons = list(
              "Dismiss" = TRUE
            )
          )
        },
        onRejected = function(e) {
          dipsaus::close_alert2()
          dipsaus::shiny_alert2(
            title = "Error!",
            icon = "error",
            danger_mode = TRUE,
            text = paste(e$message, collapse = "\n"),
            auto_close = FALSE, buttons = list(
              "Dismiss" = TRUE
            )
          )
        }
      )

    }),
    input$notch_confirm,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Register outputs
  output$signal_plot <- shiny::renderPlot({
    electrode <- get_electrode()

    shiny::validate(
      shiny::need(
        length(electrode) == 1,
        message = "We cannot find any imported electrode"
      )
    )

    theme <- ravedash::current_shiny_theme()

    diagnose_notch_filters(
      subject = local_data$subject,
      electrodes = electrode,
      blocks = input$block,
      max_freq = input$pwelch_freqlim,
      winlen = input$pwelch_winlen,
      nbins = input$pwelch_nbins,
      bg = theme$background,
      fg = theme$foreground,
      cex = 3,
      std = 3,
      lwd = 0.3,
      mar = c(5.2, 5.4, 4.1, 2.1),
      mai = c(0.6, 0.8, 0.4, 0.1),
      quiet = TRUE
    )


  })


  output$download_as_pdf <- shiny::downloadHandler(
    filename = function(){
      subject <- local_data$subject
      sprintf("%s-%s-Notch_filter_diagnostic_plots.pdf", subject$project_name, subject$subject_code)
    },
    content = function(conn) {
      theme <- ravedash::current_shiny_theme()
      electrode <- local_data$imported_electrodes

      pipeline_set(
        diagnostic_plot_params = list(
          path = conn,
          window_length = input$pwelch_winlen,
          max_frequency = input$pwelch_freqlim,
          histogram_bins = input$pwelch_nbins,
          background = theme$background,
          foreground = theme$foreground,
          font_size = 2,
          quiet = FALSE
        )
      )

      dipsaus::shiny_alert2(
        title = "Exporting diagnostic plots",
        text = paste0(ravedash::be_patient_text(), " \n\n", "Please do **NOT** switch to other modules."),
        auto_close = FALSE, buttons = FALSE
      )

      results <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL,
        async = FALSE,
        shortcut = TRUE,
        names = c(
          "settings",
          "project_name",
          "subject_code",
          "diagnostic_plot_params",
          "subject",
          "imported_electrodes",
          "diagnostic_plots"
        )
      )
      # results$await(names = "diagnostic_plots")
      return(results$promise$then(
        onFulfilled = function(...){
          dipsaus::close_alert2()
        },
        onRejected = function(e){
          dipsaus::close_alert2()
          error_notification(e)
        }
      ))

    }
  )

}
