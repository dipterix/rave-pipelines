
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text",
      "analysis_ranges"
    ))
  )


  # Input validators
  sv_bandpass <- validator_bandpass(input, output, session)
  sv_notch <- validator_notch(input, output, session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      if(!sv_bandpass$is_valid()) {
        ravedash::error_notification(
          list(message = "Please check the band-passing filter settings and correct the inputs as suggested.")
        )
      }
      if(!sv_notch$is_valid()) {
        ravedash::error_notification(
          list(message = "Please check the Notch filter settings and correct the inputs as suggested.")
        )
      }

      local_data$results <- list(valid = FALSE)

      if(isTRUE(input$filter_bandpass__enabled)) {
        filter_bandpass <- list(
          enabled = TRUE,
          order = input$filter_bandpass__filter_order,
          range = sort(c(
            input$filter_bandpass__freq_lb,
            input$filter_bandpass__freq_ub
          ))
        )
      } else {
        filter_bandpass <- list(enabled = FALSE)
      }

      if( isTRUE(input$filter_notch__enabled) ){
        base_frequency <- as.numeric(input$filter_notch__base_frequency)
        harmonics <- dipsaus::parse_svec(input$filter_notch__harmonics, unique = FALSE)
        half_bandwidths <- dipsaus::parse_svec(input$filter_notch__bandwidths, unique = FALSE)
        filter_notch <- list(
          enabled = TRUE,
          lower_bounds = base_frequency * abs(harmonics) - abs(half_bandwidths),
          upper_bounds = base_frequency * abs(harmonics) + abs(half_bandwidths)
        )
      } else { filter_notch <- list(enabled = FALSE) }

      # Collect input data
      pipeline$set_settings(
        analysis_electrodes = input$electrode_text,
        analysis_block = input$analysis_block,
        filter_bandpass = filter_bandpass,
        filter_notch = filter_notch,
        pwelch_params = list(
          window_size = input$pwelch_window_size,
          noverlap = ceiling(input$pwelch_window_size * input$pwelch_window_overlap / 100)
        )
      )

      # Invalidate previous results (stop them because they are no longer needed)
      if(!is.null(local_data$pipeline_promise)) {
        try({
          ravedash::logger("Invalidating previous run", level = "trace")
          local_data$pipeline_promise$invalidate()
        }, silent = TRUE)
      }

      async <- FALSE

      if(!async) {
        dipsaus::shiny_alert2(
          title = "Running the pipeline",
          text = "Filtering signals, generating Welch-Periodograms...",
          icon = "info", danger_mode = FALSE, auto_close = FALSE, buttons = FALSE
        )
      }

      time_start <- Sys.time()

      local_data$pipeline_promise <- pipeline$run(
        names = c("settings_path", "settings", "analysis_block", "pwelch_params2",
                  "analysis_electrodes", "filter_notch", "filter_bandpass",
                  "pwelch_params", "subject", "cleaned_inputs", "analysis_electrodes2",
                  "filtered_data", "pwelch_data"),
        shortcut = TRUE, async = async, as_promise = TRUE, scheduler = "none", type = "smart"
      )

      time_end <- Sys.time()
      delta <- dipsaus::time_delta(time_start, time_end)
      if(!async && delta < 0.5) {
        Sys.sleep(0.5 - delta)
      }

      promises::then(
        local_data$pipeline_promise,
        onFulfilled = function(...) {
          shidashi::clear_notifications(class = "pipeline-error")
          dipsaus::close_alert2()
          local_reactives$update_outputs <- Sys.time()
          local_data$results <- list(valid = TRUE)
        },
        onRejected = function(e) {
          ravedash::error_notification(e, title = "Pipeline error!", class = "pipeline-error")
          local_data$results <- list(valid = FALSE)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_raw_voltage")) {
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_raw_voltage`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_raw_voltage")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # Reset preset UI & data
      component_container$reset_data()
      srate <- min(new_repository$subject$raw_sample_rates[new_repository$subject$electrodes %in% new_repository$electrode_list])
      component_container$data$repository <- new_repository
      component_container$data$sample_rate <- srate
      component_container$initialize_with_new_data()

      # source("~/Dropbox (PENN Neurotrauma)/dipterix/projects/rave-pipelines/modules/block_explorer/R/aa.R")
      # new_repository <- pipeline$read("repository")

      # Set input - analysis_block
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        choices = new_repository$blocks,
        selected = pipeline$get_settings(
          key = "analysis_block",
          default = input$analysis_block,
          constraint = new_repository$blocks
        )
      )

      # set input - electrode_text
      electrode_text <- dipsaus::deparse_svec(new_repository$electrode_list)
      shiny::updateTextInput(
        session = session,
        inputId = "electrode_text",
        label = sprintf("Electrodes to visualize (loaded: %s)", electrode_text),
        value = electrode_text
      )


      # set input group - band-passing
      current_order <- as.integer(input$filter_bandpass__filter_order)
      max_order <- floor((srate - 1) / 3)
      if(!length(current_order) || is.na(current_order) || current_order <= 0 || current_order > max_order) {
        current_order <- max_order
      }
      shiny::updateNumericInput(
        session = session,
        inputId = "filter_bandpass__filter_order",
        max = max_order,
        value = current_order
      )
      bp_lb <- input$filter_bandpass__freq_lb
      bp_ub <- input$filter_bandpass__freq_ub
      max_bp_rate <- floor(srate / 2)
      if(!length(bp_lb) || is.na(bp_lb) || bp_lb <= 0 || bp_lb > max_bp_rate) {
        bp_lb <- 0
      }
      if(!length(bp_ub) || is.na(bp_ub) || bp_ub <= bp_lb || bp_ub > max_bp_rate) {
        bp_ub <- max_bp_rate
      }
      shiny::updateSliderInput(
        session = session,
        inputId = "filter_bandpass__freq_lb",
        max = max_bp_rate, value = bp_lb
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "filter_bandpass__freq_ub",
        max = max_bp_rate, value = bp_ub
      )

      # set input group - pwelch
      pwelch_fh <- srate * 2
      pwelch_fl <- min(100, pwelch_fh)
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_window_size",
        min = pwelch_fl,
        max = pwelch_fh
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_frequency_limit",
        max = floor(srate / 2)
      )

      local_reactives$update_outputs <- FALSE

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      blocks <- component_container$data$repository$blocks
      if(!length(blocks)) { return() }
      current_idx <- which(blocks %in% input$analysis_block)
      if(length(current_idx)) {
        current_idx <- current_idx[[1]] - 1
        if(current_idx <= 0) {
          current_idx <- length(blocks)
        }
      } else {
        current_idx <- 1
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        selected = blocks[[current_idx]]
      )
    }),
    input$analysis_block__prev,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      blocks <- component_container$data$repository$blocks
      if(!length(blocks)) { return() }
      current_idx <- which(blocks %in% input$analysis_block)
      if(length(current_idx)) {
        current_idx <- current_idx[[1]] + 1
        if(current_idx > length(blocks)) {
          current_idx <- 1
        }
      } else {
        current_idx <- 1
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        selected = blocks[[current_idx]]
      )
    }),
    input$analysis_block__next,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  outputs_need_update <- function(message = "Please run the module first") {
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = message
      )
    )
    shiny::validate(
      shiny::need(
        isTRUE(local_data$results$valid),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )
  }

  # get brush
  get_brush <- shiny::reactive({
    brush <- input$plot_filtered_signals__brush
    if(!is.list(brush)) { return(NULL) }
    time_min <- max(brush$xmin, 0)
    time_max <- brush$xmax
    if(!isTRUE(time_min < time_max)) { return(NULL) }
    return(c(time_min, time_max))
  })

  get_show_hide_electrodes <- shiny::debounce(
    millis = 200,
    shiny::reactive({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){
        return(list(
          highlight = NULL,
          hide = NULL
        ))
      }
      highlight_electrodes <- dipsaus::parse_svec(input$highlight_channels, sort = TRUE)
      hide_electrodes <- dipsaus::parse_svec(input$hide_channels, sort = TRUE)

      if(!length(highlight_electrodes) && !length(hide_electrodes)) {
        return(list(
          highlight = NULL,
          hide = NULL
        ))
      }

      electrode_list <- tryCatch({
        pipeline$read("analysis_electrodes2")
      }, error = function(e) {
        component_container$data$repository$electrode_list
      })

      highlight_electrodes <- highlight_electrodes[highlight_electrodes %in% electrode_list]
      hide_electrodes <- hide_electrodes[hide_electrodes %in% electrode_list]

      return(list(
        highlight = highlight_electrodes,
        hide = hide_electrodes
      ))
    })
  )

  get_vspacing <- shiny::debounce(
    millis = 200,
    shiny::reactive({
      vspacing <- as.numeric(input$vertical_spacing)
      if(length(vspacing) != 1 || is.na(vspacing) || vspacing <= 0) {
        vspacing <- 1
      }
      vspacing
    })
  )


  # Register outputs
  ravedash::register_output(
    outputId = "plot_filtered_signals",
    render_function = shiny::renderPlot({
      outputs_need_update()

      show_hide <- get_show_hide_electrodes()
      highlight_electrodes <- show_hide$highlight
      hide_electrodes <- show_hide$hide
      vspacing <- get_vspacing()

      repository <- component_container$data$repository
      analysis_electrodes2 <- pipeline$read("analysis_electrodes2")
      col_sel <- repository$subject$electrodes %in% analysis_electrodes2 &
        (!repository$subject$electrodes %in% hide_electrodes)

      shiny::validate(
        shiny::need(
          any(col_sel),
          message = "No electrode channel to display"
        )
      )

      filtered_data <- pipeline$read("filtered_data")
      # make sure down-sample
      ntp <- nrow(filtered_data)
      nelec <- sum(col_sel)
      limit <- graphics_matplot_max_points
      sample_rate <- repository$sample_rate

      if(ntp * nelec > limit) {
        dsample <- ntp * nelec / limit
        tidx <- round(seq(1, ntp, by = dsample))
        sample_rate <- sample_rate / dsample
      } else {
        tidx <- seq_len(ntp)
      }

      plot_data <- filtered_data[tidx, col_sel, drop = FALSE]

      # construct colors, ylabels
      channels <- repository$subject$electrodes[col_sel]
      sel <- channels %in% highlight_electrodes
      if(any(sel)) {
        col <- rep("gray60", length(channels))
        col[sel] <- "orange"
      } else {
        col <- graphics::par("fg")
      }


      params <- ravetools::plot_signals(t(plot_data), sample_rate = sample_rate, tck = -0.005,
                              yaxs = "r", channel_names = channels, col = col, space = vspacing)
      local_reactives$channel_plot_params <- params
    }),
    output_opts = list(
      brush = shiny::brushOpts(
        id = ns("plot_filtered_signals__brush"),
        direction = "x",
        clip = TRUE,
        delayType = "debounce",
        delay = 300,
        opacity = 0.25,
        resetOnNew = FALSE
      )
    )
  )

  ravedash::register_output(
    outputId = "plot_filtered_signals_subset",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")
      brush <- get_brush()
      shiny::validate(shiny::need(length(brush) == 2, message = "Please draw a range from figure to my left."))

      show_hide <- get_show_hide_electrodes()
      highlight_electrodes <- show_hide$highlight
      hide_electrodes <- show_hide$hide
      vspacing <- local_reactives$channel_plot_params$space

      repository <- component_container$data$repository
      analysis_electrodes2 <- pipeline$read("analysis_electrodes2")
      col_sel <- repository$subject$electrodes %in% analysis_electrodes2 &
        (!repository$subject$electrodes %in% hide_electrodes)

      shiny::validate(
        shiny::need(
          any(col_sel),
          message = "No electrode channel to display"
        )
      )

      analysis_electrodes2 <- pipeline$read("analysis_electrodes2")
      filtered_data <- pipeline$read("filtered_data")
      sample_rate <- repository$sample_rate

      # make sure down-sample
      ntp <- nrow(filtered_data)
      timepoint_range <- brush * sample_rate
      timepoint_range[timepoint_range < 1] <- 1
      timepoint_range[timepoint_range > ntp] <- ntp

      ntp <- timepoint_range[2] - timepoint_range[1] + 1
      nelec <- length(analysis_electrodes2)
      limit <- graphics_matplot_max_points
      brush <- (timepoint_range - 1) / sample_rate

      if(ntp * nelec > limit) {
        dsample <- ntp * nelec / limit
        tidx <- round(seq(timepoint_range[1], timepoint_range[2], by = dsample))
        sample_rate <- sample_rate / dsample
      } else {
        tidx <- seq.int(timepoint_range[1], timepoint_range[2])
      }

      plot_data <- filtered_data[tidx, col_sel, drop = FALSE]

      # construct colors, ylabels
      channels <- repository$subject$electrodes[col_sel]
      sel <- channels %in% highlight_electrodes
      if(any(sel)) {
        col <- rep("gray60", length(channels))
        col[sel] <- "orange"
      } else {
        col <- graphics::par("fg")
      }

      ravetools::plot_signals(
        t(plot_data), sample_rate = sample_rate,
        tck = -0.005, yaxs = "r", time_shift = brush[[1]],
        main = sprintf("Data slice: %.2f sec", brush[[2]] - brush[[1]]),
        channel_names = channels, col = col, space = vspacing, space_mode = "absolute"
      )

    })
  )

  get_pwelch_data <- shiny::debounce(
    millis = 100,
    shiny::reactive({

      if(!length(local_reactives$update_outputs)) { return() }
      if(isFALSE(local_reactives$update_outputs)) { return() }
      if(!isTRUE(local_data$results$valid)) { return() }

      show_hide <- as.list(get_show_hide_electrodes())
      highlight_electrodes <- show_hide$highlight
      hide_electrodes <- show_hide$hide
      freq_range <- sort(c(input$pwelch_frequency_limit, 0, 300)[c(1,2)])

      analysis_electrodes2 <- pipeline$read("analysis_electrodes2")
      if(!length(analysis_electrodes2)) { return() }

      pwelch_data <- pipeline$read("pwelch_data")
      if(!inherits(pwelch_data, "FileArray")) { return() }

      dnames <- dimnames(pwelch_data)

      col_sel <- dnames$Electrode %in% analysis_electrodes2 & !dnames$Electrode %in% hide_electrodes
      row_sel <- dnames$Frequency >= freq_range[[1]] & dnames$Frequency <= freq_range[[2]]

      if(!any(row_sel)) {
        return("Frequency range is too small. Please adjust the input in '
Welch Periodogram Settings'.")
      }
      if(!any(col_sel)) {
        return()
      }

      display_electrodes <- dnames$Electrode[col_sel]
      highlight_sel <- display_electrodes %in% highlight_electrodes

      plot_data <- pwelch_data[row_sel, col_sel, drop = FALSE, dimnames = FALSE]

      list(
        highlight = display_electrodes[highlight_sel],
        hide = hide_electrodes,
        dnames = dnames,
        plot_data = 10*log10(plot_data),
        display_electrodes = display_electrodes,
        highlight_sel = highlight_sel,
        frequencies = dnames$Frequency[row_sel],
        has_highlight = length(display_electrodes) && any(highlight_sel)
      )

    })
  )

  ravedash::register_output(
    outputId = "plot_pwelch",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")

      pwelch_plot_data <- get_pwelch_data()

      shiny::validate(shiny::need(
        is.list(pwelch_plot_data) && length(pwelch_plot_data),
        message = paste(pwelch_plot_data, collapse = "")
      ))

      cleaned_inputs <- pipeline$read("cleaned_inputs")

      cex <- 1
      mar <- c(2.6, 3.8, 2.1, 0.6) * (0.5 + cex / 2)
      mgp <- cex * c(2, 0.5, 0)
      tck <- -0.02
      xline <- 1.2 * cex
      yline <- 2.0 * cex
      xaxs <- "i"
      yaxs <- "i"
      main <- 'Welch periodogram (no filter)'

      # themes <- ravedash::current_shiny_theme()
      cex_params <- graphics::par("fg", "bg", "mgp", "mar", "mai", "cex.main", "cex.lab", "cex.axis", "cex.sub")
      graphics::par(mar = mar, mgp = mgp)
      on.exit({
        do.call(graphics::par, cex_params)
      }, add = TRUE, after = FALSE)

      # Do not preserve dimnames to save time and space
      plot_data <- pwelch_plot_data$plot_data
      freq <- pwelch_plot_data$frequencies

      display_electrodes <- pwelch_plot_data$display_electrodes
      highlight_sel <- pwelch_plot_data$highlight_sel
      has_highlight <- any(highlight_sel)
      col <- dipsaus::col2hexStr(rep("gray60", length(display_electrodes)), alpha = 0.3)
      col[highlight_sel] <- "orange"

      # calculate mean-values
      mean1 <- rowMeans(plot_data, na.rm = TRUE)
      if(any(highlight_sel)) {
        mean2 <- rowMeans(plot_data[, highlight_sel, drop = FALSE], na.rm = TRUE)
      } else {
        mean2 <- NULL
      }

      graphics::matplot(
        x = freq, y = plot_data, col = col,
        type = 'l', cex = cex, lty = 1, lwd = 0.5, las = 1,
        axes = FALSE, xaxs = xaxs, yaxs = yaxs, cex.main = cex_params$cex.main * cex,
        main = main, log = "x", xlab = "", ylab = ""
      )
      if(isTRUE(cleaned_inputs$filter_bandpass$enabled)) {
        filter_range <- cleaned_inputs$filter_bandpass$range
        graphics::abline(v = filter_range, lty = 2)
        graphics::text(x = mean(filter_range), y = max(plot_data), labels = "Bandpass filter", adj = c(0.55, 1))
      }

      graphics::lines(x = freq, y = mean1, col = "dodgerblue3", lty = 1, lwd = 2)
      if(has_highlight) {
        graphics::lines(x = freq, y = mean2, col = "orangered", lty = 1, lwd = 3)
      }
      graphics::axis(1, at = pretty(freq), tck = -0.02,
                     cex = cex, cex.main = cex_params$cex.main * cex,
                     cex.lab = cex_params$cex.lab * cex,
                     cex.axis = cex_params$cex.axis * cex)
      graphics::axis(2, at = pretty(plot_data), tck = -0.02,
                     cex = cex, cex.main = cex_params$cex.main * cex,
                     cex.lab = cex_params$cex.lab * cex,
                     cex.axis = cex_params$cex.axis * cex)
      graphics::mtext(side = 2, text = "Power (dB)", line = yline,
                      cex = cex_params$cex.lab * cex)
      graphics::mtext(side = 1, text = "log(Frequency)", line = xline,
                      cex = cex_params$cex.lab * cex)

      if( has_highlight ) {
        lg_text <- c("Highlighted", "Mean of all displayed", "Mean of highlighted")
        lg_col <- c("orange", "dodgerblue3", "orangered")
        lg_lwd <- c(0.5, 2, 3)
      } else {
        lg_text <- c("Mean of all displayed")
        lg_col <- "dodgerblue3"
        lg_lwd <- 2
      }

      graphics::legend(
        "topright", lg_text, lty = 1, col = lg_col,
        lwd = lg_lwd, bty = "n", text.col = lg_col
      )



    })
  )

  get_pwelch_clickinfo <- shiny::reactive({
    info <- input$plot_pwelch__click
    if(!is.list(info) || length(info$x) != 1 || length(info$y) != 1) { return() }
    pwelch_plot_data <- get_pwelch_data()
    if(!(is.list(pwelch_plot_data) && length(pwelch_plot_data))) { return() }

    x <- info$x
    y <- info$y
    # find nearest frequency
    fidx <- which.min(abs(pwelch_plot_data$frequencies - x))[[1]]
    nearest_freq <- pwelch_plot_data$frequencies[fidx]

    power <- pwelch_plot_data$plot_data[fidx, ]
    pidx1 <- which.min(abs(power - y))[[1]]
    nearest_electrode1 <- pwelch_plot_data$display_electrodes[pidx1]
    power1 <- power[pidx1]

    re <- list(
      x = x, y = y,
      nearest_freq = nearest_freq,
      nearest_electrode1 = nearest_electrode1,
      power1 = power1
    )

    if(pwelch_plot_data$has_highlight) {
      highlighted_electrode <- pwelch_plot_data$display_electrodes[pwelch_plot_data$highlight_sel]
      tmp <- power[pwelch_plot_data$highlight_sel]
      pidx2 <- which.min(abs(tmp - y))[[1]]
      nearest_electrode2 <- highlighted_electrode[pidx2]
      power2 <- tmp[pidx2]

      re$nearest_electrode2 <- nearest_electrode2
      re$power2 <- power2
    }
    return(re)
  })

  output$graphic_summary <- shiny::renderUI({

    voltage_brush <- get_brush()
    pwelch_clickinfo <- get_pwelch_clickinfo()
    print(pwelch_clickinfo)

    # brush selection
    if(length(voltage_brush) < 2) {
      info_brush <- "No subset has been made"
    } else {
      voltage_brush <- sort(as.numeric(voltage_brush))
      info_brush <- sprintf("%.1f ~ %.1f (%.2f sec)",
                            voltage_brush[1], voltage_brush[2], voltage_brush[2] - voltage_brush[1])
    }

    if(!is.list(pwelch_clickinfo) || !length(pwelch_clickinfo)) {
      info_pwelch1 <- "(Please click on the left Welch-Periodogram)"
      info_pwelch2 <- ""
      info_pwelch3 <- ""
    } else {
      info_pwelch1 <- sprintf("%.1fHz", pwelch_clickinfo$nearest_freq)
      info_pwelch2 <- sprintf("%.0f (%.2f dB)",
                              pwelch_clickinfo$nearest_electrode1,
                              pwelch_clickinfo$power1)
      if(length(pwelch_clickinfo$nearest_electrode2)) {
        info_pwelch3 <- sprintf("%.0f (%.2f dB)",
                                pwelch_clickinfo$nearest_electrode2,
                                pwelch_clickinfo$power2)
      } else {
        info_pwelch3 <- "None"
      }
    }

    shiny::tags$dl(
      class = "row",

      shiny::tags$dt(class = "col-sm-2", "Subset time"),
      shiny::tags$dd(class = "col-sm-4", info_brush),

      shiny::tags$dt(class = "col-sm-2", "Frequency"),
      shiny::tags$dd(class = "col-sm-4", info_pwelch1),

      shiny::tags$dt(class = "col-sm-2", "Nearest pick"),
      shiny::tags$dd(class = "col-sm-4", info_pwelch2),

      shiny::tags$dt(class = "col-sm-2", "Highlighted"),
      shiny::tags$dd(class = "col-sm-4", info_pwelch3)

    )

  })

  ravedash::register_output(
    outputId = "plot_pwelch_subset",
    render_function = shiny::renderPlot({

      outputs_need_update(message = "")

      pwelch_plot_data <- get_pwelch_data()
      brush <- get_brush()

      shiny::validate(
        shiny::need(is.list(pwelch_plot_data) && length(pwelch_plot_data),message = ""),
        shiny::need(length(brush) == 2, message = "")
      )


    })
  )




}
