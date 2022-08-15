
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

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({


      local_data$results <- list(valid = FALSE)

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text"
        # "analysis_ranges"
      ))

      pipeline$set_settings(.list = settings)

      tryCatch({
        # repository <- pipeline$read(var_names = 'repository')
        #
        # args <- pipeline$get_settings()
        # args$repository <- repository
        # samples <- do.call(get_sample_data, args)
        # print(lobstr::obj_size(samples))

        target_names <- c("voltage_sample", "pwelch_overall", "power_sample")
        system.time({
        samples <- pipeline$eval(c("sample_electrode", "electrodes_loaded", "voltage",
                        "wavelet", "voltage_sample", "pwelch_overall", "power_sample"))
        })

        for(nm in target_names) {
          local_reactives[[nm]] <- samples[[nm]]
        }
        local_data$results <- list(valid = TRUE)
        local_reactives$update_outputs <- Sys.time()
        shidashi::clear_notifications(class = "pipeline-error")
      }, error = function(e) {
        msg <- paste(e$message, collapse = "\n")
        if(inherits(e, "error")){
          ravedash::logger(msg, level = 'error')
          ravedash::logger(traceback(e), level = 'error', .sep = "\n")
          shidashi::show_notification(
            message = msg,
            title = "Error while running pipeline", type = "danger",
            autohide = FALSE, close = TRUE, class = "pipeline-error"
          )
        }
      })

      # # Invalidate previous results (stop them because they are no longer needed)
      # if(!is.null(local_data$results)) {
      #   local_data$results$invalidate()
      #   ravedash::logger("Invalidating previous run", level = "trace")
      # }
      #
      #
      # # Collect input data
      # settings <- component_container$collect_settings(ids = c(
      #   "electrode_text"
      #   # "analysis_ranges"
      # ))
      #
      # pipeline$set_settings(.list = settings)
      #
      # results <- pipeline$run(
      #   as_promise = TRUE,
      #   scheduler = "future",
      #   type = "callr",
      #   callr_function = NULL,
      #   progress_title = "Calculating in progress",
      #   # async = TRUE,
      #   # check_interval = 0.3,
      #   shortcut = TRUE,
      #   names = c(
      #     "settings",
      #     names(settings),
      #     "baseline_method", "baseline_unit",
      #     "subject", "sample_electrode", "voltage",
      #     "wavelet", "voltage_sample", "pwelch_overall",
      #     "power_sample"
      #   )
      # )
      #
      #
      # local_data$results <- results
      # ravedash::logger("Scheduled: ", pipeline$pipeline_name,
      #                  level = 'debug', reset_timer = TRUE)
      #
      # results$promise$then(
      #   onFulfilled = function(...){
      #     ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
      #                      level = 'debug')
      #     shidashi::clear_notifications(class = "pipeline-error")
      #
      #     pipeline_data <- pipeline$read(var_names = c(
      #       "power_sample", "voltage_sample", "pwelch_overall"
      #     ))
      #
      #     local_reactives$power_sample <- pipeline_data$power_sample
      #     local_reactives$voltage_sample <- pipeline_data$voltage_sample
      #     local_reactives$pwelch_overall <- pipeline_data$pwelch_overall
      #     local_reactives$update_outputs <- Sys.time()
      #     return(TRUE)
      #   },
      #   onRejected = function(e, ...){
      #     msg <- paste(e$message, collapse = "\n")
      #     if(inherits(e, "error")){
      #       ravedash::logger(msg, level = 'error')
      #       ravedash::logger(traceback(e), level = 'error', .sep = "\n")
      #       shidashi::show_notification(
      #         message = msg,
      #         title = "Error while running pipeline", type = "danger",
      #         autohide = FALSE, close = TRUE, class = "pipeline-error"
      #       )
      #     }
      #     return(msg)
      #   }
      # )

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
      if(!inherits(new_repository, "prepare_subject_with_blocks")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `prepare_subject_with_blocks`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "prepare_subject_with_blocks")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # TODO: reset UIs to default

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
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

    brush <- input$plot_single_overall_power__brush
    if(!is.list(brush)) { return(NULL) }
    voltage_sample <- local_reactives$voltage_sample
    if(!is.list(voltage_sample)) { return(NULL) }

    pwelch_overall <- local_reactives$pwelch_overall
    if(!is.list(pwelch_overall)) { return(NULL) }

    time_min <- max(brush$xmin, 0)
    time_max <- brush$xmax
    if(time_min >= time_max) { return(NULL) }

    freq_min <- max(brush$ymin, 0)
    freq_max <- max(brush$ymax, freq_min)

    dnames <- shiny::isolate(local_reactives$power_sample$origin$dnames)

    # subset time
    sub_time <- dnames$Time[dnames$Time >= time_min & dnames$Time <= time_max]
    if(length(sub_time) < 2) { return(NULL) }
    time_range <- range(sub_time)

    # subset frequency
    freq <- dnames$Frequency
    mid_points <- c(0, (freq[-1] + freq[-length(freq)]) / 2, max(freq))
    idx_low <- c(which(mid_points <= freq_min), 1)
    idx_low <- max(idx_low)
    idx_high <- c(which(mid_points >= freq_max), length(mid_points))[[1]]
    if(idx_low < idx_high) {
      freq_range <- mid_points[c(idx_low, idx_high)]
    } else {
      freq_min <- (freq_min + freq_max) / 2
      freq_range <- freq[which.min(abs(freq - freq_min))] + c(-1, 1) * min(min(diff(mid_points)) / 2, 1)
      freq_range[freq_range < 0] <- 0
    }

    # subset voltage and calculate pwelch
    data <- voltage_sample$origin$data
    time <- seq(0, by = 1 / voltage_sample$origin$sample_rate, length.out = length(data))

    time_idx <- time >= time_range[1] & time <= time_range[2]
    freq <- pwelch_overall$freq

    win_size <- length(pwelch_overall$window)
    novlap <- min(sum(time_idx) / 200, win_size/2)
    novlap <- floor(win_size - novlap)

    subset_voltage <- data[time_idx]

    subset_pwelch <- ravetools::pwelch(
      x = subset_voltage,
      fs = pwelch_overall$fs,
      window = win_size,
      noverlap = novlap,
      nfft = pwelch_overall$nfft,
      plot = FALSE)


    re <- list(
      freq_range = freq_range,
      time_range = time_range,
      subset = list(
        time = time[time_idx],
        voltage = subset_voltage,
        pwelch = subset_pwelch
      )
    )
    re

  })

  # Register outputs
  ravedash::register_output(
    outputId = "plot_single_overall_power",
    render_function = shiny::renderPlot({
      outputs_need_update()

      power_sample <- local_reactives$power_sample
      time <- power_sample$origin$dnames$Time
      freq <- power_sample$origin$dnames$Frequency
      data <- t(power_sample$origin$data)

      sd <- stats::sd(data)
      zlim <- quantile(data, c(0.005, 0.995))
      zlim <- c(-1, 1) * max(abs(zlim))

      # pal <- c("#FFFFFF", "#FFFFFF", "#FFFFFF", "purple3", "orange")
      pal <- c("#053060", "darkgreen", "#FFFFFF", "orange", "#66001F")
      bias <- log(3 * sd / zlim[2]) / log(0.5)
      col <- c(
        rev(colorRampPalette(pal[c(3,2,1)], bias = bias)(100)),
        pal[[3]],
        colorRampPalette(pal[c(3,4,5)], bias = bias)(100)
      )


      fastplot_power_over_freq_time(data, time, freq, col, zlim = zlim)

      brush <- get_brush()
      if(is.list(brush)) {
        time_range <- brush$time_range
        freq_range <- brush$freq_range
        if(length(time_range) == 2 && length(freq_range) == 2) {
          graphics::rect(
            xleft = time_range[[1]],
            ybottom = freq_range[[1]],
            xright = time_range[[2]],
            ytop = freq_range[[2]],
            border = "#003366",
            col = grDevices::adjustcolor("#99ccff", 0.25)
          )
        }

      }




    }),
    output_opts = list(
      brush = shiny::brushOpts(
        id = ns("plot_single_overall_power__brush"),
        direction = "xy",
        clip = TRUE,
        delayType = "debounce",
        delay = 300,
        opacity = 0.25,
        resetOnNew = FALSE
      )
    )
  )

  ravedash::register_output(
    outputId = "plot_single_overall_voltge",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")
      brush <- get_brush()

      voltage_sample <- local_reactives$voltage_sample
      time_range <- brush$time_range

      rg <- range(voltage_sample$origin$data)

      res <- fastplot_signal_trace(
        data = voltage_sample$origin$data,
        sample_rate = voltage_sample$origin$sample_rate,
        method = "slide_max"
      )

      if(length(time_range) == 2) {
        graphics::rect(
          xleft = time_range[[1]],
          ybottom = rg[[1]],
          xright = time_range[[2]],
          ytop = rg[[2]],
          border = "#003366",
          col = grDevices::adjustcolor("#99ccff", 0.25)
        )
      }
    })
  )

  ravedash::register_output(
    outputId = "plot_single_overall_pwelch",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")
      brush <- get_brush()

      pwelch_overall <- local_reactives$pwelch_overall
      voltage_sample <- local_reactives$voltage_sample

      freq_range <- brush$freq_range

      pwelch_ranges <- fastplot_pwelch(pwelch_overall, se = 3, adj = 0.95, add_lm = TRUE)

      if(length(freq_range) == 2) {
        freq <- pwelch_overall$freq
        freq_range[[1]] <- max(freq_range[[1]], min(freq))
        freq_range <- log10(freq_range)
        graphics::rect(
          xleft = freq_range[[1]],
          ybottom = pwelch_ranges$ylim[[1]],
          xright = freq_range[[2]],
          ytop = pwelch_ranges$ylim[[2]],
          border = "#003366",
          col = grDevices::adjustcolor("#99ccff", 0.25)
        )
      }

      subset_data <- brush$subset

      if(is.list(subset_data)) {
        pwelch_sub <- subset_data$pwelch
        plot(pwelch_sub, add = TRUE, col = "purple3", lwd = 2)
      }


    })
  )


  ravedash::register_output(
    outputId = "plot_single_sub_power",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")

      brush <- get_brush()
      shiny::validate(
        shiny::need(length(brush) >= 2, "Please choose a time & frequency ranges from the power heatmap")
      )
      freq_range <- brush$freq_range
      time_range <- brush$time_range

      power_sample <- local_reactives$power_sample
      time <- power_sample$origin$dnames$Time
      freq <- power_sample$origin$dnames$Frequency
      data <- t(power_sample$origin$data)

      sd <- stats::sd(data)
      zlim <- quantile(data, c(0.005, 0.995))
      zlim <- c(-1, 1) * max(abs(zlim))
      bias <- log(3 * sd / zlim[2]) / log(0.5)
      pal <- c("#053060", "darkgreen", "#FFFFFF", "orange", "#66001F")
      col <- c(
        rev(colorRampPalette(pal[c(3,2,1)], bias = bias)(100)),
        pal[[3]],
        colorRampPalette(pal[c(3,4,5)], bias = bias)(100)
      )

      # subset
      freq_idx <- freq >= freq_range[1] & freq <= freq_range[2]
      time_idx <- time >= time_range[1] & time <= time_range[2]

      freq <- freq[freq_idx]
      time <- time[time_idx]
      data <- data[time_idx, freq_idx, drop = FALSE]

      fastplot_power_over_freq_time(data, time, freq, col, zlim = zlim)

    })
  )

  ravedash::register_output(
    outputId = "plot_single_sub_voltge",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")
      brush <- get_brush()
      shiny::validate(
        shiny::need(length(brush) >= 3, "")
      )
      freq_range <- brush$freq_range
      time_range <- brush$time_range
      subset_data <- brush$subset

      res <- fastplot_signal_trace(
        data = subset_data$voltage,
        time = subset_data$time,
        method = "decimate",
        sd = stats::sd(subset_data$voltage)
      )
    })
  )

  ravedash::register_output(
    outputId = "plot_single_sub_pwelch",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")
      brush <- get_brush()
      shiny::validate(
        shiny::need(length(brush) >= 3, "")
      )
      pwelch_overall <- local_reactives$pwelch_overall
      freq_range <- brush$freq_range
      subset_data <- brush$subset
      pwelch_sub <- subset_data$pwelch

      pwelch_sub$spec <- pwelch_sub$spec / pwelch_overall$spec[seq_along(pwelch_sub$spec)]
      pwelch_ranges <- fastplot_pwelch(pwelch_sub, xlim = c(0, 400))

      abline(h = 0, col = 'gray60', lty = 2)

      freq <- pwelch_sub$freq
      freq_range[[1]] <- max(freq_range[[1]], min(freq))
      freq_range <- log10(freq_range)
      graphics::rect(
        xleft = freq_range[[1]],
        ybottom = pwelch_ranges$ylim[[1]],
        xright = freq_range[[2]],
        ytop = pwelch_ranges$ylim[[2]],
        border = "#003366",
        col = grDevices::adjustcolor("#99ccff", 0.25)
      )


    })
  )


}
