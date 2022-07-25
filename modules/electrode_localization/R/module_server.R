
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  # ravedash::module_server_common(module_id = module_id, check_data_loaded = check_data_loaded, )
  server_tools <- ravedash::get_default_handlers(session = session)


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

  get_preview <- shiny::reactive({
    table <- local_reactives$table_preview
    if(!is.data.frame(table)) { return(NULL) }

    sel <- table$Coord_x != 0 | table$Coord_y != 0 | table$Coord_z != 0
    tkrRAS <- sprintf("%.0f,%.0f,%.0f", table$Coord_x, table$Coord_y, table$Coord_z)
    t1RAS <- sprintf("%.0f,%.0f,%.0f", table$T1R, table$T1A, table$T1S)
    mni305 <- sprintf("%.0f,%.0f,%.0f", table$MNI305_x, table$MNI305_y, table$MNI305_z)
    mni152 <- sprintf("%.0f,%.0f,%.0f", table$MNI152_x, table$MNI152_y, table$MNI152_z)

    return(data.frame(
      row.names = table$Electrode,
      Label = table$Label,
      Dimension = table$Dimension,
      LocationType = table$LocationType,
      FSIndex = table$FSIndex,
      FSLabel = table$FSLabel,
      tkrRAS = tkrRAS,
      T1RAS = t1RAS,
      MNI305 = mni305,
      MNI152 = mni152
    ))

  })

  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger("Check and save electrode table to subject.", level = "trace")

      dipsaus::shiny_alert2(
        title = "Please wait...",
        text = "Finalizing the electrode table, calculating surface mapping (if applicable).",
        icon = "info", auto_close = FALSE, buttons = FALSE
      )

      res <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL,
        async = TRUE,
        names = "localization_result_final"
      )

      res$promise$then(
        onFulfilled = function(...){
          dipsaus::close_alert2()
          table <- pipeline$read('localization_result_final')
          subject <- component_container$data$subject
          raveio::save_meta2(
            data = table,
            meta_type = "electrodes",
            project_name = subject$project_name,
            subject_code = subject$subject_code
          )

          # backup unsaved.csv as it's not useful anymore
          unlink(file.path(subject$meta_path, "electrodes_unsaved.csv"))

          dipsaus::shiny_alert2(
            title = "Success!",
            icon = 'success',
            text = "Electrode table has been exported to subject > rave > meta > electrodes.csv",
            auto_close = TRUE, buttons = list("OK" = TRUE)
          )
        },
        onRejected = function(e){
          dipsaus::close_alert2()
          error_notification(e)
        }
      )


    }),
    input$save_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$electrode_table_preview <- DT::renderDataTable({
    df <- get_preview()
    shiny::validate(
      shiny::need(is.data.frame(df), message = "No preview generated.")
    )
    DT::datatable(
      data = df,
      selection ="none",
      rownames = TRUE,
      class = "compact stripe",
      filter = "none",
      editable = "none",
      options = list(
        ordering = FALSE,
        bFilter = 0,
        paging = FALSE,
        keys = TRUE
      )
    )
  })

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Collect input data
      local_reactives$table_preview <- NULL
      pipeline$set_settings(localization_list = local_data$plan_list)

      results <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL,
        async = FALSE,
        names = "localization_result_initial"
      )

      results$promise$then(
        onFulfilled = function(...){
          ravedash::logger("Fulfilled: ", pipeline$pipeline_name, " - localization_result_initial", level = 'debug')

          table_preview <- pipeline$read("localization_result_initial")
          local_reactives$table_preview <- table_preview

          shidashi::clear_notifications(class = "pipeline-error")
          shiny::showModal(shiny::modalDialog(
            title = "Electrode table",
            easyClose = FALSE,
            size = "xl",
            shiny::div(
              class = "overflow-auto max-height-vh70",
              DT::dataTableOutput(ns("electrode_table_preview"), width = "auto")
            ),
            footer = shiny::tagList(
              shiny::modalButton("Dismiss"),
              dipsaus::actionButtonStyled(ns("save_btn"), "Save to subject")
            )
          ))
        },
        onRejected = function(e, ...){
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
          return(msg)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  reload_plan <- function(){
    subject <- component_container$data$subject
    if(is.null(subject)) {
      local_data$plan_list <- NULL
    }
    plan_file <- file.path(subject$meta_path, "electrodes_unsaved.csv")
    if(!file.exists(plan_file)) {
      local_data$plan_list <- NULL
    }
    table <- raveio::safe_read_csv(plan_file)
    plan_list <- split(table, ~ LabelPrefix + Dimension + LocationType)
    plan_list <- plan_list[vapply(plan_list, function(x){ nrow(x) > 0 }, FALSE)]

    local_data$plan_list <- plan_list[order(vapply(plan_list, function(x){ as.integer(min(x$Electrode)) }, FUN.VALUE = 1L))]
    local_data$plan_list
  }


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      subject <- pipeline$read("subject")
      ct_exists <- pipeline$read('ct_exists')
      brain <- pipeline$read('brain')
      fslut <- pipeline$read('fslut')

      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # Reset preset UI & data
      component_container$reset_data()
      local_data$plan_list <- NULL
      component_container$data$subject <- raveio::as_rave_subject(subject$subject_id, strict = FALSE)
      component_container$data$ct_exists <- ct_exists
      component_container$data$brain <- brain
      component_container$data$fslut <- fslut


      # load plan table
      reload_plan()

      component_container$initialize_with_new_data()
      local_reactives$refresh <- Sys.time()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )





  # Register outputs
  output$label_selectors_wrapper <- shiny::bindEvent(
    shiny::renderUI({

      if(!isTRUE(ravedash::watch_data_loaded())) { return() }

      shiny::validate(shiny::need(length(local_data$plan_list) > 0, message = "Cannot find localization plan list. Please reload this subject."))

      local_reactives$active_plan <- NULL
      nms <- names(local_data$plan_list)

      lapply(seq_along(nms), function(ii) {
        nm <- nms[[ii]]
        dipsaus::actionButtonStyled(
          ns(sprintf("switch_plan_btn_%d", ii)),
          label = sprintf("%s [%s]", nm, dipsaus::deparse_svec(local_data$plan_list[[nm]]$Electrode)),
          type = "default",
          class = "margin-5 btn-xs"
        )
      })
    }),
    local_reactives$refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  lapply(seq_len(100), function(ii){
    shiny::bindEvent(
      ravedash::safe_observe({
        nms <- names(local_data$plan_list)
        if(ii > length(nms)) { return() }

        nm <- nms[[ii]]
        ravedash::logger("Switching to localization plan {ii}: {nm}", use_glue = TRUE, level = "trace")

        old_plan <- shiny::isolate(local_reactives$active_plan)
        local_reactives$active_plan <- ii

        # update button style
        if(length(old_plan)) {
          dipsaus::updateActionButtonStyled(session, inputId = sprintf("switch_plan_btn_%s", old_plan), type = "default")
        }

        dipsaus::updateActionButtonStyled(session, inputId = sprintf("switch_plan_btn_%d", ii), type = "primary")
      }),
      input[[sprintf("switch_plan_btn_%d", ii)]],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  })

  output$localization_viewer <- threeBrain::renderBrain({
    local_reactives$refresh

    subject <- component_container$data$subject
    shiny::validate(
      shiny::need(
        isTRUE(ravedash::watch_data_loaded()) && !is.null(subject),
        message = "Waiting for the data..."
      )
    )

    theme <- shiny::isolate(ravedash::current_shiny_theme())

    brain <- component_container$data$brain
    brain$electrodes$objects <- list()
    # brain <- threeBrain::freesurfer_brain2(
    #   fs_subject_folder = subject$freesurfer_path,
    #   subject_name = subject$subject_code
    # )

    ct_in_t1 <- pipeline$read("ct_in_t1")

    control_presets <- c("localization", "animation", "display_highlights")
    controllers <- list()
    controllers[["Highlight Box"]] <- FALSE
    controllers[["Overlay Coronal"]] <- TRUE
    controllers[["Overlay Axial"]] <- TRUE
    controllers[["Overlay Sagittal"]] <- TRUE
    controllers[["Show Panels"]] <- FALSE
    controllers[["Show Time"]] <- FALSE

    dipsaus::shiny_alert2(
      title = "Finalizing...",
      text = "Generating 3D viewer, rendering voxel data...",
      auto_close = FALSE,
      danger_mode = FALSE,
      icon = "info",
      buttons = FALSE
    )
    on.exit({
      dipsaus::close_alert2()
    }, add = TRUE)

    if(!is.null(ct_in_t1) && is.list(ct_in_t1)) {
      class(ct_in_t1) <- "threeBrain.nii"
      viewer <- brain$localize(
        ct_in_t1, show_modal = FALSE,
        controllers = list(
          `Overlay Coronal` = TRUE,
          `Overlay Axial` = TRUE,
          `Overlay Sagittal` = TRUE,
          `Edit Mode` = "disabled",
          `Show Panels` = FALSE
        ))
    } else {
      viewer <- brain$localize(
        show_modal = FALSE,
        controllers = list(
          `Overlay Coronal` = TRUE,
          `Overlay Axial` = TRUE,
          `Overlay Sagittal` = TRUE,
          `Edit Mode` = "disabled",
          `Show Panels` = FALSE
        ))
    }

    viewer
  })
  brain_proxy <- threeBrain::brain_proxy("localization_viewer")

  ravedash::safe_observe({
    theme <- ravedash::current_shiny_theme()
    brain_proxy$set_background(dipsaus::col2hexStr(theme$background))
  })

  show_group <- function(group_id, reset = FALSE) {
    if(missing(group_id)) {
      group_id <- local_reactives$active_plan
    }
    if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
    if(group_id > length(local_data$plan_list)) { return() }
    group_table <- local_data$plan_list[[group_id]]
    brain_proxy$clear_localization(update_shiny = reset)

    ii <- 1L
    if(!reset) {
      for(ii in seq_len(nrow(group_table))) {
        row <- group_table[ii, ]
        if(all(c(row$Coord_x, row$Coord_y, row$Coord_z) == 0)) {
          break
        }
        if(isTRUE(row$FSIndex > 0)) {
          brain_proxy$add_localization_electrode(row[,c(
            "Coord_x", "Coord_y", "Coord_z", "Label", "FSIndex", "FSLabel"
          )], update_shiny = FALSE)
        } else {
          brain_proxy$add_localization_electrode(row[,c(
            "Coord_x", "Coord_y", "Coord_z", "Label"
          )], update_shiny = FALSE)
        }
      }
      brain_proxy$set_localization_electrode(
        which = -1, update_shiny = TRUE,
        list(Coord_x = 0, Coord_y = 0, Coord_z = 0))
    } else {
      group_table$Coord_x <- 0
      group_table$Coord_y <- 0
      group_table$Coord_z <- 0
      local_data$plan_list[[group_id]] <- group_table
    }

    ct_exists <- isTRUE(component_container$data$ct_exists)
    brain_proxy$set_controllers(list(
      `Edit Mode` = ifelse(ct_exists, "CT/volume", "MRI slice")
    ))

    brain_proxy$set_controllers(list(
      `Interpolate Size` = max(1, tryCatch({
        dim <- as.integer(dipsaus::parse_svec(group_table$Dimension[[1]], sep = "[,x]", unique = FALSE))
        dim <- dim[!is.na(dim)]
        if(length(dim) > 1) {
          dim[[1]] - 2L
        } else {
          nrow(group_table) - 2L
        }
      }, error = function(e){
        nrow(group_table) - 2L
      }))
    ))

    return(list(
      current_id = ii,
      group_table = group_table
    ))
  }

  current_group <- shiny::reactive({
    local_reactives$refresh
    local_reactives$refresh_table
    group_id <- local_reactives$active_plan
    if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
    if(group_id > length(local_data$plan_list)) { return() }
    group_table <- local_data$plan_list[[group_id]]

    batch_size <- tryCatch({
      dim <- as.integer(dipsaus::parse_svec(group_table$Dimension[[1]], sep = "[,x]", unique = FALSE))
      dim <- dim[!is.na(dim)]
      if(length(dim) > 1) {
        dim[[1]]
      } else {
        nrow(group_table)
      }
    }, error = function(e){
      nrow(group_table)
    })

    return(list(
      group_id = group_id,
      group_table = group_table,
      batch_size = batch_size,
      label_prefix = group_table$LabelPrefix[[1]]
    ))
  })


  # render group table here
  format_group_table <- function(df) {
    mni152 <- raveio:::MNI305_to_MNI152 %*% rbind(df$MNI305_x, df$MNI305_y, df$MNI305_z, 1)
    sel <- df$Coord_x == 0 & df$Coord_y == 0 & df$Coord_z == 0
    mni152[, sel] <- 0
    mni152_text <- sprintf("%.0f,%.0f,%.0f", mni152[1,], mni152[2,], mni152[3,])
    mni152_link <- sprintf("https://neurosynth.org/locations/?x=%.0f&y=%.0f&z=%.0f", mni152[1,], mni152[2,], mni152[3,])
    mni152_col <- sprintf('<a href="%s" target="_blank">%s</a>', mni152_link, mni152_text)
    mni152_col[sel] <- ""
    table_output <- data.frame(
      row.names = df$Electrode,
      Label = df$Label,
      tkrRAS = sprintf("%.0f,%.0f,%.0f", df$Coord_x, df$Coord_y, df$Coord_z),
      MNI152 = mni152_col,
      FSLabel = df$FSLabel
    )
    table_output
  }
  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()
      if(!(is.list(ginfo) && is.data.frame(ginfo$group_table))) {
        local_reactives$table_output <- NULL
      }
      df <- ginfo$group_table
      group_id <- ginfo$group_id

      table_output <- format_group_table(df)

      if(identical(local_data$last_group_id, group_id)) {
        DT::replaceData(proxy_table, data = table_output)
      } else {
        local_reactives$table_output <- table_output
        local_data$last_group_id <- group_id
      }
    }),
    current_group()
  )

  proxy_table <- DT::dataTableProxy(outputId = "group_table")

  group_table_selected <- shiny::reactive({
    ridx <- input$group_table_rows_selected
    if(length(ridx) != 1 || ridx < 1) { return(FALSE) }
    group_id <- local_reactives$active_plan
    if(length(group_id) != 1 || !is.numeric(group_id)) { return(FALSE) }
    if(group_id > length(local_data$plan_list)) { return(FALSE) }
    group_table <- local_data$plan_list[[group_id]]
    if(!is.data.frame(group_table)) { return(FALSE) }
    if(nrow(group_table) < ridx) { return(FALSE) }
    return(TRUE)
  })

  output$fsindex_selector <- shiny::renderUI({
    if(!isTRUE(group_table_selected())) { return() }
    ridx <- shiny::isolate(input$group_table_rows_selected)

    labels <- c("[unchanged]", unname(component_container$data$fslut$labels))
    selected <- shiny::isolate(input$fsindex_label) %OF% labels
    shiny::tagList(
      shiny::div(
        class = "margin-top-5",
        shiny::p("If you want to change the FreeSurfer label, please select one from below:")
      ),
      shidashi::flex_container(
        style = "align-items: end;",
        shidashi::flex_item(
          size = 2,
          shinyWidgets::pickerInput(
            inputId = ns("fsindex_label"), label = "FreeSurfer Label",
            choices = labels, selected = selected,
            multiple = FALSE, width = "100%",
            options = list(
              `live-search` = TRUE
            )
          )
        ),
        shidashi::flex_item(
          shiny::div(
            class = "form-group",
            dipsaus::actionButtonStyled(ns("fsindex_label_next"), "Save & Next", width = "100%")
          )
        )
      )
    )
  })


  shiny::bindEvent(
    ravedash::safe_observe({
      if(!isTRUE(group_table_selected())) { return() }

      ridx <- input$group_table_rows_selected
      # if(length(ridx) != 1 || ridx < 1) { return() }

      group_id <- local_reactives$active_plan
      # if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
      # if(group_id > length(local_data$plan_list)) { return() }

      label <- input$fsindex_label
      group_table <- local_data$plan_list[[group_id]]
      # if(!is.data.frame(group_table)) { return() }
      print(label)
      if(isTRUE(label %in% component_container$data$fslut$labels)) {
        # change label
        # if(nrow(group_table) < ridx) { return() }
        group_table$FSIndex[[ridx]] <- component_container$data$fslut$cmap$get_key(label)
        group_table$FSLabel[[ridx]] <- label
        local_data$plan_list[[group_id]] <- group_table
      }

      next_ridx <- ridx + 1L
      if(next_ridx > nrow(group_table)) {
        next_ridx <- 1L
      }
      DT::replaceData(proxy_table, data = format_group_table(group_table))

      DT::selectRows(proxy = proxy_table, selected = next_ridx)

    }),
    input$fsindex_label_next,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$group_table <- DT::renderDataTable({
    table_output <- local_reactives$table_output
    shiny::validate(
      shiny::need(is.data.frame(table_output), message = "")
    )

    DT::datatable(
      table_output,
      selection = list(mode = "single", target = "row"),
      rownames = TRUE,
      class = "compact cell-border stripe",
      filter = "none",
      editable = FALSE, escape = -4,
      options = list(ordering = FALSE, bFilter = 0, paging = FALSE)
    )
  })

  output$instruction_text <- shiny::renderUI({
    ginfo <- current_group()
    if(!is.list(ginfo)) { return() }

    dim <- ginfo$group_table$Dimension[[1]]
    dim <- as.integer(dipsaus::parse_svec(dim, sep = "[,x]"))
    dim <- dim[!is.na(dim)]

    label_prefix <- ginfo$label_prefix
    batch_size <- ginfo$batch_size

    shiny::div(
      shiny::p("Example instruction to localize electrode ", dipsaus::deparse_svec(ginfo$group_table$Electrode), ":"),
      shiny::tags$ul(
        shiny::tags$li(
          "Make sure the ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Edit Mode"),
          " is ",
          shiny::pre(class="pre-compact no-padding display-inline", "CT/volume"), " (with CT) or ",
          shiny::pre(class="pre-compact no-padding display-inline", "MRI slice"), " (w/o CT)"
        ),
        shiny::tags$li(
          sprintf("Double-click on the viewer to mark electrode %s1", label_prefix)
        ),
        shiny::tags$li(
          sprintf("Double-click on the viewer to mark electrode %s%d", label_prefix, batch_size)
        ),
        shiny::tags$li(
          "Set ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Interpolate Size"),
          " to ", batch_size - 2L
        ),
        shiny::tags$li(
          "Click on ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Interpolate from Recently Added"),
        )
      ),
      shiny::hr(),
      shiny::p("Example instruction to refine electrode locations:"),
      shiny::tags$ul(
        shiny::tags$li(
          "Make sure the ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Edit Mode"),
          " is set to ",
          shiny::pre(class="pre-compact no-padding display-inline", "refine")
        ),
        shiny::tags$li(
          "Click ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Auto-Adjust All")
        ),
        shiny::tags$li(
          "Alternatively, if you want to adjust single electrode, ",
          shiny::tags$ul(
            shiny::tags$li("Double-click an electrode, the highlighted electrode will turn red"),
            shiny::tags$li(
              "Use keyboard ",
              shiny::pre(class="pre-compact no-padding display-inline", "1"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "2"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "3"),
              " or ",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+1"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+2"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+3"),
              " to move the highlighted electrode around"
            )
          )
        )
      )
    )
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      show_group(reset = TRUE)
      local_reactives$refresh_table <- Sys.time()
    }),
    input$action_reset_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      show_group()
    }),
    local_reactives$active_plan,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      table <- brain_proxy$localization_table
      ginfo <- current_group()
      if(!is.data.frame(table)) { return() }
      if(!is.list(ginfo)) { return() }

      # return(list(
      #   group_id = group_id,
      #   group_table = group_table,
      #   batch_size = batch_size,
      #   label_prefix = group_table$LabelPrefix[[1]]
      # ))
      group_id <- ginfo$group_id
      group_table <- ginfo$group_table
      # local_data$plan_list[[group_id]]
      n <- min(nrow(table), nrow(group_table))
      if(n <= 0) { return() }
      idx <- seq_len(n)

      group_table$Coord_x[idx] <- table$Coord_x[idx]
      group_table$Coord_y[idx] <- table$Coord_y[idx]
      group_table$Coord_z[idx] <- table$Coord_z[idx]
      group_table$MNI305_x[idx] <- table$MNI305_x[idx]
      group_table$MNI305_y[idx] <- table$MNI305_y[idx]
      group_table$MNI305_z[idx] <- table$MNI305_z[idx]
      group_table$FSIndex[idx] <- table$FSIndex[idx]
      group_table$FSLabel[idx] <- table$FSLabel[idx]

      local_data$plan_list[[group_id]] <- group_table
      local_reactives$refresh_table <- Sys.time()
    }),
    brain_proxy$localization_table,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
