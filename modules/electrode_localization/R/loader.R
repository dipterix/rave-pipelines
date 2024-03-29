# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              loader_sync1$ui_func(),
              shiny::br(),
              loader_sync2$ui_func(),
              shiny::br(),
              loader_sync3$ui_func()
            )
          ),
          ravedash::flex_group_box(
            title = "Data to load",

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_method"),
                label = "Localization method",
                choices = c(
                  "CT (IJK) to MR (RAS) transform + Raw CT",
                  "FSL transform + Raw CT + MRI",
                  "Re-sampled CT",
                  "Localize without CT"
                )
              )
            ),

            shidashi::flex_break(),

            shiny::conditionalPanel(
              condition = sprintf("input['%s']!=='Localize without CT'", ns("loader_method")),
              class = "padding-5",
              style = "flex:1; ",

              shidashi::flex_container(
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("loader_ct_fname"),
                    label = "Choose CT",
                    choices = character(0L)
                  )
                ),
                shidashi::flex_item(
                  shiny::fileInput(
                    inputId = ns("loader_ct_upload"),
                    label = "Upload .nii file", multiple = FALSE,
                    accept = c(".nii", ".gz")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']!=='Re-sampled CT'", ns("loader_method")),

                shidashi::flex_container(
                  shidashi::flex_item(
                    shiny::selectInput(
                      inputId = ns("loader_mri_fname"),
                      label = "Choose raw MRI",
                      choices = character(0L)
                    )
                  ),
                  shidashi::flex_item(
                    shiny::selectInput(
                      inputId = ns("loader_transform_fname"),
                      label = "Transform matrix",
                      choices = character(0L)
                    )
                  )
                )

              ),

              shiny::div(
                class = "float-right",
                shiny::actionLink(ns("loader_ct_refresh"), "Refresh file list")
              )

            )
          )



        )
      ),
      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Electrode Plan",
          class_header = "",
          dipsaus::compoundInput2(
            max_height = "80vh",
            inputId = ns("loader_plan"),
            label = "Electrode group",
            initial_ncomp = 1L,
            min_ncomp = 1L,
            max_ncomp = 100L,
            label_color = "#c8c9ca",
            components = shidashi::flex_container(
              class = "margin-m10",
              shidashi::flex_item(shiny::textInput("label", "Group label")),
              shidashi::flex_item(shiny::textInput("dimension", "Dimension")),
              shidashi::flex_item(shiny::selectInput("type", "Type", choices = raveio::LOCATION_TYPES)),
              shidashi::flex_item(shiny::selectInput("hemisphere", "Hemisphere",
                                                     choices = c("auto", "left", "right"))),
              shidashi::flex_break(),
              shidashi::flex_item(shiny::tags$small(
                shiny::textOutput("info", inline = TRUE)
              ))
            )
          ),
          footer = shiny::tagList(
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

  get_plan <- shiny::debounce(shiny::reactive({
    input$loader_plan
  }), millis = 300)

  shiny::bindEvent(
    ravedash::safe_observe({
      plan <- get_plan()
      n <- 0
      labels <- list()
      for(ii in seq_along(plan)) {
        item <- plan[[ii]]
        dimension <- dipsaus::parse_svec(item$dimension, unique = FALSE, sep = "[,x]")
        dimension <- dimension[!is.na(dimension)]
        if(!length(dimension) || any(dimension <= 0)) {
          ne <- 0
        } else {
          ne <- prod(dimension)
        }

        label <- trimws(item$label)
        if(!nchar(label)) {
          label <- "NoLabel"
        }
        if(!label %in% names(labels)) {
          labels[[label]] <- 0
        }
        type <- item$type

        if(ne == 0) {
          msg <- "No electrode in this group; please enter a valid electrode dimension."
        } else if(ne == 1){
          msg <- sprintf("Electrode %d (%s%.0f): total 1 %s electrode",
                         n + 1, label, labels[[label]] + 1, type)
        } else {
          msg <- sprintf("Electrode %d (%s%.0f) - %d (%s%.0f): total %.0f %s electrodes",
                         n + 1, label, labels[[label]] + 1, n + ne, label, labels[[label]] + ne, ne, type)
        }
        n <- n + ne
        labels[[label]] <- labels[[label]] + ne

        session$sendCustomMessage(
          "shidashi.set_html",
          list(
            selector = sprintf("#%s", ns(sprintf("loader_plan_info_%s", ii))),
            content = msg
          )
        )
      }
    }),
    get_plan(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  load_coreg_params <- function() {
    project_name <- loader_project$get_sub_element_input()
    subject_code <- loader_subject$get_sub_element_input()
    if(!loader_subject$sv$is_valid() || !length(project_name) || !length(subject_code) ||
       is.na(project_name) || is.na(subject_code) || project_name == '' || subject_code == '') {
      return()
    }
    subject <- raveio::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = FALSE)
    fs_path <- subject$freesurfer_path
    if( !file.exists(fs_path) ) { return() }

    path_coreg_conf <- file.path(subject$preprocess_settings$raw_path, "rave-imaging",
                                 "derivative", "conf-coregistration.yaml")
    coreg_files <- list.files(
      file.path(subject$preprocess_settings$raw_path,
                "rave-imaging",
                "coregistration")
    )

    localization_method <- "Re-sampled CT"

    if( file.exists(path_coreg_conf) ) {
      coreg_conf <- raveio::load_yaml(path_coreg_conf)

      # check outputs
      if("CT_IJK_to_MR_RAS" %in% names(coreg_conf$outputs)) {
        localization_method <- "CT (IJK) to MR (RAS) transform + Raw CT"
        ct_name <- unique(basename(coreg_conf$input_image$backup))
        ct_name <- ct_name[ct_name %in% coreg_files]
        mri_name <- unique(basename(coreg_conf$reference_image$backup))
        mri_name <- mri_name[mri_name %in% coreg_files]
        trans_name <- basename(coreg_conf$outputs$CT_IJK_to_MR_RAS$path)
      } else if ("ct2t1" %in% names(coreg_conf$outputs)) {
        localization_method <- "FSL transform + Raw CT + MRI"
        ct_name <- unique(basename(coreg_conf$input_image$backup))
        ct_name <- ct_name[ct_name %in% coreg_files]
        mri_name <- unique(basename(coreg_conf$reference_image$backup))
        mri_name <- mri_name[mri_name %in% coreg_files]
        trans_name <- unique(basename(coreg_conf$outputs$ct2t1$path))
      } else if ("ct_in_t1" %in% names(coreg_conf$outputs)) {
        localization_method <- "Re-sampled CT"
        ct_name <- basename(coreg_conf$outputs$ct_in_t1$path)
        mri_name <- NULL
        trans_name <- NULL
      }
    } else {
      selected_method <- subject$get_default(
        "transform_space", default_if_missing = NULL,
        namespace = "electrode_localization")
      if( length(selected_method) == 1 ) {
        localization_method <- names(LOCALIZATION_METHODS)[
          unlist(LOCALIZATION_METHODS) == selected_method]
      }
      ct_name <- subject$get_default(
        "path_ct", default_if_missing = NULL,
        namespace = "electrode_localization")
      if(length(ct_name) == 1) {
        ct_name <- basename(ct_name)
        ct_name <- ct_name[ct_name %in% coreg_files]
      }
      if(!length(ct_name)) {
        ct_name <- coreg_files[grepl("^CT.*\\.nii(\\.gz|)$", coreg_files)]
        if(length(ct_name)) { ct_name <- ct_name[[1]] }
      }

      mri_name <- subject$get_default(
        "path_mri", default_if_missing = shiny::isolate(input$loader_mri_fname),
        namespace = "electrode_localization")
      if(length(mri_name) == 1) {
        mri_name <- basename(mri_name)
        mri_name <- mri_name[mri_name %in% coreg_files]
      }
      if(!length(mri_name)) {
        mri_name <- coreg_files[grepl("^MR.*\\.nii(\\.gz|)$", coreg_files)]
        if(length(mri_name)) { mri_name <- mri_name[[1]] }
      }

      trans_name <- subject$get_default(
        "path_transform", default_if_missing = shiny::isolate(input$loader_transform_fname),
        namespace = "electrode_localization")
      if(length(trans_name) == 1) {
        trans_name <- basename(trans_name)
        trans_name <- trans_name[trans_name %in% coreg_files]
      }
      if(!length(trans_name)) {
        trans_name <- coreg_files[grepl("\\.(mat|txt)$", coreg_files)]
        if(length(trans_name)) { trans_name <- trans_name[[1]] }
      }

      if( length(trans_name) == 1 &&
          (
            identical(localization_method, "Re-sampled CT") ||
            !isTRUE(localization_method %in% names(LOCALIZATION_METHODS))
          ) ) {
        if( startsWith(trans_name, "CT_IJK") ) {
          localization_method <- "CT (IJK) to MR (RAS) transform + Raw CT"
        } else if ( startsWith(trans_name, "ct2t1") ) {
          localization_method <- "FSL transform + Raw CT + MRI"
        }
      }
    }

    return(list(
      method = localization_method,
      ct_filename = ct_name,
      mr_filename = mri_name,
      transform_filename = trans_name,
      project_name = project_name,
      subject_code = subject_code,
      subject = subject
    ))

  }

  refresh_ct_chocies <- function(value_ct = NULL, value_mri = NULL, value_transform = NULL, reset_method = FALSE){
    coreg_params <- load_coreg_params()

    # coreg_params <- list(
    #   method = '',
    #   ct_filename = ct_name,
    #   mr_filename = mri_name,
    #   transform_filename = trans_name,
    #   project_name = project_name,
    #   subject_code = subject_code,
    #   subject = subject
    # )

    if( !length(coreg_params) ) {
      shiny::updateSelectInput(
        session = session, inputId = "loader_ct_fname",
        choices = character(0L)
      )
      shiny::updateSelectInput(
        session = session, inputId = "loader_mri_fname",
        choices = character(0L)
      )
      shiny::updateSelectInput(
        session = session, inputId = "loader_transform_fname",
        choices = character(0L)
      )
      return()
    }

    subject <- coreg_params$subject

    coreg_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging", "coregistration")

    nifti_files <- list.files(coreg_path, pattern = "nii(?:\\.gz)?$",
                              recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                              full.names = FALSE, all.files = FALSE)
    transform_files <- list.files(coreg_path, pattern = "(mat|txt)$",
                                  recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                                  full.names = FALSE, all.files = FALSE)
    transform_files <- unique(c(transform_files[transform_files %in% c("CT_IJK_to_MR_RAS.txt", "ct2t1.mat")], transform_files))


    shiny::updateSelectInput(
      session = session, inputId = "loader_method", selected = coreg_params$method)
    shiny::updateSelectInput(
      session = session, inputId = "loader_ct_fname", choices = nifti_files,
      selected = coreg_params$ct_filename
    )
    shiny::updateSelectInput(
      session = session, inputId = "loader_mri_fname", choices = nifti_files,
      selected = coreg_params$mr_filename
    )
    shiny::updateSelectInput(
      session = session, inputId = "loader_transform_fname", choices = transform_files,
      selected = coreg_params$transform_filename
    )

    electrode_file <- file.path(subject$meta_path, c("electrodes_unsaved.csv", "electrodes.csv"))
    electrode_file <- electrode_file[file.exists(electrode_file)]

    plan <- list()
    if(length(electrode_file)) {
      electrode_file <- electrode_file[[1]]
      table <- raveio::safe_read_csv(electrode_file)
      if(all(c('Electrode', "Label") %in% names(table))) {
        if(!"LocationType" %in% names(table)) {
          table$LocationType <- raveio::LOCATION_TYPES[[1]]
        } else {
          table$LocationType[!table$LocationType %in% raveio::LOCATION_TYPES] <- raveio::LOCATION_TYPES[[1]]
        }
        if(!"Dimension" %in% names(table)) {
          table$Dimension <- ""
        }
        if(!"Hemisphere" %in% names(table)) {
          table$Hemisphere <- "auto"
        }

        table$LabelPrefix <- gsub("[0-9]+$", "", table$Label)
        table <- table[order(table$Electrode),]

        current_dim <- ""
        current_prefix <- ""
        current_type <- "iEEG"
        current_hemisphere <- "auto"
        current_e <- NULL

        for(ii in seq_len(nrow(table))) {
          sub <- table[ii,]
          if(!length(current_e)) {
            current_e <- c(current_e, sub$Electrode)
            current_dim <- sub$Dimension
            current_prefix <- sub$LabelPrefix
            current_type <- sub$LocationType
            current_hemisphere <- sub$Hemisphere
          } else {
            if(!identical(current_dim, sub$Dimension) ||
               !identical(current_prefix, sub$LabelPrefix) ||
               !identical(current_type, sub$LocationType) ||
               !identical(current_hemisphere, sub$Hemisphere) ||
               sub$Electrode != current_e[[length(current_e)]] + 1) {

              # check dimension
              dimension <- length(current_e)
              if(current_dim != "") {
                dm <- dipsaus::parse_svec(current_dim, unique = TRUE, sep = "[,x]")
                dm <- dm[!is.na(dm)]
                if(length(dm) && prod(dm) == dimension) {
                  dimension <- current_dim
                }
              }
              plan[[length(plan) + 1]] <- list(
                label = current_prefix,
                dimension = as.character(dimension),
                type = current_type,
                hemisphere = current_hemisphere
              )
              current_e <- sub$Electrode
              current_dim <- sub$Dimension
              current_prefix <- sub$LabelPrefix
              current_type <- sub$LocationType
              current_hemisphere <- sub$Hemisphere

            } else {
              current_e <- c(current_e, sub$Electrode)
            }
          }
        }

        if(length(current_e)) {
          # check dimension
          dimension <- length(current_e)
          if(current_dim != "") {
            dm <- dipsaus::parse_svec(current_dim, unique = TRUE, sep = "[,x]")
            dm <- dm[!is.na(dm)]
            if(length(dm) && prod(dm) == dimension) {
              dimension <- current_dim
            }
          }
          plan[[length(plan) + 1]] <- list(
            label = current_prefix,
            dimension = as.character(dimension),
            type = current_type,
            hemisphere = current_hemisphere
          )
        }

      }
    }

    if(!length(plan)) {
      plan <- list(list(
        label = "NoLabel",
        dimension = as.character(length(subject$preprocess_settings$electrodes)),
        type = "iEEG",
        hemisphere = "auto"
      ))
    }
    dipsaus::updateCompoundInput2(
      session = session,
      inputId = "loader_plan",
      value = plan,
      ncomp = length(plan)
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      refresh_ct_chocies()
    }),
    input$loader_ct_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      if(!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        loading_error("Invalid project/subject. Please specify a valid subject first before uploading CT.")
        refresh_ct_chocies()
        return()
      }
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      subject <- raveio::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      fs_path <- subject$freesurfer_path
      if(length(fs_path) == 0 || is.na(fs_path) || !dir.exists(fs_path)) {
        loading_error("Cannot find surface/volume reconstruction directory. Please at least run FreeSurfer autorecon1 (only ~10 min)")
        refresh_ct_chocies()
        return()
      }

      finfo <- input$loader_ct_upload
      fname <- NULL
      if(nrow(finfo) == 1) {
        fname <- finfo$name
        if(!grepl("nii(?:\\.gz)?$", x = fname, ignore.case = TRUE)) {
          shidashi::show_notification("Please make sure the file ends with [nii] or [nii.gz]. Do not try to decompress or to remove the nii postfix in the file.", title = "Invalid extension", type = "danger", autohide = TRUE, class = "error_notif")
        }
        fname <- sprintf(
          "upload-%s-%s",
          strftime(Sys.time(), "%y%m%d-%H%M%S"),
          fname
        )
        pdir <- raveio::dir_create2(file.path(fs_path, "coregistration"))
        file.copy(finfo$datapath, file.path(pdir, fname), overwrite = TRUE, recursive = FALSE)
      }

      refresh_ct_chocies()
    }),
    input$loader_ct_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      refresh_ct_chocies(reset_method = TRUE)
    }),
    loader_project$get_sub_element_input(),
    loader_subject$get_sub_element_input(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


  loading_error <- function(message) {
    dipsaus::shiny_alert2(
      title = "Errors",
      text = paste(
        "Found an error while loading the data:\n\n",
        message
      ),
      icon = "error",
      danger_mode = TRUE,
      auto_close = FALSE
    )
  }


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

      if(!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        loading_error("Invalid project/subject.")
        return()
      }

      # add your own input values to the settings file
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      subject <- raveio::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      fs_path <- subject$freesurfer_path
      if(length(fs_path) == 0 || is.na(fs_path) || !dir.exists(fs_path)) {
        loading_error("Cannot find surface/volume reconstruction directory. Please at least run FreeSurfer autorecon1 (only ~10 min)")
        return()
      }

      check_path <- function(fname, type) {
        if(length(fname) != 1 || is.na(fname) || fname == "") {
          loading_error(sprintf("Invalid %s file. Please specify or upload your own.", type))
          return(NULL)
        }
        fpath <- file.path(fs_path, "..", "coregistration", fname)
        if(!file.exists(fpath)) {
          loading_error(sprintf("Invalid %s path. Please check or upload your own.", type))
          return(NULL)
        }
        file.path("{subject$freesurfer_path}", "..", "coregistration", fname)
      }

      path_ct <- NULL
      path_mri <- NULL
      path_transform <- NULL
      transform_space <- "resampled"
      switch(
        paste(input$loader_method),
        "Re-sampled CT" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
        },
        "FSL transform + Raw CT + MRI" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          if(is.null(path_mri)) { return() }
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
          if(is.null(path_transform)) { return() }
          transform_space <- "fsl"
        },
        "CT (IJK) to MR (RAS) transform + Raw CT" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          if(is.null(path_mri)) { return() }
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
          if(is.null(path_transform)) { return() }
          transform_space <- "ijk2ras"
        }
      )

      # Save the variables into pipeline settings file
      pipeline$set_settings(
        path_ct = path_ct,
        path_mri = path_mri,
        path_transform = path_transform,
        transform_space = transform_space,
        localization_plan = input$loader_plan,
        .list = settings
      )

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = "Loading the viewer and CT file. It will take a while if this is the first time.",
        icon = "info",
        auto_close = FALSE, buttons = FALSE
      )

      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
      }, after = TRUE, add = TRUE)
      res <- pipeline$run(
        as_promise = FALSE,
        async = FALSE,
        names = c("plan_list", "pial_envelope", "brain", "localize_data",
                  "ct_exists", "fslut"),
        type = "vanilla",
        scheduler = "none"
      )

      dipsaus::close_alert2()

      # Let the module know the data has been changed
      ravedash::fire_rave_event('data_changed', Sys.time())
      ravedash::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = project_name,
        subject_code = subject_code
      )

    }, error_wrapper = "alert"),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )


}
