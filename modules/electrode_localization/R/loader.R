# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(width = 3L),
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
            )
          ),
          ravedash::flex_group_box(
            title = "Locate the CT aligned to T1",

            shiny::conditionalPanel(
              condition = sprintf("input['%s']!==true", ns("loader_ct_skip")),
              class = "padding-5",
              style = "flex:1; ",

              shiny::div(
                class = "float-right",
                shiny::actionLink(ns("loader_ct_refresh"), "Refresh file list")
              ),
              shiny::selectInput(
                inputId = ns("loader_ct_fname"),
                label = "Choose CT",
                choices = character(0L)
              )
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']!==true", ns("loader_ct_skip")),
              class = "flex-break",
              " "
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']==='[Upload]' && input['%s']!==true",
                                  ns("loader_ct_fname"), ns("loader_ct_skip")),
              class = "padding-5",
              style = "flex:1; ",
              shiny::fileInput(
                inputId = ns("loader_ct_upload"),
                label = "Upload CT in Nifti format", multiple = FALSE,
                accept = c(".nii", ".gz")
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::checkboxInput(
                inputId = ns("loader_ct_skip"),
                label = "Do not localize with CT",
                value = FALSE
              )
            )

          ),

          footer = shiny::tagList(
            shiny::actionLink(
              inputId = ns("loader_sync_btn"),
              label = "Sync from [Import LFP] module"
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

  refresh_ct_chocies <- function(value = NULL){
    project_name <- loader_project$get_sub_element_input()
    subject_code <- loader_subject$get_sub_element_input()

    if(!loader_subject$sv$is_valid() || !length(project_name) || !length(subject_code) ||
       is.na(project_name) || is.na(subject_code) || project_name == '' || subject_code == '') {
      shiny::updateSelectInput(
        session = session, inputId = "loader_ct_fname",
        choices = character(0L)
      )
    }

    subject <- raveio::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = FALSE)
    fs_path <- subject$freesurfer_path
    if(length(fs_path) != 1 || is.na(fs_path) || !isTRUE(dir.exists(fs_path))) {
      shiny::updateSelectInput(
        session = session, inputId = "loader_ct_fname",
        choices = character(0L)
      )
    }

    f1 <- list.files(file.path(fs_path, "coregistration"), pattern = "nii(?:\\.gz)?$",
                     recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                     full.names = FALSE, all.files = FALSE)
    f2 <- list.files(file.path(fs_path, "..", "coregistration"), pattern = "nii(?:\\.gz)?$",
                     recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                     full.names = FALSE, all.files = FALSE)
    files <- c(f1, f2)
    files[duplicated(files)] <- sprintf("%s (2)", files[duplicated(files)])

    files <- c(files, "[Upload]")

    selected <- c(value, shiny::isolate(input$loader_ct_fname)) %OF% files
    if(selected == "[Upload]") {
      selected <- files[[1]]
    }

    shiny::updateSelectInput(
      session = session, inputId = "loader_ct_fname", choices = files,
      selected = selected
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
        refresh_ct_chocies(NULL)
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
        refresh_ct_chocies(NULL)
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

      refresh_ct_chocies(fname)
    }),
    input$loader_ct_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      refresh_ct_chocies(NULL)
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

      if(isTRUE(input$loader_ct_skip)) {
        rpath <- NULL
      } else {
        fname <- input$loader_ct_fname
        if(length(fname) != 1 || is.na(fname) || fname == "" || fname == "[Upload]") {
          loading_error("Invalid CT file. Please specify or upload your own.")
          return()
        }
        if(endsWith(fname, " (2))")) {
          fname <- substr(fname, 1L, nchar(fname) - 4)
          fpath <- file.path(fs_path, "..", "coregistration", fname)
          rpath <- file.path("{subject$freesurfer_path}", "..", "coregistration", fname)
        } else {
          fpath <- c(
            file.path(fs_path, "coregistration", fname),
            file.path(fs_path, "..", "coregistration", fname)
          )
          rpath <- c(
            file.path("{subject$freesurfer_path}", "coregistration", fname),
            file.path("{subject$freesurfer_path}", "..", "coregistration", fname)
          )
        }
        sel <- file.exists(fpath)
        fpath <- fpath[sel]
        rpath <- rpath[sel]
        if(!length(fpath)) {
          loading_error(sprintf("Cannot find file [%s]", fname))
          return()
        }
        fpath <- normalizePath(fpath[[1]])
        rpath <- rpath[[1]]
      }


      # Save the variables into pipeline settings file
      pipeline_set(
        path_ct_in_t1 = rpath,
        .list = settings
      )

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = "Loading the viewer and CT file. If this is the first time, it might take a while to generate cache files.",
        icon = "info",
        auto_close = FALSE, buttons = FALSE
      )

      res <- raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = c("brain", "ct_in_t1", "ct_exists"),
        type = "vanilla",
        scheduler = "none",
        check_interval = 1,
        progress_title = "Loading data"
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
