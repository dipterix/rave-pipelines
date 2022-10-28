library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R",
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path",
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings",
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")),
    input_surface_types = targets::tar_target_raw("surface_types",
        quote({
            settings[["surface_types"]]
        }), deps = "settings"), input_template_ok = targets::tar_target_raw("template_ok",
        quote({
            settings[["template_ok"]]
        }), deps = "settings"), input_data_source_project = targets::tar_target_raw("data_source_project",
        quote({
            settings[["data_source_project"]]
        }), deps = "settings"), input_controllers = targets::tar_target_raw("controllers",
        quote({
            settings[["controllers"]]
        }), deps = "settings"), input_data_source = targets::tar_target_raw("data_source",
        quote({
            settings[["data_source"]]
        }), deps = "settings"), input_data_source_pipeline_target = targets::tar_target_raw("data_source_pipeline_target",
        quote({
            settings[["data_source_pipeline_target"]]
        }), deps = "settings"), input_data_source_pipeline = targets::tar_target_raw("data_source_pipeline",
        quote({
            settings[["data_source_pipeline"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_shiny_outputId = targets::tar_target_raw("shiny_outputId",
        quote({
            settings[["shiny_outputId"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_main_camera = targets::tar_target_raw("main_camera",
        quote({
            settings[["main_camera"]]
        }), deps = "settings"), input_uploaded_source = targets::tar_target_raw("uploaded_source",
        quote({
            settings[["uploaded_source"]]
        }), deps = "settings"), get_valid_project_name = targets::tar_target_raw(name = "loaded_brain",
        command = quote({
            {
                rave_path <- raveio::raveio_getopt("data_dir")
                raw_path <- raveio::raveio_getopt("raw_data_dir")
                brain <- NULL
                electrode_table <- NULL
                if (!project_name %in% c("[Auto]", "[Upload]",
                  "[None]")) {
                  if (isTRUE(has_fsdir(project_name = project_name,
                    subject_code = subject_code, rave_path = rave_path,
                    raw_path = raw_path))) {
                    subject <- raveio::RAVESubject$new(project_name = project_name,
                      subject_code = subject_code)
                    brain <- raveio::rave_brain(subject, surfaces = c("pial",
                      surface_types))
                    brain$electrodes$objects <- list()
                  }
                  electrode_table <- raveio::load_meta2(meta_type = "electrodes",
                    project_name = project_name, subject_code = subject_code)
                }
                if (!is.data.frame(electrode_table) && !project_name %in%
                  c("[None]")) {
                  electrode_table <- raveio::pipeline_load_extdata("suggested_electrode_table")
                }
                if (is.null(brain)) {
                  projects <- get_projects_with_scode(subject_code = subject_code,
                    refresh = TRUE, rave_path = rave_path)
                  for (project in projects) {
                    if (has_fsdir(project_name = project, subject_code = subject_code,
                      rave_path = rave_path, raw_path = raw_path)) {
                      subject <- raveio::RAVESubject$new(project_name = project,
                        subject_code = subject_code)
                      brain <- raveio::rave_brain(subject, surfaces = c("pial",
                        surface_types))
                      brain$electrodes$objects <- list()
                      project_name <- project
                      break
                    }
                  }
                }
                if (is.null(brain)) {
                  if (template_ok) {
                    brain <- threeBrain::merge_brain(template_surface_types = c("pial",
                      surface_types))
                    brain <- brain$template_object
                  } else {
                    stop("Cannot find a valid 3D model for this subject. Please construct the 3D models, or use a MNI template brain.")
                  }
                }
                if (!is.null(brain) && is.data.frame(electrode_table)) {
                  electrode_table$Subject <- brain$subject_code
                  brain$set_electrodes(electrode_table)
                }
                loaded_brain <- list(subject_code = subject_code,
                  project_name = project_name, brain = brain,
                  electrode_table = electrode_table, surface_types = brain$surface_types)
            }
            return(loaded_brain)
        }), deps = c("project_name", "subject_code", "surface_types",
        "template_ok"), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), render_initial_viewer = targets::tar_target_raw(name = "initial_brain_widget",
        command = quote({
            {
                library(dipsaus)
                force(shiny_outputId)
                controllers <- as.list(controllers)
                main_camera <- as.list(main_camera)
                background <- controllers[["Background Color"]]
                if (length(background) != 1) {
                  background <- "#FFFFFF"
                }
                zoom_level <- main_camera$zoom
                if (length(zoom_level) != 1 || zoom_level <=
                  0) {
                  zoom_level <- 1
                }
                position <- as.numeric(unname(unlist(main_camera$position)))
                up <- as.numeric(unname(unlist(main_camera$up)))
                if (length(position) != 3 || length(up) != 3 ||
                  all(position == 0) || all(up == 0) || any(is.na(position)) ||
                  any(is.na(up))) {
                  position <- c(0, 0, 500)
                  up <- c(0, 1, 0)
                } else {
                  position <- position/sqrt(sum(position^2)) *
                    500
                  up <- up/sqrt(sum(up^2))
                }
                if (!isTRUE(controllers[["Show Panels"]])) {
                  controllers[["Show Panels"]] <- FALSE
                }
                initial_brain_widget <- loaded_brain$brain$plot(show_modal = FALSE,
                  background = background, controllers = controllers,
                  start_zoom = zoom_level, custom_javascript = raveio::glue("\n    // Remove the focus box\n    if( canvas.focus_box ) {\n      canvas.focus_box.visible = false;\n    }\n    \n    // set camera\n    canvas.main_camera.position.set(\n      {{ position[[1]] }} , \n      {{ position[[2]] }} , \n      {{ position[[3]] }}\n    );\n    canvas.main_camera.up.set(\n      {{ up[[1]] }} , \n      {{ up[[2]] }} , \n      {{ up[[3]] }}\n    )\n    canvas.main_camera.updateProjectionMatrix();\n\n    // Let shiny know the viewer is ready\n    if( canvas.shiny_mode ) {\n      Shiny.setInputValue(\"{{ shiny_outputId }}\", \"{{Sys.time()}}\");\n    }\n\n    // Force render one frame (update the canvas)\n    canvas.start_animation(0);\n    ",
                    .open = "{{", .close = "}}"))
            }
            return(initial_brain_widget)
        }), deps = c("shiny_outputId", "controllers", "main_camera",
        "loaded_brain"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), find_data_path = targets::tar_target_raw(name = "path_datatable",
        command = quote({
            {
                if (!length(data_source)) {
                  data_source <- "None"
                }
                path_datatable <- switch(data_source, Uploads = {
                  get_subject_imaging_datapath(uploaded_source,
                    subject_code = loaded_brain$subject_code,
                    type = "uploads")
                }, `Saved pipelines/modules` = {
                  project_name <- data_source_project
                  saved_pipeline <- data_source_pipeline
                  saved_target <- data_source_pipeline_target
                  if (!length(project_name)) {
                    stop("Trying to get saved pipeline, but no project name has been given. Please assign a valid [data_source_project] variable. If you are running in RAVE's web interface, make sure the project name is set correctly")
                  }
                  if (!length(saved_pipeline) || !length(saved_target)) {
                    stop("Trying to get saved pipeline, but no pipeline name nor target has been given. Please assign a valid [data_source_pipeline] & [data_source_pipeline_target] variable. If you are running in RAVE's web interface, make sure the pipeline is set with no errors.")
                  }
                  pipepath <- get_subject_imaging_datapath(saved_pipeline,
                    subject_code = loaded_brain$subject_code,
                    project_name = project_name, type = "pipeline")
                  if (!length(pipepath) || is.na(pipepath) ||
                    !dir.exists(pipepath)) {
                    stop("Cannot find saved pipeline under the subject [",
                      project_name, "/", loaded_brain$subject_code,
                      "]: ", saved_pipeline)
                  }
                  structure(pipepath, target = saved_target)
                }, {
                  NULL
                })
            }
            return(path_datatable)
        }), deps = c("data_source", "uploaded_source", "loaded_brain",
        "data_source_project", "data_source_pipeline", "data_source_pipeline_target"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), load_data_table = targets::tar_target_raw(name = "brain_with_data",
        command = quote({
            {
                loaded_datatable <- NULL
                if (length(path_datatable) == 1) {
                  if (!length(data_source)) {
                    data_source <- "None"
                  }
                  switch(data_source, Uploads = {
                    if (grepl("\\.fst$", path_datatable, ignore.case = TRUE)) {
                      loaded_datatable <- raveio::load_fst(path_datatable,
                        as.data.table = TRUE)
                    }
                  }, `Saved pipelines/modules` = {
                    var <- raveio::pipeline_read(var_names = attr(path_datatable,
                      "target"), pipe_dir = path_datatable, ifnotfound = NULL)
                    if (length(var)) {
                      try({
                        if ("Electrode" %in% names(var)) {
                          if (!is.data.frame(var)) {
                            var <- as.data.frame(var)
                          }
                          loaded_datatable <- var
                        }
                      })
                    }
                  })
                }
                nms <- names(loaded_datatable)
                if (!"Electrode" %in% nms && length(path_datatable)) {
                  if (isTRUE(data_source %in% c("Uploads", "Saved pipelines/modules"))) {
                    stop("Cannot set electrode values. Please make sure the data is a named table, and one of the table names must be [Electrode] (case-sensitive).")
                  }
                }
                if (is.data.frame(loaded_datatable) && "Electrode" %in%
                  nms) {
                  if (!data.table::is.data.table(loaded_datatable)) {
                    loaded_datatable <- data.table::as.data.table(loaded_datatable)
                  }
                  if ("Subject" %in% nms) {
                    template_subject <- raveio::raveio_getopt("threeBrain_template_subject",
                      default = "N27")
                    if (!identical(loaded_brain$brain$subject_code,
                      template_subject)) {
                      loaded_datatable <- loaded_datatable[loaded_datatable$Subject %in%
                        loaded_brain$brain$subject_code, ]
                    }
                  }
                  if ("Time" %in% names(loaded_datatable)) {
                    fct <- sprintf("%.2f__%.0f", loaded_datatable$Time,
                      loaded_datatable$Electrode)
                    nms <- nms[!nms %in% c("Time", "Electrode",
                      "Trial", "Frequency", "Block", "Subject")]
                    coltypes <- sapply(nms, function(nm) {
                      is.numeric(loaded_datatable[[nm]])
                    }, simplify = FALSE, USE.NAMES = TRUE)
                    new_table <- lapply(split(loaded_datatable,
                      fct), function(sub) {
                      if (nrow(sub) == 1) {
                        return(sub[, c(nms, "Electrode", "Time"),
                          drop = FALSE])
                      }
                      re <- sapply(nms, function(nm) {
                        v <- sub[[nm]]
                        if (coltypes[[nm]]) {
                          v <- v[!is.na(v)]
                          if (!length(v)) {
                            return(NA_real_)
                          }
                          return(mean(v))
                        } else {
                          v <- table(v)
                          v <- v[!is.na(v)]
                          if (!length(v)) {
                            return(NA)
                          }
                          v <- names(v)[v == max(v)]
                          if (length(v) > 1) {
                            return(NA)
                          }
                          return(v)
                        }
                      }, simplify = FALSE, USE.NAMES = TRUE)
                      re$Electrode <- sub$Electrode[[1]]
                      re$Time <- sub$Time[[1]]
                      as.data.frame(re)
                    })
                    loaded_datatable <- data.table::rbindlist(new_table,
                      use.names = TRUE)
                  }
                  nms <- names(loaded_datatable)
                  invalids <- vapply(nms, function(nm) {
                    all(is.na(loaded_datatable[[nm]]))
                  }, FALSE)
                  nms <- nms[!invalids]
                  if (nrow(loaded_datatable)) {
                    loaded_brain$brain$set_electrode_values(loaded_datatable)
                  } else {
                    nms <- NULL
                  }
                }
                brain_with_data <- list(brain = loaded_brain$brain,
                  variables = nms)
            }
            return(brain_with_data)
        }), deps = c("path_datatable", "data_source", "loaded_brain"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), render_viewer = targets::tar_target_raw(name = "brain_widget",
        command = quote({
            {
                library(dipsaus)
                force(shiny_outputId)
                controllers <- as.list(controllers)
                main_camera <- as.list(main_camera)
                background <- controllers[["Background Color"]]
                if (length(background) != 1) {
                  background <- "#FFFFFF"
                }
                zoom_level <- main_camera$zoom
                if (length(zoom_level) != 1 || zoom_level <=
                  0) {
                  zoom_level <- 1
                }
                position <- as.numeric(unname(unlist(main_camera$position)))
                up <- as.numeric(unname(unlist(main_camera$up)))
                if (length(position) != 3 || length(up) != 3 ||
                  all(position == 0) || all(up == 0) || any(is.na(position)) ||
                  any(is.na(up))) {
                  position <- c(0, 0, 500)
                  up <- c(0, 1, 0)
                } else {
                  position <- position/sqrt(sum(position^2)) *
                    500
                  up <- up/sqrt(sum(up^2))
                }
                dnames <- brain_with_data$variables
                dnames <- dnames[!dnames %in% c("Project", "Subject",
                  "Electrode", "Time", "Label")]
                dname <- controllers[["Display Data"]] %OF% dnames
                if (!identical(controllers[["Display Data"]],
                  dname) && length(dname)) {
                  controllers[["Display Data"]] <- dname
                  controllers[["Display Range"]] <- ""
                }
                if (!isTRUE(controllers[["Show Panels"]])) {
                  controllers[["Show Panels"]] <- FALSE
                }
                brain_widget <- brain_with_data$brain$plot(show_modal = FALSE,
                  background = background, controllers = controllers,
                  start_zoom = zoom_level, custom_javascript = raveio::glue("\n    // Remove the focus box\n    if( canvas.focus_box ) {\n      canvas.focus_box.visible = false;\n    }\n    \n    // set camera\n    canvas.main_camera.position.set(\n      {{ position[[1]] }} , \n      {{ position[[2]] }} , \n      {{ position[[3]] }}\n    );\n    canvas.main_camera.up.set(\n      {{ up[[1]] }} , \n      {{ up[[2]] }} , \n      {{ up[[3]] }}\n    )\n    canvas.main_camera.updateProjectionMatrix();\n\n    // Let shiny know the viewer is ready\n    if( canvas.shiny_mode ) {\n      Shiny.setInputValue(\"{{ shiny_outputId }}\", \"{{Sys.time()}}\");\n    }\n\n    // Force render one frame (update the canvas)\n    canvas.start_animation(0);\n    ",
                    .open = "{{", .close = "}}"))
            }
            return(brain_widget)
        }), deps = c("shiny_outputId", "controllers", "main_camera",
        "brain_with_data"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"))
