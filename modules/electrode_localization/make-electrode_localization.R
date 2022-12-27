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
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_path_ct = targets::tar_target_raw("path_ct", quote({
        settings[["path_ct"]]
    }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_transform = targets::tar_target_raw("path_transform", 
        quote({
            settings[["path_transform"]]
        }), deps = "settings"), input_transform_space = targets::tar_target_raw("transform_space", 
        quote({
            settings[["transform_space"]]
        }), deps = "settings"), `__extern_path_localization_list` = targets::tar_target_raw("settings_path._localization_list_", 
        "./data/localization_list.json", format = "file"), input_localization_list = targets::tar_target_raw("localization_list", 
        quote({
            asNamespace("raveio")$pipeline_load_extdata(name = "localization_list", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_list_"), `__extern_path_localization_plan` = targets::tar_target_raw("settings_path._localization_plan_", 
        "./data/localization_plan.json", format = "file"), input_localization_plan = targets::tar_target_raw("localization_plan", 
        quote({
            asNamespace("raveio")$pipeline_load_extdata(name = "localization_plan", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_plan_"), load_FreeSurfer_LUT = targets::tar_target_raw(name = "fslut", 
        command = quote({
            .__target_expr__. <- quote({
                fslut_path <- system.file("palettes", "datacube2", 
                  "FreeSurferColorLUT.json", package = "threeBrain")
                cmap <- threeBrain::load_colormap(fslut_path)
                fslut <- list(cmap = cmap, labels = sapply(cmap$map, 
                  "[[", "Label"))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(fslut)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "fslut", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "fslut", target_expr = quote({
                {
                  fslut_path <- system.file("palettes", "datacube2", 
                    "FreeSurferColorLUT.json", package = "threeBrain")
                  cmap <- threeBrain::load_colormap(fslut_path)
                  fslut <- list(cmap = cmap, labels = sapply(cmap$map, 
                    "[[", "Label"))
                }
                fslut
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("never"), pattern = NULL, iteration = "list"), 
    load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject", 
            target_export = "subject", target_expr = quote({
                {
                  subject <- raveio::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code, strict = FALSE)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), find_CT_Nifti_files = targets::tar_target_raw(name = "ct_candidates", 
        command = quote({
            .__target_expr__. <- quote({
                fs_path <- subject$freesurfer_path
                if (length(fs_path) != 1 || is.na(fs_path) || 
                  !dir.exists(fs_path)) {
                  stop("Cannot find surface/volume reconstruction folder.")
                }
                f1 <- list.files(file.path(fs_path, "coregistration"), 
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE, 
                  all.files = FALSE)
                f2 <- list.files(file.path(fs_path, "..", "coregistration"), 
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE, 
                  all.files = FALSE)
                files <- c(f1, f2)
                files[duplicated(files)] <- sprintf("%s (2)", 
                  files[duplicated(files)])
                ct_candidates <- structure(files, paths = list(f1 = f1, 
                  f2 = f2))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ct_candidates)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "ct_candidates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "ct_candidates", target_expr = quote({
                {
                  fs_path <- subject$freesurfer_path
                  if (length(fs_path) != 1 || is.na(fs_path) || 
                    !dir.exists(fs_path)) {
                    stop("Cannot find surface/volume reconstruction folder.")
                  }
                  f1 <- list.files(file.path(fs_path, "coregistration"), 
                    pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                    ignore.case = TRUE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE)
                  f2 <- list.files(file.path(fs_path, "..", "coregistration"), 
                    pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                    ignore.case = TRUE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE)
                  files <- c(f1, f2)
                  files[duplicated(files)] <- sprintf("%s (2)", 
                    files[duplicated(files)])
                  ct_candidates <- structure(files, paths = list(f1 = f1, 
                    f2 = f2))
                }
                ct_candidates
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    check_localization_plan = targets::tar_target_raw(name = "plan_list", 
        command = quote({
            .__target_expr__. <- quote({
                count <- fastmap2()
                count$n <- 0
                count$labels <- list()
                plan_list <- NULL
                plan_table <- lapply(localization_plan, function(item) {
                  dim <- as.integer(parse_svec(item$dimension, 
                    unique = FALSE, sep = "[,x]"))
                  dim <- dim[!is.na(dim)]
                  if (!length(dim) || any(dim <= 0)) {
                    return(NULL)
                  }
                  ne <- prod(dim)
                  if (ne <= 0) {
                    return(NULL)
                  }
                  label <- as.character(item$label)
                  if (length(label) != 1 || is.na(label) || trimws(label) == 
                    "") {
                    label <- "NoLabel"
                  }
                  if (!label %in% names(count$labels)) {
                    count$labels[[label]] <- 0
                  }
                  type <- item$type
                  if (!isTRUE(type %in% raveio::LOCATION_TYPES)) {
                    type <- raveio::LOCATION_TYPES[[1]]
                  }
                  re <- data.frame(Electrode = count$n + seq_len(ne), 
                    Label = sprintf("%s%d", label, count$labels[[label]] + 
                      seq_len(ne)), LabelPrefix = label, Dimension = paste(dim, 
                      collapse = "x"), LocationType = type)
                  count$n <- count$n + ne
                  count$labels[[label]] <- count$labels[[label]] + 
                    ne
                  re
                })
                plan_table <- drop_nulls(plan_table)
                if (length(plan_table)) {
                  plan_table <- do.call("rbind", unname(plan_table))
                  electrodes <- sort(subject$preprocess_settings$electrodes)
                  if (length(electrodes) == 0) {
                    electrodes <- plan_table$Electrode
                  }
                  if (length(electrodes) != length(plan_table$Electrode)) {
                    stop(sprintf("The electrode plan table (n=%d) has inconsistent length with registered electrode length (n=%d).", 
                      length(electrodes), length(plan_table$Electrode)))
                  }
                  plan_table$Electrode <- electrodes
                  files <- file.path(subject$meta_path, c("electrodes_unsaved.csv", 
                    "electrodes.csv"))
                  files <- files[file.exists(files)]
                  try({
                    if (length(files)) {
                      files <- files[[1]]
                      electrode_table <- raveio::safe_read_csv(files)
                      tname1 <- c("Electrode", "Coord_x", "Coord_y", 
                        "Coord_z")
                      tname2 <- c("Electrode", "Coord_x", "Coord_y", 
                        "Coord_z", "Radius", "SurfaceType", "Hemisphere", 
                        "FSIndex", "FSLabel", "MNI305_x", "MNI305_y", 
                        "MNI305_z")
                      if (all(tname1 %in% names(electrode_table))) {
                        tname2 <- tname2[tname2 %in% names(electrode_table)]
                        electrode_table <- electrode_table[, 
                          tname2]
                        if (!setequal(as.integer(electrode_table$Electrode), 
                          as.integer(plan_table$Electrode)) || 
                          nrow(electrode_table) != nrow(plan_table)) {
                          stop("Existing electrode table has different row numbers to the plan table")
                        }
                        plan_table <- merge(electrode_table, 
                          plan_table, by = "Electrode")
                      }
                    }
                  }, silent = TRUE)
                  plan_table$Coord_x %?<-% 0
                  plan_table$Coord_y %?<-% 0
                  plan_table$Coord_z %?<-% 0
                  plan_table$Radius %?<-% 1
                  plan_table$SurfaceType %?<-% "pial"
                  plan_table$MNI305_x %?<-% 0
                  plan_table$MNI305_y %?<-% 0
                  plan_table$MNI305_z %?<-% 0
                  plan_table$VertexNumber <- -1
                  plan_table$FSIndex %?<-% 0
                  plan_table$FSLabel %?<-% "Unknown"
                  plan_table$SurfaceElectrode <- plan_table$LocationType %in% 
                    c("ECoG")
                  etypes <- subject$preprocess_settings$electrode_types
                  if (!length(etypes)) {
                    etypes <- "LFP"
                  }
                  plan_table$SignalType <- etypes
                  plan_list <- split(plan_table, plan_table$LabelPrefix)
                  raveio::dir_create2(subject$meta_path)
                  utils::write.csv(plan_table, file = file.path(subject$meta_path, 
                    "electrodes_unsaved.csv"), row.names = FALSE)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plan_list)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plan_list", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plan_list", target_expr = quote({
                {
                  count <- fastmap2()
                  count$n <- 0
                  count$labels <- list()
                  plan_list <- NULL
                  plan_table <- lapply(localization_plan, function(item) {
                    dim <- as.integer(parse_svec(item$dimension, 
                      unique = FALSE, sep = "[,x]"))
                    dim <- dim[!is.na(dim)]
                    if (!length(dim) || any(dim <= 0)) {
                      return(NULL)
                    }
                    ne <- prod(dim)
                    if (ne <= 0) {
                      return(NULL)
                    }
                    label <- as.character(item$label)
                    if (length(label) != 1 || is.na(label) || 
                      trimws(label) == "") {
                      label <- "NoLabel"
                    }
                    if (!label %in% names(count$labels)) {
                      count$labels[[label]] <- 0
                    }
                    type <- item$type
                    if (!isTRUE(type %in% raveio::LOCATION_TYPES)) {
                      type <- raveio::LOCATION_TYPES[[1]]
                    }
                    re <- data.frame(Electrode = count$n + seq_len(ne), 
                      Label = sprintf("%s%d", label, count$labels[[label]] + 
                        seq_len(ne)), LabelPrefix = label, Dimension = paste(dim, 
                        collapse = "x"), LocationType = type)
                    count$n <- count$n + ne
                    count$labels[[label]] <- count$labels[[label]] + 
                      ne
                    re
                  })
                  plan_table <- drop_nulls(plan_table)
                  if (length(plan_table)) {
                    plan_table <- do.call("rbind", unname(plan_table))
                    electrodes <- sort(subject$preprocess_settings$electrodes)
                    if (length(electrodes) == 0) {
                      electrodes <- plan_table$Electrode
                    }
                    if (length(electrodes) != length(plan_table$Electrode)) {
                      stop(sprintf("The electrode plan table (n=%d) has inconsistent length with registered electrode length (n=%d).", 
                        length(electrodes), length(plan_table$Electrode)))
                    }
                    plan_table$Electrode <- electrodes
                    files <- file.path(subject$meta_path, c("electrodes_unsaved.csv", 
                      "electrodes.csv"))
                    files <- files[file.exists(files)]
                    try({
                      if (length(files)) {
                        files <- files[[1]]
                        electrode_table <- raveio::safe_read_csv(files)
                        tname1 <- c("Electrode", "Coord_x", "Coord_y", 
                          "Coord_z")
                        tname2 <- c("Electrode", "Coord_x", "Coord_y", 
                          "Coord_z", "Radius", "SurfaceType", 
                          "Hemisphere", "FSIndex", "FSLabel", 
                          "MNI305_x", "MNI305_y", "MNI305_z")
                        if (all(tname1 %in% names(electrode_table))) {
                          tname2 <- tname2[tname2 %in% names(electrode_table)]
                          electrode_table <- electrode_table[, 
                            tname2]
                          if (!setequal(as.integer(electrode_table$Electrode), 
                            as.integer(plan_table$Electrode)) || 
                            nrow(electrode_table) != nrow(plan_table)) {
                            stop("Existing electrode table has different row numbers to the plan table")
                          }
                          plan_table <- merge(electrode_table, 
                            plan_table, by = "Electrode")
                        }
                      }
                    }, silent = TRUE)
                    plan_table$Coord_x %?<-% 0
                    plan_table$Coord_y %?<-% 0
                    plan_table$Coord_z %?<-% 0
                    plan_table$Radius %?<-% 1
                    plan_table$SurfaceType %?<-% "pial"
                    plan_table$MNI305_x %?<-% 0
                    plan_table$MNI305_y %?<-% 0
                    plan_table$MNI305_z %?<-% 0
                    plan_table$VertexNumber <- -1
                    plan_table$FSIndex %?<-% 0
                    plan_table$FSLabel %?<-% "Unknown"
                    plan_table$SurfaceElectrode <- plan_table$LocationType %in% 
                      c("ECoG")
                    etypes <- subject$preprocess_settings$electrode_types
                    if (!length(etypes)) {
                      etypes <- "LFP"
                    }
                    plan_table$SignalType <- etypes
                    plan_list <- split(plan_table, plan_table$LabelPrefix)
                    raveio::dir_create2(subject$meta_path)
                    utils::write.csv(plan_table, file = file.path(subject$meta_path, 
                      "electrodes_unsaved.csv"), row.names = FALSE)
                  }
                }
                plan_list
            }), target_depends = c("localization_plan", "subject"
            )), deps = c("localization_plan", "subject"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), load_brain = targets::tar_target_raw(name = "brain", 
        command = quote({
            .__target_expr__. <- quote({
                brain <- threeBrain::freesurfer_brain2(fs_subject_folder = subject$freesurfer_path, 
                  subject_name = subject$subject_code)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "brain", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "brain", target_expr = quote({
                {
                  brain <- threeBrain::freesurfer_brain2(fs_subject_folder = subject$freesurfer_path, 
                    subject_name = subject$subject_code)
                }
                brain
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    Loading_brain_and_CT_if_exists = targets::tar_target_raw(name = "localize_data", 
        command = quote({
            .__target_expr__. <- quote({
                force(subject)
                resolve_path <- function(path) {
                  if (length(path) != 1) {
                    stop("Cannot resolve path: ", paste(format(path), 
                      collapse = "\n"))
                  }
                  if (startsWith(path, "{")) {
                    s <- strsplit(path, "/|\\\\")[[1]]
                    s[[1]] <- raveio::glue(s[[1]])
                    s <- do.call(file.path, as.list(s))
                    path <- normalizePath(s, mustWork = TRUE)
                  } else {
                    path <- normalizePath(path, mustWork = TRUE)
                  }
                  path
                }
                has_ct <- FALSE
                ct_path <- character(0L)
                mri_path <- character(0L)
                mri_data <- NULL
                transform_matrix <- NULL
                if (length(path_ct)) {
                  ct_path <- resolve_path(path_ct)
                  has_ct <- TRUE
                  subject$set_default("path_ct", path_ct, namespace = "electrode_localization")
                  ct_data <- threeBrain:::read_nii2(ct_path)
                  transform_space <- tolower(transform_space)
                  if (transform_space %in% c("fsl")) {
                    mri_path <- resolve_path(path_mri)
                    mri_data <- threeBrain:::read_nii2(mri_path, 
                      head_only = TRUE)
                  }
                  subject$set_default("path_mri", path_mri, namespace = "electrode_localization")
                  if (transform_space %in% c("fsl", "ijk2ras")) {
                    transform_matrix <- as.matrix(read.table(resolve_path(path_transform)))
                    dimnames(transform_matrix) <- NULL
                    if (length(transform_matrix) != 16L || !is.numeric(transform_matrix)) {
                      stop("Invalid transform matrix. Must be a 4x4 matrix.")
                    }
                    subject$set_default("path_transform", path_transform, 
                      namespace = "electrode_localization")
                  } else {
                    transform_space <- "resampled"
                  }
                } else {
                  subject$set_default("path_ct", NULL, namespace = "electrode_localization")
                  ct_data <- NULL
                  transform_space <- "no_ct"
                }
                subject$set_default("transform_space", transform_space, 
                  namespace = "electrode_localization")
                localize_data <- list(transform_space = transform_space, 
                  ct_data = ct_data, ct_path = ct_path, mri_path = mri_path, 
                  mri_data = mri_data, transform_matrix = transform_matrix)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localize_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localize_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localize_data", target_expr = quote({
                {
                  force(subject)
                  resolve_path <- function(path) {
                    if (length(path) != 1) {
                      stop("Cannot resolve path: ", paste(format(path), 
                        collapse = "\n"))
                    }
                    if (startsWith(path, "{")) {
                      s <- strsplit(path, "/|\\\\")[[1]]
                      s[[1]] <- raveio::glue(s[[1]])
                      s <- do.call(file.path, as.list(s))
                      path <- normalizePath(s, mustWork = TRUE)
                    } else {
                      path <- normalizePath(path, mustWork = TRUE)
                    }
                    path
                  }
                  has_ct <- FALSE
                  ct_path <- character(0L)
                  mri_path <- character(0L)
                  mri_data <- NULL
                  transform_matrix <- NULL
                  if (length(path_ct)) {
                    ct_path <- resolve_path(path_ct)
                    has_ct <- TRUE
                    subject$set_default("path_ct", path_ct, namespace = "electrode_localization")
                    ct_data <- threeBrain:::read_nii2(ct_path)
                    transform_space <- tolower(transform_space)
                    if (transform_space %in% c("fsl")) {
                      mri_path <- resolve_path(path_mri)
                      mri_data <- threeBrain:::read_nii2(mri_path, 
                        head_only = TRUE)
                    }
                    subject$set_default("path_mri", path_mri, 
                      namespace = "electrode_localization")
                    if (transform_space %in% c("fsl", "ijk2ras")) {
                      transform_matrix <- as.matrix(read.table(resolve_path(path_transform)))
                      dimnames(transform_matrix) <- NULL
                      if (length(transform_matrix) != 16L || 
                        !is.numeric(transform_matrix)) {
                        stop("Invalid transform matrix. Must be a 4x4 matrix.")
                      }
                      subject$set_default("path_transform", path_transform, 
                        namespace = "electrode_localization")
                    } else {
                      transform_space <- "resampled"
                    }
                  } else {
                    subject$set_default("path_ct", NULL, namespace = "electrode_localization")
                    ct_data <- NULL
                    transform_space <- "no_ct"
                  }
                  subject$set_default("transform_space", transform_space, 
                    namespace = "electrode_localization")
                  localize_data <- list(transform_space = transform_space, 
                    ct_data = ct_data, ct_path = ct_path, mri_path = mri_path, 
                    mri_data = mri_data, transform_matrix = transform_matrix)
                }
                localize_data
            }), target_depends = c("subject", "path_ct", "transform_space", 
            "path_mri", "path_transform")), deps = c("subject", 
        "path_ct", "transform_space", "path_mri", "path_transform"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), generate_indicator = targets::tar_target_raw(name = "ct_exists", 
        command = quote({
            .__target_expr__. <- quote({
                ct_exists <- isTRUE(!is.null(localize_data$ct_data) && 
                  is.list(localize_data$ct_data))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ct_exists)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "ct_exists", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "ct_exists", target_expr = quote({
                {
                  ct_exists <- isTRUE(!is.null(localize_data$ct_data) && 
                    is.list(localize_data$ct_data))
                }
                ct_exists
            }), target_depends = "localize_data"), deps = "localize_data", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    generate_localization_viewer = targets::tar_target_raw(name = "viewer", 
        command = quote({
            .__target_expr__. <- quote({
                force(ct_exists)
                viewer <- brain$localize(ct_path = localize_data$ct_data, 
                  transform_space = localize_data$transform_space, 
                  transform_matrix = localize_data$transform_matrix, 
                  mri_path = localize_data$mri_path)
                if (interactive()) {
                  print(viewer)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(viewer)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "viewer", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "viewer", target_expr = quote({
                {
                  force(ct_exists)
                  viewer <- brain$localize(ct_path = localize_data$ct_data, 
                    transform_space = localize_data$transform_space, 
                    transform_matrix = localize_data$transform_matrix, 
                    mri_path = localize_data$mri_path)
                  if (interactive()) {
                    print(viewer)
                  }
                }
                viewer
            }), target_depends = c("ct_exists", "brain", "localize_data"
            )), deps = c("ct_exists", "brain", "localize_data"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), merge_localization_list = targets::tar_target_raw(name = "localization_result_initial", 
        command = quote({
            .__target_expr__. <- quote({
                localization_result_initial <- NULL
                re <- lapply(localization_list, function(item) {
                  item$FSIndex %?<-% 0
                  item$FSLabel %?<-% "Unknown"
                  item$Radius %?<-% 1
                  tbl <- data.frame(Electrode = item$Electrode, 
                    Coord_x = item$Coord_x, Coord_y = item$Coord_y, 
                    Coord_z = item$Coord_z, Label = item$Label, 
                    LabelPrefix = item$LabelPrefix, Dimension = item$Dimension, 
                    LocationType = item$LocationType, Radius = item$Radius, 
                    MNI305_x = item$MNI305_x, MNI305_y = item$MNI305_y, 
                    MNI305_z = item$MNI305_z, FSIndex = item$FSIndex, 
                    FSLabel = item$FSLabel)
                  if (!nrow(tbl)) {
                    return(NULL)
                  }
                  tbl
                })
                re <- do.call("rbind", drop_nulls(re))
                if (length(re) && nrow(re)) {
                  rownames(re) <- NULL
                  re <- re[order(re$Electrode), ]
                  re$SurfaceElectrode <- re$LocationType %in% 
                    c("ECoG")
                  re$SurfaceType <- "pial"
                  re$VertexNumber <- -1
                  empty_sel <- (re$Coord_x)^2 + (re$Coord_y)^2 + 
                    (re$Coord_z)^2
                  empty_sel <- is.na(empty_sel) | empty_sel == 
                    0
                  tkrRAS <- rbind(re$Coord_x, re$Coord_y, re$Coord_z, 
                    1)
                  mr_voxel <- solve(brain$Torig) %*% tkrRAS
                  t1 <- brain$Norig %*% mr_voxel
                  mni305 <- brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% 
                    tkrRAS
                  mni152 <- raveio:::MNI305_to_MNI152 %*% mni305
                  mni305[, empty_sel] <- 0
                  mni152[, empty_sel] <- 0
                  t1[, empty_sel] <- 0
                  mr_voxel[, empty_sel] <- 0
                  re$MNI305_x <- mni305[1, ]
                  re$MNI305_y <- mni305[2, ]
                  re$MNI305_z <- mni305[3, ]
                  re$T1R <- t1[1, ]
                  re$T1A <- t1[2, ]
                  re$T1S <- t1[3, ]
                  re$MNI152_x <- mni152[1, ]
                  re$MNI152_y <- mni152[2, ]
                  re$MNI152_z <- mni152[3, ]
                  re$MRVoxel_I <- round(mr_voxel[1, ])
                  re$MRVoxel_J <- round(mr_voxel[2, ])
                  re$MRVoxel_K <- round(mr_voxel[3, ])
                  save_path <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                  raveio::dir_create2(dirname(save_path))
                  utils::write.csv(re, save_path, row.names = FALSE)
                  localization_result_initial <- re
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_initial)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localization_result_initial", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_initial", target_expr = quote({
                {
                  localization_result_initial <- NULL
                  re <- lapply(localization_list, function(item) {
                    item$FSIndex %?<-% 0
                    item$FSLabel %?<-% "Unknown"
                    item$Radius %?<-% 1
                    tbl <- data.frame(Electrode = item$Electrode, 
                      Coord_x = item$Coord_x, Coord_y = item$Coord_y, 
                      Coord_z = item$Coord_z, Label = item$Label, 
                      LabelPrefix = item$LabelPrefix, Dimension = item$Dimension, 
                      LocationType = item$LocationType, Radius = item$Radius, 
                      MNI305_x = item$MNI305_x, MNI305_y = item$MNI305_y, 
                      MNI305_z = item$MNI305_z, FSIndex = item$FSIndex, 
                      FSLabel = item$FSLabel)
                    if (!nrow(tbl)) {
                      return(NULL)
                    }
                    tbl
                  })
                  re <- do.call("rbind", drop_nulls(re))
                  if (length(re) && nrow(re)) {
                    rownames(re) <- NULL
                    re <- re[order(re$Electrode), ]
                    re$SurfaceElectrode <- re$LocationType %in% 
                      c("ECoG")
                    re$SurfaceType <- "pial"
                    re$VertexNumber <- -1
                    empty_sel <- (re$Coord_x)^2 + (re$Coord_y)^2 + 
                      (re$Coord_z)^2
                    empty_sel <- is.na(empty_sel) | empty_sel == 
                      0
                    tkrRAS <- rbind(re$Coord_x, re$Coord_y, re$Coord_z, 
                      1)
                    mr_voxel <- solve(brain$Torig) %*% tkrRAS
                    t1 <- brain$Norig %*% mr_voxel
                    mni305 <- brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% 
                      tkrRAS
                    mni152 <- raveio:::MNI305_to_MNI152 %*% mni305
                    mni305[, empty_sel] <- 0
                    mni152[, empty_sel] <- 0
                    t1[, empty_sel] <- 0
                    mr_voxel[, empty_sel] <- 0
                    re$MNI305_x <- mni305[1, ]
                    re$MNI305_y <- mni305[2, ]
                    re$MNI305_z <- mni305[3, ]
                    re$T1R <- t1[1, ]
                    re$T1A <- t1[2, ]
                    re$T1S <- t1[3, ]
                    re$MNI152_x <- mni152[1, ]
                    re$MNI152_y <- mni152[2, ]
                    re$MNI152_z <- mni152[3, ]
                    re$MRVoxel_I <- round(mr_voxel[1, ])
                    re$MRVoxel_J <- round(mr_voxel[2, ])
                    re$MRVoxel_K <- round(mr_voxel[3, ])
                    save_path <- file.path(subject$meta_path, 
                      "electrodes_unsaved.csv")
                    raveio::dir_create2(dirname(save_path))
                    utils::write.csv(re, save_path, row.names = FALSE)
                    localization_result_initial <- re
                  }
                }
                localization_result_initial
            }), target_depends = c("localization_list", "brain", 
            "subject")), deps = c("localization_list", "brain", 
        "subject"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), calculate_std141_mappings = targets::tar_target_raw(name = "localization_result_final", 
        command = quote({
            .__target_expr__. <- quote({
                src <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                if (file.exists(src)) {
                  localization_result_final <- utils::read.csv(file.path(subject$meta_path, 
                    "electrodes_unsaved.csv"))
                  try({
                    re <- localization_result_final
                    brain$set_electrodes(electrodes = re)
                    re <- brain$calculate_template_coordinates(save_to = FALSE)
                    sel <- is.na(re$Hemisphere) | !re$Hemisphere %in% 
                      c("left", "right")
                    if (any(sel)) {
                      is_left <- grepl(pattern = "(left|lh-)", 
                        re$FSLabel, ignore.case = TRUE)
                      is_right <- grepl(pattern = "(right|rh-)", 
                        re$FSLabel, ignore.case = TRUE)
                      re$Hemisphere[sel & (is_left | re$MNI305_x < 
                        0)] <- "left"
                      re$Hemisphere[sel & !is_left & (is_right | 
                        re$MNI305_x > 0)] <- "right"
                    }
                    localization_result_final <- re
                  })
                } else {
                  localization_result_final <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_final)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localization_result_final", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_final", target_expr = quote({
                {
                  src <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                  if (file.exists(src)) {
                    localization_result_final <- utils::read.csv(file.path(subject$meta_path, 
                      "electrodes_unsaved.csv"))
                    try({
                      re <- localization_result_final
                      brain$set_electrodes(electrodes = re)
                      re <- brain$calculate_template_coordinates(save_to = FALSE)
                      sel <- is.na(re$Hemisphere) | !re$Hemisphere %in% 
                        c("left", "right")
                      if (any(sel)) {
                        is_left <- grepl(pattern = "(left|lh-)", 
                          re$FSLabel, ignore.case = TRUE)
                        is_right <- grepl(pattern = "(right|rh-)", 
                          re$FSLabel, ignore.case = TRUE)
                        re$Hemisphere[sel & (is_left | re$MNI305_x < 
                          0)] <- "left"
                        re$Hemisphere[sel & !is_left & (is_right | 
                          re$MNI305_x > 0)] <- "right"
                      }
                      localization_result_final <- re
                    })
                  } else {
                    localization_result_final <- NULL
                  }
                }
                localization_result_final
            }), target_depends = c("subject", "brain")), deps = c("subject", 
        "brain"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
