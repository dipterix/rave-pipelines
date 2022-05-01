library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_ct_in_t1 = targets::tar_target_raw("path_ct_in_t1", 
        quote({
            settings[["path_ct_in_t1"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                library(raveio)
                subject <- RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                print(subject)
                print(subject$freesurfer_path)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), find_CT_Nifti_files = targets::tar_target_raw(name = "ct_candidates", 
        command = quote({
            {
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
            }
            return(ct_candidates)
        }), deps = "subject", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_brain = targets::tar_target_raw(name = "brain", 
        command = quote({
            {
                brain <- raveio::rave_brain(subject)
            }
            return(brain)
        }), deps = "subject", cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), Loading_brain_and_CT = targets::tar_target_raw(name = "ct_in_t1", 
        command = quote({
            {
                force(subject)
                has_ct <- FALSE
                ct_path <- character(0L)
                if (length(path_ct_in_t1)) {
                  if (startsWith(path_ct_in_t1, "{")) {
                    s <- strsplit(path_ct_in_t1, "/|\\\\")[[1]]
                    s[[1]] <- raveio::glue(s[[1]])
                    s <- do.call(file.path, as.list(s))
                    ct_path <- normalizePath(s, mustWork = TRUE)
                  } else {
                    ct_path <- normalizePath(path_ct_in_t1, mustWork = TRUE)
                  }
                  has_ct <- TRUE
                }
                if (has_ct) {
                  ct_in_t1 <- threeBrain:::read_nii2(ct_path)
                } else {
                  ct_in_t1 <- NA
                }
            }
            return(ct_in_t1)
        }), deps = c("subject", "path_ct_in_t1"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_indicator = targets::tar_target_raw(name = "ct_exists", 
        command = quote({
            {
                ct_exists <- isTRUE(!is.null(ct_in_t1) && is.list(ct_in_t1))
            }
            return(ct_exists)
        }), deps = "ct_in_t1", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_localization_viewer = targets::tar_target_raw(name = "viewer", 
        command = quote({
            {
                control_presets <- c("localization")
                controllers <- list()
                controllers[["Highlight Box"]] <- FALSE
                controllers[["Overlay Coronal"]] <- TRUE
                controllers[["Overlay Axial"]] <- TRUE
                controllers[["Overlay Sagittal"]] <- TRUE
                if (!is.null(ct_in_t1) && is.list(ct_in_t1)) {
                  ct <- ct_in_t1
                  ct_shift <- ct$get_center_matrix()
                  ct_qform <- ct$get_qform()
                  matrix_world <- brain$Torig %*% solve(brain$Norig) %*% 
                    ct_qform %*% ct_shift
                  threeBrain::add_voxel_cube(brain, "CT", ct$get_data(), 
                    size = ct$get_size(), matrix_world = matrix_world)
                  key <- seq(0, max(ct$get_range()))
                  cmap <- threeBrain::create_colormap(gtype = "volume", 
                    dtype = "continuous", key = key, value = key, 
                    color = c("white", "green", "darkgreen"))
                  controllers[["Left Opacity"]] <- 0.4
                  controllers[["Right Opacity"]] <- 0.4
                  controllers[["Voxel Type"]] <- "CT"
                  controllers[["Voxel Display"]] <- "normal"
                  controllers[["Voxel Min"]] <- 3000
                  controllers[["Edit Mode"]] <- "CT/volume"
                  viewer <- brain$plot(control_presets = control_presets, 
                    voxel_colormap = cmap, controllers = controllers)
                } else {
                  controllers[["Edit Mode"]] <- "MRI slice"
                  controllers[["Left Opacity"]] <- 0.1
                  controllers[["Right Opacity"]] <- 0.1
                  viewer <- brain$plot(control_presets = control_presets, 
                    controllers = controllers)
                }
                if (interactive()) {
                  print(viewer)
                }
            }
            return(viewer)
        }), deps = c("ct_in_t1", "brain"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
