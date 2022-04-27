library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_path_ct = targets::tar_target_raw("path_ct", quote({
        settings[["path_ct"]]
    }), deps = "settings"), input_flirt_path = targets::tar_target_raw("flirt_path", 
        quote({
            settings[["flirt_path"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_skip_coregistration = targets::tar_target_raw("skip_coregistration", 
        quote({
            settings[["skip_coregistration"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_skip_recon = targets::tar_target_raw("skip_recon", 
        quote({
            settings[["skip_recon"]]
        }), deps = "settings"), input_dcm2niix_path = targets::tar_target_raw("dcm2niix_path", 
        quote({
            settings[["dcm2niix_path"]]
        }), deps = "settings"), input_freesurfer_path = targets::tar_target_raw("freesurfer_path", 
        quote({
            settings[["freesurfer_path"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), check_commandline_tools = targets::tar_target_raw(name = "cmd_tools", 
        command = quote({
            {
                dry_run <- raveio::is_dry_run()
                dcm2niix <- tryCatch({
                  dcm2niix <- raveio::normalize_commandline_path(path = dcm2niix_path, 
                    unset = raveio:::cmd_dcm2niix(error_on_missing = FALSE))
                  if (!isTRUE(file.exists(dcm2niix))) {
                    dcm2niix <- NULL
                  }
                  dcm2niix
                }, error = function(e) {
                  NULL
                })
                freesurfer <- tryCatch({
                  freesurfer <- raveio::normalize_commandline_path(path = freesurfer_path, 
                    unset = raveio:::cmd_freesurfer_home(error_on_missing = FALSE))
                  if (!isTRUE(dir.exists(freesurfer))) {
                    freesurfer <- NULL
                  }
                  freesurfer
                }, error = function(e) {
                  NULL
                })
                flirt <- tryCatch({
                  flirt <- raveio::normalize_commandline_path(path = flirt_path, 
                    unset = raveio:::cmd_fsl_flirt(error_on_missing = FALSE))
                  if (!isTRUE(dir.exists(flirt))) {
                    flirt <- NULL
                  }
                  flirt
                }, error = function(e) {
                  NULL
                })
                cmd_tools <- list(dry_run = dry_run, dcm2niix = dcm2niix, 
                  freesurfer = freesurfer, flirt = flirt)
            }
            return(cmd_tools)
        }), deps = c("dcm2niix_path", "freesurfer_path", "flirt_path"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
                print(subject)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_subject_data = targets::tar_target_raw(name = "check_result", 
        command = quote({
            {
                msgs <- character(0L)
                warns <- character(0L)
                fs_path <- subject$freesurfer_path
                if (!length(fs_path) || is.na(fs_path) || !isTRUE(dir.exists(fs_path))) {
                  fs_path <- file.path(dirname(subject$rave_path), 
                    "fs")
                  fs_reconstructed <- FALSE
                } else {
                  fs_reconstructed <- threeBrain::check_freesurfer_path(fs_path, 
                    autoinstall_template = FALSE, check_volume = TRUE, 
                    check_surface = FALSE)
                }
                mri <- NULL
                if (!skip_recon) {
                  if (is.null(cmd_tools$dcm2niix) || is.null(cmd_tools$freesurfer)) {
                    warns <- append(warns, "Cannot find command-line `dcm2niix` and/or `FreeSurfer`: the reconstruction will be skipped")
                    skip_recon <- TRUE
                  } else {
                    mri <- file.path(subject$preprocess_settings$raw_path, 
                      path_mri)
                    if (!isTRUE(dir.exists(mri))) {
                      warns <- append(warns, "No MRI folder found, the reconstruction will be skipped")
                      skip_recon <- TRUE
                    }
                  }
                  if (!skip_recon && fs_reconstructed) {
                    warns <- append(warns, sprintf("Found existing FreeSurfer reconstructed directory; this directory will be renamed: %s", 
                      fs_path))
                  }
                }
                ct <- NULL
                if (!skip_coregistration) {
                  if (is.null(cmd_tools$flirt)) {
                    warns <- append(warns, "Cannot find FSL-flirt; the co-registration will be skipped")
                    skip_coregistration <- TRUE
                  } else if ((!fs_reconstructed && (is.null(cmd_tools$dcm2niix) || 
                    is.null(cmd_tools$freesurfer)))) {
                    warns <- append(warns, "Cannot find FreeSurfer reconstruction, nor at least one of the following commands exists: `dcm2niix`, `FreeSurfer`. The co-registration will be skipped anyway")
                    skip_coregistration <- TRUE
                  } else {
                    ct <- file.path(subject$preprocess_settings$raw_path, 
                      path_ct)
                    if (!isTRUE(dir.exists(ct))) {
                      warns <- append(warns, "No CT folder found, the co-registration will be skipped")
                      skip_coregistration <- TRUE
                    }
                  }
                }
                if (!skip_recon) {
                  msgs <- append(msgs, sprintf("New FreeSurfer reconstruction will be created from %s", 
                    mri))
                  msgs <- append(msgs, sprintf("MRI default folder will be set: %s", 
                    path_mri))
                }
                if (!skip_coregistration) {
                  msgs <- append(msgs, sprintf("CT will be co-registered to MRI for electrode localization; CT path: %s", 
                    ct))
                  msgs <- append(msgs, sprintf("CT default folder will be set: %s", 
                    path_ct))
                }
                check_result <- list(project_name = subject$project_name, 
                  subject_code = subject$subject_code, fs_path = fs_path, 
                  fs_reconstructed = fs_reconstructed, skip_recon = skip_recon, 
                  skip_coregistration = skip_coregistration, 
                  has_dcm2niix = !is.null(cmd_tools$dcm2niix), 
                  has_freesurfer = !is.null(cmd_tools$freesurfer), 
                  has_flirt = !is.null(cmd_tools$flirt), path_mri = mri, 
                  path_ct = ct, messages = msgs, warnings = warns)
            }
            return(check_result)
        }), deps = c("subject", "skip_recon", "cmd_tools", "path_mri", 
        "skip_coregistration", "path_ct"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), set_default_paths = targets::tar_target_raw(name = "default_paths", 
        command = quote({
            {
                if (!check_result$skip_recon && isTRUE(dir.exists(check_result$path_mri))) {
                  ravedash::logger("Setting default MRI folder: ", 
                    path_mri, level = "info")
                  subject$set_default("raw_mri_path", path_mri, 
                    namespace = "surface_reconstruction")
                }
                if (!check_result$skip_coregistration && isTRUE(dir.exists(check_result$path_ct))) {
                  ravedash::logger("Setting default CT folder: ", 
                    path_ct, level = "info")
                  subject$set_default("raw_ct_path", path_ct, 
                    namespace = "surface_reconstruction")
                }
                default_paths <- subject$get_default(c("raw_mri_path", 
                  "raw_ct_path"), namespace = "surface_reconstruction")
                default_paths
            }
            return(default_paths)
        }), deps = c("check_result", "path_mri", "subject", "path_ct"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
