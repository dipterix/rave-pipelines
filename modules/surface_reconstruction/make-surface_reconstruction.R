library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = FALSE, chdir = TRUE)
})
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_skip_coregistration = targets::tar_target_raw("skip_coregistration", 
        quote({
            settings[["skip_coregistration"]]
        }), deps = "settings"), input_path_ct = targets::tar_target_raw("path_ct", 
        quote({
            settings[["path_ct"]]
        }), deps = "settings"), input_dcm2niix_path = targets::tar_target_raw("dcm2niix_path", 
        quote({
            settings[["dcm2niix_path"]]
        }), deps = "settings"), input_params = targets::tar_target_raw("params", 
        quote({
            settings[["params"]]
        }), deps = "settings"), input_afni_path = targets::tar_target_raw("afni_path", 
        quote({
            settings[["afni_path"]]
        }), deps = "settings"), input_skip_recon = targets::tar_target_raw("skip_recon", 
        quote({
            settings[["skip_recon"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_freesurfer_path = targets::tar_target_raw("freesurfer_path", 
        quote({
            settings[["freesurfer_path"]]
        }), deps = "settings"), input_fsl_path = targets::tar_target_raw("fsl_path", 
        quote({
            settings[["fsl_path"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), check_commandline_tools = targets::tar_target_raw(name = "cmd_tools", 
        command = quote({
            {
                default_dcm2niix_path <- raveio:::cmd_dcm2niix(error_on_missing = FALSE)
                dcm2niix <- tryCatch({
                  dcm2niix <- raveio::normalize_commandline_path(path = dcm2niix_path, 
                    unset = default_dcm2niix_path)
                  if (!path_is_valid(dcm2niix)) {
                    dcm2niix <- NULL
                  } else if (!identical(default_dcm2niix_path, 
                    dcm2niix)) {
                    raveio::raveio_setopt("dcm2niix_path", dcm2niix)
                  }
                  dcm2niix
                }, error = function(e) {
                  NULL
                })
                default_fs_path <- raveio:::cmd_freesurfer_home(error_on_missing = FALSE)
                freesurfer <- tryCatch({
                  freesurfer <- raveio::normalize_commandline_path(path = freesurfer_path, 
                    unset = default_fs_path, type = "freesurfer")
                  if (!path_is_valid(freesurfer, dir_ok = TRUE)) {
                    freesurfer <- NULL
                  } else if (!identical(default_fs_path, freesurfer)) {
                    raveio::raveio_setopt("freesurfer_path", 
                      freesurfer)
                  }
                  freesurfer
                }, error = function(e) {
                  NULL
                })
                default_fsl_path <- raveio:::cmd_fsl_home(error_on_missing = FALSE)
                flirt <- tryCatch({
                  fsl <- raveio::normalize_commandline_path(path = fsl_path, 
                    type = "fsl", unset = default_fsl_path)
                  flirt <- NULL
                  if (path_is_valid(fsl, dir_ok = TRUE)) {
                    if (!identical(default_fsl_path, fsl)) {
                      raveio::raveio_setopt("fsl_path", fsl)
                    }
                    flirt <- file.path(fsl, "bin", "flirt")
                  }
                  flirt
                }, error = function(e) {
                  NULL
                })
                default_afni_path <- raveio:::cmd_afni_home(error_on_missing = FALSE)
                afni <- tryCatch({
                  afni <- raveio::normalize_commandline_path(path = afni_path, 
                    type = "afni", unset = default_afni_path)
                  if (path_is_valid(afni, dir_ok = TRUE)) {
                    if (!identical(default_afni_path, afni)) {
                      raveio::raveio_setopt("afni_path", afni)
                    }
                  } else {
                    afni
                  }
                  afni
                }, error = function(e) {
                  NULL
                })
                cmd_tools <- list(dcm2niix = dcm2niix, freesurfer = freesurfer, 
                  flirt = flirt, afni = afni)
            }
            return(cmd_tools)
        }), deps = c("dcm2niix_path", "freesurfer_path", "fsl_path", 
        "afni_path"), cue = targets::tar_cue("always"), pattern = NULL, 
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
                if (!path_is_valid(fs_path, dir_ok = TRUE)) {
                  fs_path <- file.path(subject$path, "fs")
                  fs_reconstructed <- FALSE
                } else {
                  fs_reconstructed <- threeBrain::check_freesurfer_path(fs_path, 
                    autoinstall_template = FALSE, check_volume = TRUE, 
                    check_surface = FALSE)
                }
                mri <- file.path(subject$preprocess_settings$raw_path, 
                  path_mri)
                if (is.null(cmd_tools$dcm2niix) || is.null(cmd_tools$freesurfer)) {
                  warns <- append(warns, "Cannot find command-line `dcm2niix` and/or `FreeSurfer`: the reconstruction will fail. Please make sure these programs paths are entered correctly.")
                } else {
                  if (!path_is_valid(mri, dir_ok = TRUE)) {
                    warns <- append(warns, "No MRI folder found, the reconstruction will result in errors")
                  }
                }
                if (!skip_recon && fs_reconstructed) {
                  warns <- append(warns, sprintf("Found existing FreeSurfer reconstructed directory. `recon-all` will ignore the imported T1 images. Instead, FreeSurfer will continue working on this directory unless you manually remove it: %s", 
                    fs_path))
                }
                ct <- file.path(subject$preprocess_settings$raw_path, 
                  path_ct)
                if (is.null(cmd_tools$afni) && is.null(cmd_tools$flirt)) {
                  warns <- append(warns, "Cannot find AFNI-3dAllineate nor FSL-flirt; the co-registration will result in errors. Please make sure `AFNI` or `FSL` home path is specified correctly.")
                } else {
                  if (!path_is_valid(ct, dir_ok = TRUE)) {
                    warns <- append(warns, "The CT path is invalid: co-registration will result in errors.")
                  }
                }
                if (!skip_recon) {
                  msgs <- append(msgs, sprintf("New FreeSurfer reconstruction will be created from %s", 
                    mri))
                  msgs <- append(msgs, sprintf("MRI default DICOM folder/Nifti file is set: %s", 
                    path_mri))
                }
                if (!skip_coregistration) {
                  msgs <- append(msgs, sprintf("CT will be co-registered to MRI for electrode localization; CT path: %s", 
                    ct))
                  msgs <- append(msgs, sprintf("CT default DICOM folder/Nifti file is set: %s", 
                    path_ct))
                }
                path_temp <- file.path(subject$preprocess_settings$raw_path, 
                  "rave-imaging")
                path_log <- file.path(path_temp, "log")
                check_result <- list(project_name = subject$project_name, 
                  subject_code = subject$subject_code, fs_path = fs_path, 
                  fs_reconstructed = fs_reconstructed, skip_recon = skip_recon, 
                  skip_coregistration = skip_coregistration, 
                  has_dcm2niix = !is.null(cmd_tools$dcm2niix), 
                  has_freesurfer = !is.null(cmd_tools$freesurfer), 
                  has_flirt = !is.null(cmd_tools$flirt), has_3dallineate = !is.null(cmd_tools$afni), 
                  path_mri = mri, path_ct = ct, path_temp = path_temp, 
                  path_log = path_log, messages = msgs, warnings = warns)
            }
            return(check_result)
        }), deps = c("subject", "path_mri", "cmd_tools", "skip_recon", 
        "path_ct", "skip_coregistration"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), import_T1_MRI = targets::tar_target_raw(name = "import_T1", 
        command = quote({
            {
                import_T1 <- tryCatch({
                  raveio::cmd_run_dcm2niix(subject = subject, 
                    src_path = check_result$path_mri, type = "MRI", 
                    merge = params$dcm2niix$merge %OF% c("Auto", 
                      "No", "Yes"), float = params$dcm2niix$float %OF% 
                      c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                      c("No", "Yes", "Ignore"), overwrite = TRUE, 
                    verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            }
            return(import_T1)
        }), deps = c("subject", "check_result", "params", "cmd_tools"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), import_CT = targets::tar_target_raw(name = "import_CT", 
        command = quote({
            {
                import_CT <- tryCatch({
                  raveio::cmd_run_dcm2niix(subject = subject, 
                    src_path = check_result$path_ct, type = "CT", 
                    merge = params$dcm2niix$merge %OF% c("Auto", 
                      "No", "Yes"), float = params$dcm2niix$float %OF% 
                      c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                      c("No", "Yes", "Ignore"), overwrite = TRUE, 
                    verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            }
            return(import_CT)
        }), deps = c("subject", "check_result", "params", "cmd_tools"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), FreeSurfer_reconstruction = targets::tar_target_raw(name = "fs_recon", 
        command = quote({
            {
                fs_recon <- tryCatch({
                  mri_path <- params$nii_t1
                  mri_root <- file.path(check_result$path_temp, 
                    "inputs", "MRI")
                  mri_path <- file.path(mri_root, mri_path)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a Nifti file under ", 
                      mri_root)
                  }
                  autorecon_flags <- c("-autorecon1", "-all", 
                    "-autorecon2", "-autorecon3", "-autorecon2-cp", 
                    "-autorecon2-wm", "-autorecon2-pial")
                  flag <- params$freesurfer$flag %OF% autorecon_flags
                  raveio::cmd_run_recon_all(subject = subject, 
                    mri_path = mri_path, args = flag, overwrite = params$freesurfer$fresh_start, 
                    dry_run = TRUE, verbose = FALSE, command_path = cmd_tools$freesurfer)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            }
            return(fs_recon)
        }), deps = c("params", "check_result", "subject", "cmd_tools"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), CT_MR_coregistration_via_FSL = targets::tar_target_raw(name = "coreg_flirt", 
        command = quote({
            {
                coreg_flirt <- tryCatch({
                  mri_path <- params$nii_t1
                  mri_root <- file.path(check_result$path_temp, 
                    "inputs", "MRI")
                  mri_path <- file.path(mri_root, mri_path)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file under ", 
                      mri_root)
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file under ", 
                      ct_root)
                  }
                  raveio::cmd_run_flirt(subject = subject, mri_path = mri_path, 
                    ct_path = ct_path, overwrite = FALSE, command_path = cmd_tools$flirt, 
                    dry_run = TRUE, verbose = FALSE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            }
            return(coreg_flirt)
        }), deps = c("params", "check_result", "subject", "cmd_tools"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), CT_MR_coregistration_via_AFNI = targets::tar_target_raw(name = "coreg_3dallineate", 
        command = quote({
            {
                coreg_3dallineate <- tryCatch({
                  mri_path <- params$nii_t1
                  mri_root <- file.path(check_result$path_temp, 
                    "inputs", "MRI")
                  mri_path <- file.path(mri_root, mri_path)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file under ", 
                      mri_root)
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file under ", 
                      ct_root)
                  }
                  raveio::cmd_run_3dAllineate(subject = subject, 
                    mri_path = mri_path, ct_path = ct_path, overwrite = FALSE, 
                    command_path = cmd_tools$afni, dry_run = TRUE, 
                    verbose = FALSE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            }
            return(coreg_3dallineate)
        }), deps = c("params", "check_result", "subject", "cmd_tools"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
