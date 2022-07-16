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
    input_dryrun = targets::tar_target_raw("dryrun", quote({
        settings[["dryrun"]]
    }), deps = "settings"), input_path_ct = targets::tar_target_raw("path_ct", 
        quote({
            settings[["path_ct"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_fsl_path = targets::tar_target_raw("fsl_path", 
        quote({
            settings[["fsl_path"]]
        }), deps = "settings"), input_params_dcm2niix = targets::tar_target_raw("params_dcm2niix", 
        quote({
            settings[["params_dcm2niix"]]
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
        }), deps = "settings"), input_params = targets::tar_target_raw("params", 
        quote({
            settings[["params"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), check_commandline_tools = targets::tar_target_raw(name = "cmd_tools", 
        command = quote({
            {
                dry_run <- raveio::is_dry_run() || isTRUE(dryrun)
                default_dcm2niix_path <- raveio:::cmd_dcm2niix(error_on_missing = FALSE)
                dcm2niix <- tryCatch({
                  dcm2niix <- raveio::normalize_commandline_path(path = dcm2niix_path, 
                    unset = default_dcm2niix_path)
                  if (length(dcm2niix) != 1 || is.na(dcm2niix) || 
                    !isTRUE(file.exists(dcm2niix))) {
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
                  if (length(freesurfer) != 1 || is.na(freesurfer) || 
                    !isTRUE(dir.exists(freesurfer))) {
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
                  if (length(fsl) == 1 && !is.na(fsl) && isTRUE(dir.exists(fsl))) {
                    if (!identical(default_fsl_path, fsl)) {
                      raveio::raveio_setopt("fsl_path", fsl)
                    }
                    flirt <- file.path(fsl, "bin", "flirt")
                  }
                  flirt
                }, error = function(e) {
                  NULL
                })
                cmd_tools <- list(dry_run = dry_run, dcm2niix = dcm2niix, 
                  freesurfer = freesurfer, flirt = flirt)
            }
            return(cmd_tools)
        }), deps = c("dryrun", "dcm2niix_path", "freesurfer_path", 
        "fsl_path"), cue = targets::tar_cue("always"), pattern = NULL, 
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
                  fs_path <- file.path(subject$path, "fs")
                  fs_reconstructed <- FALSE
                } else {
                  fs_reconstructed <- threeBrain::check_freesurfer_path(fs_path, 
                    autoinstall_template = FALSE, check_volume = TRUE, 
                    check_surface = FALSE)
                }
                mri <- file.path(subject$preprocess_settings$raw_path, 
                  path_mri)
                if (!skip_recon) {
                  if (is.null(cmd_tools$dcm2niix) || is.null(cmd_tools$freesurfer)) {
                    warns <- append(warns, "Cannot find command-line `dcm2niix` and/or `FreeSurfer`: the reconstruction will be skipped")
                    skip_recon <- TRUE
                  } else {
                    if (!isTRUE(file.exists(mri))) {
                      warns <- append(warns, "No MRI folder found, the reconstruction will be skipped")
                      skip_recon <- TRUE
                    }
                  }
                  if (!skip_recon && fs_reconstructed) {
                    warns <- append(warns, sprintf("Found existing FreeSurfer reconstructed directory; this directory will be renamed: %s", 
                      fs_path))
                  }
                }
                ct <- file.path(subject$preprocess_settings$raw_path, 
                  path_ct)
                if (!skip_coregistration) {
                  if (is.null(cmd_tools$flirt)) {
                    warns <- append(warns, "Cannot find FSL-flirt; the co-registration will be skipped")
                    skip_coregistration <- TRUE
                  } else if ((!fs_reconstructed && (is.null(cmd_tools$dcm2niix) || 
                    is.null(cmd_tools$freesurfer)))) {
                    warns <- append(warns, "Cannot find FreeSurfer reconstruction, nor at least one of the following commands exists: `dcm2niix`, `FreeSurfer`. The co-registration will be skipped anyway")
                    skip_coregistration <- TRUE
                  } else {
                    if (!isTRUE(file.exists(ct))) {
                      warns <- append(warns, "No CT folder found, the co-registration will be skipped")
                      skip_coregistration <- TRUE
                    }
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
                  has_flirt = !is.null(cmd_tools$flirt), path_mri = mri, 
                  path_ct = ct, path_temp = path_temp, path_log = path_log, 
                  messages = msgs, warnings = warns)
            }
            return(check_result)
        }), deps = c("subject", "path_mri", "skip_recon", "cmd_tools", 
        "path_ct", "skip_coregistration"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), set_default_paths = targets::tar_target_raw(name = "default_paths", 
        command = quote({
            {
                if (isTRUE(file.exists(check_result$path_mri))) {
                  ravedash::logger("Setting default MRI path: ", 
                    path_mri, level = "info")
                  subject$set_default("raw_mri_path", path_mri, 
                    namespace = "surface_reconstruction")
                }
                if (isTRUE(file.exists(check_result$path_ct))) {
                  ravedash::logger("Setting default CT path: ", 
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
        iteration = "list"), convert_DICOM_to_Nifti_dryrun = targets::tar_target_raw(name = "script_dcm2nii", 
        command = quote({
            {
                require(ravedash)
                dcm2niix <- as.character(cmd_tools$dcm2niix)
                if (!isTRUE(file.exists(dcm2niix))) {
                  dcm2niix <- "dcm2niix"
                }
                path_temp <- check_result$path_temp
                indir_mri <- check_result$path_mri
                indir_ct <- check_result$path_ct
                outdir_mri <- file.path(path_temp, "inputs", 
                  "MRI")
                outdir_ct <- file.path(path_temp, "inputs", "CT")
                merge <- params$dcm2niix$merge %OF% c("Auto", 
                  "No", "Yes")
                merge <- c("-m n ", "-m y ", "")[[which(c("No", 
                  "Yes", "Auto") == merge)]]
                float <- params$dcm2niix$float %OF% c("Yes", 
                  "No")
                float <- c("-p y ", "-p n ")[[which(c("Yes", 
                  "No") == float)]]
                crop <- params$dcm2niix$crop %OF% c("No", "Yes", 
                  "Ignore")
                crop <- c("-x n", "-x y", "-x i")[[which(c("No", 
                  "Yes", "Ignore") == crop)]]
                args <- paste0(merge, float, crop)
                path_log <- normalizePath(check_result$path_log, 
                  winslash = "/", mustWork = FALSE)
                log_file <- strftime(Sys.time(), "log-import-mri-ct.log")
                cmd_type <- "cmd"
                cmd <- c("#!/usr/bin/env sh", "", "# This is your dcm2niix binary path", 
                  sprintf("cmd_dcm2niix=%s", dcm2niix), "", "# Prepare log file", 
                  sprintf("log_dir=%s", shQuote(path_log)), sprintf("log_file=%s", 
                    shQuote(log_file)), "mkdir -p \"$log_dir\" && touch \"$log_dir/$log_file\" && echo \"Started: $(date -u)\" > \"$log_dir/$log_file\"", 
                  "echo --------------------------------------------------------", 
                  "echo Log file: \"$log_dir/$log_file\"", "echo --------------------------------------------------------")
                error <- TRUE
                if (length(indir_mri) == 1) {
                  cmdadd <- c("# ------------------- MRI ---------------------", 
                    "# Assign MRI input & output folders", sprintf("subj_mri_in=%s", 
                      shQuote(indir_mri, type = cmd_type)), sprintf("subj_mri_out=%s", 
                      shQuote(outdir_mri, type = cmd_type)), 
                    "[ -d \"$subj_mri_out\" ] && rm -r \"$subj_mri_out\"", 
                    "mkdir -p \"$subj_mri_out\"", "")
                  if (dir.exists(indir_mri)) {
                    error <- FALSE
                    cmdadd <- c(cmdadd, "# Convert MRI (DICOM => Nifti)", 
                      sprintf("$cmd_dcm2niix %s -o \"$subj_mri_out\" \"$subj_mri_in\" >> \"$log_dir/$log_file\" 2>&1", 
                        args))
                  } else if (grepl(pattern = "nii($|\\.gz$)", 
                    x = indir_mri, ignore.case = TRUE)) {
                    error <- FALSE
                    cmdadd <- c(cmdadd, "# Copy MRI Nifti file to RAVE temporary directory", 
                      "cp -r \"$subj_mri_in\" \"$subj_mri_out\"")
                  } else {
                    cmdadd <- NULL
                  }
                  cmd <- c(cmd, cmdadd)
                }
                if (length(indir_ct) == 1) {
                  cmdadd <- c("", "# ------------------- CT ----------------------", 
                    "# Assign CT input & output folders", sprintf("subj_ct_in=%s", 
                      shQuote(indir_ct, type = cmd_type)), sprintf("subj_ct_out=%s", 
                      shQuote(outdir_ct, type = cmd_type)), "[ -d \"$subj_ct_out\" ] && rm -r \"$subj_ct_out\"", 
                    "mkdir -p \"$subj_ct_out\"", "")
                  if (dir.exists(indir_ct)) {
                    error <- FALSE
                    cmdadd <- c(cmdadd, "# Convert CT (DICOM => Nifti)", 
                      sprintf("$cmd_dcm2niix %s -o \"$subj_ct_out\" \"$subj_ct_in\" >> \"$log_dir/$log_file\" 2>&1", 
                        args))
                    cmd <- c(cmd, cmdadd)
                  } else if (grepl(pattern = "nii($|\\.gz$)", 
                    x = indir_ct, ignore.case = TRUE)) {
                    error <- FALSE
                    cmdadd <- c(cmdadd, "# Copy MRI Nifti file to RAVE temporary directory", 
                      "cp -r \"$subj_ct_in\" \"$subj_ct_out\"")
                  } else {
                    cmdadd <- NULL
                  }
                  cmd <- c(cmd, cmdadd)
                }
                cmd <- c(cmd, "echo Done. >> \"$log_dir/$log_file\" && echo Done.", 
                  "")
                script_dcm2nii <- list(script = cmd, path_temp = path_temp, 
                  error = error, log_file = normalizePath(file.path(path_log, 
                    log_file), winslash = "/", mustWork = FALSE))
            }
            return(script_dcm2nii)
        }), deps = c("cmd_tools", "check_result", "params"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    convert_DICOM_to_Nifti = targets::tar_target_raw(name = "conversion_mri", 
        command = quote({
            {
                if (!isFALSE(script_dcm2nii$error)) {
                  stop("Cannot convert DICOM to Nifti, invalid MRI or CT files")
                }
                dry_run <- raveio::is_dry_run() || isTRUE(dryrun)
                path_temp <- script_dcm2nii$path_temp
                outdir_mri <- file.path(path_temp, "inputs", 
                  "MRI")
                outdir_ct <- file.path(path_temp, "inputs", "CT")
                script_path <- file.path(raveio::dir_create2(file.path(path_temp, 
                  "scripts")), "cmd-import-mri-ct.sh")
                backup_dir <- raveio::dir_create2(file.path(path_temp, 
                  "scripts", "backups"))
                backup_path <- raveio::backup_file(script_path, 
                  remove = TRUE)
                if (!isFALSE(backup_path) && isTRUE(file.exists(backup_path))) {
                  file.rename(backup_path, file.path(backup_dir, 
                    basename(backup_path)))
                }
                writeLines(script_dcm2nii$script, con = script_path)
                cmd <- sprintf("bash %s", shQuote(normalizePath(script_path, 
                  mustWork = TRUE)))
                if (!dry_run) {
                  unlink(script_dcm2nii$log_file)
                  system(cmd, wait = TRUE)
                  raveio::backup_file(script_dcm2nii$log_file, 
                    remove = FALSE)
                }
                conversion_mri <- list(command = cmd, dry_run = dry_run, 
                  script_path = script_path)
            }
            return(conversion_mri)
        }), deps = c("script_dcm2nii", "dryrun"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), command_FreeSurfer_recon_dryrun = targets::tar_target_raw(name = "script_recon", 
        command = quote({
            {
                require(ravedash)
                fs_home <- as.character(cmd_tools$freesurfer)
                if (isTRUE(check_result$skip_recon)) {
                  script_recon <- list(error = TRUE, reason = list(message = "FreeSurfer reconstruction is manually skipped"))
                } else if (!isTRUE(dir.exists(fs_home))) {
                  script_recon <- list(error = TRUE, reason = list(message = "Cannot find FreeSurfer home directory"))
                } else {
                  fs_home <- normalizePath(fs_home, winslash = "/")
                  path_temp <- check_result$path_temp
                  fs_sdir <- normalizePath(file.path(check_result$fs_path, 
                    ".."), mustWork = FALSE)
                  infile <- params$nii_t1
                  infile_absolute <- file.path(path_temp, "inputs", 
                    "MRI", infile)
                  if (length(infile_absolute) != 1 || is.na(infile_absolute) || 
                    !file.exists(infile_absolute)) {
                    script_recon <- list(error = TRUE, reason = list(message = "No MRI input specified"))
                  } else {
                    path_temp <- normalizePath(path_temp, mustWork = FALSE)
                    if (grepl(" ", path_temp)) {
                      symlink_root <- file.path(tools::R_user_dir("rave", 
                        which = "cache"), "FreeSurferSubjects", 
                        subject_code)
                      raveio::dir_create2(symlink_root)
                      symlink_path <- symlink_root
                      symlink_cmd1 <- c("", "# Orginal subject path contains spaces, FreeSurfer will fail", 
                        sprintf("symlink_path=%s", shQuote(symlink_path)), 
                        "if [ -d $symlink_path/rave-imaging ]; then", 
                        "  rm \"$symlink_path/rave-imaging\"", 
                        "fi", sprintf("ln -s \"%s\" \"$symlink_path\"", 
                          path_temp), "")
                      symlink_cmd2 <- c("", "rm \"$symlink_path/rave-imaging\"", 
                        "# Try to remove temp path, but it is OK to fail here", 
                        "rm \"$symlink_path\"", "")
                      path_temp <- file.path(symlink_path, "rave-imaging")
                    } else {
                      raveio::dir_create2(path_temp)
                      symlink_cmd1 <- NULL
                      symlink_cmd2 <- NULL
                    }
                    autorecon_flags <- c("-all", "-autorecon1", 
                      "-autorecon2", "-autorecon3", "-autorecon2-cp", 
                      "-autorecon2-wm", "-autorecon2-pial")
                    flag <- params$freesurfer$steps %OF% autorecon_flags
                    path_log <- normalizePath(check_result$path_log, 
                      winslash = "/", mustWork = FALSE)
                    log_file <- sprintf("log-fs-recon%s.log", 
                      flag)
                    cmd_type <- "cmd"
                    cmd <- c("#!/usr/bin/env bash", "", "# Set FreeSurfer home directory & initialize", 
                      sprintf("FREESURFER_HOME=%s", shQuote(fs_home)), 
                      "source $FREESURFER_HOME/SetUpFreeSurfer.sh", 
                      "", sprintf("SUBJECTS_DIR=%s", shQuote(path_temp)), 
                      sprintf("mri_infile=\"$SUBJECTS_DIR/inputs/MRI/%s\"", 
                        infile), symlink_cmd1, "# Prepare log file", 
                      sprintf("log_dir=%s", shQuote(path_log)), 
                      sprintf("log_file=%s", shQuote(log_file)), 
                      "mkdir -p \"$log_dir\" && touch \"$log_dir/$log_file\" && echo \"Started: $(date -u)\" > \"$log_dir/$log_file\"", 
                      "echo --------------------------------------------------------", 
                      "echo Log file: \"$log_dir/$log_file\"", 
                      "echo --------------------------------------------------------", 
                      local({
                        if (isTRUE(params$freesurfer$fresh_start)) {
                          c("# Force removing FreeSurfer path and re-run the reconstruction", 
                            "# You need original Nifti files to run", 
                            "if [ -d \"$SUBJECTS_DIR/fs\" ]; then", 
                            "  rm -r \"$SUBJECTS_DIR/fs\"", "fi", 
                            sprintf("recon-all -sd \"$SUBJECTS_DIR\" -sid fs -i \"$mri_infile\" %s >> \"$log_dir/$log_file\" 2>&1", 
                              flag))
                        } else {
                          c("if [ -d \"$SUBJECTS_DIR/fs/mri\" ]; then", 
                            "  # Use existing FreeSurfer directory to continue analysis", 
                            "  # You do not need original Nifti files to run", 
                            sprintf("  recon-all -sd \"$SUBJECTS_DIR\" -sid fs %s", 
                              flag), "else", "  # Remove invalid FreeSurfer path and re-run the reconstruction", 
                            "  # You need original Nifti files to run", 
                            "  if [ -d \"$SUBJECTS_DIR/fs\" ]; then", 
                            "    rm -r \"$SUBJECTS_DIR/fs\"", 
                            "  fi", sprintf("  recon-all -sd \"$SUBJECTS_DIR\" -sid fs -i \"$mri_infile\" %s >> \"$log_dir/$log_file\" 2>&1", 
                              flag), "fi")
                        }
                      }))
                    cmd <- c(cmd, symlink_cmd2, "echo Done. >> \"$log_dir/$log_file\" && echo Done.", 
                      "")
                    script_recon <- list(script = cmd, path_temp = path_temp, 
                      flag = flag, error = FALSE, log_file = normalizePath(file.path(path_log, 
                        log_file), winslash = "/", mustWork = FALSE))
                  }
                }
            }
            return(script_recon)
        }), deps = c("cmd_tools", "check_result", "params", "subject_code"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), command_FreeSurfer_recon = targets::tar_target_raw(name = "fs_recon", 
        command = quote({
            {
                if (!isFALSE(script_recon$error)) {
                  stop(c(script_recon$reason$message, "Cannot run FreeSurfer commands")[[1]])
                }
                dry_run <- raveio::is_dry_run() || isTRUE(dryrun)
                path_temp <- script_recon$path_temp
                script_path <- file.path(raveio::dir_create2(file.path(path_temp, 
                  "scripts")), sprintf("cmd-fs-recon%s.sh", script_recon$flag))
                backup_dir <- raveio::dir_create2(file.path(path_temp, 
                  "scripts", "backups"))
                backup_path <- raveio::backup_file(script_path, 
                  remove = TRUE)
                if (!isFALSE(backup_path) && isTRUE(file.exists(backup_path))) {
                  file.rename(backup_path, file.path(backup_dir, 
                    basename(backup_path)))
                }
                writeLines(script_recon$script, con = script_path)
                cmd <- sprintf("bash %s", shQuote(normalizePath(script_path, 
                  mustWork = TRUE)))
                if (!dry_run) {
                  unlink(script_recon$log_file)
                  system(cmd, wait = TRUE)
                  raveio::backup_file(script_recon$log_file, 
                    remove = FALSE)
                }
                ravedash::logger("Setting default T1 nii file: ", 
                  params$nii_t1, level = "info")
                subject$set_default("nii_t1", params$nii_t1, 
                  namespace = "surface_reconstruction")
                fs_recon <- list(command = cmd, dry_run = dry_run, 
                  script_path = script_path)
            }
            return(fs_recon)
        }), deps = c("script_recon", "dryrun", "params", "subject"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), command_coreg_dryrun = targets::tar_target_raw(name = "script_coreg", 
        command = quote({
            {
                require(ravedash)
                flirt <- as.character(cmd_tools$flirt)
                if (isTRUE(file.exists(flirt))) {
                  fsl_dir <- normalizePath(dirname(dirname(flirt)))
                } else {
                  fsl_dir <- "/usr/local/fsl"
                  flirt <- "flirt"
                }
                nii_t1 <- file.path(check_result$path_temp, "inputs", 
                  "MRI", params$nii_t1)
                nii_ct <- file.path(check_result$path_temp, "inputs", 
                  "CT", params$nii_ct)
                outdir <- raveio::dir_create2(file.path(check_result$path_temp, 
                  "coregistration"))
                error <- FALSE
                err_msg <- NULL
                if (!isTRUE(file.exists(nii_t1))) {
                  error <- TRUE
                  err_msg <- "Cannot find T1 file"
                }
                if (!isTRUE(file.exists(nii_ct))) {
                  error <- TRUE
                  err_msg <- c(err_msg, "Cannot find CT file")
                }
                nii_t1 <- normalizePath(nii_t1, winslash = "/", 
                  mustWork = FALSE)
                nii_ct <- normalizePath(nii_ct, winslash = "/", 
                  mustWork = FALSE)
                path_log <- normalizePath(check_result$path_log, 
                  winslash = "/", mustWork = FALSE)
                log_file <- strftime(Sys.time(), "log-ct-coregistration-t1.log")
                cmd <- c("#!/usr/bin/env bash", "", "# FSL setup", 
                  sprintf("export FSLDIR=%s", shQuote(fsl_dir)), 
                  "source ${FSLDIR}/etc/fslconf/fsl.sh", "PATH=${FSLDIR}/bin:${PATH}", 
                  "", "# Set input T1 & CT files", sprintf("infile_ct=%s", 
                    shQuote(nii_ct)), sprintf("infile_t1=%s", 
                    shQuote(nii_t1)), sprintf("outdir=%s", shQuote(outdir)), 
                  "", "# Prepare log file", sprintf("log_dir=%s", 
                    shQuote(path_log)), sprintf("log_file=%s", 
                    shQuote(log_file)), "mkdir -p \"$log_dir\" && touch \"$log_dir/$log_file\" && echo \"Started: $(date -u)\" > \"$log_dir/$log_file\"", 
                  "echo --------------------------------------------------------", 
                  "echo Log file: \"$log_dir/$log_file\"", "echo --------------------------------------------------------", 
                  "", "# Run flirt", "flirt -in \"$infile_ct\" -ref \"$infile_t1\" \\", 
                  "  -out \"$outdir/ct_in_t1.nii\" -omat \"$outdir/ct2t1.mat\" \\", 
                  "  -interp trilinear -cost mutualinfo -dof 6 -searchcost mutualinfo \\", 
                  "  -searchrx -180 180 -searchry -180 180 -searchrz -180 180 -v >> \"$log_dir/$log_file\" 2>&1", 
                  "echo Done. >> \"$log_dir/$log_file\" && echo Done.", 
                  "")
                script_coreg <- list(script = cmd, path_temp = check_result$path_temp, 
                  error = FALSE, reason = list(message = paste(err_msg, 
                    collapse = "; ")), log_file = normalizePath(file.path(path_log, 
                    log_file), winslash = "/", mustWork = FALSE))
            }
            return(script_coreg)
        }), deps = c("cmd_tools", "check_result", "params"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    command_coreg = targets::tar_target_raw(name = "coreg_results", 
        command = quote({
            {
                if (!isFALSE(script_coreg$error)) {
                  stop(c(script_coreg$reason$message, "Cannot run flirt commands")[[1]])
                }
                dry_run <- raveio::is_dry_run() || isTRUE(dryrun)
                if (check_result$skip_coregistration) {
                  dry_run <- TRUE
                }
                path_temp <- script_coreg$path_temp
                script_path <- file.path(raveio::dir_create2(file.path(path_temp, 
                  "scripts")), "cmd-ct2t1-flirt.sh")
                backup_dir <- raveio::dir_create2(file.path(path_temp, 
                  "scripts", "backups"))
                backup_path <- raveio::backup_file(script_path, 
                  remove = TRUE)
                if (!isFALSE(backup_path) && isTRUE(file.exists(backup_path))) {
                  file.rename(backup_path, file.path(backup_dir, 
                    basename(backup_path)))
                }
                writeLines(script_coreg$script, con = script_path)
                cmd <- sprintf("bash %s", shQuote(normalizePath(script_path, 
                  mustWork = TRUE)))
                if (!dry_run) {
                  unlink(script_coreg$log_file)
                  system(cmd, wait = TRUE)
                  raveio::backup_file(script_coreg$log_file, 
                    remove = FALSE)
                }
                ravedash::logger("Setting default CT nii file: ", 
                  params$nii_ct, level = "info")
                subject$set_default("nii_ct", params$nii_ct, 
                  namespace = "surface_reconstruction")
                coreg_results <- list(command = cmd, dry_run = dry_run, 
                  script_path = script_path)
            }
            return(coreg_results)
        }), deps = c("script_coreg", "dryrun", "check_result", 
        "params", "subject"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
