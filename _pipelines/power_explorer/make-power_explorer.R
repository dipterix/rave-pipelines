library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path"), input_reference_name = targets::tar_target_raw("reference_name", 
    quote({
        settings[["reference_name"]]
    }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
    quote({
        settings[["subject_code"]]
    }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
    quote({
        settings[["project_name"]]
    }), deps = "settings"), input_trial_starts = targets::tar_target_raw("trial_starts", 
    quote({
        settings[["trial_starts"]]
    }), deps = "settings"), input_trial_ends = targets::tar_target_raw("trial_ends", 
    quote({
        settings[["trial_ends"]]
    }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name", 
    quote({
        settings[["epoch_name"]]
    }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
    quote({
        settings[["loaded_electrodes"]]
    }), deps = "settings"), create_subject_instance = targets::tar_target_raw(name = "subject", 
    command = quote({
        {
            library(raveio)
            subject_id <- sprintf("%s/%s", project_name, subject_code)
            subject <- as_rave_subject(subject_id, strict = TRUE)
            subject
        }
        return(subject)
    }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough")), 
    check_epoch = targets::tar_target_raw(name = "epoch_table", 
        command = quote({
            {
                if (length(epoch_name) != 1) {
                  stop("Only one epoch is allowed at a time.")
                }
                if (!isTRUE(epoch_name %in% subject$epoch_names)) {
                  stop("Subject ", subject_id, " has no epoch name called: ", 
                    sQuote(epoch_name), "\n  Please check folder\n    ", 
                    subject$meta_path, "\n  and make sure ", 
                    sQuote(sprintf("epoch_%s.csv", epoch_name)), 
                    " exists.")
                }
                epoch_table <- subject$meta_data(meta_type = "epoch", 
                  meta_name = epoch_name)
                if (!is.data.frame(epoch_table)) {
                  stop("Cannot load epoch file correctly. A typical RAVE-epoch file contains 4 columns (case-sensitive): Block (characters), Time (numerical), Trial (integer), Condition (characters).")
                }
                invalid_epochs <- epoch_table$Time + trial_starts < 
                  0
                if (any(invalid_epochs)) {
                  stop("Trial ", dipsaus::deparse_svec(epoch_table$Trial[invalid_epochs]), 
                    " start too soon after the beginning of the sessions (less than ", 
                    sprintf("%.2f seconds", -trial_starts), "). Please adjust the trial start time (i.e. ", 
                    sQuote("Pre"), " if you are using the RAVE application).")
                }
                head(epoch_table)
            }
            return(epoch_table)
        }), deps = c("epoch_name", "subject", "trial_starts"), 
        cue = targets::tar_cue("thorough")), check_reference = targets::tar_target_raw(name = "reference_table", 
        command = quote({
            {
                if (length(reference_name) != 1) {
                  stop("Only one reference is allowed at a time.")
                }
                if (!isTRUE(reference_name %in% subject$reference_names)) {
                  stop("Subject ", subject_id, " has no reference name called: ", 
                    sQuote(reference_name), "\n  Please check folder\n    ", 
                    subject$meta_path, "\n  and make sure ", 
                    sQuote(sprintf("reference_%s.csv", reference_name)), 
                    " exists.")
                }
                reference_table <- subject$meta_data(meta_type = "reference", 
                  meta_name = reference_name)
                if (!is.data.frame(reference_table)) {
                  stop("Cannot load reference file correctly. A typical RAVE-reference file contains 4 columns (case-sensitive): Electrode (integer), Group (characters), Reference (characters), Type (characters).")
                }
                head(reference_table)
            }
            return(reference_table)
        }), deps = c("reference_name", "subject"), cue = targets::tar_cue("thorough")), 
    check_electrodes = targets::tar_target_raw(name = "electrode_table", 
        command = quote({
            {
                load_electrodes <- dipsaus::parse_svec(loaded_electrodes)
                valid_electrodes <- subject$valid_electrodes(reference_name = reference_name)
                load_electrodes <- load_electrodes[load_electrodes %in% 
                  valid_electrodes]
                if (!length(load_electrodes)) {
                  stop("There is no valid electrodes to be loaded. The valid electrodes are: ", 
                    dipsaus::deparse_svec(valid_electrodes), 
                    ".")
                }
                preproc <- subject$preprocess_settings
                all_electrodes <- subject$electrodes
                sel <- all_electrodes %in% load_electrodes
                if (!all(preproc$has_wavelet[sel])) {
                  imcomplete <- all_electrodes[all_electrodes %in% 
                    load_electrodes & !preproc$has_wavelet]
                  stop("The following electrodes do not have power spectrum: \n  ", 
                    dipsaus::deparse_svec(imcomplete), "\nPlease run wavelet module first.")
                }
                electrode_table <- subject$meta_data("electrodes")
                if (!is.data.frame(electrode_table)) {
                  stop("Cannot load electrode.csv correctly. A basic RAVE-electrode file contains 5 columns (case-sensitive): Electrode (integer), Coord_x (numerical), Coord_y (numerical), Coord_y (numerical), Label (characters).")
                }
                electrode_table <- merge(electrode_table, reference_table, 
                  by = "Electrode", all.x = TRUE, all.y = FALSE)
                electrode_table$isLoaded <- electrode_table$Electrode %in% 
                  load_electrodes
            }
            return(electrode_table)
        }), deps = c("loaded_electrodes", "subject", "reference_name", 
        "reference_table"), cue = targets::tar_cue("thorough")), 
    check_frequencies = targets::tar_target_raw(name = "frequency_table", 
        command = quote({
            {
                frequency_table <- subject$meta_data("frequencies")
            }
            return(frequency_table)
        }), deps = "subject", cue = targets::tar_cue("thorough")), 
    load_epoch = targets::tar_target_raw(name = "epoch", command = quote({
        {
            epoch <- raveio::RAVEEpoch$new(subject = subject, 
                name = epoch_name)
            epoch
        }
        return(epoch)
    }), deps = c("subject", "epoch_name"), cue = targets::tar_cue("thorough")), 
    load_electrode_power = targets::tar_target_raw(name = "power_list", 
        command = quote({
            {
                electrode_list <- unique(electrode_table$Electrode[electrode_table$isLoaded])
                ref_names <- unique(reference_table$Reference[reference_table$Electrode %in% 
                  electrode_list])
                dipsaus::make_forked_clusters(workers = raveio::raveio_getopt("max_worker"), 
                  clean = TRUE)
                dipsaus::lapply_async2(ref_names, function(ref_name) {
                  ref <- raveio::LFP_electrode$new(subject = subject, 
                    ref_name, is_reference = TRUE)
                  ref$set_epoch(epoch)
                  ref$trial_intervals <- list(c(trial_starts, 
                    trial_ends))
                  ref$load_data(type = "power")
                  NULL
                }, callback = function(ref_name) {
                  msg <- sprintf("Loading reference | %s", ref_name)
                  msg
                }, plan = FALSE)
                power_list <- dipsaus::lapply_async2(electrode_list, 
                  function(e) {
                    ref_name <- reference_table$Reference[reference_table$Electrode == 
                      e]
                    el <- raveio::LFP_electrode$new(subject = subject, 
                      e, is_reference = FALSE)
                    ref <- raveio::LFP_electrode$new(subject = subject, 
                      ref_name, is_reference = TRUE)
                    el$set_reference(ref)
                    el$set_epoch(epoch)
                    el$trial_intervals <- list(c(trial_starts, 
                      trial_ends))
                    el$load_data(type = "power")
                  }, .callback = function(el) {
                    msg <- sprintf("Loading electrode | %s", 
                      el)
                    msg
                  }, plan = FALSE)
            }
            return(power_list)
        }), deps = c("electrode_table", "reference_table", "subject", 
        "epoch", "trial_starts", "trial_ends"), cue = targets::tar_cue("always")), 
    load_dimension_names = targets::tar_target_raw(name = "power_dimnames", 
        command = quote({
            {
                power_dimnames <- dimnames(power_list[[1]])
                power_dimnames$Electrode <- electrode_table$Electrode[electrode_table$isLoaded]
            }
            return(power_dimnames)
        }), deps = c("power_list", "electrode_table"), cue = targets::tar_cue("thorough")))
