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
    }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), check_load_epoch = targets::tar_target_raw(name = "epoch", 
    command = quote({
        {
            epoch <- subject$get_epoch(epoch_name = epoch_name, 
                trial_starts = trial_starts)
        }
        return(epoch)
    }), deps = c("subject", "epoch_name", "trial_starts"), cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), check_load_reference = targets::tar_target_raw(name = "reference_table", 
    command = quote({
        {
            reference_table <- subject$get_reference(reference_name = reference_name)
        }
        return(reference_table)
    }), deps = c("subject", "reference_name"), cue = targets::tar_cue("thorough"), 
    pattern = NULL, iteration = "list"), check_load_electrodes = targets::tar_target_raw(name = "electrode_table", 
    command = quote({
        {
            electrode_table <- subject$get_electrode_table(electrodes = loaded_electrodes, 
                reference_name = reference_name)
        }
        return(electrode_table)
    }), deps = c("subject", "loaded_electrodes", "reference_name"
    ), cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    load_frequencies = targets::tar_target_raw(name = "frequency_table", 
        command = quote({
            {
                frequency_table <- subject$get_frequency(simplify = FALSE)
            }
            return(frequency_table)
        }), deps = "subject", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_electrode_list = targets::tar_target_raw(name = "electrode_list", 
        command = quote({
            {
                loading <- subset(electrode_table, subset = electrode_table$isLoaded)
                electrode_list <- unique(loading$Electrode)
            }
            return(electrode_list)
        }), deps = "electrode_table", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_references_list = targets::tar_target_raw(name = "references_list", 
        command = quote({
            {
                ref_table <- subset(reference_table, Electrode %in% 
                  electrode_list)
                references_list <- unique(ref_table$Reference)
            }
            return(references_list)
        }), deps = c("reference_table", "electrode_list"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_references = targets::tar_target_raw(name = "reference_data", 
        command = quote({
            {
                ref_name <- references_list[[1]]
                ref <- raveio::LFP_electrode$new(subject = subject, 
                  ref_name, is_reference = TRUE)
                ref$set_epoch(epoch)
                ref$trial_intervals <- list(c(trial_starts, trial_ends))
                reference_data <- ref$load_data(type = "power")
            }
            return(reference_data)
        }), deps = c("references_list", "subject", "epoch", "trial_starts", 
        "trial_ends"), cue = targets::tar_cue("always"), pattern = expression(
            map(references_list)), iteration = "list"), load_electrode_power = targets::tar_target_raw(name = "power_list", 
        command = quote({
            {
                force(reference_data)
                e <- electrode_list[[1]]
                ref_name <- reference_table$Reference[reference_table$Electrode == 
                  e]
                el <- raveio::LFP_electrode$new(subject = subject, 
                  e, is_reference = FALSE)
                ref <- raveio::LFP_electrode$new(subject = subject, 
                  ref_name, is_reference = TRUE)
                el$set_reference(ref)
                el$set_epoch(epoch)
                el$trial_intervals <- list(c(trial_starts, trial_ends))
                power_list <- el$load_data(type = "power")
            }
            return(power_list)
        }), deps = c("reference_data", "electrode_list", "reference_table", 
        "subject", "epoch", "trial_starts", "trial_ends"), cue = targets::tar_cue("always"), 
        pattern = expression(map(electrode_list)), iteration = "list"), 
    load_dimension_names = targets::tar_target_raw(name = "power_dimnames", 
        command = quote({
            {
                if (is.list(power_list)) {
                  tmp <- power_list[[1]]
                } else {
                  tmp <- power_list
                }
                power_dimnames <- dimnames(tmp)
                power_dimnames$Electrode <- electrode_table$Electrode[electrode_table$isLoaded]
            }
            return(power_dimnames)
        }), deps = c("power_list", "electrode_table"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
