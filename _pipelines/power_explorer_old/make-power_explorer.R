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
    }), deps = "settings"), check_load_power = targets::tar_target_raw(name = "repository", 
    command = quote({
        {
            library(raveio)
            subject_id <- sprintf("%s/%s", project_name, subject_code)
            repository <- prepare_subject_power(subject = subject_id, 
                electrodes = loaded_electrodes, epoch_name = epoch_name, 
                reference_name = reference_name, time_windows = c(trial_starts, 
                  trial_ends))
            repository
        }
        return(repository)
    }), deps = c("project_name", "subject_code", "loaded_electrodes", 
    "epoch_name", "reference_name", "trial_starts", "trial_ends"
    ), cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"))
