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
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_imported_electrodes = targets::tar_target_raw(name = "imported_electrodes", 
        command = quote({
            {
                imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
                if (!length(imported_electrodes)) {
                  stop("The subject exists but its signal has not been imported yet.")
                }
            }
            return(imported_electrodes)
        }), deps = "subject", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
