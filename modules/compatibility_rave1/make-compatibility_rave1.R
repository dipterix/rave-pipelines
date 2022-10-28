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
    input_epoch_choice = targets::tar_target_raw("epoch_choice",
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes",
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name",
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_validation = targets::tar_target_raw("validation",
        quote({
            settings[["validation"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends",
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts",
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), load_subject_instance = targets::tar_target_raw(name = "subject",
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name,
                  subject_code = subject_code, strict = FALSE)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), validate_subject = targets::tar_target_raw(name = "validate",
        command = quote({
            {
                validate <- raveio::validate_subject(subject = subject,
                  method = validation$mode, version = validation$version)
            }
            return(validate)
        }), deps = c("subject", "validation"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"))
