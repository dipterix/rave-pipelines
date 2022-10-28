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
    input_baseline_windows = targets::tar_target_raw("baseline_windows",
        quote({
            settings[["baseline_windows"]]
        }), deps = "settings"), input_unit_of_analysis = targets::tar_target_raw("unit_of_analysis",
        quote({
            settings[["unit_of_analysis"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes",
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), input_baseline__windows = targets::tar_target_raw("baseline__windows",
        quote({
            settings[["baseline__windows"]]
        }), deps = "settings"), input_trial_starts = targets::tar_target_raw("trial_starts",
        quote({
            settings[["trial_starts"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name",
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_trial_ends = targets::tar_target_raw("trial_ends",
        quote({
            settings[["trial_ends"]]
        }), deps = "settings"), input_analysis_electrodes__category = targets::tar_target_raw("analysis_electrodes__category",
        quote({
            settings[["analysis_electrodes__category"]]
        }), deps = "settings"), input_electrode_category_selector = targets::tar_target_raw("electrode_category_selector",
        quote({
            settings[["electrode_category_selector"]]
        }), deps = "settings"), input_analysis_ranges = targets::tar_target_raw("analysis_ranges",
        quote({
            settings[["analysis_ranges"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice",
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name",
        quote({
            settings[["epoch_name"]]
        }), deps = "settings"), input_baseline__global_baseline_choice = targets::tar_target_raw("baseline__global_baseline_choice",
        quote({
            settings[["baseline__global_baseline_choice"]]
        }), deps = "settings"), input_baseline__unit_of_analysis = targets::tar_target_raw("baseline__unit_of_analysis",
        quote({
            settings[["baseline__unit_of_analysis"]]
        }), deps = "settings"), input_global_baseline_choice = targets::tar_target_raw("global_baseline_choice",
        quote({
            settings[["global_baseline_choice"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes",
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_analysis_ranges__lock = targets::tar_target_raw("analysis_ranges__lock",
        quote({
            settings[["analysis_ranges__lock"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups",
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_analysis_lock = targets::tar_target_raw("analysis_lock",
        quote({
            settings[["analysis_lock"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends",
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts",
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), check_load_power = targets::tar_target_raw(name = "repository",
        command = quote({
            {
                subject_id <- sprintf("%s/%s", project_name,
                  subject_code)
                repository <- raveio::prepare_subject_power(subject = subject_id,
                  electrodes = loaded_electrodes, epoch_name = epoch_choice,
                  reference_name = reference_name, time_windows = c(epoch_choice__trial_starts,
                    epoch_choice__trial_ends))
                repository
            }
            return(repository)
        }), deps = c("project_name", "subject_code", "loaded_electrodes",
        "epoch_choice", "reference_name", "epoch_choice__trial_starts",
        "epoch_choice__trial_ends"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), check_requested_electrodes = targets::tar_target_raw(name = "requested_electrodes",
        command = quote({
            {
                requested_electrodes <- dipsaus::parse_svec(analysis_electrodes,
                  sep = ",|;", connect = ":-")
                requested_electrodes <- requested_electrodes[requested_electrodes %in%
                  repository$power$LFP$electrodes]
                if (!length(requested_electrodes)) {
                  stop("No electrode selected")
                }
            }
            return(requested_electrodes)
        }), deps = c("analysis_electrodes", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), check_analysis_ranges = targets::tar_target_raw(name = "analysis_ranges_index",
        command = quote({
            {
                library(rutabaga)
                analysis_ranges_index <- lapply(analysis_ranges,
                  function(x) {
                    x$frequency <- unlist(x$frequency)
                    x$time <- unlist(x$time)
                    if (length(x$frequency) == 2) {
                      if (!any(repository$frequency %within%
                        x$frequency)) {
                        stop(sprintf("No frequencies available within specified range: %.1f ~ %.1f",
                          x$frequency[[1]], x$frequency[[2]]))
                      }
                    } else {
                      return(NULL)
                    }
                    if (length(x$time) == 2) {
                      if (!any(repository$time_points %within%
                        x$time)) {
                        stop(sprintf("No time-points available within specified range: %.2f ~ %.2f",
                          x$time[1], x$time[2]))
                      }
                    } else {
                      return(NULL)
                    }
                    list(Frequency = which(repository$frequency %within%
                      x$frequency), Time = which(repository$time_points %within%
                      x$time))
                  })
            }
            return(analysis_ranges_index)
        }), deps = c("analysis_ranges", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), check_condition_groups = targets::tar_target_raw(name = "cond_groups",
        command = quote({
            {
                cond_groups <- lapply(seq_along(condition_groups),
                  function(idx) {
                    g <- condition_groups[[idx]]
                    g_cond <- sort(unlist(g$group_conditions))
                    Trial_num <- repository$epoch$table$Trial[repository$epoch$table$Condition %in%
                      g_cond]
                    list(name = g$group_name, Trial_num = Trial_num,
                      group_index = idx, has_trials = length(Trial_num) >
                        0, conditions = g_cond)
                  })
                has_trials <- vapply(cond_groups, "[[", FALSE,
                  "has_trials")
                if (!has_trials) {
                  stop("No trial condition selected")
                }
            }
            return(cond_groups)
        }), deps = c("condition_groups", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), calculate_baseline = targets::tar_target_raw(name = "bl_power",
        command = quote({
            {
                baseline_along_choices <- c("Per frequency, trial, and electrode",
                  "Across electrode", "Across trial", "Across trial and electrode")
                baseline_choice <- which(baseline_along_choices %in%
                  baseline__global_baseline_choice)
                units <- list(c("Trial", "Frequency", "Electrode"),
                  c("Trial", "Frequency"), c("Frequency", "Electrode"),
                  c("Frequency"))[[baseline_choice]]
                method <- list(`% Change Power` = "percentage",
                  `% Change Amplitude` = "sqrt_percentage", `z-score Power` = "zscore",
                  `z-score Amplitude` = "sqrt_zscore", Decibel = "decibel")[[baseline__unit_of_analysis]]
                old_file <- repository$baselined$LFP$.filebase
                raveio::power_baseline(x = repository, baseline_windows = unlist(baseline__windows),
                  method = method, units = units, signal_types = "LFP",
                  electrodes = requested_electrodes)
                new_file <- repository$baselined$LFP$.filebase
                if (length(old_file) == 1 && !identical(old_file,
                  new_file) && dir.exists(old_file)) {
                  unlink(old_file)
                }
                bl_power <- repository$baselined$LFP
            }
            return(bl_power)
        }), deps = c("baseline__global_baseline_choice", "baseline__unit_of_analysis",
        "repository", "baseline__windows", "requested_electrodes"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), collapse_data = targets::tar_target_raw(name = "collapsed_data",
        command = quote({
            {
                collapsed_data <- lapply(cond_groups, function(g) {
                  if (!g$has_trials) {
                    return(g)
                  }
                  analysis_idx <- structure(lapply(analysis_ranges_index,
                    function(x) {
                      x$Trial <- g$Trial_num
                      x
                    }), names = sprintf("range_%d", seq_along(analysis_ranges_index)))
                  g$collasped <- raveio::collapse_power(bl_power,
                    analysis_idx)
                  g
                })
            }
            return(collapsed_data)
        }), deps = c("cond_groups", "analysis_ranges_index",
        "bl_power"), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"))
