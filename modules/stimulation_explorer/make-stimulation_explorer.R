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
    input_analysis_event = targets::tar_target_raw("analysis_event",
        quote({
            settings[["analysis_event"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes",
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice",
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes",
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_downsample = targets::tar_target_raw("downsample",
        quote({
            settings[["downsample"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name",
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_baseline_settings = targets::tar_target_raw("baseline_settings",
        quote({
            settings[["baseline_settings"]]
        }), deps = "settings"), input_interpolate = targets::tar_target_raw("interpolate",
        quote({
            settings[["interpolate"]]
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups",
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends",
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts",
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject",
        command = quote({
            {
                library(raveio)
                subject <- RAVESubject$new(project_name = project_name,
                  subject_code = subject_code)
                subject
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), load_repository = targets::tar_target_raw(name = "repository",
        command = quote({
            {
                trial_interval <- sort(c(epoch_choice__trial_starts,
                  epoch_choice__trial_ends))
                repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject,
                  electrodes = loaded_electrodes, epoch = epoch_choice,
                  reference_name = reference_name, time_windows = trial_interval,
                  quiet = TRUE)
                repository
            }
            return(repository)
        }), deps = c("epoch_choice__trial_starts", "epoch_choice__trial_ends",
        "subject", "loaded_electrodes", "epoch_choice", "reference_name"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), get_analysis_electrodes = targets::tar_target_raw(name = "analysis_electrodes2",
        command = quote({
            {
                analysis_electrodes2 <- dipsaus::parse_svec(analysis_electrodes)
                analysis_electrodes2 <- analysis_electrodes2[analysis_electrodes2 %in%
                  repository$electrode_list]
                if (!length(analysis_electrodes2)) {
                  stop("No valid analysis electrodes set.")
                }
                dipsaus::deparse_svec(analysis_electrodes2)
            }
            return(analysis_electrodes2)
        }), deps = c("analysis_electrodes", "repository"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), get_baseline_settings = targets::tar_target_raw(name = "baseline_settings2",
        command = quote({
            {
                if (is.list(baseline_settings)) {
                  baseline_method <- baseline_settings$method
                  baseline_source <- baseline_settings$source
                  baseline_global <- isTRUE(baseline_settings$global)
                  baseline_window <- unlist(baseline_settings$window)
                  if (length(baseline_method) == 1 && is.character(baseline_method)) {
                    baseline_method <- match.arg(tolower(baseline_method),
                      c("percentage", "zscore", "subtract_mean"))
                    if (length(baseline_source) && nzchar(trimws(baseline_source))) {
                      baseline_from_block <- TRUE
                      baseline_source <- unique(unlist(baseline_source))
                      missing_blocks <- baseline_source[!baseline_source %in%
                        subject$blocks]
                      if (length(missing_blocks)) {
                        stop(sprintf("Baseline from session block: the block [%s] does not exist or have not been imported.",
                          paste(missing_blocks, collapse = ",")))
                      }
                      baseline_settings2 <- list(has_baseline = TRUE,
                        method = baseline_method, global = baseline_global,
                        source = baseline_source, per_trial = FALSE)
                    } else {
                      if (baseline_global) {
                        stop("Per-trial baseline does not support global baseline.")
                      }
                      baseline_window <- raveio::validate_time_window(baseline_window)
                      baseline_settings2 <- list(has_baseline = TRUE,
                        method = baseline_method, global = FALSE,
                        window = baseline_window, per_trial = TRUE)
                    }
                  }
                } else {
                  nelec <- length(repository$electrode_list)
                  baseline_settings2 <- list(has_baseline = FALSE)
                }
            }
            return(baseline_settings2)
        }), deps = c("baseline_settings", "subject", "repository"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), get_baseline_data = targets::tar_target_raw(name = "baseline_data",
        command = quote({
            {
                if (!baseline_settings2$has_baseline) {
                  raveio::voltage_baseline(x = repository, method = baseline_settings2$method,
                    units = c("Trial", "Electrode"), baseline_mean = rep(0,
                      length(repository$electrode_list)), baseline_sd = rep(1,
                      length(repository$electrode_list)))
                } else {
                  if (!baseline_settings2$per_trial) {
                    .NotYetImplemented()
                  } else {
                    raveio::voltage_baseline(x = repository,
                      baseline_windows = baseline_settings2$window,
                      method = baseline_settings2$method, units = c("Trial",
                        "Electrode"), electrodes = analysis_electrodes2)
                  }
                }
                baseline_data <- repository$voltage$baselined
                baseline_data$.mode <- "readwrite"
                baseline_data$set_header("sample_rate", repository$sample_rate)
                baseline_data$.mode <- "readonly"
            }
            return(baseline_data)
        }), deps = c("baseline_settings2", "repository", "analysis_electrodes2"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), get_interpolate_settings = targets::tar_target_raw(name = "interpolate_settings",
        command = quote({
            {
                interpolate_settings <- FALSE
                if (is.list(interpolate)) {
                  method <- match.arg(interpolate$method, c("linear",
                    "none", "B-spline"))
                  if (method != "none") {
                    cname1 <- sprintf("Event_%s", interpolate$from)
                    cname2 <- sprintf("Event_%s", interpolate$to)
                    epoch_columns <- tolower(c("event_onset",
                      names(repository$epoch_table)))
                    if (isTRUE(tolower(cname1) %in% epoch_columns) &&
                      isTRUE(tolower(cname2) %in% epoch_columns) &&
                      !identical(tolower(cname1), tolower(cname2))) {
                      col1 <- which(epoch_columns == tolower(cname1)) -
                        1
                      col2 <- which(epoch_columns == tolower(cname2)) -
                        1
                      if (col1 == 0) {
                        interp_starts <- repository$epoch_table$Time
                      } else {
                        interp_starts <- repository$epoch_table[[col1]]
                      }
                      if (col2 == 0) {
                        interp_ends <- repository$epoch_table$Time
                      } else {
                        interp_ends <- repository$epoch_table[[col2]]
                      }
                      interpolate_settings <- list(method = method,
                        starts = interp_starts - repository$epoch_table$Time,
                        ends = interp_ends - repository$epoch_table$Time,
                        sample_rate = repository$sample_rate)
                    } else {
                      warning(sprintf("No interpolation will be done: interpolation start [%s] and to [%s] must be length of one, non-identical, and must exist in epoch table.",
                        paste(cname1, collapse = ","), paste(cname2,
                          collapse = ",")))
                    }
                  }
                }
            }
            return(interpolate_settings)
        }), deps = c("interpolate", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), interpolate_signals = targets::tar_target_raw(name = "interpolated_data",
        command = quote({
            {
                interpolated_data <- baseline_data
                dnames <- dimnames(baseline_data)
                electrodes <- as.integer(dnames$Electrode)
                time <- as.double(dnames$Time)
                fbase <- baseline_data$.filebase
                time_windows <- unlist(repository$time_windows)
                if (is.list(interpolate_settings)) {
                  fbase2 <- file.path(fbase, "interpolated")
                  interpolated_data <- filearray::filearray_load_or_create(filebase = fbase2,
                    dimension = dim(baseline_data), type = "float",
                    partition_size = 1L, initialize = FALSE,
                    symlink_ok = FALSE, verbose = FALSE, mode = "readwrite",
                    interpolate_settings = interpolate_settings,
                    on_missing = function(arr) {
                      dimnames(arr) <- dnames
                    })
                  interpolated_electrodes <- interpolated_data$get_header("interpolated_electrodes",
                    NULL)
                  todos <- analysis_electrodes2[!analysis_electrodes2 %in%
                    interpolated_electrodes & analysis_electrodes2 %in%
                    electrodes]
                  stim_onset <- floor((interpolate_settings$starts -
                    time_windows[[1]]) * interpolate_settings$sample_rate) +
                    1
                  stim_offset <- ceiling((interpolate_settings$ends -
                    time_windows[[1]]) * interpolate_settings$sample_rate) +
                    1
                  if (length(todos)) {
                    switch(interpolate_settings$method, linear = {
                      dipsaus::lapply_async2(todos, function(e) {
                        sel <- which(electrodes %in% e)[[1]]
                        slice <- baseline_data[, , sel, drop = TRUE,
                          dimnames = FALSE]
                        tmp <- sapply(seq_along(stim_offset),
                          function(ii) {
                            s <- slice[, ii]
                            onset_idx <- stim_onset[[ii]]
                            offset_idx <- stim_offset[[ii]]
                            if (onset_idx < 1) {
                              onset_idx <- 1
                            }
                            if (onset_idx > length(s)) {
                              onset_idx <- length(s)
                            }
                            if (offset_idx > length(s)) {
                              offset_idx <- length(s)
                            }
                            if (offset_idx - onset_idx > 1) {
                              a <- s[onset_idx]
                              b <- s[offset_idx]
                              idx <- seq.int(0, offset_idx -
                                onset_idx)
                              s[idx + onset_idx] <- (b - a)/(offset_idx -
                                onset_idx) * idx + a
                            }
                            s
                          })
                        interpolated_data[, , sel] <- tmp
                        return()
                      }, plan = FALSE)
                    }, {
                      stop("Interpolate method hasn't been implemented yet: ",
                        interpolate_settings$method)
                    })
                    interpolated_electrodes <- sort(c(interpolated_electrodes,
                      todos))
                    interpolated_data$set_header("interpolated_electrodes",
                      interpolated_electrodes)
                  }
                  interpolated_data$.mode <- "readonly"
                }
            }
            return(interpolated_data)
        }), deps = c("baseline_data", "repository", "interpolate_settings",
        "analysis_electrodes2"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), shift_and_downsample_data = targets::tar_target_raw(name = "shifted_downsampled_data",
        command = quote({
            {
                shifted_downsampled_data <- interpolated_data
                analysis_event2 <- NULL
                if (length(analysis_event) == 1 && tolower(analysis_event) !=
                  "onset") {
                  sel <- names(repository$epoch_table) == sprintf("Event_%s",
                    analysis_event)
                  if (any(sel)) {
                    analysis_event2 <- names(repository$epoch_table)[sel][[1]]
                  }
                }
                downsample <- as.integer(downsample)
                if (isTRUE(downsample > 1) || length(analysis_event2)) {
                  fbase <- file.path(interpolated_data$.filebase,
                    "shifted_downsampled")
                  dnames <- dimnames(interpolated_data)
                  electrodes <- as.integer(dnames$Electrode)
                  time <- as.double(dnames$Time)
                  dim0 <- dim(interpolated_data)
                  decimated_time <- as.double(time[seq(1, length(time),
                    by = downsample)])
                  dnames$Time <- decimated_time
                  dim <- dim0
                  dim[[1]] <- length(decimated_time)
                  shifted_downsampled_data <- filearray::filearray_load_or_create(filebase = fbase,
                    dimension = dim, type = "float", symlink_ok = FALSE,
                    mode = "readwrite", initialize = FALSE, partition_size = 1L,
                    verbose = FALSE, analysis_event = analysis_event2,
                    downsample = downsample, epoch_table = repository$epoch_table,
                    on_missing = function(arr) {
                      dimnames(arr) <- dnames
                      arr$set_header("sample_rate", repository$sample_rate/downsample)
                    })
                  if (length(analysis_event2)) {
                    event_time <- repository$epoch_table[[analysis_event2]]
                    shift_amount <- round((event_time - repository$epoch_table$Time) *
                      repository$sample_rate/downsample)
                  } else {
                    shift_amount <- NULL
                  }
                  shifted_electrodes <- shifted_downsampled_data$get_header("shifted_electrodes",
                    NULL)
                  todos <- analysis_electrodes2[!analysis_electrodes2 %in%
                    shifted_electrodes & analysis_electrodes2 %in%
                    electrodes]
                  if (length(todos)) {
                    dipsaus::lapply_async2(todos, function(e) {
                      idx <- which(electrodes == e)[[1]]
                      slice <- interpolated_data[, , idx, drop = FALSE,
                        dimnames = FALSE, reshape = dim0[c(1,
                          2)]]
                      if (downsample > 1) {
                        slice <- apply(slice, 2, function(s) {
                          ravetools::decimate(s, q = downsample,
                            ftype = "fir")
                        })
                      }
                      if (length(shift_amount)) {
                        slice <- dipsaus::shift_array(slice,
                          shift_idx = 1L, shift_by = 2L, shift_amount = shift_amount)
                        slice[!is.finite(slice)] <- NA_real_
                      }
                      shifted_downsampled_data[, , idx] <- slice
                      return()
                    }, plan = FALSE, callback = function(e) {
                      sprintf("Down-sample & event alignment|Electrode %d",
                        e)
                    })
                    shifted_electrodes <- sort(c(shifted_electrodes,
                      todos))
                    shifted_downsampled_data$set_header("shifted_electrodes",
                      shifted_electrodes)
                  }
                }
            }
            return(shifted_downsampled_data)
        }), deps = c("interpolated_data", "analysis_event", "repository",
        "downsample", "analysis_electrodes2"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_condition_groups = targets::tar_target_raw(name = "condition_groups2",
        command = quote({
            {
                condition_groups2 <- dipsaus::drop_nulls(lapply(seq_along(condition_groups),
                  function(ii) {
                    g <- condition_groups[[ii]]
                    group_name <- g$group_name
                    if (length(group_name) != 1 || !nzchar(trimws(group_name))) {
                      group_name <- sprintf("Group %d", ii)
                    }
                    conditions <- sort(unique(unlist(g$group_conditions)))
                    selection <- repository$epoch_table$Condition %in%
                      conditions
                    if (!length(conditions) || !any(selection)) {
                      return(NULL)
                    }
                    list(order = ii, group_name = group_name,
                      conditions = conditions, selection = selection)
                  }))
            }
            return(condition_groups2)
        }), deps = c("condition_groups", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), plot_data_single_electrode_trial_by_time = targets::tar_target_raw(name = "plot_data_trial_by_time",
        command = quote({
            {
                dnames <- dimnames(shifted_downsampled_data)
                sample_rate <- shifted_downsampled_data$get_header("sample_rate")
                array_data <- shifted_downsampled_data[, , repository$electrode_list %in%
                  analysis_electrodes2, dimnames = FALSE, drop = FALSE]
                array_data <- ravetools::collapse(array_data,
                  keep = c(1, 2))
                data <- lapply(condition_groups2, function(g) {
                  sub <- array_data[, g$selection, drop = FALSE]
                  mse <- apply(sub, 1, dipsaus::mean_se, na.rm = TRUE,
                    se_na_as_zero = FALSE)
                  g$mean <- mse[1, ]
                  g$se <- mse[2, ]
                  rgs <- g$mean[is.finite(g$mean)]
                  if (length(rgs)) {
                    g$range <- range(rgs)
                  } else {
                    g$range <- c(-1e-06, 1e-06)
                  }
                  g
                })
                group_order <- sapply(data, "[[", "order")
                group_names <- sapply(data, "[[", "group_name")
                group_mean <- sapply(data, "[[", "mean")
                plot_data_trial_by_time <- list(time = as.double(dnames$Time),
                  sample_rate = sample_rate, group_order = group_order,
                  group_names = group_names, group_mean = group_mean,
                  group_data = data)
            }
            return(plot_data_trial_by_time)
        }), deps = c("shifted_downsampled_data", "repository",
        "analysis_electrodes2", "condition_groups2"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"))
