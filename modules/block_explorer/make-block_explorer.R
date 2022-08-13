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
    input_baseline_unit = targets::tar_target_raw("baseline_unit", 
        quote({
            settings[["baseline_unit"]]
        }), deps = "settings"), input_requested_electrode = targets::tar_target_raw("requested_electrode", 
        quote({
            settings[["requested_electrode"]]
        }), deps = "settings"), input_load_electrodes = targets::tar_target_raw("load_electrodes", 
        quote({
            settings[["load_electrodes"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_block = targets::tar_target_raw("block", 
        quote({
            settings[["block"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                subject
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), parse_requested_electrode = targets::tar_target_raw(name = "sample_electrode", 
        command = quote({
            {
                sample_electrode <- dipsaus::parse_svec(requested_electrode)
                if (length(sample_electrode) != 1) {
                  stop("Sample electrode number must have length of one")
                }
            }
            return(sample_electrode)
        }), deps = "requested_electrode", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), calculate_electrodes_to_load = targets::tar_target_raw(name = "electrodes_to_load", 
        command = quote({
            {
                electrodes_to_load <- sort(c(dipsaus::parse_svec(load_electrodes), 
                  sample_electrode))
            }
            return(electrodes_to_load)
        }), deps = c("load_electrodes", "sample_electrode"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    load_repository = targets::tar_target_raw(name = "repository", 
        command = quote({
            {
                repository <- with_future_parallel({
                  prepare_subject_with_blocks(subject = subject, 
                    electrodes = load_electrodes, reference_name = reference_name, 
                    blocks = block)
                })
            }
            return(repository)
        }), deps = c("subject", "load_electrodes", "reference_name", 
        "block"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), extract_loaded_electrodes = targets::tar_target_raw(name = "electrodes_loaded", 
        command = quote({
            {
                electrodes_loaded <- repository$block_data[[block]]$voltage$data$get_header("cached_electrodes")
                electrodes_loaded <- sort(electrodes_loaded)
                if (!isTRUE(sample_electrode %in% electrodes_loaded)) {
                  stop(sprintf("Sample electrode [%s] has not been loaded successfully", 
                    paste(sample_electrode, collapse = ", ")))
                }
            }
            return(electrodes_loaded)
        }), deps = c("repository", "block", "sample_electrode"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), single_out_voltage_data = targets::tar_target_raw(name = "voltage", 
        command = quote({
            {
                voltage <- repository$block_data[[block]]$voltage
            }
            return(voltage)
        }), deps = c("repository", "block"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), single_out_wavelet_data = targets::tar_target_raw(name = "wavelet", 
        command = quote({
            {
                wavelet <- repository$block_data[[block]]$wavelet
            }
            return(wavelet)
        }), deps = c("repository", "block"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_sample_for_voltage_analysis = targets::tar_target_raw(name = "voltage_sample", 
        command = quote({
            {
                sample_rate <- voltage$sample_rate
                voltage_single <- subset(voltage$data, Electrode ~ 
                  Electrode == sample_electrode, drop = TRUE)
                n_timepoints <- length(voltage_single)
                if (n_timepoints > 10000) {
                  decimate_rate <- ceiling(n_timepoints/10000)
                  voltage_single_decimated <- ravetools::decimate(x = voltage_single, 
                    q = decimate_rate)
                  time <- ravetools::decimate(voltage$dnames$Time, 
                    q = decimate_rate)
                } else {
                  decimate_rate <- 1L
                  voltage_single_decimated <- voltage_single
                  time <- voltage$dnames$Time
                }
                voltage_sample <- list(origin = list(sample_rate = sample_rate, 
                  time = voltage$dnames$Time, data = voltage_single), 
                  decimated = list(decimate_factor = decimate_rate, 
                    sample_rate = sample_rate/decimate_rate, 
                    time = time, data = voltage_single_decimated))
            }
            return(voltage_sample)
        }), deps = c("voltage", "sample_electrode"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), `calculate_Welch-Periodogram` = targets::tar_target_raw(name = "pwelch_overall", 
        command = quote({
            {
                data <- voltage_sample$origin$data
                srate <- voltage_sample$origin$sample_rate
                pwelch_overall <- ravetools::pwelch(x = data, 
                  fs = srate, plot = FALSE, window = srate * 
                    2, noverlap = srate)
            }
            return(pwelch_overall)
        }), deps = "voltage_sample", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), `prepare_sample_for_time-frequency_analysis` = targets::tar_target_raw(name = "wavelet_sample", 
        command = quote({
            {
                sample_rate <- wavelet$sample_rate
                wavelet_single <- subset(wavelet$data, Electrode ~ 
                  Electrode == sample_electrode, drop = TRUE)
                n_timepoints <- ncol(wavelet_single)
                n_freq <- nrow(wavelet_single)
                if (n_freq * n_timepoints > 50000 && n_timepoints >= 
                  16) {
                  decimate_factor <- ceiling(n_freq * n_timepoints/50000)
                  tidx <- seq(1, n_timepoints, by = decimate_factor)
                } else {
                  decimate_factor <- 1
                  tidx <- seq_len(n_timepoints)
                }
                wavelet_decimated <- wavelet_single[, tidx, drop = FALSE]
                wavelet_sample <- list(origin = list(sample_rate = sample_rate, 
                  dnames = wavelet$dnames[c(1, 2)], data = wavelet_single), 
                  decimated = list(decimate_factor = decimate_factor, 
                    sample_rate = sample_rate/decimate_factor, 
                    dnames = list(Frequency = wavelet$dnames$Frequency, 
                      Time = wavelet$dnames$Time[tidx]), data = wavelet_decimated))
            }
            return(wavelet_sample)
        }), deps = c("wavelet", "sample_electrode"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), baseline_power = targets::tar_target_raw(name = "power_sample", 
        command = quote({
            {
                power_sample <- wavelet_sample
                switch(baseline_method, `Welch-Periodogram` = {
                  apf <- approxfun(x = pwelch_overall$freq, y = pwelch_overall$spec)
                  bl <- apf(wavelet_sample$origin$dnames$Frequency)
                }, `Block-Average` = {
                  bl <- rowMeans(Mod(wavelet_sample$origin$data)^2)
                }, `Block-Median` = {
                  bl <- apply(Mod(wavelet_sample$origin$data)^2, 
                    1, median)
                }, {
                  bl <- 1
                })
                power_sample$origin$data <- Mod(wavelet_sample$origin$data)^2/bl
                power_sample$decimated$data <- Mod(wavelet_sample$decimated$data)^2/bl
                switch(baseline_unit, Decibel = {
                  power_sample$origin$data <- 10 * log10(power_sample$origin$data)
                  power_sample$decimated$data <- 10 * log10(power_sample$decimated$data)
                }, `Percentage-Change` = {
                  power_sample$origin$data <- 100 * power_sample$origin$data - 
                    100
                  power_sample$decimated$data <- 100 * power_sample$decimated$data - 
                    100
                }, {
                  stop("Unsupported baseline unit")
                })
            }
            return(power_sample)
        }), deps = c("wavelet_sample", "baseline_method", "pwelch_overall", 
        "baseline_unit"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
