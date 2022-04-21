library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_kernel_table = targets::tar_target_raw("kernel_table", 
        quote({
            settings[["kernel_table"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_pre_downsample = targets::tar_target_raw("pre_downsample", 
        quote({
            settings[["pre_downsample"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_precision = targets::tar_target_raw("precision", 
        quote({
            settings[["precision"]]
        }), deps = "settings"), input_target_sample_rate = targets::tar_target_raw("target_sample_rate", 
        quote({
            settings[["target_sample_rate"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                print(subject)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), get_notch_filter_timestamp = targets::tar_target_raw(name = "notch_filtere_stamp", 
        command = quote({
            {
                notch_filtere_stamp <- subject$get_default(namespace = "notch_filter", 
                  "parameters", default_if_missing = Sys.time())
            }
            return(notch_filtere_stamp)
        }), deps = "subject", cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_prerequisite = targets::tar_target_raw(name = "notch_filtered_electrodes", 
        command = quote({
            {
                electrodes <- subject$electrodes
                etypes <- subject$electrode_types
                notch_filtered_electrodes <- electrodes[subject$preprocess_settings$notch_filtered && 
                  etypes %in% c("LFP", "EKG", "Audio")]
                if (!length(notch_filtered_electrodes)) {
                  stop("There is no electrode available to the wavelet module")
                }
            }
            return(notch_filtered_electrodes)
        }), deps = "subject", cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), get_raw_sample_rates = targets::tar_target_raw(name = "sample_rates", 
        command = quote({
            {
                sample_rates <- subject$preprocess_settings$sample_rates
                electrodes <- subject$electrodes
                sample_rates <- sapply(notch_filtered_electrodes, 
                  function(e) {
                    re <- sample_rates[electrodes == e]
                    if (!length(re)) {
                      stop("Electrode ", e, " does not have sample rate. The data might not be imported correctly and some configurations are missing.")
                    }
                    re[[1]]
                  })
            }
            return(sample_rates)
        }), deps = c("subject", "notch_filtered_electrodes"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    check_kernel = targets::tar_target_raw(name = "kernels", 
        command = quote({
            {
                freqs <- unlist(kernel_table$Frequency)
                cycles <- unlist(kernel_table$Cycles)
                if (any(is.na(freqs) | freqs <= 0)) {
                  stop("The wavelet kernel table contains negative `Frequency` value(s)")
                }
                if (length(cycles) != 2) {
                  if (length(freqs) != length(cycles)) {
                    stop("The wavelet kernel table lengths are inconsistent: the lenth of `Cycles` must be either 2 (a range on which wavelet cycles that will be interpolated log-linearly), or the same length as `Frequency`.")
                  }
                }
                if (any(is.na(cycles) | cycles <= 1)) {
                  stop("The wavelet kernel table contains negative or 0 `Cycles` value(s): wavelet cycles must be greater equal than 1 (also integer numbers are strongly recommended)")
                }
                if (length(freqs) != 2 && length(cycles) == 2) {
                  if (cycles[[1]] > cycles[[2]]) {
                    stop("The wavelet kernel table: when `Cycles` is a range, (length of 2), it must be in non-decreasing order.")
                  }
                }
                if (length(precision) != 1 || !precision %in% 
                  c("float", "double")) {
                  precision <- "float"
                }
                kernels <- list(freqs = freqs, cycles = cycles, 
                  precision = precision)
            }
            return(kernels)
        }), deps = c("kernel_table", "precision"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), run_wavelet = targets::tar_target_raw(name = "wavelet_params", 
        command = quote({
            {
                force(notch_filtere_stamp)
                blocks <- subject$preprocess_settings$blocks
                electrodes <- notch_filtered_electrodes
                srates <- sample_rates
                compress_rates <- srates/target_sample_rate
                kernels_precision <- kernels$precision
                pre_decimate <- pre_downsample
                overall_progress <- dipsaus::progress2("Wavelet overall progress", 
                  max = 5, shiny_auto_close = TRUE)
                overall_progress$inc("Creating directories")
                subject$initialize_paths(include_freesurfer = FALSE)
                raveio::dir_create2(file.path(subject$data_path, 
                  "power"))
                raveio::dir_create2(file.path(subject$data_path, 
                  "phase"))
                raveio::dir_create2(file.path(subject$data_path, 
                  "voltage"))
                overall_progress$inc("Generating wavelet kernels")
                ravetools <- asNamespace("ravetools")
                generate_kernel <- ravetools[[sprintf("wavelet_kernels2_%s", 
                  kernels_precision)]]
                sample_file = file.path(subject$preprocess_path, 
                  "voltage", sprintf("electrode_%d.h5", electrodes[[1]]))
                if (!file.exists(sample_file) || !raveio::h5_valid(sample_file)) {
                  stop("Electrode file is missing (preprocess, electrode ", 
                    electrodes[[1]], ")")
                }
                sample_names <- gsub("^/", "", raveio::h5_names(sample_file))
                lapply(unique(srates), function(srate) {
                  lapply(blocks, function(block) {
                    sample_name <- sprintf("notch/%s", block)
                    if (!sample_name %in% sample_names) {
                      stop(sprintf("I can find the imported signal file for Electrode %s, but cannot find any notch-filtered signal for block %s. The data file might be corrupted.", 
                        electrodes[[1]], block))
                    }
                    sample_data <- raveio::load_h5(sample_file, 
                      name = sample_name, ram = FALSE, read_only = TRUE)
                    data_length <- ceiling(length(sample_data)/pre_downsample)
                    if (data_length <= 0) {
                      stop(sprintf("Electrode %s has zero-length signal (/notch/%s). The data file might be corrupted.", 
                        electrodes[[1]], block))
                    }
                    generate_kernel(freqs = kernels$freqs, srate = srate, 
                      wave_num = kernels$cycles, data_length = data_length)
                  })
                })
                overall_progress$inc("Removing previously generated wavelet coefficients")
                preproc <- raveio::RAVEPreprocessSettings$new(subject = subject$subject_id, 
                  read_only = TRUE)
                for (e in electrodes) {
                  preproc$data[[as.character(e)]]$has_wavelet <- FALSE
                }
                preproc$save()
                data_root <- subject$data_path
                lapply(electrodes, function(e) {
                  unlink(file.path(data_root, "power", sprintf("%d.h5", 
                    e)))
                  unlink(file.path(data_root, "phase", sprintf("%d.h5", 
                    e)))
                  unlink(file.path(data_root, "voltage", sprintf("%d.h5", 
                    e)))
                  for (block in blocks) {
                    unlink(file.path(data_root, "cache", "power", 
                      "raw", block, sprintf("%d.fst", e)))
                    unlink(file.path(data_root, "cache", "phase", 
                      "raw", block, sprintf("%d.fst", e)))
                  }
                })
                overall_progress$inc("Applying wavelet (a.k.a. the long step)")
                preprocess_dir <- subject$preprocess_path
                dipsaus::lapply_async2(seq_along(electrodes), 
                  function(ii) {
                    e <- electrodes[[ii]]
                    srate <- srates[[ii]]
                    for (block in blocks) {
                      sorig <- raveio::load_h5(file = file.path(preprocess_dir, 
                        "voltage", sprintf("electrode_%d.h5", 
                          e)), name = sprintf("/notch/%s", block), 
                        ram = TRUE)
                      if (pre_decimate > 1) {
                        s <- ravetools::decimate(sorig, pre_decimate, 
                          ftype = "fir")
                        compress_rate <- compress_rates[[ii]]/pre_decimate
                      } else {
                        s <- sorig
                        compress_rate <- compress_rates[[ii]]
                      }
                      data_length <- length(s)
                      re <- ravetools::morlet_wavelet(data = s, 
                        freqs = kernels$freqs, srate = srate, 
                        wave_num = kernels$cycles, precision = kernels_precision)
                      ind <- floor(seq(1, data_length, by = compress_rate))
                      if (kernels_precision == "float") {
                        coef <- t(re[ind, , drop = FALSE])
                        phase <- Arg(coef)
                        power <- Mod(coef)^2
                        re$.mode <- "readwrite"
                        re$delete()
                      } else {
                        coef <- t(re$real[ind, , drop = FALSE] + 
                          (0+1i) * re$imag[ind, , drop = FALSE])
                        phase <- Arg(coef)
                        power <- Mod(coef)^2
                        re$real$.mode <- "readwrite"
                        re$real$delete()
                        re$imag$.mode <- "readwrite"
                        re$imag$delete()
                      }
                      fname <- sprintf("%d.h5", e)
                      wavelet_h5chunk <- c(length(kernels$freqs), 
                        128)
                      raveio::save_h5(x = power, file = file.path(data_root, 
                        "power", fname), name = sprintf("/raw/power/%s", 
                        block), chunk = wavelet_h5chunk, replace = TRUE)
                      raveio::save_h5(x = phase, file = file.path(data_root, 
                        "phase", fname), name = sprintf("/raw/phase/%s", 
                        block), chunk = wavelet_h5chunk, replace = TRUE)
                      raveio::save_h5(x = sorig, file = file.path(data_root, 
                        "voltage", fname), name = sprintf("/raw/voltage/%s", 
                        block), chunk = 1024, replace = TRUE)
                    }
                  }, plan = FALSE, callback = function(ii) {
                    sprintf(sprintf("Applying wavelet|Electrode - %s", 
                      electrodes[[ii]]))
                  })
                overall_progress$inc("Saving configurations and update log files")
                preproc <- raveio::RAVEPreprocessSettings$new(subject = subject$subject_id, 
                  read_only = TRUE)
                for (e in electrodes) {
                  preproc$data[[as.character(e)]]$has_wavelet <- TRUE
                }
                wavelet_params <- list(channels = electrodes, 
                  electrodes = electrodes, downsample_to = target_sample_rate, 
                  target_srate = target_sample_rate, frequencies = kernels$freqs, 
                  wave_num = kernels$cycles, cycle = kernels$cycles, 
                  precision = kernels_precision, pre_downsample = pre_downsample)
                wavelet_logs <- as.list(preproc$data$wavelet_logs)
                wavelet_logs[[length(wavelet_logs) + 1]] <- wavelet_params
                preproc$data$wavelet_logs <- wavelet_logs
                wavelet_params <- wavelet_params[c("electrodes", 
                  "downsample_to", "frequencies", "cycle", "precision", 
                  "pre_downsample")]
                wavelet_params$timestamp <- strftime(Sys.time(), 
                  usetz = TRUE)
                preproc$data$wavelet_params <- wavelet_params
                preproc$save()
                subject$set_default(namespace = "wavelet_module", 
                  key = "parameters", wavelet_params)
                raveio::safe_write_csv(file = file.path(subject$meta_path, 
                  "reference_noref.csv"), row.names = FALSE, 
                  data.frame(Electrode = subject$electrodes, 
                    Group = "Default", Reference = "noref", Type = "No Reference"))
                utils::write.csv(file = file.path(subject$meta_path, 
                  "frequencies.csv"), row.names = FALSE, data.frame(Frequency = kernels$freqs, 
                  Cycle = kernels$cycles, Method = "Wavelet"))
            }
            return(wavelet_params)
        }), deps = c("notch_filtere_stamp", "subject", "notch_filtered_electrodes", 
        "sample_rates", "target_sample_rate", "kernels", "pre_downsample"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
