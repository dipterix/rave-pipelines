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
    input_analysis_block = targets::tar_target_raw("analysis_block", 
        quote({
            settings[["analysis_block"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes", 
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), input_filter_notch = targets::tar_target_raw("filter_notch", 
        quote({
            settings[["filter_notch"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_filter_bandpass = targets::tar_target_raw("filter_bandpass", 
        quote({
            settings[["filter_bandpass"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_block = targets::tar_target_raw("block", 
        quote({
            settings[["block"]]
        }), deps = "settings"), input_pwelch_params = targets::tar_target_raw("pwelch_params", 
        quote({
            settings[["pwelch_params"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), validate_inputs = targets::tar_target_raw(name = "repository", 
        command = quote({
            {
                load_electrodes <- unique(as.integer(dipsaus::parse_svec(loaded_electrodes)))
                electrodes <- as.integer(subject$electrodes)
                electrodes <- electrodes[subject$preprocess_settings$data_imported]
                load_electrodes <- load_electrodes[load_electrodes %in% 
                  electrodes]
                root_path <- file.path(subject$preprocess_path, 
                  "voltage")
                if (length(load_electrodes)) {
                  data_files <- file.path(root_path, sprintf("electrode_%d.h5", 
                    load_electrodes))
                  load_electrodes <- load_electrodes[file.exists(data_files)]
                }
                if (!length(load_electrodes)) {
                  stop("None electrode will be loaded. Please make sure at least one electrode is imported.")
                }
                sample_rates <- subject$raw_sample_rates[subject$electrodes %in% 
                  load_electrodes]
                sample_rate <- unique(sample_rates)
                if (length(sample_rate) == 0) {
                  stop("Cannot find proper sample rate for the selected electrodes")
                }
                if (anyNA(sample_rates)) {
                  stop("Some electrodes have missing sampling frequency. Please make sure all selected electrode channels have been imported.")
                }
                if (length(sample_rate) > 1) {
                  stop("Multiple sample rates are found: [", 
                    paste(sprintf("%.1f", sample_rate), collapse = ", "), 
                    "]. Please choose electrode channels with the sample rates.")
                }
                blocks <- block[block %in% subject$blocks]
                if (!length(blocks)) {
                  stop("All the blocks are invalid. Please choose one or more blocks from the following choices: ", 
                    paste(subject$blocks, collapse = ", "))
                }
                sample_electrode <- load_electrodes[[1]]
                sample_instance <- raveio::new_electrode(subject, 
                  sample_electrode, quiet = TRUE)
                sample_data <- sample_instance$load_blocks(blocks, 
                  type = "raw-voltage", simplify = FALSE)
                electrodes <- as.integer(subject$electrodes)
                cache_root <- file.path(subject$cache_path, "rave2", 
                  "raw-voltage")
                progress <- dipsaus::progress2("Loading raw-voltage", 
                  max = length(blocks) + 1, shiny_auto_close = TRUE, 
                  quiet = !dipsaus::shiny_is_running())
                loaded_data <- sapply(blocks, function(block) {
                  progress$inc(sprintf("Loading block %s", block))
                  data_length <- length(sample_data[[block]])
                  dm <- c(data_length, length(electrodes))
                  cache_path <- file.path(cache_root, block)
                  arr <- filearray::filearray_load_or_create(filebase = cache_path, 
                    dimension = dm, type = "double", mode = "readwrite", 
                    partition_size = 1L, symlink_ok = FALSE, 
                    verbose = FALSE, electrodes = as.integer(electrodes), 
                    block = block)
                  loaded_electrodes <- arr$get_header(key = "loaded_electrodes", 
                    default = NULL)
                  toload <- load_electrodes[!load_electrodes %in% 
                    loaded_electrodes]
                  if (length(toload)) {
                    dipsaus::lapply_async2(toload, function(e) {
                      inst <- raveio::new_electrode(subject = subject, 
                        number = e)
                      dat <- inst$load_blocks(blocks = block, 
                        type = "raw-voltage", simplify = TRUE)
                      arr[, electrodes == e] <- dat
                      invisible()
                    }, plan = FALSE, callback = function(e) {
                      sprintf("Loading electrode %d", e)
                    })
                    arr$set_header("loaded_electrodes", sort(c(loaded_electrodes, 
                      toload)))
                  }
                  arr$.mode <- "readonly"
                  arr
                }, simplify = FALSE, USE.NAMES = TRUE)
                progress$inc("Done")
                reference_name <- "noref" %OF% subject$reference_names
                repository <- raveio::prepare_subject_bare0(subject = subject, 
                  electrodes = load_electrodes, reference_name = reference_name)
                repository$data <- loaded_data
                repository$blocks <- blocks
                repository$sample_rate <- sample_rate
                digest_key <- list(subject_id = subject$subject_id, 
                  electrodes = load_electrodes, electrode_signal_types = repository$electrode_signal_types, 
                  blocks = blocks, sample_rate = sample_rate)
                repository$signature <- structure(dipsaus::digest(digest_key), 
                  contents = names(digest_key))
                class(repository) <- c("rave_prepare_raw_voltage", 
                  class(repository))
            }
            return(repository)
        }), deps = c("loaded_electrodes", "subject", "block"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    check_analysis_inputs = targets::tar_target_raw(name = "cleaned_inputs", 
        command = quote({
            {
                sample_rate <- repository$sample_rate
                if (length(sample_rate) != 1) {
                  stop("Cannot find sample rate from the repository. This is code error. Please contact the RAVE development team and report this issue")
                }
                filter_bandpass <- as.list(filter_bandpass)
                if (isTRUE(filter_bandpass$enabled)) {
                  filter_order <- as.integer(filter_bandpass$order)
                  if (length(filter_order) != 1 || is.na(filter_order) || 
                    filter_order <= 0) {
                    stop("Band-passing filter order must be postive integers")
                  }
                  if (!isTRUE(filter_order * 3 < sample_rate)) {
                    stop("Band-passing filter order must be less than ", 
                      floor((sample_rate - 1)/3))
                  }
                  filter_range <- as.numeric(filter_bandpass$range)
                  filter_range <- filter_range[!is.na(filter_range) & 
                    filter_range >= 0]
                  filter_range <- sort(unique(filter_range))
                  if (length(filter_range)) {
                    if (filter_range[[1]] == 0) {
                      filter_range[[1]] <- 0.001
                    }
                    filter_range <- sort(unique(filter_range))
                  }
                  if (length(filter_range) != 2) {
                    stop("Band-passing filter must have a postive frequency range with lower-bound smaller than the upper-bound")
                  }
                  if (any(filter_range > sample_rate/2)) {
                    stop("Band-passing filter range must not exceed half sample-rate (Nyquist frequency) to avoid aliasing.")
                  }
                  filter_bandpass <- list(enabled = TRUE, order = filter_order, 
                    range = filter_range)
                } else {
                  filter_bandpass <- list(enabled = FALSE)
                }
                filter_notch <- as.list(filter_notch)
                if (isTRUE(filter_notch$enabled)) {
                  lower_bounds <- filter_notch$lower_bounds
                  lower_bounds <- lower_bounds[!is.na(lower_bounds) & 
                    lower_bounds > 0]
                  upper_bounds <- filter_notch$upper_bounds
                  upper_bounds <- upper_bounds[!is.na(upper_bounds) & 
                    upper_bounds > 0]
                  if (!length(lower_bounds) || length(upper_bounds) != 
                    length(lower_bounds)) {
                    stop(sprintf("Notch filter lower-bounds [%s] have unequal/zero lengths compared to the upper-bounds [%s]. Please make sure the frequencies have equal positive lengths.", 
                      paste(sprintf("%.1fHz", lower_bounds), 
                        collapse = ","), paste(sprintf("%.1fHz", 
                        upper_bounds), collapse = ",")))
                  }
                  if (any(lower_bounds >= upper_bounds)) {
                    stop(sprintf("Notch filter lower-bounds [%s] must be smaller than upper-bounds [%s], respectively.", 
                      paste(sprintf("%.1fHz", lower_bounds), 
                        collapse = ","), paste(sprintf("%.1fHz", 
                        upper_bounds), collapse = ",")))
                  }
                  if (any(upper_bounds > sample_rate/2)) {
                    stop("Notch filter upper-bounds [%s] must not exceed the half sample-rate (Nyquist frequency) to avoid aliasing.")
                  }
                  filter_notch <- list(enabled = TRUE, lower_bounds = lower_bounds, 
                    upper_bounds = upper_bounds)
                } else {
                  filter_notch <- list(enabled = FALSE)
                }
                cleaned_inputs <- list(filter_bandpass = filter_bandpass, 
                  filter_notch = filter_notch, sample_rate = sample_rate)
            }
            return(cleaned_inputs)
        }), deps = c("repository", "filter_bandpass", "filter_notch"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), check_pwelch_params = targets::tar_target_raw(name = "pwelch_params2", 
        command = quote({
            {
                pwelch_params <- as.list(pwelch_params)
                window_size <- as.integer(pwelch_params$window_size)
                if (!length(window_size) || !isTRUE(window_size > 
                  0)) {
                  stop("Welch-Periodogram window size must be an integer greater than 1")
                }
                noverlap <- as.integer(pwelch_params$noverlap)
                if (!length(noverlap) || is.na(noverlap) || noverlap < 
                  0) {
                  stop("Welch-Periodogram window overlap size must be a positive integer")
                }
                if (noverlap/window_size > 0.95) {
                  ravedash::logger("Welch-Periodogram window overlap size exceeds 0.95 x window size. The `pwelch` calculation will be slow.")
                }
                pwelch_params2 <- list(window_size = window_size, 
                  noverlap = noverlap)
            }
            return(pwelch_params2)
        }), deps = "pwelch_params", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_analysis_electrodes = targets::tar_target_raw(name = "analysis_electrodes2", 
        command = quote({
            {
                analysis_electrodes <- dipsaus::parse_svec(analysis_electrodes)
                if (!length(analysis_electrodes)) {
                  analysis_electrodes <- repository$electrode_list
                }
                analysis_electrodes2 <- analysis_electrodes[analysis_electrodes %in% 
                  repository$electrode_list]
                analysis_electrodes2 <- unique(sort(analysis_electrodes2))
            }
            return(analysis_electrodes2)
        }), deps = c("analysis_electrodes", "repository"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), filter_signals = targets::tar_target_raw(name = "filtered_data", 
        command = quote({
            {
                force(analysis_electrodes2)
                tmpdir <- ravedash::temp_dir(persist = "app-session")
                input_data <- repository$data[[analysis_block]]
                filtered_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir, 
                  "block_explorer", repository$signature, "filtered", 
                  analysis_block), dimension = dim(input_data), 
                  type = "double", symlink_ok = FALSE, mode = "readwrite", 
                  initialize = FALSE, partition_size = 1L, verbose = FALSE, 
                  repository_signature = repository$signature, 
                  block = analysis_block, filters = cleaned_inputs[c("filter_bandpass", 
                    "filter_notch")], on_missing = function(arr) {
                    dimnames(arr) <- list(NULL, Electrode = repository$subject$electrodes)
                  })
                filtered_electrodes <- filtered_data$get_header("filtered_electrodes", 
                  NULL)
                electrodes_to_filter <- analysis_electrodes2[!analysis_electrodes2 %in% 
                  filtered_electrodes]
                electrodes <- subject$electrodes
                sample_rate <- repository$sample_rate
                if (length(electrodes_to_filter)) {
                  dipsaus::lapply_async2(electrodes_to_filter, 
                    function(e) {
                      sel <- which(electrodes == e)
                      if (!length(sel)) {
                        stop("cannot find data of the electrode ", 
                          e)
                      }
                      sel <- sel[[1]]
                      signal <- input_data[, sel]
                      filter_bandpass <- cleaned_inputs$filter_bandpass
                      if (isTRUE(filter_bandpass$enabled)) {
                        signal <- ravetools::band_pass1(x = signal, 
                          sample_rate = sample_rate, lb = filter_bandpass$range[[1]], 
                          ub = filter_bandpass$range[[2]])
                      }
                      if (isTRUE(filter_notch$enabled)) {
                        signal <- ravetools::notch_filter(signal, 
                          sample_rate = sample_rate, lb = filter_notch$lower_bounds, 
                          ub = filter_notch$upper_bounds)
                      }
                      filtered_data[, sel] <- signal
                      return()
                    }, plan = FALSE, callback = function(e) {
                      sprintf("Filter signals | Electrode channel %.0f", 
                        e)
                    })
                  filtered_electrodes <- sort(c(filtered_electrodes, 
                    electrodes_to_filter))
                  filtered_data$set_header(key = "filtered_electrodes", 
                    value = filtered_electrodes)
                }
            }
            return(filtered_data)
        }), deps = c("analysis_electrodes2", "repository", "analysis_block", 
        "cleaned_inputs", "subject", "filter_notch"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), calculate_pwelch = targets::tar_target_raw(name = "pwelch_data", 
        command = quote({
            {
                force(analysis_electrodes2)
                input_data <- repository$data[[analysis_block]]
                sample_rate <- repository$sample_rate
                window_len <- pwelch_params2$window_size
                noverlap <- pwelch_params2$noverlap
                x_len <- nrow(input_data)
                nfft <- max(min(256, x_len), window_len)
                NN <- floor((nfft + 1)/2)
                dm <- c(NN, ncol(input_data))
                freq <- seq(1, sample_rate/2, length.out = NN)
                tmpdir <- ravedash::temp_dir(persist = "app-session")
                pwelch_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir, 
                  "block_explorer", repository$signature, "pwelch", 
                  analysis_block), dimension = dm, type = "double", 
                  symlink_ok = FALSE, mode = "readwrite", initialize = FALSE, 
                  partition_size = 1L, verbose = FALSE, repository_signature = repository$signature, 
                  block = analysis_block, inputs = cleaned_inputs, 
                  on_missing = function(arr) {
                    dimnames(arr) <- list(Frequency = freq, Electrode = repository$subject$electrodes)
                  })
                processed_electrodes <- pwelch_data$get_header("processed_electrodes", 
                  NULL)
                electrodes_to_pwelch <- analysis_electrodes2[!analysis_electrodes2 %in% 
                  processed_electrodes]
                electrodes <- subject$electrodes
                if (length(electrodes_to_pwelch)) {
                  dipsaus::lapply_async2(electrodes_to_pwelch, 
                    function(e) {
                      sel <- which(electrodes == e)
                      if (!length(sel)) {
                        stop("cannot find data of the electrode ", 
                          e)
                      }
                      sel <- sel[[1]]
                      signal <- input_data[, sel]
                      pwelch_result <- ravetools::pwelch(x = signal, 
                        fs = sample_rate, window = pwelch_params2$window_size, 
                        noverlap = pwelch_params2$noverlap)
                      pwelch_data[, sel] <- pwelch_result$spec
                      return()
                    }, plan = FALSE, callback = function(e) {
                      sprintf("Welch-Periodogram | Electrode channel %.0f", 
                        e)
                    })
                  processed_electrodes <- sort(c(processed_electrodes, 
                    electrodes_to_pwelch))
                  pwelch_data$set_header(key = "processed_electrodes", 
                    value = processed_electrodes)
                }
            }
            return(pwelch_data)
        }), deps = c("analysis_electrodes2", "repository", "analysis_block", 
        "pwelch_params2", "cleaned_inputs", "subject"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
