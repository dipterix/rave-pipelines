library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_diagnostic_plot_params = targets::tar_target_raw("diagnostic_plot_params", 
        quote({
            settings[["diagnostic_plot_params"]]
        }), deps = "settings"), input_notch_filter_upperbound = targets::tar_target_raw("notch_filter_upperbound", 
        quote({
            settings[["notch_filter_upperbound"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_notch_filter_lowerbound = targets::tar_target_raw("notch_filter_lowerbound", 
        quote({
            settings[["notch_filter_lowerbound"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_imported_electrodes = targets::tar_target_raw(name = "imported_electrodes", 
        command = quote({
            {
                imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
                if (!length(imported_electrodes)) {
                  stop("The subject exists but its signal has not been imported yet.")
                }
            }
            return(imported_electrodes)
        }), deps = "subject", cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_filter_settings = targets::tar_target_raw(name = "filter_settings", 
        command = quote({
            {
                lb <- unlist(notch_filter_lowerbound)
                ub <- unlist(notch_filter_upperbound)
                if (length(lb) != length(ub)) {
                  stop(sprintf("Notch filter lower bound length should match with the upper bound length (%d vs %d)", 
                    length(lb), length(ub)))
                }
                if (length(lb)) {
                  if (!all(lb < ub)) {
                    sel <- lb >= ub
                    lb <- lb[sel]
                    ub <- ub[sel]
                    stop("Notch filter lower bounds must be uniformly smaller than the upper bounds: (", 
                      paste0(lb, ">", ub, collapse = ", "), ")")
                  }
                }
                filter_settings <- list(lb = lb, ub = ub, domain = 1)
            }
            return(filter_settings)
        }), deps = c("notch_filter_lowerbound", "notch_filter_upperbound"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), apply_Notch_filters = targets::tar_target_raw(name = "apply_notch", 
        command = quote({
            {
                blocks <- subject$preprocess_settings$blocks
                electrodes <- imported_electrodes
                filters <- filter_settings
                fmt <- file.path(subject$preprocess_path, "voltage", 
                  "electrode_%d.h5")
                sample_rates <- subject$raw_sample_rates
                sample_rates <- sapply(electrodes, function(e) {
                  sample_rates[subject$electrodes == e]
                })
                dipsaus::lapply_async2(seq_along(electrodes), 
                  function(ii) {
                    e <- electrodes[[ii]]
                    srate <- sample_rates[[ii]]
                    h5_path <- sprintf(fmt, e)
                    h5_names <- gsub("^/", "", raveio::h5_names(h5_path))
                    sel <- sprintf("raw/%s", blocks) %in% h5_names
                    if (!all(sel)) {
                      stop(sprintf("Cannot find imported block(s): %s (electrode %s)", 
                        blocks[!sel], e))
                    }
                    for (block in blocks) {
                      cat(block, e, "\n")
                      signal <- raveio::load_h5(h5_path, sprintf("raw/%s", 
                        block), ram = TRUE)
                      signal <- ravetools::notch_filter(s = signal, 
                        sample_rate = srate, lb = filters$lb, 
                        ub = filters$ub, domain = filters$domain)
                      raveio::save_h5(x = signal, file = h5_path, 
                        name = sprintf("notch/%s", block), chunk = 1024, 
                        replace = TRUE, ctype = "numeric")
                    }
                  }, plan = FALSE, callback = function(ii) {
                    sprintf("Applying Notch filters|Electrode - %s", 
                      electrodes[[ii]])
                  })
                preproc <- raveio::RAVEPreprocessSettings$new(subject = subject$subject_id)
                for (e in electrodes) {
                  preproc$data[[as.character(e)]]$notch_filtered <- TRUE
                }
                preproc$save()
                apply_notch <- list(electrodes = electrodes, 
                  notch_filter_lowerbound = filters$lb, notch_filter_upperbound = filters$ub, 
                  timestamp = strftime(Sys.time(), usetz = TRUE))
                subject$set_default(namespace = "notch_filter", 
                  key = "parameters", value = apply_notch)
            }
            return(apply_notch)
        }), deps = c("subject", "imported_electrodes", "filter_settings"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_diagnostic_plots = targets::tar_target_raw(name = "diagnostic_plots", 
        command = quote({
            {
                diagnostic_plots <- FALSE
                params <- as.list(diagnostic_plot_params)
                background <- params$background
                foreground <- params$foreground
                if (!length(background) == 1) {
                  background <- "white"
                }
                if (!length(foreground) == 1) {
                  foreground <- "black"
                }
                if (length(params$path) == 1) {
                  grDevices::pdf(file = params$path, width = 12, 
                    height = 7, onefile = TRUE, bg = background, 
                    fg = foreground, useDingbats = FALSE)
                  on.exit({
                    grDevices::dev.off()
                  }, add = TRUE)
                }
                winlen <- as.numeric(params$window_length)
                if (!length(winlen) || is.na(winlen)) {
                  winlen <- "auto"
                }
                max_freq <- as.numeric(params$max_frequency)
                if (!length(max_freq) || is.na(max_freq)) {
                  max_freq <- 300
                }
                nbins <- as.numeric(params$histogram_bins)
                if (!length(nbins) || is.na(nbins)) {
                  nbins <- 50
                }
                font_size <- as.numeric(params$font_size)
                if (!length(font_size) || is.na(font_size)) {
                  font_size <- 2
                }
                quiet <- isTRUE(params$quiet)
                source("./R/diagnose_plot.R", local = TRUE)
                diagnostic_plots <- diagnose_notch_filters(subject = subject, 
                  electrodes = imported_electrodes, max_freq = max_freq, 
                  winlen = winlen, nbins = nbins, bg = background, 
                  fg = foreground, cex = font_size, std = 3, 
                  lwd = 0.3, mar = c(5.2, 5.1, 4.1, 2.1), mai = c(0.6, 
                    0.6, 0.4, 0.1), quiet = quiet)
            }
            return(diagnostic_plots)
        }), deps = c("diagnostic_plot_params", "subject", "imported_electrodes"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
