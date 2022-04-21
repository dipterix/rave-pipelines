library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path",
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings",
    quote({
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")),
    input_reference_name = targets::tar_target_raw("reference_name",
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_electrode_group = targets::tar_target_raw("electrode_group",
        quote({
            settings[["electrode_group"]]
        }), deps = "settings"), input_changes = targets::tar_target_raw("changes",
        quote({
            settings[["changes"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject",
        command = quote({
            {
                library(raveio)
                subject <- RAVESubject$new(project_name = project_name,
                  subject_code = subject_code)
                print(subject)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), load_presets = targets::tar_target_raw(name = "reference_table_initial",
        command = quote({
            {
                if (isTRUE(reference_name %in% subject$reference_names)) {
                  tryCatch({
                    reference_table_initial <- subject$get_reference(reference_name,
                      simplify = FALSE)
                    reference_table_initial <- reference_table_initial[,
                      c("Electrode", "Group", "Reference", "Type")]
                  }, error = function(e) {
                    ravedash::logger("Unable to load reference [",
                      reference_name, "]", level = "error")
                    ravedash::logger_error_condition(e)
                    stop("Unable to load reference [", reference_name,
                      "]. Please check your reference file or simply start with a blank reference profile.")
                  })
                } else {
                  reference_table_initial <- data.frame(Electrode = subject$electrodes,
                    Group = "Default", Reference = "noref", Type = "No Reference")
                }
                unsaved_meta <- file.path(subject$meta_path,
                  "reference__unsaved.csv")
                utils::write.csv(reference_table_initial, unsaved_meta)
            }
            return(reference_table_initial)
        }), deps = c("reference_name", "subject"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), load_3D_brain = targets::tar_target_raw(name = "brain",
        command = quote({
            {
                brain <- raveio::rave_brain(subject = subject)
            }
            return(brain)
        }), deps = "subject", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), load_voltage_data = targets::tar_target_raw(name = "voltage_data",
        command = quote({
            {
                electrodes <- subject$electrodes
                blocks <- subject$blocks
                cache_root <- file.path(subject$cache_path, "rave2",
                  "voltage")
                raveio::dir_create2(cache_root)
                notch_params <- subject$preprocess_settings$notch_params
                first_e <- electrodes[[1]]
                first_inst <- raveio::new_electrode(subject = subject,
                  number = first_e)
                progress <- dipsaus::progress2("Check cache data",
                  max = length(blocks), shiny_auto_close = TRUE)
                voltage_signals <- lapply(blocks, function(block) {
                  progress$inc(sprintf("%s", block))
                  sample_signal <- raveio::load_h5(first_inst$voltage_file,
                    name = sprintf("/raw/voltage/%s", block),
                    ram = FALSE)
                  signal_length <- length(sample_signal)
                  block_path <- file.path(cache_root, block)
                  exists <- TRUE
                  arr <- tryCatch({
                    filearray::filearray_checkload(filebase = block_path,
                      mode = "readwrite", symlink_ok = FALSE,
                      subject_id = subject$subject_id, blocks = blocks,
                      electrodes = electrodes, notch_params = notch_params,
                      sample_rates = subject$raw_sample_rates,
                      signal_length = as.integer(signal_length),
                      staged = TRUE)
                  }, error = function(e) {
                    unlink(block_path, recursive = TRUE)
                    arr <- filearray::filearray_create(filebase = block_path,
                      dimension = c(signal_length, length(electrodes)),
                      type = "double", partition_size = 1L)
                    arr$.header$subject_id <- subject$subject_id
                    arr$.header$blocks <- blocks
                    arr$.header$electrodes <- electrodes
                    arr$.header$notch_params <- notch_params
                    arr$.header$sample_rates <- subject$raw_sample_rates
                    arr$.header$signal_length <- as.integer(signal_length)
                    arr$.save_header()
                    exists <<- FALSE
                    arr
                  })
                  list(exists = exists, array = arr)
                })
                names(voltage_signals) <- blocks
                exists <- vapply(voltage_signals, "[[", FALSE,
                  "exists")
                missing_blocks <- blocks[!exists]
                subject_id <- subject$subject_id
                if (length(missing_blocks)) {
                  dipsaus::lapply_async2(seq_along(electrodes),
                    function(ii) {
                      e <- electrodes[[ii]]
                      inst <- raveio::new_electrode(subject = subject_id,
                        number = e)
                      voltage_file <- inst$voltage_file
                      for (block in missing_blocks) {
                        s <- raveio::load_h5(voltage_file, sprintf("/raw/voltage/%s",
                          block), ram = TRUE)
                        voltage_signals[[block]]$array[, ii] <- s
                      }
                    }, plan = FALSE, callback = function(ii) {
                      sprintf("Creating cache|Electrode %s",
                        electrodes[[ii]])
                    })
                }
                voltage_data <- list(data = structure(lapply(voltage_signals,
                  function(item) {
                    if (!item$exists) {
                      item$array$set_header("staged", TRUE)
                    }
                    item$array$.mode <- "readonly"
                    item$array
                  }), names = blocks), electrodes = electrodes)
            }
            return(voltage_data)
        }), deps = "subject", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), validate_electrode_groups = targets::tar_target_raw(name = "reference_group",
        command = quote({
            {
                ngroups <- length(electrode_group)
                group_names <- NULL
                electrodes <- NULL
                reference_group <- reference_table_initial
                reference_group$GroupID <- 0
                id <- 1
                for (x in electrode_group) {
                  e <- dipsaus::parse_svec(x$electrodes)
                  if (length(e)) {
                    e <- unique(e)
                    if (length(x$name) != 1 || !is.character(x$name)) {
                      stop("Each electrode group must have a unique name")
                    }
                    x$name <- trimws(x$name)
                    if (x$name %in% group_names) {
                      stop("Electrode groups must have unique names (violation: ",
                        x$name, ")")
                    }
                    group_names <- c(group_names, x$name)
                    emissing <- e[!e %in% subject$electrodes]
                    if (length(emissing)) {
                      stop("Electrodes ", dipsaus::deparse_svec(emissing),
                        " are not declared/imported. Please remove from group [",
                        x$name, "]")
                    }
                    edup <- e[e %in% electrodes]
                    if (length(edup)) {
                      stop("Electrodes ", dipsaus::deparse_svec(edup),
                        " have been included in multiple groups. Please fix this issue by ensuring each of these electrodes only belongs to one group at a time.")
                    }
                    electrodes <- c(electrodes, e)
                    sel <- reference_group$Electrode %in% e
                    reference_group$Group[sel] <- x$name
                    reference_group$GroupID[sel] <- id
                  }
                  id <- id + 1
                }
                head(reference_group)
            }
            return(reference_group)
        }), deps = c("electrode_group", "reference_table_initial",
        "subject"), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), validate_and_apply_changes = targets::tar_target_raw(name = "reference_updated",
        command = quote({
            {
                reference_choices <- c("No Reference", "Common Average Reference",
                  "White-matter Reference", "Bipolar Reference")
                for (item in changes) {
                  evec <- dipsaus::parse_svec(item$electrodes)
                  sel <- reference_group$Electrode %in% evec
                  if (!any(sel)) {
                    next
                  }
                  if (length(item$reference_type) != 1) {
                    stop("Reference type is missing for electrode ",
                      dipsaus::deparse_svec(evec))
                  }
                  reference_group$Type[sel] <- item$reference_type
                  if (item$reference_type == reference_choices[[1]]) {
                    reference_group$Reference[sel] <- "noref"
                  } else if (item$reference_type %in% reference_choices[c(2,
                    3)]) {
                    if (length(item$reference_signal) != 1) {
                      stop("Reference signal must be the same within the group if the group reference type is common average or white-matter reference")
                    }
                    reference_group$Reference[sel] <- item$reference_signal
                  } else if (item$reference_type == reference_choices[[4]]) {
                    if (length(item$reference_signal) != sum(sel)) {
                      stop("For Bipolar reference, `reference_signal` must have the same size as the number of electrodes")
                    }
                    reference_group$Reference[sel] <- item$reference_signal
                  }
                }
                reference_updated <- reference_group
                unsaved_meta <- file.path(subject$meta_path,
                  "reference__unsaved.csv")
                utils::write.csv(reference_updated, unsaved_meta)
            }
            return(reference_updated)
        }), deps = c("changes", "reference_group", "subject"),
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"))
