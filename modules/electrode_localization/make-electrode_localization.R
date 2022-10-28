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
    input_path_ct_in_t1 = targets::tar_target_raw("path_ct_in_t1",
        quote({
            settings[["path_ct_in_t1"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_localization_list = targets::tar_target_raw("localization_list",
        quote({
            settings[["localization_list"]]
        }), deps = "settings"), input_localization_plan = targets::tar_target_raw("localization_plan",
        quote({
            settings[["localization_plan"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), load_FreeSurfer_LUT = targets::tar_target_raw(name = "fslut",
        command = quote({
            {
                fslut_path <- system.file("palettes", "datacube2",
                  "FreeSurferColorLUT.json", package = "threeBrain")
                cmap <- threeBrain::load_colormap(fslut_path)
                fslut <- list(cmap = cmap, labels = sapply(cmap$map,
                  "[[", "Label"))
            }
            return(fslut)
        }), deps = character(0), cue = targets::tar_cue("never"),
        pattern = NULL, iteration = "list"), load_subject = targets::tar_target_raw(name = "subject",
        command = quote({
            {
                subject <- raveio::RAVESubject$new(project_name = project_name,
                  subject_code = subject_code, strict = FALSE)
                print(subject)
                print(subject$freesurfer_path)
            }
            return(subject)
        }), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), find_CT_Nifti_files = targets::tar_target_raw(name = "ct_candidates",
        command = quote({
            {
                fs_path <- subject$freesurfer_path
                if (length(fs_path) != 1 || is.na(fs_path) ||
                  !dir.exists(fs_path)) {
                  stop("Cannot find surface/volume reconstruction folder.")
                }
                f1 <- list.files(file.path(fs_path, "coregistration"),
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE,
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE,
                  all.files = FALSE)
                f2 <- list.files(file.path(fs_path, "..", "coregistration"),
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE,
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE,
                  all.files = FALSE)
                files <- c(f1, f2)
                files[duplicated(files)] <- sprintf("%s (2)",
                  files[duplicated(files)])
                ct_candidates <- structure(files, paths = list(f1 = f1,
                  f2 = f2))
            }
            return(ct_candidates)
        }), deps = "subject", cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), check_localization_plan = targets::tar_target_raw(name = "plan_list",
        command = quote({
            {
                require(dipsaus)
                count <- dipsaus::fastmap2()
                count$n <- 0
                count$labels <- list()
                plan_list <- NULL
                plan_table <- lapply(localization_plan, function(item) {
                  dim <- as.integer(dipsaus::parse_svec(item$dimension,
                    unique = FALSE, sep = "[,x]"))
                  dim <- dim[!is.na(dim)]
                  if (!length(dim) || any(dim <= 0)) {
                    return(NULL)
                  }
                  ne <- prod(dim)
                  if (ne <= 0) {
                    return(NULL)
                  }
                  label <- as.character(item$label)
                  if (length(label) != 1 || is.na(label) || trimws(label) ==
                    "") {
                    label <- "NoLabel"
                  }
                  if (!label %in% names(count$labels)) {
                    count$labels[[label]] <- 0
                  }
                  type <- item$type
                  if (!isTRUE(type %in% raveio::LOCATION_TYPES)) {
                    type <- raveio::LOCATION_TYPES[[1]]
                  }
                  re <- data.frame(Electrode = count$n + seq_len(ne),
                    Label = sprintf("%s%d", label, count$labels[[label]] +
                      seq_len(ne)), LabelPrefix = label, Dimension = paste(dim,
                      collapse = "x"), LocationType = type)
                  count$n <- count$n + ne
                  count$labels[[label]] <- count$labels[[label]] +
                    ne
                  re
                })
                plan_table <- dipsaus::drop_nulls(plan_table)
                if (length(plan_table)) {
                  plan_table <- do.call("rbind", unname(plan_table))
                  electrodes <- sort(subject$preprocess_settings$electrodes)
                  if (length(electrodes) == 0) {
                    electrodes <- plan_table$Electrode
                  }
                  if (length(electrodes) != length(plan_table$Electrode)) {
                    stop(sprintf("The electrode plan table (n=%d) has inconsistent length with registered electrode length (n=%d).",
                      length(electrodes), length(plan_table$Electrode)))
                  }
                  plan_table$Electrode <- electrodes
                  files <- file.path(subject$meta_path, c("electrodes_unsaved.csv",
                    "electrodes.csv"))
                  files <- files[file.exists(files)]
                  try({
                    if (length(files)) {
                      files <- files[[1]]
                      electrode_table <- raveio::safe_read_csv(files)
                      tname1 <- c("Electrode", "Coord_x", "Coord_y",
                        "Coord_z")
                      tname2 <- c("Electrode", "Coord_x", "Coord_y",
                        "Coord_z", "Radius", "SurfaceType", "Hemisphere",
                        "FSIndex", "FSLabel", "MNI305_x", "MNI305_y",
                        "MNI305_z")
                      if (all(tname1 %in% names(electrode_table))) {
                        tname2 <- tname2[tname2 %in% names(electrode_table)]
                        electrode_table <- electrode_table[,
                          tname2]
                        if (!setequal(as.integer(electrode_table$Electrode),
                          as.integer(plan_table$Electrode)) ||
                          nrow(electrode_table) != nrow(plan_table)) {
                          stop("Existing electrode table has different row numbers to the plan table")
                        }
                        plan_table <- merge(electrode_table,
                          plan_table, by = "Electrode")
                      }
                    }
                  }, silent = TRUE)
                  plan_table$Coord_x %?<-% 0
                  plan_table$Coord_y %?<-% 0
                  plan_table$Coord_z %?<-% 0
                  plan_table$Radius %?<-% 1
                  plan_table$SurfaceType %?<-% "pial"
                  plan_table$MNI305_x %?<-% 0
                  plan_table$MNI305_y %?<-% 0
                  plan_table$MNI305_z %?<-% 0
                  plan_table$VertexNumber <- -1
                  plan_table$FSIndex %?<-% 0
                  plan_table$FSLabel %?<-% "Unknown"
                  plan_table$SurfaceElectrode <- plan_table$LocationType %in%
                    c("ECoG")
                  etypes <- subject$preprocess_settings$electrode_types
                  if (!length(etypes)) {
                    etypes <- "LFP"
                  }
                  plan_table$SignalType <- etypes
                  plan_list <- split(plan_table, plan_table$LabelPrefix)
                  raveio::dir_create2(subject$meta_path)
                  utils::write.csv(plan_table, file = file.path(subject$meta_path,
                    "electrodes_unsaved.csv"), row.names = FALSE)
                }
            }
            return(plan_list)
        }), deps = c("localization_plan", "subject"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), load_brain = targets::tar_target_raw(name = "brain",
        command = quote({
            {
                brain <- threeBrain::freesurfer_brain2(fs_subject_folder = subject$freesurfer_path,
                  subject_name = subject$subject_code)
            }
            return(brain)
        }), deps = "subject", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), Loading_brain_and_CT_if_exists = targets::tar_target_raw(name = "ct_in_t1",
        command = quote({
            {
                force(subject)
                has_ct <- FALSE
                ct_path <- character(0L)
                if (length(path_ct_in_t1)) {
                  if (startsWith(path_ct_in_t1, "{")) {
                    s <- strsplit(path_ct_in_t1, "/|\\\\")[[1]]
                    s[[1]] <- raveio::glue(s[[1]])
                    s <- do.call(file.path, as.list(s))
                    ct_path <- normalizePath(s, mustWork = TRUE)
                  } else {
                    ct_path <- normalizePath(path_ct_in_t1, mustWork = TRUE)
                  }
                  has_ct <- TRUE
                }
                if (has_ct) {
                  subject$set_default("ct_path", path_ct_in_t1,
                    namespace = "electrode_localization")
                  ct_in_t1 <- threeBrain:::read_nii2(ct_path)
                } else {
                  subject$set_default("ct_path", NULL, namespace = "electrode_localization")
                  ct_in_t1 <- NA
                }
            }
            return(ct_in_t1)
        }), deps = c("subject", "path_ct_in_t1"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), generate_indicator = targets::tar_target_raw(name = "ct_exists",
        command = quote({
            {
                ct_exists <- isTRUE(!is.null(ct_in_t1) && is.list(ct_in_t1))
            }
            return(ct_exists)
        }), deps = "ct_in_t1", cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), generate_localization_viewer = targets::tar_target_raw(name = "viewer",
        command = quote({
            {
                if (!is.null(ct_in_t1) && is.list(ct_in_t1)) {
                  viewer <- brain$localize(coregistered_ct = ct_in_t1)
                } else {
                  viewer <- brain$localize()
                }
                if (interactive()) {
                  print(viewer)
                }
            }
            return(viewer)
        }), deps = c("ct_in_t1", "brain"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), merge_localization_list = targets::tar_target_raw(name = "localization_result_initial",
        command = quote({
            {
                require(dipsaus)
                re <- lapply(localization_list, function(item) {
                  item$FSIndex %?<-% 0
                  item$FSLabel %?<-% "Unknown"
                  item$Radius %?<-% 1
                  tbl <- data.frame(Electrode = item$Electrode,
                    Coord_x = item$Coord_x, Coord_y = item$Coord_y,
                    Coord_z = item$Coord_z, Label = item$Label,
                    LabelPrefix = item$LabelPrefix, Dimension = item$Dimension,
                    LocationType = item$LocationType, Radius = item$Radius,
                    MNI305_x = item$MNI305_x, MNI305_y = item$MNI305_y,
                    MNI305_z = item$MNI305_z, FSIndex = item$FSIndex,
                    FSLabel = item$FSLabel)
                  if (!nrow(tbl)) {
                    return(NULL)
                  }
                  tbl
                })
                re <- do.call("rbind", dipsaus::drop_nulls(re))
                if (!length(re)) {
                  return(NULL)
                }
                rownames(re) <- NULL
                re <- re[order(re$Electrode), ]
                re$SurfaceElectrode <- re$LocationType %in% c("ECoG")
                re$SurfaceType <- "pial"
                re$VertexNumber <- -1
                empty_sel <- (re$Coord_x)^2 + (re$Coord_y)^2 +
                  (re$Coord_z)^2
                empty_sel <- is.na(empty_sel) | empty_sel ==
                  0
                tkrRAS <- rbind(re$Coord_x, re$Coord_y, re$Coord_z,
                  1)
                t1 <- brain$Norig %*% solve(brain$Torig) %*%
                  tkrRAS
                mni305 <- brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*%
                  tkrRAS
                mni152 <- raveio:::MNI305_to_MNI152 %*% mni305
                mni305[, empty_sel] <- 0
                mni152[, empty_sel] <- 0
                t1[, empty_sel] <- 0
                re$MNI305_x <- mni305[1, ]
                re$MNI305_y <- mni305[2, ]
                re$MNI305_z <- mni305[3, ]
                re$T1R <- t1[1, ]
                re$T1A <- t1[2, ]
                re$T1S <- t1[3, ]
                re$MNI152_x <- mni152[1, ]
                re$MNI152_y <- mni152[2, ]
                re$MNI152_z <- mni152[3, ]
                save_path <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                raveio::dir_create2(dirname(save_path))
                utils::write.csv(re, save_path, row.names = FALSE)
                localization_result_initial <- re
            }
            return(localization_result_initial)
        }), deps = c("localization_list", "brain", "subject"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    calculate_std141_mappings = targets::tar_target_raw(name = "localization_result_final",
        command = quote({
            {
                src <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                if (file.exists(src)) {
                  re <- utils::read.csv(file.path(subject$meta_path,
                    "electrodes_unsaved.csv"))
                  brain$set_electrodes(electrodes = re)
                  re <- brain$calculate_template_coordinates(save_to = FALSE)
                  sel <- is.na(re$Hemisphere) | !re$Hemisphere %in%
                    c("left", "right")
                  if (any(sel)) {
                    is_left <- grepl(pattern = "(left|lh-)",
                      re$FSLabel, ignore.case = TRUE)
                    is_right <- grepl(pattern = "(right|rh-)",
                      re$FSLabel, ignore.case = TRUE)
                    re$Hemisphere[sel & (is_left | re$MNI305_x <
                      0)] <- "left"
                    re$Hemisphere[sel & !is_left & (is_right |
                      re$MNI305_x > 0)] <- "right"
                  }
                  localization_result_final <- re
                } else {
                  localization_result_final <- NULL
                }
            }
            return(localization_result_final)
        }), deps = c("subject", "brain"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"))
