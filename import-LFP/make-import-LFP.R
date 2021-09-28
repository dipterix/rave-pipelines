library(targets)

source("common.R")
# tar_option_set(packages = c("raveio"))

...targets <- list(
  check_settings_file = tar_target(
    settings_path,
    "settings.yaml",
    format = "file"
  ),
  load_settings = tar_target(
    settings,
    {
      settings <- raveio::load_yaml(settings_path)
      settings$electrodes <- dipsaus::parse_svec(settings$electrodes)
      if(!settings$physical_unit %in% c("V", "mV", "uV")){
        settings$physical_unit <- NA
      }
      settings
    }
  ),
  creating_virtual_subject_instance = tar_target(
    subject,
    {
      subject <- raveio::RAVESubject$new(
        project_name = settings$project_name,
        subject_code = settings$subject_code,
        strict = FALSE
      )
      subject$initialize_paths(include_freesurfer = FALSE)
      subject
    }
  ),
  preparing_preprocess_unit = tar_target(
    subject_preprocess,
    {
      subject_preprocess <- raveio::RAVEPreprocessSettings$new(
        subject = subject, read_only = FALSE
      )
      sel <- settings$blocks %in% subject_preprocess$all_blocks
      if(!all(sel)){
        stop("Some block folders are missing: ", paste(settings$blocks[!sel], collapse = ", "))
      }
      sel <- subject_preprocess$electrodes %in% settings$electrodes
      if(length(sel) && any(subject_preprocess$data_imported[sel])){
        if(!setequal(settings$blocks, subject_preprocess$blocks)){
          stop("The subject has been imported before. The block was ",
               paste(subject_preprocess$blocks, collapse = ", "),
               ". However, block ",
               paste(settings$blocks, collapse = ", "), " was asked")
        }
      } else {
        subject_preprocess$set_blocks(settings$blocks, force = TRUE)
      }
      add_es <- settings$electrodes[
        !settings$electrodes %in% subject_preprocess$electrodes
      ]
      if(length(add_es)){
        subject_preprocess$set_electrodes(settings$electrodes, type = "LFP", add = TRUE)
      }

      subject_preprocess$set_sample_rates(settings$sample_rate, type = 'LFP')

      es <- subject_preprocess$electrodes
      et <- subject_preprocess$electrode_types
      raveio::catgl(paste(
        "Setting subject [{subject_preprocess$subject$subject_id}]:\n",
        "    Blocks: ", paste(subject_preprocess$blocks, collapse = ", "), "\n",
        "    Electrodes: ", dipsaus::deparse_svec(es[et == "LFP"]), "\n",
        "    LFP sample rate: ", subject_preprocess$`@lfp_ecog_sample_rate`, "\n",
        sep = ""), level = "INFO")

      subject_preprocess$save()
      subject_preprocess
    }
  ),
  import_LFP_analog_traces = tar_target(
    import_LFP_timestamp,
    {
      on.exit({
        future::plan("sequential")
      }, add = TRUE)
      es <- subject_preprocess$electrodes
      et <- subject_preprocess$electrode_types
      es <- es[et == "LFP" & !subject_preprocess$data_imported]
      if(length(es)){
        raveio::rave_import(
          project_name = subject$project_name,
          subject_code = subject$subject_code,
          blocks = subject_preprocess$blocks,
          electrodes = es[et == "LFP"],
          format = settings$file_format,
          sample_rate = subject_preprocess$`@lfp_ecog_sample_rate`,
          conversion = settings$physical_unit,
          data_type = "LFP",
        )
      }
      subject_preprocess$save()
      tstamp <- Sys.time()
      dir <- file.path(subject$pipeline_path, "_shared")
      raveio::dir_create2(dir)
      saveRDS(tstamp, file = file.path(dir, "import_timestamp"))
      tstamp
    }
  ),
  save_pipeline = tar_target(
    save_pipeline_import_LFP,
    {
      # per-update the pipeline, run
      force(import_LFP_timestamp)
      if(settings$save_pipeline){
        src <- normalizePath(getwd())
        dst <- file.path(subject$pipeline_path, !!target_name)
        dst <- normalizePath(dst, mustWork = FALSE)
        if(dst != src){
          file.copy(src, subject$pipeline_path,
                    recursive = TRUE, copy.date = TRUE)
        }
      }
      Sys.time()
    }
  )
)



