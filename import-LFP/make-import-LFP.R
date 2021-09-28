library(targets)

source("common.R")
# tar_option_set(packages = c("raveio"))

...targets <- list(
  load_settings = tar_target(
    settings,  # "settings.yaml"
    {
      settings <- raveio::load_yaml(settings_path)
      settings$electrodes <- dipsaus::parse_svec(settings$electrodes)
      if(!settings$physical_unit %in% c("V", "mV", "uV")){
        settings$physical_unit <- NA
      }
      settings
    }
  ),
  initialize_and_check_subject = tar_target(
    checked_settings,
    {
      sel <- settings$blocks %in% preprocess_instance$all_blocks
      if(!all(sel)){
        stop("Some block folders are missing: ", paste(settings$blocks[!sel], collapse = ", "))
      }
      sel <- preprocess_instance$electrodes %in% settings$electrodes
      if(length(sel) && any(preprocess_instance$data_imported[sel])){
        if(!setequal(settings$blocks, preprocess_instance$blocks)){
          stop("The subject has been imported before. The block was ",
               paste(preprocess_instance$blocks, collapse = ", "),
               ". However, block ",
               paste(settings$blocks, collapse = ", "), " was asked")
        }
      } else {
        preprocess_instance$set_blocks(settings$blocks, force = TRUE)
      }
      add_es <- settings$electrodes[
        !settings$electrodes %in% preprocess_instance$electrodes
      ]
      if(length(add_es)){
        preprocess_instance$set_electrodes(settings$electrodes, type = "LFP", add = TRUE)
      }

      preprocess_instance$set_sample_rates(settings$sample_rate, type = 'LFP')

      es <- preprocess_instance$electrodes
      et <- preprocess_instance$electrode_types
      raveio::catgl(paste(
        "Setting subject [{preprocess_instance$subject$subject_id}]:\n",
        "    Blocks: ", paste(preprocess_instance$blocks, collapse = ", "), "\n",
        "    Electrodes: ", dipsaus::deparse_svec(es[et == "LFP"]), "\n",
        "    LFP sample rate: ", preprocess_instance$`@lfp_ecog_sample_rate`, "\n",
        sep = ""), level = "INFO")

      # initialize
      subject$initialize_paths(include_freesurfer = FALSE)
      preprocess_instance$save()
      preprocess_instance

    }
  ),
  import_LFP_analog_traces = tar_target(
    import_LFP,
    {
      on.exit({
        future::plan("sequential")
      }, add = TRUE)
      es <- checked_settings$electrodes
      et <- checked_settings$electrode_types
      es <- es[et == "LFP" & !checked_settings$data_imported]
      if(length(es)){
        raveio::rave_import(
          project_name = subject$project_name,
          subject_code = subject$subject_code,
          blocks = checked_settings$blocks,
          electrodes = es[et == "LFP"],
          format = settings$file_format,
          sample_rate = checked_settings$`@lfp_ecog_sample_rate`,
          conversion = settings$physical_unit,
          data_type = "LFP",
        )
      }
      checked_settings$save()
      tstamp <- Sys.time()
      dir <- file.path(subject$pipeline_path, "_shared")
      raveio::dir_create2(dir)
      saveRDS(tstamp, file = file.path(dir, "import_timestamp"))
      tstamp
    }
  )
)



