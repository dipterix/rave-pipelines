library(targets)

source("common.R")
# tar_option_set(packages = c("raveio"))

...targets <- list(
  load_settings = tar_target(
    settings,  # "settings.yaml"
    {
      settings <- raveio::load_yaml(settings_path)
      if(!isTRUE(dir.exists(settings$freesurfer_directory))){
        stop("FreeSurfer directory not found.")
      }
      settings$freesurfer_directory <- normalizePath(settings$freesurfer_directory)
      raveio::catgl("FreeSurfer directory exists... yes", level = "DEFAULT")
      if(length(settings$ct_aligned_t1)){
        if(!endsWith(tolower(settings$ct_aligned_t1), "nii")){
          stop("CT file needs to be `nii` format")
        }
        if(!file.exists(settings$ct_aligned_t1)){
          stop("CT not found. You can leave it blank (null) if it's missing")
        }
        raveio::catgl("NIFTI file that contains CT aligned to T1... yes", level = "DEFAULT")
      } else {
        raveio::catgl("NIFTI file that contains CT aligned to T1... missing (will be ignored)")
      }

      if(!is.na(subject$freesurfer_path)){

        if(isTRUE(settings$overwrite)){

        } else {
          stop("FreeSurfer path has been imported at ", subject$freesurfer_path, ". Please use overwrite option if you want to replace the previous version (make sure you back up before doing so)")
        }

      }

      if(isTRUE(settings$fs_within_rave)){
        fs_path <- file.path(subject$rave_path, 'fs')
      } else {
        fs_path <- file.path(subject$path, 'fs')
      }
      # if(dir.exists(fs_path)){
      #   unlink(fs_path, recursive = TRUE)
      # }
      settings$fs_path <- fs_path

      suppressWarnings({
        port <- as.integer(settings$shiny_options$port)
      })
      if(is.na(port)){
        port <- NULL
      }
      settings$shiny_options$port <- port
      settings$shiny_options$launch.browser <- isTRUE(settings$shiny_options$launch.browser)

      settings
    }
  ),

  copy_or_create_symlink = tar_target(
    copy_or_create_symlink,
    {
      if(isTRUE(settings$symlink)){
        file.symlink(settings$freesurfer_directory, dirname(settings$fs_path))
        fname <- unlist(strsplit(settings$freesurfer_directory, "/|\\\\"))
        fname <- fname[fname!=""]
        fname <- fname[length(fname)]
        file.rename(file.path(dirname(settings$fs_path), fname),
                    settings$fs_path)
      } else {
        fs <- list.files(settings$freesurfer_directory, all.files = TRUE, full.names = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = TRUE)

        raveio::dir_create2(settings$fs_path)

        file.copy(
          file.path(settings$freesurfer_directory, fs),
          settings$fs_path,
          recursive = TRUE, copy.date = TRUE
        )
      }
      list(
        time_stamp = Sys.time(),
        fs_path = settings$fs_path,
        subject_code = settings$subject_code,
        ct_aligned_t1 = settings$ct_aligned_t1
      )
    }
  ),

  create_cache = tar_target(
    create_cache,
    {
      import_res <- copy_or_create_symlink
      cache_path <- file.path(import_res$fs_path, "RAVE")
      if(dir.exists(cache_path)){
        unlink(cache_path, recursive = TRUE)
      }
      threeBrain::import_from_freesurfer(
        fs_path = import_res$fs_path,
        subject_name = import_res$subject_code
      )
      if(length(import_res$ct_aligned_t1)){
        file.copy(
          import_res$ct_aligned_t1,
          file.path(import_res$fs_path, "RAVE", "ct_in_t1.nii")
        )
      }
      list(
        timstamp = Sys.time(),
        ct_path = file.path(import_res$fs_path, "RAVE", "ct_in_t1.nii"),
        has_ct = (length(import_res$ct_aligned_t1) > 0)
      )
    }
  ),

  # localization
  electrode_localization = tar_target(
    electrode_localization,
    {
      if(isTRUE(settings$localize) && create_cache$has_ct){

        if(dipsaus::shiny_is_running()){
          # launch back-end process

        } else {
          localize_electrodes(
            subject,
            file.path(settings$fs_path, "RAVE", "ct_in_t1.nii")
          )
        }
      }
      Sys.time()
    },
    cue = tar_cue("always")
  )
)






