library(targets)
library(dipsaus)

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
      settings$lower_bound <- as.numeric(settings$lower_bound)
      settings$upper_bound <- as.numeric(settings$upper_bound)

      if(length(settings$lower_bound) != length(settings$upper_bound)){
        stop("Notch filter lower bound frequency lengths should equal to the upper bound lengths. Lower bounds: ", paste(settings$lower_bound, collapse = ", "), ", upper bounds: ", paste(settings$upper_bound, collapse = ", "))
      }
      ub <- mapply(max, settings$lower_bound, settings$upper_bound)
      lb <- mapply(min, settings$lower_bound, settings$upper_bound)
      settings$lower_bound <- lb
      settings$upper_bound <- ub
      settings
    }
  ),
  preparing_preprocess_tool = tar_target(
    preprocess_tool,
    {
      subject <- raveio::RAVESubject$new(
        project_name = settings$project_name,
        subject_code = settings$subject_code,
        strict = FALSE
      )
      ret <- raveio::RAVEPreprocessSettings$new(
        subject = subject, read_only = FALSE
      )
      es <- ret$electrodes
      et <- ret$electrode_types
      es <- es[et %in% settings$electrode_types]
      if(!length(es)){
        warning("No ", paste(settings$electrode_types, collapse = ", "), " electrode is found. Nothing will be notch-filtered")
      } else {
        raveio::catgl("Notch filter will be applied to ", dipsaus::deparse_svec(es), level = "INFO")
      }
      ret
    }
  ),
  saving_pwelch_plots = tar_target(
    notch_plots,
    {
      force(notch_timestamp)
      if(!settings$save_pwelch){
        return(NULL)
      }
      es <- preprocess_tool$electrodes[preprocess_tool$electrode_types == 'LFP']
      subject <- preprocess_tool$subject
      chntx = dipsaus::deparse_svec(es)
      if( stringr::str_length(chntx) > 20 ){
        chntx = sprintf('total %d electrodes')
      }

      fname <- sprintf('[%s][%s] notch inspection [%s].zip',
              subject$project_name,
              subject$subject_code, chntx)
      blocks <- preprocess_tool$blocks
      srates <- preprocess_tool$sample_rates

      winlen <- sapply(srates, function(sample_rate){
        as.integer(glue::glue(as.character(settings$plots$pwelch_window_size)))
      })
      winlen[is.na(winlen)] <- ceiling(srates[is.na(winlen)] * 2)
      freq_lim <- settings$plots$pwelch_max_frequency
      nclass <- settings$plots$histogram_bin_count

      cex <- settings$plots$font_size_level
      cex %?<-% 2
      fore_col <- settings$plots$pwelch_filtered_color
      fore_col %?<-% "black"
      back_col <- settings$plots$pwelch_original_color
      back_col %?<-% "grey80"

      path <- file.path(subject$pipeline_path, "_plots", "notch-inspect")

      raveutils::export_diagnose_voltage(
        subject = subject, electrodes = es, blocks = blocks,
        h5_names = c("/notch/{block}", settings$source),
        winlens = winlen, freq_lims = freq_lim,
        save_dir = path,
        nclass = nclass, cex = cex, onefile = TRUE,
        fore_col = fore_col, back_col = back_col
      )
      Sys.time()
    }
  ),
  saving_pipeline = tar_target(
    save_pipeline_notch,
    {
      # per-update the pipeline, run
      force(notch_timestamp)
      if(settings$save_pipeline){
        src <- normalizePath(getwd())
        subject <- preprocess_tool$subject
        dst <- file.path(subject$pipeline_path, !!target_name)
        dst <- normalizePath(dst, mustWork = FALSE)
        if(dst != src){
          file.copy(src, subject$pipeline_path,
                    recursive = TRUE, copy.date = TRUE)
        }
      }
      Sys.time()
    }
  ),
  apply_notch_filter = tar_target(
    notch_timestamp,
    {
      es <- preprocess_tool$electrodes
      et <- preprocess_tool$electrode_types
      electrodes <- es[et %in% settings$electrode_types]
      blocks = preprocess_tool$blocks
      sample_rates <- preprocess_tool$sample_rates
      preprocess_path <- preprocess_tool$subject$preprocess_path

      e <- electrodes[[1]]
      h5_path <- file.path(preprocess_path, 'voltage', sprintf('electrode_%d.h5', e))
      if(!file.exists(h5_path)){
        stop("Cannot find preprocess files. Have you imported data yet?")
      }
      nms <- raveio::h5_names(h5_path)
      for(block in blocks){
        if(!glue::glue(settings$source) %in% nms){
          stop("Cannot find signal names in the imported files: ", settings$source, " (with block: ", block, ")")
        }
      }
      rhdf5::h5closeAll()

      dipsaus::lapply_callr(seq_along(electrodes), dipsaus::new_function2(
        alist(ii=), bquote({
          electrodes <- .(electrodes)
          blocks <- .(blocks)
          settings <- .(settings)
          sample_rates <- .(sample_rates)

          e <- electrodes[[ii]]
          h5_path <- file.path(.(preprocess_path), 'voltage', sprintf('electrode_%d.h5', e))

          # load all data
          signals <- structure(lapply(blocks, function(block){
            h5_name <- glue::glue(settings$source)
            raveio::load_h5(h5_path, h5_name, ram = TRUE)
          }), names = blocks)

          lapply(blocks, function(block){
            filtered <- raveutils::notch_filter(
              signals[[block]],
              sample_rate = sample_rates[[ii]],
              lb = settings$lower_bound,
              ub = settings$upper_bound
            )
            raveio::save_h5(
              x = as.vector(filtered),
              file = h5_path,
              name = sprintf('/notch/%s', block),
              chunk = c(1024),
              replace = TRUE
            )
          })
          rhdf5::h5closeAll()
        }), quote_type = 'quote')
        , .callback = function(el, ii){
          sprintf('Applying Notch filters...|Electrode %d', electrodes[[i]])
        }, .ncores = raveio::raveio_getopt("max_worker"), .rs = FALSE
      )

      # save subject data
      preprocess_tool$data$notch_params$frequencies <- 0.5 * (
        settings$lower_bound + settings$upper_bound
      )
      preprocess_tool$data$notch_params$half_bandwidths <-
        settings$upper_bound - settings$lower_bound
      for(e in electrodes){
        preprocess_tool$data[[e]]$notch_filtered <- TRUE
      }
      preprocess_tool$save()
    }
  )
)



...targets
