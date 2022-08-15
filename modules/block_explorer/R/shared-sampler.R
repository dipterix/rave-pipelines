
get_sample_data <- function(repository, analysis_electrodes,
                            baseline_method, baseline_unit, block = NULL, ...) {
  block <- block %OF% repository$blocks
  voltage <- repository$block_data[[block]]$voltage
  wavelet <- repository$block_data[[block]]$wavelet

  sample_rate <- voltage$sample_rate
  voltage_single <- as.vector(subset(voltage$data, Electrode ~ Electrode == analysis_electrodes, drop = TRUE))
  n_timepoints <- length(voltage_single)
  # if( n_timepoints > 10000 ) {
  #   decimate_rate <- ceiling(n_timepoints / 10000)
  #   voltage_single_decimated <- ravetools::decimate(
  #     x = voltage_single,
  #     q = decimate_rate
  #   )
  #   time <- ravetools::decimate(voltage$dnames$Time,
  #                               q = decimate_rate)
  # } else {
  #   decimate_rate <- 1L
  #   voltage_single_decimated <- voltage_single
  #   time <- voltage$dnames$Time
  # }

  voltage_sample <- list(
    origin = list(
      sample_rate = sample_rate,
      time = voltage$dnames$Time,
      data = voltage_single,
      range = range(voltage_single),
      sd = stats::sd(voltage_single)
    )
  )
  # ```{rave calculate_Welch-Periodogram, language = "R", export = "pwelch_overall"}
  data <- voltage_sample$origin$data
  srate <- voltage_sample$origin$sample_rate
  pwelch_overall <- ravetools::pwelch(
    x = data, fs = srate, plot = FALSE,
    window = srate * 2, noverlap = srate)

  # ```{rave prepare_sample_for_time-frequency_analysis, language = "R", export = "wavelet_sample"}
  sample_rate <- wavelet$sample_rate
  wavelet_single <- subset(wavelet$data, Electrode ~ Electrode == analysis_electrodes, drop = TRUE)

  wavelet_sample <- list(
    origin = list(
      sample_rate = sample_rate,
      dnames = wavelet$dnames[c(1, 2)],
      data = wavelet_single
    )
  )

  # ```{rave baseline_power, language = "R", export = "power_sample"}
  power_sample <- wavelet_sample
  switch (
    baseline_method,
    "Welch-Periodogram" = {
      apf <- approxfun(
        x = pwelch_overall$freq,
        y = pwelch_overall$spec_db)
      bl <- apf(wavelet_sample$origin$dnames$Frequency)
      bl <- 10^(bl / 20)
    },
    "Block-Average" = {
      bl <- rowMeans(Mod(wavelet_sample$origin$data)^2)
    },
    "Block-Median" = {
      bl <- apply(Mod(wavelet_sample$origin$data)^2, 1, median)
    },
    {
      bl <- 1
    }
  )

  power_sample$origin$data <- Mod(wavelet_sample$origin$data)^2 / bl

  switch (
    baseline_unit,
    "Decibel" = {
      power_sample$origin$data <- 10 * log10(power_sample$origin$data)
    },
    "Percentage-Change" = {
      power_sample$origin$data <- 100 * power_sample$origin$data - 100
    },
    {
      stop("Unsupported baseline unit")
    }
  )

  return(list(
    voltage_sample = voltage_sample,
    pwelch_overall = pwelch_overall,
    wavelet_sample = wavelet_sample,
    power_sample = power_sample
  ))
}