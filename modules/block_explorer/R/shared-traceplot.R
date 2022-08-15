fastplot_signal_trace <- function(
    data, time, ..., sample_rate, ylim = NULL, xaxs="i", yaxs="i",
    col = c(par("fg"), "orange", "orangered"),
    main = "Voltage trace", xlab = "Time (s)", ylab = "Voltage",
    sd, sds = c(2, 3), compress = TRUE,
    method = c("decimate", "slide", "slide_max")
) {
  method <- match.arg(method)
  if(!missing(time) && !missing(sample_rate)) {
    stop("Can only specify either `sample_rate` or `time` but not both.")
  }
  n_timepoints <- length(data)
  if(missing(time)) {
    time <- seq(0, by = 1/sample_rate, length.out = n_timepoints)
  } else {
    stopifnot(n_timepoints == length(time))
  }

  theme <- ravedash::current_shiny_theme()
  fg <- theme$foreground
  bg <- theme$background
  old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main",
                               "col.sub", "mai", "mar", "mgp"))
  on.exit({
    try({
      do.call(graphics::par, old_theme)
    }, silent = TRUE)
  }, add = TRUE)
  rg <- range(data, na.rm = TRUE)
  if(missing(sd)) {
    sd <- stats::sd(data, na.rm = TRUE)
  }

  col <- rep(col, 1 + floor(length(sds) / length(col)))

  if(!length(ylim)) {
    ylim <- rg
  } else if(length(ylim) == 1) {
    ylim <- c(-1, 1) * abs(ylim)
  }

  # compress image
  if( compress ) {
    if( n_timepoints > 10000 ) {
      decimate_rate <- ceiling(n_timepoints / 10000)
      tidx <- seq(1, n_timepoints, by = decimate_rate)
      ntidx <- length(tidx)

      switch (
        method,
        "slide" = {
          data <- data[tidx]
        },
        "slide_max" = {
          data <- matrix(
            c(data, rep(NA, decimate_rate * ntidx - n_timepoints)),
            ncol = ntidx, byrow = FALSE
          )
          data <- apply(data, 2, function(x) {
            x[is.na(x)] <- 0
            x[which.max(abs(x))[[1]]]
          })
        },
        {
          data <- ravetools::decimate(x = data, q = decimate_rate)
        }
      )
      time <- time[tidx]
    }
  }


  # graphics::layout(mat = matrix(c(1, 2, 3, 3), 2, byrow = TRUE), widths = c(1, lcm(2)))
  # par(mar = c(2.6, 3.5, 1.1, 0.6), mgp = c(2, 0.5, 0))
  graphics::par(fg = fg, bg = bg, col.axis = fg, col.lab = fg, col.main = fg,
                col.sub = fg, mar = c(2.6, 3.5, 1.1, 0.6), mgp = c(2, 0.5, 0))

  plot(
    x = time, y = data, ylim = ylim, xlab = "", ylab = "",
    type = 'l', axes = FALSE, col = col[[1]],
    xaxs = xaxs, yaxs = yaxs, ...
  )
  title(main, adj = 0.05, line = -0.5)
  axis(1, pretty(time), tck = -0.03)
  axis(2, pretty(rg), las = 1, tck = -0.03)
  mtext(side = 2, text = ylab, line = 2.5)
  mtext(side = 1, text = xlab, line = 1.5)

  abline(h = 0, col = "gray60", lty = 2, lwd = 2)

  if(length(sds)) {
    abline(h = sds * sd, col = col[seq_along(sds) + 1], lty = 2, lwd = 2)
    abline(h = -sds * sd, col = col[seq_along(sds) + 1], lty = 2, lwd = 2)
  }
  # legend(
  #   "bottomright", bg = "#FFFFFF4F", box.col = "#FFFFFFFF",
  #   c(sprintf("SDev=%.0f", vsd), "\u00B1 2 SDev", "\u00B1 3 SDev"),
  #   col = c(NA, "orange", "orangered"), lty = c(NA, 1, 1),
  #   text.col = c(par("fg"), "orange", "orangered")
  # )

  # par(mar = c(2.6, 0.1, 0.1, 0.1))
  # plot.new()

  # legend(
  #   "right", bg = dipsaus::col2hexStr(par("bg"), alpha = 0.7), box.col = NA,
  #   c(sprintf("SDev=%.0f", sd), sprintf("\u00B1 %s SDev", sds)),
  #   col = c(NA, col[seq_along(sds) + 1]), lty = c(NA, 1, 1),
  #   text.col = c(par("fg"), col[seq_along(sds) + 1])
  # )

  legend(
    "bottomright", bg = dipsaus::col2hexStr(par("bg"), alpha = 0.7), box.col = NA,
    c(sprintf("SDev=%.0f", sd), sprintf("\u00B1 %s SDev", sds)),
    col = c(NA, col[seq_along(sds) + 1]), lty = c(NA, 1, 1),
    text.col = c(par("fg"), col[seq_along(sds) + 1]), horiz = TRUE
  )

  return(invisible(list( sd = sd )))

}


fastplot_pwelch <- function(
    pwelch_overall, xlim = c(0, 400),
    xticks = c(0.1, 0.5, 1, 2, 4, 8, 12, 16, 30, 60, 80,
               120, 150, 200, 250, 300, 350, 400, 500),
    add_lm = FALSE, ...
) {
  theme <- ravedash::current_shiny_theme()
  fg <- theme$foreground
  bg <- theme$background
  graphics::par(fg = fg, bg = bg, col.axis = fg, col.lab = fg, col.main = fg, col.sub = fg)

  freq <- pwelch_overall$freq
  xticks <- c(xticks, max(freq))
  pwelch_ranges <- plot(pwelch_overall, xticks = xticks, xlim = xlim, ...)
  if(add_lm) {
    sel <- (freq < 6) | (freq < 35 & freq > 20)
    # spec <- 20 * log10(pwelch_overall$spec)
    spec <- pwelch_overall$spec_db
    if(any(sel)) {
      flm <- stats::lm(spec[sel] ~ log10(freq[sel]))
      abline(flm, lty = 3, col = "dodgerblue3")
    }
  }
  pwelch_ranges
}
