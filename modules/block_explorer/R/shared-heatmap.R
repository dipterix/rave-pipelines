fastplot_power_over_freq_time <- function(
    data, time, freq, col = grDevices::rainbow(201), zlim = NULL,
    main = "Power (dB) over frequency & time", compress = TRUE
) {
  stopifnot(
    nrow(data) == length(time) &&
      (length(freq) == 1 || ncol(data) == length(freq))
  )
  # theme <- ravedash::current_shiny_theme()
  theme <- list(
    foreground = "black",
    background = 'white'
  )
  fg <- theme$foreground
  bg <- theme$background
  old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main",
                               "col.sub", "mai", "mar", "mgp"))
  rg <- range(data)
  sd <- sd(data)

  if(!length(zlim)) {
    zlim <- quantile(data, c(0.005, 0.995))
  }

  zlim <- c(-1, 1) * max(abs(zlim))

  data[data < zlim[1]] <- zlim[1]
  data[data > zlim[2]] <- zlim[2]

  # compress image
  if( compress ) {
    n_freq <- length(freq)
    n_timepoints <- length(time)
    if( n_freq * n_timepoints > 50000 && n_timepoints >= 16 ) {
      decimate_factor <- ceiling(n_freq * n_timepoints / 50000)
      tidx <- seq(1, n_timepoints, by = decimate_factor)
      data <- data[tidx, , drop = FALSE]
      time <- time[tidx]
    }
  }


  layout(matrix(c(2, 1), nrow = 1), widths = c(1, lcm(2)))
  graphics::par(fg = fg, bg = bg, col.axis = fg, col.lab = fg, col.main = fg,
                col.sub = fg, mai = c(0.8, 0.5, 0.42, 0.1))
  # on.exit({
  #   try({
  #     layout(matrix(1, 1))
  #     do.call(graphics::par, old_theme)
  #   }, silent = TRUE)
  # }, add = TRUE)

  par(mar = c(2.6, 0.2, 2.1, 2), mgp = c(2, 0.5, 0))
  legend_data <- seq(zlim[1], zlim[2], length.out = length(col))
  graphics::image(
    y = legend_data,
    z = matrix(legend_data, nrow = 1),
    col = col,
    axes = FALSE, useRaster = TRUE
  )
  axis(side = 4, at = c(zlim, -2*sd, 2*sd, 0),
       labels = c(sprintf("%.0f", c(zlim, -2*sd, 2*sd)), "0"),
       las = 1, hadj = 0.1, tck = -0.1)
  axis(side = 3, at = 0.5, labels = sprintf("[%.0f:%.0f]", rg[1], rg[2]),
       tick = FALSE, padj = 0.2)

  par(mar = c(2.6, 3.5, 2.1, 0.1), mgp = c(2, 0.5, 0))
  image(
    x = time, y = freq, z = data, zlim = zlim,
    xlab = "", ylab = "",
    useRaster = FALSE, col = col,
    axes = FALSE
  )
  mtext(side = 2, text = "Frequency", line = 2.5)
  mtext(side = 1, text = "Time (s)", line = 1.5)
  axis(1, at = pretty(time), tck = -0.03)
  title(main, line = 0.5, adj = 0)

  axis(2, at = freq, las = 1, tck = -0.03)
}

