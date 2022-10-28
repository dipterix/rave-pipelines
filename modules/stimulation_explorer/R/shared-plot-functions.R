group_colors <- c("orange", "dodgerblue3", "darkgreen", "orangered", "brown", "purple3")

interpolate_color <- function(col, n, ...) {
  if(length(col) >= n) {
    return(col[seq_len(n)])
  }
  ncolors <- length(col)
  ratio <- ceiling(n / ncolors)
  col <- matrix(grDevices::colorRampPalette(colors = col)(ncolors * ratio),
                nrow = ratio, byrow = FALSE)
  return(t(col)[seq_len(n)])
}

plot_trial_by_time_per_condtion <- function(
    plot_data, col = group_colors, time_range = NULL, value_range = NULL,
    style = c("vstack", "shared")
) {
  style <- match.arg(style)
  # plot_data <- plot_data_trial_by_time
  group_data <- plot_data$group_data
  time <- plot_data$time
  sample_rate <- plot_data$sample_rate
  group_order <- plot_data$group_order
  group_names <- plot_data$group_names
  ngroups <- length(group_data)
  print(group_order)
  col <- interpolate_color(col, max(group_order))
  colors <- col[group_order]
  # ylim <- max(abs(unlist(lapply(group_data, "[[", 'range')))) * c(-1, 1)

  sel <- rep(TRUE, length(time))
  if(length(time_range)) {
    time_range <- time_range[!is.na(time_range)]
    if(length(time_range) >= 2) {
      sel <- time >= min(time_range) & time <= max(time_range)
    }
  }

  # obtain plotting data
  group_signals <- plot_data$group_mean[sel, , drop = FALSE]
  time_sel <- time[sel]

  # calculate ylim
  if(length(value_range)) {
    value_range <- value_range[is.finite(value_range)]
  }
  if(length(value_range) >= 2) {
    ylim <- range(value_range)
  } else if(length(value_range) == 1){
    ylim <- abs(value_range) * c(-1, 1)
  } else {
    tmp <- group_signals[is.finite(group_signals)]
    if(!length(tmp)) {
      if(dipsaus::shiny_is_running()) {
        shiny::validate(
          shiny::need(length(tmp) > 0, message = "No signal to plot")
        )
      } else {
        message("No signal to plot")
      }
      return(invisible())
    }
    ylim <- max(abs(range(tmp))) * c(-1, 1)
  }
  if(style == "vstack") {
    group_signals <- -t(group_signals)
    group_signals <- group_signals[rev(seq_len(ngroups)), , drop = FALSE]
    spacing <- ylim[[2]] - ylim[[1]]
    ravetools::plot_signals(
      signals = group_signals, sample_rate = sample_rate,
      col = rev(colors), space = spacing,
      space_mode = "absolute", time_shift = min(time_sel), ylab = "",
      channel_names = sprintf("Group %d", rev(seq_along(group_names))),
      main =  "Voltage over time"
    )
    text(x = quantile(time_sel, 0.01), y = spacing * seq_along(group_names),
         labels = rev(group_names), pos = 4,
         col = dipsaus::col2hexStr("black", 0.3))
    abline(v = 0, lty = 3, col = 'gray60')
  } else {
    plot(x = range(time_sel), y = rev(ylim), ylim = rev(ylim),
         type = 'n', xlab = "Time (s)", ylab = "", axes = FALSE,
         main = "Voltage over time")


    lapply(group_data, function(g){
      lines(time_sel, g$mean[sel], type = 'l', lty = 1, col = col[g$order])
    })
    axis(side = 1, at = pretty(time_sel))
    axis(side = 2, at = ylim[[1]], labels = sprintf("%.0f", ylim[[1]]), las = 1,
         col.axis = "gray60")
    axis(side = 2, at = pretty(ylim), las = 1)

    abline(v = 0, lty = 3, col = 'gray60')
    legend("topleft", bty = "n", lty = 1, col = colors, legend = group_names)
  }

  return(invisible(list(
    style = style,
    ylim = ylim,
    ngroups = ngroups
  )))
}
