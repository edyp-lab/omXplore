#'
#' @return A violin plot
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#' 
#' @param data xxxx
#'
#' @rdname intensity-plots
#' @import vioplot
#' @importFrom graphics plot.new plot.window title axis segments points legend
#'
violinPlot <- function(
    data,
    conds,
    subset = NULL,
    pal.name = "Set1") {
  if(!inherits(data, "matrix") || is.null(data))
    return(NULL)

  legend <- colnames(data)

  if (missing(conds)) {
    stop("'conds' is missing.")
  }


  myColors <- SampleColors(conds, pal.name)
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, ncol(data) + 1),
    ylim = c(
      min(na.omit(data)),
      max(na.omit(data))
    )
  )

  graphics::title(ylab = "Log (intensity)")
  for (i in seq_len(ncol(data))) {
    vioplot::vioplot(na.omit(data[, i]),
      col = myColors[i],
      add = TRUE,
      at = i
    )
  }

  graphics::axis(2,
    yaxp = c(
      floor(min(na.omit(data))),
      floor(max(na.omit(data))), 5
    ),
    las = 1
  )

  graphics::axis(
    side = 1,
    at = seq_len(ncol(data)),
    labels = legend
  )

  # Display of rows to highlight (index of row in subset)
  if (!is.null(subset)) {
    pal.tracker <- ExtendPalette(length(subset), "Dark2")
    n <- 0
    for (i in subset) {
      n <- n + 1
      for (c in seq_len(ncol(data) - 1)) {
        graphics::segments(
          y0 = data[i, c],
          y1 = data[i, c + 1],
          x0 = c,
          x1 = c + 1,
          pch = 16,
          col = pal.tracker[n],
          lwd = 2
        )
        graphics::points(
          y = data[i, c],
          x = c,
          pch = 16,
          col = pal.tracker[n]
        )
      }
      graphics::points(
        y = data[i, ncol(data)],
        x = ncol(data),
        pch = 16,
        col = pal.tracker[n]
      )
    }
    graphics::legend("topleft",
      legend = rownames(data)[subset],
      lty = 1,
      lwd = 2,
      col = pal.tracker,
      pch = 16,
      bg = "transparent",
      bty = "n"
    )
  }
}
