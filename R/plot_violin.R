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
    if (!inherits(data, "matrix") || is.null(data)) {
        return(NULL)
    }
    
    if (missing(conds)) {
        stop("'conds' is missing.")
    }
    
    legend <- colnames(data)
    myColors <- SampleColors(conds, pal.name)
    
    ## Smaller margins
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mar = c(8, 4, 1, 1))
    
    yrange <- range(data, na.rm = TRUE)
    ypad <- diff(yrange) * 0.05   # 5% padding
    
    ## Empty plotting area
    plot(
        NA,
        xlim = c(0.5, ncol(data) + 0.5),
        ylim = c(yrange[1] - ypad, yrange[2] + ypad),
        xlab = "",
        ylab = "Log (intensity)",
        xaxt = "n",
        yaxt = "n",
        bty = "l",
        xaxs = "i",
        yaxs = "i"
    )
    
    ## Draw violins
    for (i in seq_len(ncol(data))) {
        vioplot::vioplot(
            na.omit(data[, i]),
            col = myColors[i],
            add = TRUE,
            at = i,
            trim = TRUE
        )
    }
    
    ## Y axis
    axis(
        side = 2,
        las = 1
    )
    
    ## X axis ticks only
    axis(
        side = 1,
        at = seq_len(ncol(data)),
        labels = FALSE
    )
    
    ## Rotated labels
    text(
        x = seq_len(ncol(data)),
        y = par("usr")[3] -
            0.05 * diff(par("usr")[3:4]),
        labels = legend,
        srt = 45,
        adj = 1,
        xpd = TRUE,
        cex = 0.9
    )
    
    ## Highlight selected rows
    if (!is.null(subset)) {
        
        pal.tracker <- ExtendPalette(length(subset), "Dark2")
        
        for (n in seq_along(subset)) {
            
            i <- subset[n]
            
            for (c in seq_len(ncol(data) - 1)) {
                
                segments(
                    x0 = c,
                    y0 = data[i, c],
                    x1 = c + 1,
                    y1 = data[i, c + 1],
                    col = pal.tracker[n],
                    lwd = 2
                )
                
                points(
                    c,
                    data[i, c],
                    pch = 16,
                    col = pal.tracker[n]
                )
            }
            
            points(
                ncol(data),
                data[i, ncol(data)],
                pch = 16,
                col = pal.tracker[n]
            )
        }
        
        legend(
            "topleft",
            legend = rownames(data)[subset],
            lty = 1,
            lwd = 2,
            col = pal.tracker,
            pch = 16,
            bty = "n"
        )
    }
}
