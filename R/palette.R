#' @title Palette for samples.
#'
#' @description
#'
#' Builds a vector of `#conditions` colors for a set of samples. One color is
#' given for a given condition. This function extends a base palette from the
#' package [RColorBrewer] to 'n' colors.
#' The colors in the returned palette are always in the same order
#'
#' @param n The number of desired colors in the palette
#'
#' @param pal.name A `character(1)` which is the name of the palette from the
#' package [RColorBrewer] from which the colors are taken. Default value
#' is 'Set1'.
#'
#' @param conds A `character()` of conditions. The length of the vector is
#' the number of samples in the dataset.
#'
#' @param pal A vector of HEX color code that form the basis palette from which
#' to build the complete color vector for the conditions.
#'
#' @name palette
#'
#' @examples
#' if (interactive()) {
#' #-----------------------------------------------
#' # Builds a palette for a dataset with 3 conditions
#' # of 3 samples each.
#' #------------------------------------------------
#'
#' conds <- c(rep("A", 3), rep("B", 3), rep("C", 3))
#' SampleColors(conds)
#' SampleColors(conds, "Dark2")
#'
#'
#' #-----------------------------------------------
#' # Extend the default palette to 12 colors
#' #-----------------------------------------------
#'
#' ExtendPalette(12)
#'
#'
#' data(vdata)
#' conds <- get_group(vdata)
#' GetColorsForConditions(conds, ExtendPalette(2))
#' }
#'
NULL


#' @rdname palette
#' @export
#' @return A vector
#'
SampleColors <- function(conds, pal.name = "Set1") {
  if (missing(pal.name) || is.null(pal.name)) {
    pal.name <- "Set1"
  }

  palette.ex <- NULL
  n.colors <- length(unique(conds))
  base.pal <- ExtendPalette(n.colors, pal.name)
  for (i in seq_len(n.colors)) {
    ind <- which(conds == unique(conds)[i])
    palette.ex[ind] <- base.pal[i]
  }

  return(palette.ex)
}


#' @export
#'
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom grDevices colorRampPalette
#' @importFrom utils combn
#'
#' @rdname palette
#' @return A vector
#'
ExtendPalette <- function(n, pal.name = "Set1") {
  stopifnot(is.numeric(n))

  if (is.null(pal.name) ||
    !(pal.name %in% rownames(RColorBrewer::brewer.pal.info))) {
    pal.name <- "Set1"
  }

  extended.pal <- NULL

  nMaxColors <- RColorBrewer::brewer.pal.info[pal.name, "maxcolors"]

  limit <- nMaxColors * (nMaxColors - 1) / 2
  if (n > limit) {
    stop("Number of colors exceed limit of ", limit, " colors per palette.")
  }

  if (n > nMaxColors) {
    extended.pal <- RColorBrewer::brewer.pal(nMaxColors, pal.name)
    allComb <- combn(extended.pal, 2)

    for (i in seq(n - nMaxColors)) {
      extended.pal <- c(
        extended.pal,
        grDevices::colorRampPalette(allComb[, i])(3)[2]
      )
    }
  } else {
    extended.pal <- RColorBrewer::brewer.pal(nMaxColors, pal.name)[seq(n)]
  }
  extended.pal
}



#' @export
#' @return A vector
#' @rdname palette
#' @import RColorBrewer
#'
GetColorsForConditions <- function(conds, pal = NULL) {
  if (missing(conds)) {
    stop("'conds' is required")
  }

  if (!is.null(pal) && length(unique(conds)) != length(pal)) {
    stop("The length of `conds` must be equal to the length
            of `base_palette`.")
  }

  if (is.null(pal)) {
    pal <- ExtendPalette(length(unique(conds)))
  }

  myColors <- NULL
  for (i in seq_len(length(conds))) {
    myColors[i] <- pal[which(conds[i] == unique(conds))]
  }
  return(myColors)
}
