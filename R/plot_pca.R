#' @title PCA plots
#' @importFrom stats na.omit
#'
#' @param res.pca The result of the function `FactoMineR::PCA()`
#' @param qdata A data.frame() of quantitative data
#' @param group A vector with the name of samples
#' @param var.scaling A boolean indicating whether to scale the data or not
#' @param ncp See `FactoMineR::PCA()`
#' @param chosen.axes See the parameter 'axes' of the function 
#' `factoextra::fviz_pca_var()`
#'
#' @name ds-pca
#'
#' @examples
#' data(vdata)
#' obj <- vdata[[1]]
#' res.pca <- wrapper_pca(SummarizedExperiment::assay(obj), get_group(obj))
#' plotPCA_Eigen(res.pca)
#' plotPCA_Var(res.pca)
#' plotPCA_Eigen_hc(res.pca)
#' plotPCA_Ind(res.pca)
#'
NULL



#' @export
#' @return The result of the function FactoMineR::PCA()
#' @rdname ds-pca
#' @import FactoMineR
#'
wrapper_pca <- function(
    qdata,
  group,
    var.scaling = TRUE,
    ncp = NULL) {
  

  if (missing(qdata)) {
    stop("'qdata' is missing.")
  }

  stopifnot(inherits(qdata, "matrix"))
  

  if (is.null(var.scaling)) {
    var.scaling <- TRUE
  }

  res.pca <- NULL

  # if (length(which(is.na(obj@qdata))) > 0) {
  if (is.null(ncp)) {
    nmax <- 12
    y <- qdata
    nprot <- dim(y)[1]
    n <- dim(y)[2] # If too big, take the number of conditions.

    if (n > nmax) {
      n <- length(unique(group))
    }

    ncp <- min(n, nmax)
  }

  res.pca <- FactoMineR::PCA(qdata,
    scale.unit = var.scaling,
    ncp = ncp,
    graph = FALSE
  )

  return(res.pca)
}




#' @export
#' @import highcharter
#'
#' @rdname ds-pca
#' @return A plot
#'
plotPCA_Eigen <- function(res.pca) {
  stopifnot(!is.null(res.pca))

  hc <- highcharter::highchart() %>%
    highcharter::hc_yAxis_multiples(
      list(
        title = list(text = "% of variances"),
        lineWidth = 0,
        labels = list(format = "{value}%"),
        max = 100
      ),
      list(
        title = list(text = "Cumulative % of variances"),
        opposite = FALSE,
        max = 100
      ),
      list(
        title = list(text = "Eigen values"),
        opposite = TRUE,
        labels = list(format = "{value}%")
      )
    ) %>%
    highcharter::hc_xAxis(
      title = "Principal Components",
      categories = rownames(res.pca$eig)
    ) %>%
    highcharter::hc_add_series(data.frame(y = res.pca$eig[, 2]),
      type = "column",
      name = "% of variances",
      yAxis = 0
    ) %>%
    highcharter::hc_add_series(data.frame(y = res.pca$eig[, 3]),
      type = "line",
      color = "darkblue",
      name = "Cumulative % of variances",
      marker = "diamond",
      color = "#FF7900",
      yAxis = 0
    ) %>%
    highcharter::hc_legend(enabled = TRUE)

  hc
}





#' @return A plot
#'
#' @rdname ds-pca
#' @export
#' @import factoextra
#'
plotPCA_Var <- function(res.pca, chosen.axes = c(1, 2)) {
  # plot.PCA(res.pca, choix="var", axes = chosen.axes,
  # title="Sample factor map (PCA)")
  # Colorer en fonction du cos2: qualite de representation
  if (is.null(res.pca)) {
    return(NULL)
  }
  factoextra::fviz_pca_var(
    res.pca,
    axes = chosen.axes,
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Evite le chevauchement de texte
  )
}




#' @export
#' @rdname ds-pca
#' @return A plot
#' @import factoextra
#'
plotPCA_Ind <- function(res.pca, chosen.axes = c(1, 2)) {
  if (is.null(res.pca)) {
    return(NULL)
  }

  factoextra::fviz_pca_ind(res.pca,
    axes = chosen.axes,
    geom = "point"
  )
}





#' @import highcharter
#' @rdname ds-pca
#' @return A plot
#' @export
#'
plotPCA_Eigen_hc <- function(res.pca) {
  if (is.null(res.pca)) {
    return(NULL)
  }
  hc <- highchart() %>%
    hc_yAxis_multiples(
      list(
        title = list(text = "% of variances"),
        lineWidth = 0,
        labels = list(format = "{value}%"), max = 100
      ),
      list(
        title = list(text = "Cumulative % of variances"),
        opposite = FALSE,
        max = 100
      ),
      list(
        title = list(text = "Eigen values"),
        opposite = TRUE,
        labels = list(format = "{value}%")
      )
    ) %>%
    hc_xAxis(
      title = "Principal Components",
      categories = rownames(res.pca$eig)
    ) %>%
    hc_add_series(
      data.frame(y = res.pca$eig[, 2]),
      type = "column",
      name = "% of variances",
      yAxis = 0
    ) %>%
    hc_add_series(
      data.frame(y = res.pca$eig[, 3]),
      type = "line",
      color = "darkblue",
      name = "Cumulative % of variances",
      marker = "diamond",
      color = "#FF7900",
      yAxis = 0
    ) %>%
    hc_legend(enabled = TRUE)
}
