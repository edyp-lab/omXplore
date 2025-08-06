#' @title Convert datasets exported by the package `Prostar`
#'
#' @name Prostar-1x-compatible
#'
#' @param obj An instance of the class `MSnSet`
#' @param se An instance of the class `SummarizedExperiment`
#' @param mae An instance of the class `MultiAssayExperiment`
#'
#' @importFrom SummarizedExperiment rowData colData assays
#' @importFrom MSnbase fData pData
#' @importFrom MSnbase pData
#'
#' @examples
#' data(sub_R25)
#'
NULL


#' @rdname Prostar-1x-compatible
#' @export
#' @return An enriched instance of the class `SummarizedExperiment`
#' @importFrom MSnbase exprs pData fData
#' @importFrom MultiAssayExperiment DataFrame
#' @importFrom SummarizedExperiment rowData
#'
SE_Compatibility_with_Prostar_1.x <- function(obj, se) {
  stopifnot(inherits(obj, "MSnSet") && inherits(se, "SummarizedExperiment"))
  # Checks if metacell data exists
  # If exists, remove them from rowData and stores it
  #  as a DataFrame
  tryCatch(
    {
      ind.metacell <- grep("metacell_", colnames(MSnbase::fData(obj)))
      if (length(ind.metacell) > 0) {
        .metacell <- MultiAssayExperiment::DataFrame(MSnbase::fData(obj)[, ind.metacell])
        SummarizedExperiment::rowData(se) <- SummarizedExperiment::rowData(se)[, -ind.metacell]
        SummarizedExperiment::rowData(se)[["metacell"]] <- .metacell
      }
    },
    warning = function(w) {
      print(w)
      MultiAssayExperiment::DataFrame()
    },
    error = function(e) {
      print(e)
      MultiAssayExperiment::DataFrame()
    }
  )


  # # Checks if metacell data exists
  # # If exists, remove them from rowData and stores it
  # #  as a DataFrame
  # tryCatch({
  #   X <- experimentData(obj)@other$matAdj
  #   SummarizedExperiment::rowData(se)[['adjacencyMatrix']] <- DataFrame(X)
  # },
  #   warning = function(w) matrix(),
  #   error = function(e) matrix()
  # )


  # # Checks if metacell data exists
  # # If exists, remove them from rowData and stores it
  # #  as a DataFrame
  # tryCatch({
  #   metadata(se)[['cc']] <- experimentData(obj)@other$CC$allPep
  # },
  #   warning = function(w) list(),
  #   error = function(e) list()
  # )


  tryCatch(
    {
      adjMatrices <- experimentData(obj)@other$CC
      metadata(se)[["type"]] <- experimentData(obj)@other$typeOfData
    },
    warning = function(w) NA,
    error = function(e) NA
  )


  tryCatch(
    {
      metadata(se)[["pkg_version"]] <- experimentData(obj)@other$Prostar_Version
    },
    warning = function(w) NA,
    error = function(e) NA
  )


  tryCatch(
    {
      metadata(se)[["proteinId"]] <- experimentData(obj)@other$proteinId
    },
    warning = function(w) NA,
    error = function(e) NA
  )


  tryCatch(
    {
      metadata(se)[["colID"]] <- experimentData(obj)@other$idcol
    },
    warning = function(w) NA,
    error = function(e) NA
  )


  tryCatch(
    {
      SummarizedExperiment::colData(se)["group"] <- MSnbase::pData(obj)$Condition
    },
    warning = function(w) MultiAssayExperiment::DataFrame(),
    error = function(e) MultiAssayExperiment::DataFrame()
  )


  se
}



#' @rdname Prostar-1x-compatible
#' @export
#' @return An enriched instance of the class `MultiAssayExperiment`
#'
MAE_Compatibility_with_Prostar_1x <- function(obj, mae) {
  stopifnot(inherits(obj, "MSnSet") &&
    inherits(mae, "MultiAssayExperiment"))

  SummarizedExperiment::colData(mae)["group"] <- tryCatch(
    {
      MSnbase::pData(obj)$Condition
    },
    warning = function(w) .colData,
    error = function(e) .colData
  )

  mae
}
