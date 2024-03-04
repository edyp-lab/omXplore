#' @title xxxx
#' @description xxx
#' 
#' @name Prostar-1x-compatible
#' 
#' @param obj An instance of the class `MSnSet`
#' @param se An instance of the class `SummarizedExperiment`
#' @param mae An instance of the class `MultiAssayExperiment`
#' 
#' @examples
#' NULL
#' 
#' 
#' 
NULL


#' @rdname Prostar-1x-compatible
#' @export
SE_Compatibility_with_Prostar_1.x <- function(obj, se){
  
  stopifnot(inherits(obj, 'MSnSet') && inherits(se, 'SummarizedExperiment'))
  # Checks if metacell data exists
  # If exists, remove them from rowData and stores it
  #  as a DataFrame
  tryCatch({
    ind.metacell <- grep('metacell_', colnames(fData(obj)))
    .metacell <- DataFrame(fData(obj)[, ind.metacell])
    if (length(ind.metacell) > 0){
      rowData(se) <- rowData(se)[, -ind.metacell]
      rowData(se)[['metacell']] <- .metacell
    }
  },
    warning = function(w) DataFrame(),
    error = function(e) DataFrame()
  )
  
  
  # Checks if metacell data exists
  # If exists, remove them from rowData and stores it
  #  as a DataFrame
  tryCatch({
    adjMatrices <- experimentData(obj)@other$matAdj
    rowData(se)[['adjacencyMatrix']] <- adjMatrices[[1]]
  },
    warning = function(w) matrix(),
    error = function(e) matrix()
  )
  
  
  # Checks if metacell data exists
  # If exists, remove them from rowData and stores it
  #  as a DataFrame
  tryCatch({
    adjMatrices <- experimentData(obj)@other$CC
    metadata(se)[['cc']] <- experimentData(obj)@other$CC
  },
    warning = function(w) list(),
    error = function(e) list()
  )
  
  
  tryCatch({
    adjMatrices <- experimentData(obj)@other$CC
    metadata(se)[['type']] <- experimentData(obj)@other$typeOfData
  },
    warning = function(w) NA,
    error = function(e) NA
  )
  
  
  tryCatch({
    metadata(se)[['pkg_version']] <- experimentData(obj)@other$Prostar_Version
  },
    warning = function(w) NA,
    error = function(e) NA
  )
  
  
  tryCatch({
    metadata(se)[['proteinId']] <- experimentData(obj)@other$proteinId
  },
    warning = function(w) NA,
    error = function(e) NA
  )
  
  
  tryCatch({
    metadata(se)[['colID']] <- experimentData(obj)@other$keyId
  },
    warning = function(w) NA,
    error = function(e) NA
  )
  
  
  tryCatch({
    colData(se)['group'] <- pData(obj)$Condition
  },
    warning = function(w) DataFrame(),
    error = function(e) DataFrame()
  )
  
  
  se
  
}



#' @rdname Prostar-1x-compatible
#' @export
MAE_Compatibility_with_Prostar_1x <- function(obj, mae){
  
  stopifnot(inherits(obj, 'MSnSet') && 
      inherits(mae, 'MultiAssayExperiment'))
  
  colData(mae)['group'] <- tryCatch({
    pData(obj)$Condition
  },
    warning = function(w) .colData,
    error = function(e) .colData
  )
  
  mae
}