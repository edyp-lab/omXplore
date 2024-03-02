#' @title Accessors functions for class `VizData`
#' @description Functions to access the slots of the class `VizData`.
#' @return See individual method description for the return value.
#' @param object An object of class `VizData`
#' @param ... xxxx description
#' @name accessors
#'
#'
#' @aliases GetSlotMetadata, GetSlotMetacell, GetSlotQdata, GetSlotProteinID,
#' GetSlotColID, GetSlotConds, GetSlotAdjMat, GetSlotCc
#'
#' @return If exists, the slot value requested.
#' @examples
#'
#' ## -----------------------------------
#' ## Accessing slots from a MSnSet dataset
#' ## -----------------------------------
#' data(vdata)
#' metadata <- GetSlotMetadata(vdata[[1]])
#' qdata <- GetSlotQdata(vdata[[1]])
#' metacell <- GetSlotMetacell(vdata[[1]])
#' id <- GetSlotColID(vdata[[1]])
#' type <- GetSlotType(vdata[[1]])
#' proteinID <- GetSlotProteinID(vdata[[1]])
#' conds <- GetSlotConds(vdata[[1]])
#'
NULL


# -------------------------------------------------------------------
#          Methods to get Sample Map
#--------------------------------------------------------------------
setMethod("ExtractSampleMap", signature = "ANY",
  function(object) 
    NULL
)

setMethod("ExtractSampleMap", signature = "MSnSet",
  function(object) 
    pData(object)
)

setMethod("ExtractSampleMap", signature = "SummarizedExperiment",
  function(object) 
    NULL
)


setMethod("ExtractSampleMap", signature = "MultiAssayExperiment",
  function(object) 
    sampleMap(object)
)

setMethod("ExtractSampleMap", signature = "QFeatures",
  function(object) 
    sampleMap(object)
)



# -------------------------------------------------------------------
#          Methods to get Metadata
#--------------------------------------------------------------------
setMethod("ExtractMetadata", signature = "ANY",
  function(object) 
    NULL
)

setMethod("ExtractMetadata", signature = "MSnSet",
  function(object) 
    experimentData(object)@other
)

setMethod("ExtractMetadata", signature = "SummarizedExperiment",
  function(object) 
    NULL
)

setMethod("ExtractMetadata", signature = "MultiAssayExperiment",
  function(object) 
    metadata(object) 
)

setMethod("ExtractMetadata", signature = "QFeatures",
  function(object) 
    metadata(object) 
)



# -------------------------------------------------------------------
#          Methods to get Experiment Data
#--------------------------------------------------------------------
setMethod("ExtractExperiments", signature = "ANY",
  function(object) 
    NULL
)

setMethod("ExtractExperiments", signature = "MSnSet",
  function(object) 
    exprs(object)
)

setMethod("ExtractExperiments", signature = "SummarizedExperiment",
  function(object) 
    experiments(object)
)

setMethod("ExtractExperiments", signature = "MultiAssayExperiment",
  function(object) 
    experiments(object)
)

setMethod("ExtractExperiments", signature = "QFeatures",
  function(object) 
    experiments(object)
)



# -------------------------------------------------------------------
#          Methods to get colData
#--------------------------------------------------------------------
setMethod("ExtractColData", signature = "ANY",
  function(object) 
    NULL
)

setMethod("ExtractColData", signature = "MSnSet",
  function(object) 
    fData(object)
)

setMethod("ExtractColData", signature = "SummarizedExperiment",
  function(object) 
    NULL
)


setMethod("ExtractColData", signature = "MultiAssayExperiment",
  function(object) 
    colData(object)
)

setMethod("ExtractColData", signature = "QFeatures",
  function(object) 
    colData(object)
)
