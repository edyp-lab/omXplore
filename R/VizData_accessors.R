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
setMethod("GetSlotSampleMap", signature = "ANY",
  function(object) 
    NULL
)

setMethod("GetSlotSampleMap", signature = "MSnSet",
  function(object) 
    pData(object)
)

setMethod("GetSlotSampleMap", signature = "SummarizedExperiment",
  function(object) 
    NULL
)


setMethod("GetSlotSampleMap", signature = "MultiAssayExperiment",
  function(object) 
    colData(object)
)

setMethod("GetSlotSampleMap", signature = "QFeatures",
  function(object) 
    colData(object)
)






# -------------------------------------------------------------------
#          Methods to get Metadata
#--------------------------------------------------------------------
setMethod("GetSlotSampleMap", signature = "ANY",
  function(object) NULL
)

setMethod("GetSlotSampleMap", signature = "MSnSet",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "SummarizedExperiment",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "MultiAssayExperiment",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "QFeatures",
  function(object) object@metadata
)



# -------------------------------------------------------------------
#          Methods to get Experiment Data
#--------------------------------------------------------------------
setMethod("GetSlotSampleMap", signature = "ANY",
  function(object) NULL
)

setMethod("GetSlotSampleMap", signature = "MSnSet",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "SummarizedExperiment",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "MultiAssayExperiment",
  function(object) object@metadata
)

setMethod("GetSlotSampleMap", signature = "QFeatures",
  function(object) object@metadata
)








#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A data.frame
#'
setMethod("GetSlotMetadata",
  signature = "VizData",
  function(object) object@metadata
)



#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A data.frame
#'
setMethod("GetSlotQdata",
  signature = "VizData",
  function(object) object@qdata
)




#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A data.frame
#'
setMethod("GetSlotMetacell",
  signature = "VizData",
  function(object) object@metacell
)




#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotColID",
  signature = "VizData",
  function(object) object@colID
)




#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotType",
  signature = "VizData",
  function(object) object@type
)





#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotProteinID",
  signature = "VizData",
  function(object) object@proteinID
)



#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotConds",
  signature = "VizData",
  function(object) object@conds
)



#' @param object An instance of class `ANY`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotCc",
  signature = "ANY",
  function(object) NULL
)

#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotCc", signature = "VizData",
  function(object) object@cc
)



#' @param object An instance of class `ANY`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotAdjMat", signature = "ANY",
  function(object) NULL
)

#' @param object An instance of class `VizData`.
#' @rdname accessors
#' @return A character(0)
#'
setMethod("GetSlotAdjMat", signature = "VizData",
  function(object) object@adjMat
)





#' #' @title xxx
#' #' @description xxx
#' #' @param object An object that inherits `MultiAssayExperiment` class
#' #' (e.g. MultiAssayExperiment, QFeatures)
#' #'
#' #' @return An object of class VizData.
#' #' @export
#' #' @examples
#' #' NULL
#' #'
#' ExtractInfos <- function(object) {
#'   
#'     args <- list(
#'         qdata = GetSlotQdata(object),
#'         metacell = GetSlotMetacell(object),
#'         metadata = GetSlotMetadata(object),
#'         colID = GetSlotColID(object),
#'         proteinID = GetSlotProteinID(object),
#'         conds = GetSlotConds(object),
#'         type = GetSlotType(object),
#'         adjMat = NULL,
#'         cc = list()
#'       )
#' 
#'     if (!is.null(colnames(args$metacell))){
#'       cols2delete <- which(colnames(args$metacell) == colnames(args$metadata))
#'       if (length(cols2delete) > 0) {
#'           args$metadata <- args$metadata[, -cols2delete]
#'       }
#'     }
#' 
#'       # Delete adjacency Matrix from whole metadata
#'       if ("adjacencyMatrix" %in% names(args$metadata)) {
#'         .ind <- which(names(args$metadata) == "adjacencyMatrix")
#'         args$adjMat <- args$metadata[, "adjacencyMatrix"]
#'          args$metadata <- args$metadata[, -.ind]
#'       } else if (!is.null(args$proteinID) && args$proteinID != "") {
#'         .arg <- args$metadata[, args$proteinID]
#'         args$adjMat <- PSMatch::makeAdjacencyMatrix(.arg)
#'         rownames(args$adjMat) <- rownames(args$metadata)
#'       }
#' 
#'         if(!is.null(args$adjMat)){
#'           # Create the connected components
#'           args$cc <- PSMatch::ConnectedComponents(args$adjMat)@adjMatrices
#'         }
#' 
#'       # Fix typos
#'       args$metadata <- as.data.frame(args$metadata)
#' 
#' 
#'   return(args)
#' }
