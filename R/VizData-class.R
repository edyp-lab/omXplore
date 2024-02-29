##' @title VizData class
##'
##' @description
##'
##'  Conceptually, a `VizData` object is a standard representation of all
##'  elements from quantitative other structured data used in proteomics, such
##'  as `MSnset`, `QFeatures`, `SummarizedExperiment` or `MultAssayExperiment`.
##'  It allows to use it as a generic converter.
##'
##'  `VizData` objects are not usually created by hand but are created from
##'  the `VizList` which is the class accessible by the user.
##'
##'  The recommended way to create `VizData` objects is the use the
##' function `convert2viz()` of the class `VizList`
##'
##' @name VizData-class
##'
##' @param object An instance of the class `VizData`
##'
##' @param x An instance of class [QFeatures].
##'
##' @param i `character()`, `integer()`, `logical()` or `GRanges()`
##'     object for subsetting by rows.
##'
##' @param j `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by `colData` rows.
##'
##' @param k `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by assays
##'
##' @param l `character()`, `logical()`, or `numeric()` vector for
##'     subsetting by assays
##'
##' @param ... Additional parameters
##'
##' @param drop logical (default `TRUE`) whether to drop empty assay
##'     elements in the `ExperimentList`.
##'
##' @return See individual method description for the return value.
##'
##' @seealso
##' The `VizData()` constructor and the `convert2VizList()`
##' function. The *omXplore* vignette provides an extended example.
##'
##' @exportClass VizData
##'
##' @author Samuel Wieczorek
##'
##' @examples
##' ## ------------------------
##' ## An empty VizData object
##' ## ------------------------
##'
##' VizData()
##'
##' ## -----------------------------------
##' ## Creating a VizData object manually
##' ## -----------------------------------
##' 
##' qdata <- matrix(1:30, ncol = 6, 
##' dimnames = list(paste0('prot_', 1:5), 
##' c(paste0('C1_R', 1:3), paste0('C2_R', 1:3))))
##' colnames(qdata) <- c(paste0('C1_R', 1:3), paste0('C2_R', 1:3))
##' metacell <- data.frame(matrix(rep('Missing POV', 30), ncol = 6), 
##' row.names = paste0('prot_', 1:5))
##' colnames(metacell) <- c(paste0('metacell_C1_R', 1:3), 
##' paste0('metacell_C2_R', 1:3))
##' metadata <- data.frame(protID = paste0('proteinID_', 1:5),
##' metadata1 = paste0('meta1_', 1:5),
##' metadata2 = paste0('meta2_', 1:5))
##' conds <- c(rep('C1', 3), rep('C2', 3))
##' colID <- 'protID'
##' proteinId <- 'protID'
##' type <- "protein"
##' adjMat <- matrix()
##' cc <- list()
##' 
##' obj <- VizData(qdata, metacell, metadata, colID, proteinId, type)
##'
##' @return An instance of class `VizData`
##'
NULL



##'
##' @rdname VizData-class
##' @export VizData
##' @exportClass VizData
##' @return An instance of the class `VizData`
VizData <- setClass(
  "VizData",
  ##' @slot qdata A matrix containing the quantitative data
  ##' @slot metacell A matrix of xxx
  ##' @slot metadata A matrix of xxxx
  ##' @slot colID The name of the column which contains the ids of each row.
  ##' @slot proteinID The name of the column in the metadata which represent the
  ##' (unique) id of the protein
  ##' @slot conds A vector of the names of the condition of each sample
  ##' of the dataset
  ##' @slot type The type of entity in the dataset (peptide, protein, ...)
  ##' @slot adjMat xxx
  ##' @slot cc A list of xxx

  representation(
    qdata = "matrix",
    metacell = "data.frame",
    metadata = "data.frame",
    colID = "character",
    proteinID = "character",
    conds = "character",
    type = "character",
    adjMat = "matrix",
    cc = "list"
  ),

  # Set the default values for the slots. (optional)
  prototype(
    qdata = matrix(),
    metacell = NULL,
    metadata = NULL,
    colID = NA_character_,
    proteinID = NA_character_,
    conds = NA_character_,
    type = NA_character_,
    adjMat = NULL,
    cc = list()
  ),

  ##' @param object xxx
  validity = function(object) {
    errors <- character()

    if (nrow(object@qdata) != nrow(object@metacell)) {
      msg <- "The slots 'qdata' and 'metacell' must have the same
        number of rows."
      errors <- c(errors, msg)
    }

    if (nrow(object@qdata) != nrow(object@metadata)) {
      msg <- "The slots 'qdata' and 'metadata' must have the same
        number of rows."
      errors <- c(errors, msg)
    }

    if (ncol(object@qdata) != length(object@conds)) {
      msg <- "The slots 'qdata' must have the same number of columns as
        the length of 'conds'."
      errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
  }
)


##' @exportMethod show
##' @rdname VizData-class
##' @importFrom crayon green 
##' @return NA
##'
setMethod(
  "show", "VizData",
  ##' @param object An instance of the class `VizData`
  ##' @exportMethod show
  ##'
  function(object) {
    showVizData(object)
  }
)



##' @title Initialization method for the class `VizData`
##' @rdname VizData-class
##' @return An instance of the class `VizData`
##'
setMethod(
  "initialize", "VizData",
  ##' @param .Object xxx
  ##' @param qdata xxx
  ##' @param metacell xxx
  ##' @param metadata xxx
  ##' @param colID xxx
  ##' @param proteinID xxx
  ##' @param conds xxx
  ##' @param type xxx
  ##' @param adjMat xxx
  ##' @param cc xxx
  function(
      .Object,
      qdata = matrix(),
      metacell = data.frame(),
      metadata = data.frame(),
      colID = "",
      proteinID = "",
      conds = "",
      type = "",
      adjMat = data.frame(),
      cc = list()) {
 

    if (is.null(qdata) || !inherits(qdata, "matrix")) {
      message('qdata must be a matrix')
      .Object@qdata <- matrix()
    } else {
      .Object@qdata <- qdata
    }

    if (is.null(metacell) || !inherits(metacell, "data.frame")) {
      .Object@metacell <- data.frame()
    } else {
      .Object@metacell <- metacell
    }

    if (is.null(metadata) || !inherits(metadata, "data.frame")) {
      .Object@metadata <- data.frame()
    } else {
      .Object@metadata <- metadata
    }

    if (is.null(colID) || colID == "" || length(colID) == 0 ||
      !inherits(colID, "character")) {
      .Object@colID <- ""
    } else {
      .Object@colID <- colID
    }

    if (is.null(proteinID) || length(proteinID) == 0 ||
      !inherits(proteinID, "character")) {
      .Object@proteinID <- ""
    } else {
      .Object@proteinID <- proteinID
    }

    if (is.null(conds) || !is.vector(conds) || length(conds) == 0) {
      .Object@conds <- ""
    } else {
      .Object@conds <- conds
    }

    if (is.null(type) || length(type) == 0 || !inherits(type, "character")) {
      .Object@type <- ""
    } else {
      .Object@type <- type
    }

    if (is.null(cc) || !inherits(cc, "list")) {
      .Object@cc <- list()
    } else {
      .Object@cc <- cc
    }

    if (is.null(adjMat) || !inherits(adjMat, "matrix")) {
      .Object@adjMat <- matrix()
    } else {
      .Object@adjMat <- adjMat
    }

    return(.Object)
  }
)


##' @rdname VizData-class
##'
##' @importFrom PSMatch makeAdjacencyMatrix ConnectedComponents
##' @return NA
##'
setMethod("[",
  signature = c("VizData", "ANY", "ANY", "ANY"),
  function(x, i, j = NA, k = NA, l = NA, ..., drop) {
    if (length(i) == 1) {
      tmp <- as.matrix(t((x@qdata)[i, ]))
      rownames(tmp) <- (rownames(x@qdata))[i]
      x@qdata <- tmp
      x@metadata <- as.data.frame((x@metadata)[i, ])
      x@metacell <- as.data.frame((x@metacell)[i, ])
    } else if (length(i) > 1) {
      x@qdata <- (x@qdata)[i, ]
    }

    x@metadata <- as.data.frame((x@metadata)[i, ])
    x@metacell <- as.data.frame((x@metacell)[i, ])

    # Update adjacency matrix and connected components
    if (x@type == "peptide") {
      X <- PSMatch::makeAdjacencyMatrix((x@metadata)[, x@proteinID])
      rownames(X) <- rownames(x@metadata)

      # Create the connected components
      cc <- PSMatch::ConnectedComponents(X)@adjMatrices

      x@adjMat <- as.matrix(X)
      x@cc <- as.list(cc)
    }
    return(x)
  }
)





#' @title xxx
#' @description
#' xxxxx
#' 
#' @param object An instance of the class `VizData`
#' @export
#' 
showVizData <- function(object) {
  stopifnot(inherits(object, 'VizData'))
  
  cat(crayon::green(paste0(
    "\tdim(qdata): ",
    dim(object@qdata)[1], " x ",
    dim(object@qdata)[2], "\n"
  )))
  
  cat(crayon::green(paste0(
    "\tdim(metacell): ",
    dim(object@metacell)[1], " x ",
    dim(object@metacell)[2], "\n"
  )))
  
  cat(crayon::green("\tconds: "))
  cat(crayon::green(object@conds))
  cat(crayon::green("\n"))
  
  cat(crayon::green("\ttype: "))
  cat(crayon::green(object@type))
  cat(crayon::green("\n"))
  
  cat(crayon::green("\tcolID: "))
  cat(crayon::green(object@colID))
  cat(crayon::green("\n"))
  
  cat(crayon::green("\tproteinID: "))
  cat(crayon::green(object@proteinID))
  cat(crayon::green("\n"))
  
  cat(crayon::green("\tDimensions of adjacency matriX: "))
  cat(crayon::green(paste0(
    dim(object@adjMat)[1], " x ",
    dim(object@adjMat)[2]
  )))
  cat(crayon::green("\n"))
  
  cat(crayon::green("\tNumber of connected components: "))
  cat(crayon::green(length(object@cc)))
  cat(crayon::green("\n"))
  
  if (length(object@cc) > 0) {
    cc.infos <- GetCCInfos(object@cc)
    # cat(crayon::green('\t\tDetails:\n'))
    cat(crayon::green("\t\tOne -> One: "))
    cat(crayon::green(length(cc.infos$One_One)))
    cat(crayon::green("\n"))
    cat(crayon::green("\t\tOne -> Multi: "))
    cat(crayon::green(length(cc.infos$One_Multi)))
    cat(crayon::green("\n"))
    cat(crayon::green("\t\tMulti -> Multi: "))
    cat(crayon::green(length(cc.infos$Multi_Multi)))
    cat(crayon::green("\n"))
  }
}
