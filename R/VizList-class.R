##' @title VizList class
##'
##' @description
##'
##'  Conceptually, a `VizList` object is a standard representation of all
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
##' @name VizList-class
##'
##' @param object An instance of the class `VizList`
##'
##' @param x An instance of class [QFeatures].
##'
##' @param i `character()`, `integer()`, `logical()` or `GRanges()`
##'     object for subsetting by rows.
##'
##' @return See individual method description for the return value.
##'
##' @seealso
##' The `VizData()` constructor and the `convert2Viz()`
##' function. The *VizData* vignette provides an extended example.
##'
##' @exportClass VizList
##'
##' @author Samuel Wieczorek
##'
##' @examples
##' ## ------------------------
##' ## An empty VizData object
##' ## ------------------------
##'
##' VizList()
##'
##' ## -----------------------------------
##' ## Creating a VizData object manually
##' ## -----------------------------------
##' 
##'
NULL



##'
##' @rdname VizList-class
##' @export VizList
##' @exportClass VizList
##' @return An instance of the class `VizList`
VizList <- setClass(
  
  "VizList",
  ##' @slot ll.VizData A matrix containing the quantitative data

  representation(
    ll.VizData = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype(
    ll.VizData = list()
  ),
  
  ##' @param object xxx
  validity = function(object) {
    errors <- character()
    
    if (inherits(object, 'list')){
      msg <- "The object is not a list"
      errors <- c(errors, msg)
    }

    if (!is.listOf(obj, 'VizData')) {
      msg <- "All the items contained in this list are not of class 'VizData'"
      errors <- c(errors, msg)
    }
    
    if (length(errors) == 0) TRUE else errors
  }
)


##' @exportMethod show
##' @rdname VizList-class
##' @return NA
##'
setMethod(
  "show", "VizList",
  ##' @param object An instance of the class `VizList`
  ##' @exportMethod show
  ##'
  function(object) {
    if(length(object@ll.VizData) > 0){
      lapply(seq(length(object@ll.VizData)), 
        function(x){
          cat(names(object)[x], '\n')
          showVizData(object@ll.VizData[[x]])
          })
    } else
      message("Empty object")
  }
)



##' @title Initialization method for the class `VizList`
##' @rdname VizList-class
##' @return An instance of the class `VizList`
##'
setMethod(
  "initialize", "VizList",
  ##' @param .Object xxx
  ##' @param dataset xxx
  function(
    .Object,
    dataset = list()) {

    .Object@ll.VizData <- list()
    

    if (inherits(dataset, "MSnSet"))
      .Object@ll.VizData <- list(MsnSet2VizData(dataset))
    
    if (inherits(dataset, "QFeatures"))
    .Object@ll.VizData <- QFeatures2VizDataList(dataset)
    
    if (inherits(dataset, "Summarizedexperiment"))
    .Object@ll.VizData <- list(SE2VizData(dataset))
    
    if (inherits(dataset, "MultiAssayExperiment"))
    .Object@ll.VizData <- MAEs2VizDataList(dataset)
    
    if (inherits(dataset, "VizData"))
      .Object@ll.VizData <- list(dataset)
    
    
    if (inherits(dataset, "list")){
      
      if (is.listOf(dataset, 'VizData'))
        .Object@ll.VizData <- dataset
      
      if (is.listOf(dataset, "MSnSet")) {}
      
      
      if (is.listOf(dataset, "Summarizedexperiment")){}
      
      if (is.listOf(dataset, "list"))
        .Object@ll.VizData <- convert2VizDataList(dataset)
    
    }
    return(.Object)
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("[",
  signature = c("VizList", "ANY"),
  function(x, i) {
    return(x@ll.VizData[i])
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("[[",
  signature = c("VizList", "ANY"),
  function(x, i) {
    return(x@ll.VizData[[i]])
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("names",
  signature = c("VizList"),
  function(x) {
    return(names(x@ll.VizData))
  }
)


##' @rdname VizList-class
##'
##' @return NA
##'
setMethod("length",
  signature = c("VizList"),
  function(x) {
    return(length(x@ll.VizData))
  }
)
