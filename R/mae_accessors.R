#' @title Accessors functions
#' @description xxx
#' @return See individual method description for the return value.
#' @param object An object of
#' @param ... xxxx description
#' @name accessors
#'
#'
#' @return If exists, the slot value requested.
#' @examples
#'
#' ## -----------------------------------
#' ## Accessing slots from a MSnSet dataset
#' ## -----------------------------------
#' data(sub_R25_prot)
#' se1 <- sub_R25_prot[[1]]
#' proteinID <- get_proteinID(se1)
#' colID <- get_colID(se1)
#' type <- get_type(se1)
#' metacell <- get_metacell(se1)
#' conds <- get_group(sub_R25_prot)
#'
NULL


#' @rdname accessors
#' @exportMethod get_adjacencyMatrix
setGeneric(
  "get_adjacencyMatrix",
  function(object, ...) standardGeneric("get_adjacencyMatrix")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A DataFrame containing the adjacency matrix of the dataset
#'
setMethod("get_adjacencyMatrix", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        rowData(object)[, 'adjacencyMatrix']
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)


#' @rdname accessors
#' @exportMethod get_group
setGeneric(
  "get_group",
  function(object, ...) standardGeneric("get_group")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_group", signature = "MultiAssayExperiment",
  function(object) {
    tryCatch(
      {
        colData(object)$group
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)



#' @rdname accessors
#' @exportMethod get_metacell
setGeneric(
  "get_metacell",
  function(object, ...) standardGeneric("get_metacell")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_metacell", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        as.data.frame(rowData(object)[, 'metacell'])
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
  )


#' @rdname accessors
#' @exportMethod get_cc
setGeneric(
  "get_cc",
  function(object, ...) standardGeneric("get_cc")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_cc", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        metadata(object)$cc
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)

#' @rdname accessors
#' @exportMethod get_proteinID
setGeneric(
  "get_proteinID",
  function(object, ...) standardGeneric("get_proteinID")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_proteinID", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        metadata(object)$proteinID
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)


#' @rdname accessors
#' @exportMethod get_colID
setGeneric(
  "get_colID",
  function(object, ...) standardGeneric("get_colID")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_colID", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        metadata(object)$colID
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)


#' @rdname accessors
#' @exportMethod get_type
setGeneric(
  "get_type",
  function(object, ...) standardGeneric("get_type")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_type", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        metadata(object)$type
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)


#' @rdname accessors
#' @exportMethod get_pkg_version
setGeneric(
  "get_pkg_version",
  function(object, ...) standardGeneric("get_pkg_version")
)



#' @param object An instance of class `SummarizedExperiment`.
#' @rdname accessors
#' @return A data.frame containing the metadata of the dataset
#'
setMethod("get_pkg_version", signature = "SummarizedExperiment",
  function(object) {
    tryCatch(
      {
        metadata(object)$pkg_version
      },
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }
)

