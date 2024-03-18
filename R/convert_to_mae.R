#' @title xxx
#' @description
#' The resulting object is an instance of the  MultiAssayExperiment class.
#' F
#' 
#' @param obj An object compliant with formats xxxx
#' @param colData xxxx
#' @param se xxxx
#' @param ll A list
#' @name converters
#' 
#' @importFrom SummarizedExperiment rowData colData assay SummarizedExperiment
#' @importFrom MSnbase exprs fData pData
#' @importFrom MultiAssayExperiment MultiAssayExperiment ExperimentList
#' 
#' @return An enriched instance of the class `MultiAssayExperiment`
#' 
#' @examples
#' \donttest{
#' 
#' #-------------------------------------------
#' # Conversion of a MultiAssayExperiment instance
#' #-------------------------------------------
#' data(miniACC, package = 'MultiAssayExperiment')
#' convert_to_mae(miniACC)
#' }
#' 
NULL



#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
#' 
convert_to_mae <- function(obj){
  
  converted.obj <- NULL
  #browser()
  if (inherits(obj, "list")){
    if (is.listOf(obj, "MSnSet"))
      converted.obj <- listOfMSnSet_to_mae(obj)
    else if (is.listOf(obj, "SummarizedExperiment"))
    converted.obj <- listOfSE_to_mae(obj)
    else if (is.listOf(obj, "list"))
      converted.obj <- listOfLists_to_mae(obj)
  } else {
    # Obj is a single MSnSet
  if (inherits(obj, "MSnSet"))
    converted.obj <- MSnSet_to_mae(obj)
  else if (inherits(obj, "QFeatures"))
    converted.obj <- QFeatures_to_mae(obj) 
  else if (inherits(obj, "SummarizedExperiment"))
    converted.obj <- list(SE_to_mae(obj)) 
  else if (inherits(obj, "MultiAssayExperiment"))
    converted.obj <- MAE_to_mae(obj)
  }

  return(converted.obj)
}




#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @importFrom MSnbase exprs 
#' 
MSnSet_to_mae <- function(obj){

  mae <- NULL
  tryCatch({
  .colData <- MultiAssayExperiment::DataFrame(
    group = seq(ncol(MSnbase::exprs(obj))), 
    row.names = colnames(MSnbase::exprs(obj)))
  
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = MultiAssayExperiment::ExperimentList(original = MSnSet_to_se(obj)),
    colData = .colData,
    metadata = list(other = list())
  )
  mae <- MAE_Compatibility_with_Prostar_1x(obj, mae)

  },
    warning  = function(w) {
      print(w)
      NULL},
    error = function(e){
      print(e)
      NULL}
  )
  
  return(mae)
   
}



#' @rdname converters
#' @return An instance of `SimpleList`
#' @importFrom PSMatch ConnectedComponents
Compute_CC <- function(obj){
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  cc <- list()
  
  X <- tryCatch({
    SummarizedExperiment::rowData(obj)[, "adjacencyMatrix"]
  }, warning = function(w) NULL,
    error = function(e) NULL
  )

  if (!is.null(X)) {
    #.arg <- args$metadata[, args$proteinID]
   # args$adjMat <- PSMatch::makeAdjacencyMatrix(.arg)
    #rownames(args$adjMat) <- rownames(args$metadata)
    
    # Create the connected components
    cc <- PSMatch::ConnectedComponents(X)@adjMatrices
  }
  
  return(cc)
}

#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
QFeatures_to_mae <- function(obj){
  
  stopifnot(inherits(obj, 'QFeatures'))
    
  return(obj)
}




#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @importFrom MultiAssayExperiment MultiAssayExperiment ExperimentList
#' 
SE_to_mae <- function(obj){
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = MultiAssayExperiment::ExperimentList(original = obj),
    metadata = list(other = list())
  )
  
  return(mae)
}



#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
MAE_to_mae <- function(obj){
  stopifnot(inherits(obj, 'MultiAssayExperiment'))
  
  return(obj)
}



#' @export
#' @rdname converters
#' @return A `boolean(1)`
Check_se_Consistency <- function(obj){
  stopifnot(is.listOf(obj, 'MSnSet'))
  
  passed <- TRUE

  return(passed)
}


#' @rdname converters
#' @export
#' @return An enriched instance of the class `SummarizedExperiment`
#' 
list_to_se <- function(ll){
  
  .proteinID <- tryCatch({
    ll$proteinID
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  .metacell <- tryCatch({
    ll$metacell
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  .colID <- tryCatch({
    ll$keyid
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  # If exists, extracts type of dataset info
  .type <- tryCatch({
    ll$type
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  .pkg_version <- tryCatch({
    ll$pkg_Version
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  .assay <- tryCatch({
    ll$assay
  }, warning = function(w) matrix(),
    error = function(e) matrix()
  )
  
  
  .rowData<- tryCatch({
    df <- MultiAssayExperiment::DataFrame(ll$metadata)
    df[['metacell']] <- ll$metacell
    df
  }, warning = function(w) MultiAssayExperiment::DataFrame(),
    error = function(e) MultiAssayExperiment::DataFrame()
  )
  
  
  
  
  # Prebuild metadata info for SE
  se.meta <- list(
    pkg_version = .pkg_version,
    type = .type,
    colID = .colID,
    proteinID = .proteinID,
    cc = list()
  )
  
  # Builds the SE corresponding to MSnSet 
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = .assay,
    metadata = se.meta,
    rowData = .rowData
  )
  
  
  se <- Build_X_CC(se)
  
  return(se)
}


#' @rdname converters
#' @return A `boolean(1)`
Check_List_consistency <- function(ll){
  passed <- TRUE
  
  for (i in seq(length(ll)-1)){
    test1 <- ll[[i]]
    test2 <- ll[[i+1]]
    
    passed <- passed && identical(colnames(test1$assay), colnames(test2$assay))
    passed <- passed && identical(rownames(test1$assay), rownames(test2$assay))
    
    passed <- passed && identical(colnames(test1$metacell), colnames(test2$metacell))
    passed <- passed && identical(rownames(test1$metacell), rownames(test2$metacell))
    
    passed <- passed && identical(rownames(test1$metacell), rownames(test1$assay))
    
  }

  return(passed)
}



#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
#' 
listOfLists_to_mae <- function(obj, colData = NULL){
  #stopifnot(is.listOf(obj, "list"))
  
  # Checks structure of each item of the list
  stopifnot(Check_List_consistency(obj))

  # Check if all items are named
  if(length(names(obj)) != length(obj))
    names(obj) <- paste0('original_', seq.int(length(obj)))
  
  ll.se <- lapply(obj, function(x){list_to_se(x)})
  
  names(ll.se) <- names(obj)
  
  .assay1 <- SummarizedExperiment::assay(ll.se[[1]])
  if(is.null(colData)){
    colData <- MultiAssayExperiment::DataFrame(group = seq(ncol(.assay1)), 
    row.names = colnames(.assay1))
  }
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = MultiAssayExperiment::ExperimentList(ll.se),
    colData = colData,
    metadata = list(other = list())
  )

  return(mae)
  
}



#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
listOfSE_to_mae <- function(obj){
  
  stopifnot(is.listOf(obj, "SummarizedExperiment"))
  stopifnot(Check_se_Consistency(obj))
  
  if(length(names(obj) != length(obj)))
    names(obj) <- paste0('original_', seq.int(length(obj)))
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = obj,
    colData = MultiAssayExperiment::DataFrame(),
    metadata = list(other = list())
  )
  
  return(mae)
}


#' @export
#' @rdname converters
#' @return A `boolean(1)`
#' @importFrom MSnbase exprs pData fData
#' 
Check_MSnSet_Consistency <- function(obj){
  stopifnot(is.listOf(obj, 'MSnSet'))
  
  passed <- TRUE
  
  # Checks versions
  ll.ver <- lapply(obj, function(x){
    tryCatch({
      experimentData(x)@other$Prostar_Version
    }, warning = function(w) NA,
      error = function(e) NA
    )
  })
  
  passed <- passed && length(unique(unlist(ll.ver)))==1
  
  for (i in seq(length(obj)-1)){
    test1 <- obj[[i]]
    test2 <- obj[[i+1]]
    passed <- passed && identical(MSnbase::pData(test1), MSnbase::pData(test2))
    #passed <- passed && identical(rownames(exprs(test1)), rownames(fData(test2)))
    passed <- passed && identical(colnames(MSnbase::exprs(test1)), rownames(MSnbase::pData(test2)))
    #passed <- passed && identical(rownames(exprs(test1)), rownames(exprs(test2)))
    
  }
  
return(passed)
}


#' @rdname converters
#' @export
#' @return An enriched instance of the class `SummarizedExperiment`
#' 
MSnSet_to_se <- function(obj){
stopifnot(inherits(obj, 'MSnSet'))
  
  .proteinID <- tryCatch({
    obj@experimentData@other$proteinId
  }, warning = function(w) {
    print(w)
    NA},
    error = function(e) {
      print(e)
      NA
    }
  )
  
  
  .colID <- tryCatch({
    obj@experimentData@other$keyId
  }, warning = function(w) {
    print(w)
    NA},
    error = function(e) {
      print(e)
      NA
    }
  )
  
  
  # If exists, extracts type of dataset info
  .type <- tryCatch({
    obj@experimentData@other$typeOfData
  }, warning = function(w) {
    print(w)
    NA},
    error = function(e) {
      print(e)
      NA
    }
  )
  
  
  .pkg_version <- tryCatch({
    obj@experimentData@other$Prostar_Version
  }, warning = function(w) {
    print(w)
    NA},
    error = function(e) {
      print(e)
      NA
    }
  )

  
  # Prebuild metadata info for SE
  se.meta <- list(
    pkg_version = .pkg_version,
    type = .type,
    colID = .colID,
    proteinID = .proteinID,
    cc = list(),
    conds = MSnbase::pData(obj)[,'Condition']
  )
  
  # Builds the SE corresponding to MSnSet 
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = MSnbase::exprs(obj),
    metadata = se.meta,
    rowData = MSnbase::fData(obj)
  )
  
  # If the MSnSet has been created with Prostar_1.x
  se <- SE_Compatibility_with_Prostar_1.x(obj, se)

  
  se <- Build_X_CC(se)
  
  return(se)
  
}



#' @export
#' @rdname converters
#' @return An enriched instance of the class `SummarizedExperiment`
#' @importFrom PSMatch ConnectedComponents makeAdjacencyMatrix
Build_X_CC <- function(se){
  
  original.se <- se
  res <- tryCatch({
    X <- PSMatch::makeAdjacencyMatrix((
      SummarizedExperiment::rowData(se))[, get_proteinID(se)])
    rownames(X) <- rownames(
      SummarizedExperiment::rowData(se))
    SummarizedExperiment::rowData(se)[['adjacencyMatrix']] <- X
    
    cc <- PSMatch::ConnectedComponents(X)
    metadata(se)[['cc']] <- lapply(cc@adjMatrices, function(x) x)
    
    se
  }, warning = function(w) original.se,
    error = function(e) original.se
  )
  
  
  return(res)
}

#' @export
#' @rdname converters
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @importFrom MultiAssayExperiment MultiAssayExperiment MultiAssayExperiment
#' @importFrom MSnbase exprs
#' 
listOfMSnSet_to_mae <- function(obj){
  stopifnot(is.listOf(obj, "MSnSet"))
  stopifnot(Check_MSnSet_Consistency(obj))
  
  ll.se <- lapply(obj, function(x){
    MSnSet_to_se(x)})
  
  names(ll.se) <- names(obj)

  .colData <- MultiAssayExperiment::DataFrame(
    group = seq(ncol(MSnbase::roxygexprs(obj[[1]]))), 
    row.names = colnames(MSnbase::exprs(obj[[1]])))
  
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = MultiAssayExperiment::ExperimentList(ll.se),
    colData = .colData,
    metadata = list(other = list())
  )
  
  return(mae)
  
}

