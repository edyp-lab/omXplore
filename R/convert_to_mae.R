#' @title xxx
#' @description
#' The resulting object is an instance of the  MultiAssayExperiment class.
#' F
#' 
#' @param obj An object compliant with formats xxxx
#' @name converters
#' 
#' 
#' @examples
#' 
#' #-------------------------------------------
#' # Conversion of a list of MSnSet instances
#' #-------------------------------------------
#' ll.msnset <- list(data1 = Exp1_R25_prot, data2 = Exp1_R25_prot, data3 = Exp1_R25_prot)
#' convert_to_mae(ll.msnset)
#' 
#' #-------------------------------------------
#' # Conversion of a MSnSet instance
#' #-------------------------------------------
#' convert_to_mae(Exp1_R25_prot)
#' 
#' #-------------------------------------------
#' # Conversion of a list of SummarizedExperiment instances
#' #-------------------------------------------
#' 
#' 
#' 
#' #-------------------------------------------
#' # Conversion of a SummarizedExperiment instance
#' #-------------------------------------------
#' mae <- convert_to_mae(Exp1_R25_prot)
#' convert_to_mae(mae[[1]])
#' 
#' 
#' #-------------------------------------------
#' # Conversion of a MultiAssayExperiment instance
#' #-------------------------------------------
#' mae <- convert_to_mae(Exp1_R25_prot)
#' convert_to_mae(mae)
#' 
NULL



#' @export
#' @rdname converters
#' 
convert_to_mae <- function(obj){
  
  converted.obj <- NULL
  
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

  converted.obj
}




#' @export
#' @rdname converters
#' 
MSnSet_to_mae <- function(obj){

  .colData <- DataFrame(group = seq(ncol(exprs(obj))), 
    row.names = colnames(exprs(obj)))
  
  
  mae <- MultiAssayExperiment(
    experiments = ExperimentList(original = MSnSet_to_se(obj)),
    colData = .colData,
    metadata = list(other = list())
  )
  
  mae <- MAE_Compatibility_with_Prostar_1x(obj, mae)
  
  
  mae
   
}


Compute_CC <- function(obj){
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  cc <- list()
  
  X <- tryCatch({
    rowData(obj)[, "adjacencyMatrix"]
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
  
  cc
}

#' @export
#' @rdname converters
#' 
QFeatures_to_mae <- function(obj){
  
  stopifnot(inherits(obj, 'QFeatures'))
    
  return(obj)
}




#' @export
#' @rdname converters
#' 
SE_to_mae <- function(obj){
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  MultiAssayExperiment(
    experiments = ExperimentList(original = obj),
    metadata = list(other = list())
  )
}



#' @export
#' @rdname converters
#' 
MAE_to_mae <- function(obj){
  stopifnot(inherits(obj, 'MultiAssayExperiment'))
  
  obj
}



#' @export
#' @rdname converters
#' 
Check_se_Consistency <- function(obj){
  stopifnot(is.listOf(obj, 'MSnSet'))
  
  passed <- TRUE

  return(passed)
}


#' @export
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
    df <- DataFrame(ll$metadata)
    df[['metacell']] <- ll$metacell
    df
  }, warning = function(w) DataFrame(),
    error = function(e) DataFrame()
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
  se <- SummarizedExperiment(
    assays = .assay,
    metadata = se.meta,
    rowData = .rowData
  )
  
  
  se <- Build_X_CC(se)
  
  se
}



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

  passed
}



#' @export
#' @rdname converters
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
  
  .assay1 <- assay(ll.se[[1]])
  if(is.null(colData)){
    colData <- DataFrame(group = seq(ncol(.assay1)), 
    row.names = colnames(.assay1))
  }
  
  MultiAssayExperiment(
    experiments = ExperimentList(ll.se),
    colData = colData,
    metadata = list(other = list())
  )

  
}



#' @export
#' @rdname converters
#' 
listOfSE_to_mae <- function(obj){
  
  stopifnot(is.listOf(obj, "SummarizedExperiment"))
  stopifnot(Check_se_Consistency(obj))
  
  if(length(names(obj) != length(obj)))
    names(obj) <- paste0('original_', seq.int(length(obj)))
  
  MultiAssayExperiment(
    experiments = obj,
    colData = DataFrame(),
    metadata = list(other = list())
  )
}


#' @export
#' @rdname converters
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
    passed <- passed && identical(pData(test1), pData(test2))
    passed <- passed && identical(rownames(exprs(test1)), rownames(fData(test2)))
    passed <- passed && identical(colnames(exprs(test1)), rownames(pData(test2)))
    passed <- passed && identical(rownames(exprs(test1)), rownames(exprs(test2)))
    
  }
  
return(passed)
}


#' @export
MSnSet_to_se <- function(obj){
stopifnot(inherits(obj, 'MSnSet'))
  
  .proteinID <- tryCatch({
    experimentData(obj)@other$proteinId
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  .colID <- tryCatch({
    experimentData(obj)@other$keyid
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  # If exists, extracts type of dataset info
  .type <- tryCatch({
    experimentData(obj)@other$typeOfData
  }, warning = function(w) NA,
    error = function(e) NA
  )
  
  
  .pkg_version <- tryCatch({
    experimentData(obj)@other$Prostar_Version
  }, warning = function(w) NA,
    error = function(e) NA
  )

  
  # Prebuild metadata info for SE
  se.meta <- list(
    pkg_version = .pkg_version,
    type = .type,
    colID = .colID,
    proteinID = .proteinID,
    cc = list(),
    conds = pData(obj)[,'Condition']
  )
  
  # Builds the SE corresponding to MSnSet 
  se <- SummarizedExperiment(
    assays = exprs(obj),
    metadata = se.meta,
    rowData = fData(obj)
  )
  
  # If the MSnSet has been created with Prostar_1.x
  se <- SE_Compatibility_with_Prostar_1.x(obj, se)

  
  se <- Build_X_CC(se)
  
  se
  
}



#' @export
#' @rdname converters
#' 
Build_X_CC <- function(se){
  
  original.se <- se
  tryCatch({
    X <- PSMatch::makeAdjacencyMatrix((rowData(se))[, get_proteinID(se)])
    rownames(X) <- rownames(rowData(se))
    cc <- PSMatch::ConnectedComponents(X)@adjMatrices
    rowData(se)[['adjacencyMatrix']] <- X
    
    metadata(se)[['cc']] <- cc
    se
  }, warning = function(w) original.se,
    error = function(e) original.se
  )
  
}

#' @export
#' @rdname converters
#' 
listOfMSnSet_to_mae <- function(obj){
  stopifnot(is.listOf(obj, "MSnSet"))
  stopifnot(Check_MSnSet_Consistency(obj))
  
  ll.se <- lapply(obj, function(x){
    MSnSet_to_se(x)})
  
  names(ll.se) <- names(obj)

  .colData <- DataFrame(group = seq(ncol(exprs(obj[[1]]))), 
    row.names = colnames(exprs(obj[[1]])))
  
  MultiAssayExperiment(
    experiments = ExperimentList(ll.se),
    colData = .colData,
    metadata = list(other = list())
  )
  
}

