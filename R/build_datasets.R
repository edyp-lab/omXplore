#' @title Build toy datasets for omXplore modules
#' @description
#' xxxxx
#' 
#' @param name The name of the dataset
#' 
#' @name build_example_datasets
#' @examples
#' build_toylist_example('myData')
#' build_VizData_example()
#' build_VizList_example()
#' build_VizList_example_from_VizData()
#' 
NULL



#' @export
#' @rdname build_example_datasets
#' 
build_toylist_example <- function(name){
  qdata <- matrix(1:30, ncol = 6, 
    dimnames = list(paste0('prot_', 1:5), 
      c(paste0('C1_R', 1:3), paste0('C2_R', 1:3))))
  colnames(qdata) <- c(paste0(name, '_C1_R', 1:3), 
    paste0(name, '_C2_R', 1:3))
  
  metacell <- data.frame(matrix(rep('Missing POV', 30), ncol = 6), 
    row.names = paste0('prot_', 1:5))
  colnames(metacell) <- c(paste0(name, '_metacell_C1_R', 1:3), 
    paste0(name, '_metacell_C2_R', 1:3))
  
  metadata <- data.frame(
    protID = paste0('proteinID_', 1:5),
    metadata1 = paste0('meta1_', 1:5),
    metadata2 = paste0('meta2_', 1:5))
  
  conds <- c(rep('C1', 3), rep('C2', 3))
  colID <- 'protID'
  proteinID <- 'protID'
  type <- "protein"
  adjMat <- matrix()
  cc <- list()
  
  
  list(
    qdata = qdata,
    metadata = metadata,
    metacell = metacell,
    conds = conds,
    colID = colID,
    proteinID = proteinID,
    type = type,
    adjMat = adjMat,
    cc = cc
  )
}


#' @rdname build_example_datasets
#' @export
#' 
build_VizData_example <- function(name = 'obj'){
  ll <- build_toylist_example(name)
  do.call(VizData, ll)
}


#' @rdname build_example_datasets
#' @export
#' 
build_VizList_example <- function(name = 'obj'){
  ll1 <- build_toylist_example('obj1')
  ll2 <- build_toylist_example('obj2')
  ll3 <- build_toylist_example('obj3')
  VizList(list(ll1, ll2, ll3))
}


#' @rdname build_example_datasets
#' @export
#' 
build_VizList_example_from_VizData <- function(name = 'obj'){
  ll1 <- build_VizData_example('obj1')
  ll2 <- build_VizData_example('obj2')
  ll3 <- build_VizData_example('obj3')
  VizList(list(ll1, ll2, ll3))
}
