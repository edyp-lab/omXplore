library(omXplore)

## ---------------------------------------------------------
## Create the vdata dataset
## ---------------------------------------------------------
test <- list(
  data1 = build_toylist_example(), 
  data2 = build_toylist_example(), 
  data3 = build_toylist_example())

colData <- DataFrame(
  group = c('A', 'A', 'A', 'B', 'B', 'B'), 
  row.names = colnames(test$data1$assay)
  )

vdata <- listOfLists_to_mae(test, colData)
save(vdata, file = 'data/vdata.rda')






#' @title Build toy datasets for omXplore modules
#' @description
#' xxxxx
#' 
#' @param name The name of the dataset
#' 
#' @name build_example_datasets
#' @examples
#' build_toylist_example('myData')
#' 
NULL



#' @title Build an example list
#' @description Creates a list which contains example info to be
#' used to create instances of `MultiAssayExperiment` 
#' @export
#' @rdname build_example_datasets
#' @return A list
#' 
build_toylist_example <- function(name = 'original'){

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
  
  #conds <- c(rep('C1', 3), rep('C2', 3))
  colID <- 'protID'
  proteinID <- 'protID'
  type <- "protein"
  adjMat <- matrix()
  cc <- list()
  
  
  list(
    assay = qdata,
    metadata = metadata,
    metacell = metacell,
    colID = colID,
    proteinID = proteinID,
    type = type,
    adjMat = adjMat,
    cc = cc
  )
}
