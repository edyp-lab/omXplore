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




# # Convert simple QFeatures
# data("feat1", package = 'QFeatures')
# data("feat2", package = 'QFeatures')
# convert2VizList(feat1)
# convert2VizList(feat2)
# 
# 
# 
# 
# 
# # Convert enriched `QFeatures` datasets to VizList class
# data("Exp1_R2_pept", package = 'DaparToolshedData')
# data("Exp1_R2_prot", package = 'DaparToolshedData')
# convert2VizList(Exp1_R2_pept)
# convert2VizList(Exp1_R2_prot)
# 
# 
# 
# # Build data examples with `QFeatures` datasets.
# 
# data("Exp1_R25_pept", package='DaparToolshedData')
# data("Exp1_R25_prot", package='DaparToolshedData')
# vData_ft <- convert2VizList(Exp1_R25_pept[150:170])
# vData_ft2 <- convert2VizList(Exp1_R25_prot[1:21])
# vData_ft[['processed_1']] <- vData_ft2[[1]]
# 
# 
# save(vData_ft, file='data/vData_ft.RData')
# 
# 
# # Convert `MSnSet` datasets to VizList class
# data("Exp1_R2_pept", package = 'DAPARdata')
# data("Exp1_R2_prot", package = 'DAPARdata')
# convert2VizList(Exp1_R2_pept)
# convert2VizList(Exp1_R2_prot)
# 
# 
# # Build data examples from a named list of MSnSet datasets
# ll.msnset <- list(original1 = Exp1_R2_pept,
#   original2 = Exp1_R2_prot)
# convert2VizList(ll.msnset)
# 
# 
# # Build data examples from an unnamed list of MSnSet datasets
# ll.msnset <- list(Exp1_R2_pept, Exp1_R2_prot)
# convert2VizList(ll.msnset)


# Convert `SummarizedExperiment` datasets to VizList class



# Convert `MultiAssayExperiment` datasets to VizList class


# Convert formated list to VizList class

# build_VizData_example <- function(prefix){
#   qdata <- matrix(1:30, ncol = 6, 
#     dimnames = list(paste0('prot_', 1:5), 
#     c(paste0('C1_R', 1:3), paste0('C2_R', 1:3))))
#   colnames(qdata) <- c(paste0(prefix, '_C1_R', 1:3), 
#     paste0(prefix, '_C2_R', 1:3))
#   
#   metacell <- data.frame(matrix(rep('Missing POV', 30), ncol = 6), 
#     row.names = paste0('prot_', 1:5))
#   colnames(metacell) <- c(paste0(prefix, '_metacell_C1_R', 1:3), 
#     paste0(prefix, '_metacell_C2_R', 1:3))
#   
#   metadata <- data.frame(
#     protID = paste0('proteinID_', 1:5),
#     metadata1 = paste0('meta1_', 1:5),
#     metadata2 = paste0('meta2_', 1:5))
#   
#   conds <- c(rep('C1', 3), rep('C2', 3))
#   colID <- 'protID'
#   proteinID <- 'protID'
#   type <- "protein"
#   adjMat <- NULL
#   cc <- NULL
# 
# 
#   VizData(
#     qdata = qdata,
#     metadata = metadata,
#     metacell = metacell,
#     conds = conds,
#     colID = colID,
#     proteinID = proteinID,
#     type = type,
#     adjMat = adjMat,
#     cc = cc
#     )
# }
# 
# data1 <- build_VizData_example('data1')
# data2 <- build_VizData_example('data2')
# 
# data <- convert2VizList(list(data1, data2))
# data <- convert2VizList(list(original_1 = data1, 
#   original_2 = data2))
# 




