library(omXplore)



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
  
  metacell <- DataFrame(matrix(rep('Missing POV', 30), ncol = 6), 
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


## ---------------------------------------------------------
## Create small datasets based on `DAPARdata` package
## ---------------------------------------------------------
data("Exp1_R25_pept", package = 'DAPARdata')
data("Exp1_R25_prot", package = 'DAPARdata')

sub_R25 <- convert_to_mae(
  list(
    peptide = Exp1_R25_pept[150:170],
    protein = Exp1_R25_prot[1:21]
  )
)

save(sub_R25, file = 'data/sub_R25.rda')
