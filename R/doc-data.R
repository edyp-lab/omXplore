
#' @title Feature example data
#'
#' @description 
#'`vdata` is a small object for testing and
#' demonstration.  `vdata_na`
#' is a tiny test set that contains missing values used to
#' demonstrate and test the impact of missing values on data
#' processing.
#'
#' @docType data
#' @keywords data
#' @keywords datasets
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @source
#'
#' `vdata` was built from the source code available in
#' [`inst/scripts/build_datasets.R`](https://github.com/prostarproteomics/omXplore/blob/main/inst/scripts/build_datasets.R)
#' 
#'
"vdata"


#' @title Feature example data
#'
#' @description
#' `sub_R25_prot` is a protein subset of the dataset 'Exp1_R25_prot' in the 
#' package 'DAPARdata'. 
#'
#' @source
#'
#' `sub_R25_prot` was built from the source code available in
#' [`inst/scripts/build_datasets.R`](https://github.com/prostarproteomics/omXplore/blob/main/inst/scripts/build_datasets.R)
#'  
#' The `DAPARdata` package: \url{https://github.com/prostarproteomics/DAPARdata}
#'
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @examples
#' 
#' data(sub_R25_prot)
#' view_dataset(sub_R25_prot)
#' 
"sub_R25_prot"

#' @title Feature example data
#'
#' @description
#' `sub_R25_pept` is a protein subset of the dataset 'sub_R25_pept' in the 
#' package 'DAPARdata'. 
#'
#' @source
#'
#' `sub_R25_pept` was built from the source code available in
#' [`inst/scripts/build_datasets.R`](https://github.com/prostarproteomics/omXplore/blob/main/inst/scripts/build_datasets.R)
#'  
#' The `DAPARdata` package: \url{https://github.com/prostarproteomics/DAPARdata}
#'
#' @return An enriched instance of the class `MultiAssayExperiment`
#' @examples
#' 
#' data(sub_R25_pept)
#' view_dataset(sub_R25_pept)
#' 
#'
"sub_R25_pept"
