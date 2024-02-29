library(omXplore)

data(vData_ft)
# Replace missing values for the example
vData_ft[[1]]@qdata[which(is.na(vData_ft[[1]]@qdata))] <- 0
omXplore_pca(vData_ft[[1]])
