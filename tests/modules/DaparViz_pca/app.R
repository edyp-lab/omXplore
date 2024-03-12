library(omXplore)

data(vdata)
# Replace missing values for the example
shiny::runApp(omXplore_pca(vdata, 1))
