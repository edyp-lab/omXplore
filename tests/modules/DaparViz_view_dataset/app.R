library(omXplore)

data(vdata)
# addon <- list(DaparToolshed=c('omXplore_metacell'))
shiny::runApp(view_dataset(vdata, 1))
