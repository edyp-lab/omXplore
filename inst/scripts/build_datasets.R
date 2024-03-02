library(omXplore)

## ---------------------------------------------------------
## Creating vdata dataset
## ---------------------------------------------------------
vdata <- build_VizList_example_from_VizData()
save(vdata, file = 'data/vdata.rda')