app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$setInputs(`plot-showLabels` = TRUE)
app$snapshot()
app$setInputs(`plot-showLabels` = FALSE)
app$snapshot()
