app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(`iplot-choosePlot` = "box")
app$snapshot()
