app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$setInputs(`tracker-typeSelect` = "List", wait_ = FALSE, values_ = FALSE)
app$snapshot()
app$setInputs(
  `tracker-listSelect` = "ADTITAQEIQQFK",
  wait_ = FALSE,
  values_ = FALSE
)
app$snapshot()
