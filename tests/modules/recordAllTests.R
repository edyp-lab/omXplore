library(shinytest)

recordTest("tests/modules/omXplore_plots_tracking", loadTimeout = 100000)

recordTest("tests/modules/omXplore_density", loadTimeout = 100000)
recordTest("tests/modules/omXplore_corrmatrix", loadTimeout = 100000)
recordTest("tests/modules/omXplore_intensity", loadTimeout = 100000)
recordTest("tests/modules/omXplore_pca", loadTimeout = 100000)
recordTest("tests/modules/omXplore_plots_tracking", loadTimeout = 100000)
recordTest("tests/modules/omXplore_tabExplorer", loadTimeout = 100000)
recordTest("tests/modules/omXplore_variance", loadTimeout = 100000)
recordTest("tests/modules/omXplore_view_dataset", loadTimeout = 100000)
