.onLoad <- function(libname, pkgname) {
    shiny::addResourcePath(
        prefix = "omXplore_images",
        directoryPath = system.file("images", package = "omXplore")
    )
}

.onUnload <- function(libname, pkgname) {
    shiny::removeResourcePath("omXplore_images")
}
