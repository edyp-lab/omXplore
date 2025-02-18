#' @title Shiny modules used by `omXplore` 
#'
#' @description
#' These functions are relative to external modules that can be added into 
#' `omXplore` UI:
#' * `listShinyApps()`: Show the shiny modules recognized by `omXplore` and 
#' ready to bu integrated in the UI of the function view_dataset()
#' * `listPlotModules()`: Show the shiny modules function names (only prefixes)
#'  recognized by `omXplore` and ready to use in the UI.
#' * `addModules()`: Add external shiny module(s) to the R global environment in
#' such a way (specific prefix renaming of the functions) that it can be
#'  discovered by the function view_dataset() of the package `omXplore` during 
#'  its launch. 
#'
#' @param location A `character(0)` to indicate which modules to list. Available
#' values are: 'builtin', 'external' and 'both' (default).
#' @param addons A `list` in which each item:
#' * is named by the name of a package containing the modules to add,
#' * contains the name of the shiny modules to integrate (without '_ui' nor
#' '_server' suffixes)
#'
#'
#' @name omXplore-modules
#'
#' @examples
#' listShinyApps()
#' listPlotModules()
#' 
#' #####################################################
#' # Integration of a module in the package 'mypackage'
#' #####################################################
#' \dontrun{
#' addons <- list(omXplore = c("extFoo1", "extFoo2"))
#' addModules(addons)
#' }
#'
NULL


#' @export
#' @rdname omXplore-modules
#' @return NA
#'
addModules <- function(addons = list()) {
  
  addon.isValid <- function(addons) {
    passed <- TRUE
    passed <- passed && inherits(addons, "list")

    passed
  }
  
  
  
  if (length(addons) == 0 || !inherits(addons, "list")) {
    message('No external module was found')
    return(NULL)
  } else if (!addon.isValid(addons)){
    warning('addons is not in a correct format')
    return(NULL)
  }

  f_assign <- function(fun, pkg, suffix) {
    f_original_name <- paste0(fun, "_", suffix)
    f_dest_name <- paste0('addon_', pkg, "_", fun, "_", suffix)
    f_fullname <- paste0(pkg, "::", f_original_name)
    assign(f_dest_name, eval(parse(text = f_fullname)), envir = globalenv())
  }


  for (pkg in names(addons)) {
    for (func in addons[[pkg]]) {
      f_assign(func, pkg, "ui")
      f_assign(func, pkg, "server")
    }
    
    tryCatch({
        addResourcePath(
        prefix = paste0(pkg, '_images'),
        directoryPath = system.file('images', package = pkg)
        )
      },
      warning = function(w) message(w),
      error = function(e) message(e)
      )
  }
}



#' @export
#' @rdname omXplore-modules
#' @return A vector
#'
listShinyApps <- function(location = "both") {
  stopifnot(location %in% c("both", "external", "builtin"))

  builtinApps <- gsub("", "", listPlotModules("builtin"))
  builtinApps <- paste0(builtinApps, "()")

  externalApps <- listPlotModules("external")
  if (length(externalApps) > 0) {
    externalApps <- gsub("", "", )
    externalApps <- paste0(externalApps, "()")
  } else {
    externalApps <- NULL
  }

  bothApps <- c(builtinApps, externalApps)

  value <- switch(location,
    both = bothApps,
    external = externalApps,
    builtin = builtinApps
  )

  value
}


#' @export
#' @rdname omXplore-modules
#' @return A vector
#'
listPlotModules <- function(location = "both") {
  stopifnot(location %in% c("both", "external", "builtin"))
  
  

  builtin <- ls("package:omXplore")
  builtin <- builtin[grep("omXplore_", builtin)]
  builtin <- gsub("_server", "", builtin)
  builtin <- gsub("_ui", "", builtin)
  builtin <- unique(builtin)

  # Lists module in the global environment
  external <- utils::lsf.str(envir = globalenv())
  external <- external[grep("addon_", external)]
  external <- gsub("_server", "", external)
  external <- gsub("_ui", "", external)
  external <- unique(external)
  external <- setdiff(external, builtin)
  if (length(external) == 0) {
    external <- NULL
  }

  value <- switch(location,
    both = c(builtin, external),
    external = external,
    builtin = builtin
  )

  value
}
