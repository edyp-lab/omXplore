
#' @title xxx
#' @description xxx
#' @export
#' @examples
#' globals()
#'
#' @return A `list`
#'
globals <- function(){
  list(
    general_style = "display:inline-block; vertical-align: middle; padding: 7px",
    actionBtnClass = "btn-primary",
    bad_format_txt = "Dataset in not in correct format.
omXplore can handle MSnset and QFeatures files or instances of class VizData
If you use MSnset or QFeatures datasets, please use the function convert2vizList()"
  )
}
  
  

#' @title xxx
#' @description xxx
#' @export
#' @param object A `list`
#' @param obj.class The name of the class to search in items of the list.
#' @examples
#' NULL
#'
#' @return A `character(1)` with the name of the package or xxx
#'
is.listOf <- function(object, obj.class=NULL){
  
  res <- NULL
  
  if(is.null(obj.class)){
    ll <- unlist(lapply(object, function(x) class(x)[[1]]))
    if (length(unique(ll)) == 1)
      res <- unique(ll)
  } else {
    
    res <- TRUE
    
    res <- res && inherits(object, 'list')
    res <- res && 
      all(unlist(lapply(object, 
        function(x) class(x)[[1]]==obj.class)))
    
  }
  
  res
}



#' @title Package version
#' @description Gets the version number of a package
#' @export
#' @param pkg The name of the package
#' @examples
#' GetPkgVersion('omXplore')
#'
#' @return A `character(1)` with the name of the package and 
#' its version number.
#' @importFrom utils installed.packages
#' 
GetPkgVersion <- function(pkg){
  tryCatch({
    ind <- which(utils::installed.packages()[, 'Package'] == pkg)
    version <- utils::installed.packages()[ind, 'Version']
    paste0(pkg, "_", version)
  },
    warning = function(w) cat(w),
    error = function(e) cat(e)
  )
}




#' #' @title Customised contextual menu of highcharts plots
#'
#' @param hc A highcharter object
#' @param fname The filename under which the plot has to be saved
#'
#' @return A contextual menu for highcharts plots
#'
#' @author Samuel Wieczorek
#'
#' @rdname customExportMenu_HC
#'
#' @examples
#' NULL
#'
#' @export
#' @import highcharter
#'
customExportMenu <- function(hc, fname) {
  highcharter::hc_exporting(hc,
    enabled = TRUE,
    filename = fname,
    buttons = list(
      contextButton = list(
        menuItems = list(
          "downloadPNG",
          "downloadSVG",
          "downloadPDF"
        )
      )
    )
  )
  hc
}




#' @title Customised resetZoom Button of highcharts plots
#'
#' @param hc A highcharter object
#' @param chartType The type of the plot
#' @param zoomType The type of the zoom (one of "x", "y", "xy", "None")
#' @param width xxx
#' @param height xxx
#'
#' @return A highchart plot
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' library(highcharter)
#' if (interactive()) {
#'   hc <- highchart()
#'   hc_chart(hc, type = "line")
#'   hc_add_series(hc, data = c(29, 71, 40))
#'   customChart(hc)
#' }
#'
#' @export
#'
#' @import highcharter
#'
customChart <- function(
    hc,
    chartType = "scatter",
    zoomType = "None",
    width = 0,
    height = 0) {
  hc %>%
    hc_chart(
      type = chartType,
      zoomType = zoomType,
      showAxes = TRUE,
      width = width,
      height = height,
      resetZoomButton = list(
        position = list(
          align = "left",
          verticalAlign = "top"
        )
      )
    )
}



#' @noRd
#' @export
#' @return NA
#' @importFrom DT JS
#'
.initComplete <- function() {
  return(DT::JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({
        'background-color': 'darkgrey',
        'color': 'black'});",
    "}"
  ))
}




#' @title Constructs a dataset suitable to use with the module format_DT.
#'
#' @description
#' This function builds the skeleton of a dataset which can be used by the
#' module formatDT. It creates additional columns to be used to style the table.
#' to colors cells.
#'
#' @param se An instance of the class `SummarizedExperiment`
#' @param digits An 'integer(1)' to specify the number of digits to display
#' in the tables for numerical values. Default is 2.
#'
#' @return A data.frame
#'
#' @export
#'
FormatDataForDT <- function(
    se,
    digits = 2) {
  stopifnot(inherits(se, 'SummarizedExperiment')
    )
  test.table <- as.data.frame(round(assay(se)))
  if (!is.null(names(get_metacell(se)))) {
    test.table <- cbind(round(assay(se), digits = digits), get_metacell(se))
  } else {
    test.table <- cbind(
      test.table,
      as.data.frame(
        matrix(rep(NA, ncol(test.table) * nrow(test.table)),
          nrow = nrow(test.table)
        )
      )
    )
  }
  return(test.table)
}





#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param type The type od dataset
#'
#' @export
#'
#' @return A list
#'
BuildColorStyles <- function(type) {
  mc <- metacell.def(type)
  colors <- as.list(setNames(mc$color, mc$node))
  colors
}

