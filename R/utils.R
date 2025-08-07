#' @title Global variables
#' @description Defines the global variables for the package `omXplore`
#' @export
#' @examples
#' globals()
#'
#' @return A `list`
#'
globals <- function() {
    list(
        general_style = "display:inline-block; vertical-align: middle; padding: 7px",
        actionBtnClass = "btn-primary",
        bad_format_txt = "Dataset in not in correct format.
omXplore can handle MSnset and QFeatures files.
    Please use the function convert_2_mae()"
    )
}


#' @title Loads packages
#'
#' @description Checks if a package is available to load it
#'
#' @param ll.deps A `character()` vector which contains packages names
#'
#' @examples
#' pkgs.require("omXplore")
#'
#' @export
#' @return NA
#'
#' @author Samuel Wieczorek
#'
pkgs.require <- function(ll.deps) {
    lapply(ll.deps, function(x) {
        if (!requireNamespace(x, quietly = TRUE)) {
            stop(paste0("Please install ", x, ": BiocManager::install('", x, "')"))
        }
    })
}




#' @title Checks the class of a list's slots
#' @description Checks if all slots of the given list are of the same class.
#' @export
#' @param object A `list`
#' @param obj.class The name of the class to search in items of the list.
#' @examples
#' ll <- as.list(LETTERS[1:3])
#' is.listOf(ll, "data.frame")
#' is.listOf(ll, "character")
#'
#' @return A `character(1)` with the name of the package or NULL
#'
is.listOf <- function(object, obj.class = NULL) {
    res <- NULL

    if (is.null(obj.class)) {
        ll <- unlist(lapply(object, function(x) class(x)[[1]]))
        if (length(unique(ll)) == 1) {
            res <- unique(ll)
        }
    } else {
        res <- TRUE

        res <- res && inherits(object, "list")
        res <- res &&
            all(unlist(lapply(
                object,
                function(x) class(x)[[1]] == obj.class
            )))
    }

    res
}



#' @title Package version
#' @description Gets the version number of a package
#' @export
#' @param pkg The name of the package
#' @examples
#' GetPkgVersion("omXplore")
#'
#' @return A `character(1)` with the name of the package and
#' its version number.
#' @importFrom utils installed.packages
#'
GetPkgVersion <- function(pkg) {
    tryCatch(
        {
            ind <- which(utils::installed.packages()[, "Package"] == pkg)
            version <- utils::installed.packages()[ind, "Version"]
            paste0(pkg, "_", version)
        },
        warning = function(w) message(w),
        error = function(e) message(e)
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
#' @param width The width of the plot
#' @param height The height of the plot
#'
#' @return A highchart plot
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' if (interactive()) {
#'     library(highcharter)
#'     hc <- highchart()
#'     hc_chart(hc, type = "line")
#'     hc_add_series(hc, data = c(29, 71, 40))
#'     customChart(hc)
#' }
#'
#' @export
#' @examples
#' NULL
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
    stopifnot(inherits(se, "SummarizedExperiment"))
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





#' @title Build color style for DT tables
#'
#' @description
#' This function builds a list which is used for styling DT tables with the
#' function `DT::styleEqual()`
#'
#' @param type The type of dataset. Available values are `protein` and `peptide`
#'
#' @export
#'
#' @return A list
#' @examples
#' NULL
#'
BuildColorStyles <- function(type) {
    mc <- metacell.def(type)
    colors <- as.list(setNames(mc$color, mc$node))
    colors
}



#' @title Builds enriched assay with cell metadata info
#'
#' @description
#' If the cell metadata exists in the object of class `SummarizedExperiment`,
#' then these information are added to the quantitative data so as to use
#' styles with the functions of the package `DT`.
#'
#' @param obj An instance of the class `SummarizedExperiment`
#'
#' @export
#'
#' @return A data.frame with new colums corresponding to the cell metadata
#' (if exists)
#'
#' @examples
#' NULL
#'
Build_enriched_qdata <- function(obj) {
    stopifnot(inherits(obj, "SummarizedExperiment"))

    .keyId <- enriched_df <- NULL
    .row <- SummarizedExperiment::rowData(obj)
    .colId <- get_colID(obj)
    .metacell <- get_metacell(obj)

    if (.colId != "" && ncol(.row) > 0 && nrow(.row) > 0) {
        .keyId <- (.row)[, .colId]
    } else {
        .keyId <- rownames(SummarizedExperiment::assay(obj))
    }

    .qdata <- SummarizedExperiment::assay(obj)

    .qdata.exists <- (!is.null(.qdata) &&
        ncol(.qdata) > 0) &&
        (nrow(.qdata) > 0)

    .metacell.exists <- (!is.null(.metacell) &&
        ncol(.metacell) > 0) &&
        (nrow(.metacell) > 0)


    # if (.qdata.exists){
    if (.metacell.exists) {
        enriched_df <- cbind(keyId = .keyId, .qdata, .metacell)
    } else {
        enriched_df <- cbind(keyId = .keyId, .qdata)
    }
    # }

    return(enriched_df)
}
