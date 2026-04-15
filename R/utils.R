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
#' pkgs.require2("omXplore")
#'
#' @export
#' @return NA
#'
#' @author Samuel Wieczorek
#'
pkgs.require2 <- function(ll.deps) {
    lapply(ll.deps, function(x) {
        if (!requireNamespace(x, quietly = TRUE)) {
            txt <- paste0("Please install ", x, ": BiocManager::install('", x, "')")
            stop(txt)
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
#' @importFrom SummarizedExperiment assay
#'
#' @param se An instance of the class `SummarizedExperiment`
#' @param digits An 'integer(1)' to specify the number of digits to display
#' in the tables for numerical values. Default is 2.
#'
#' @return A data.frame
#'
#' @export
#' 
#' @examples
#' NULL
#'
FormatDataForDT <- function(
        se,
        digits = 2) {
    stopifnot(inherits(se, "SummarizedExperiment"))
    test.table <- as.data.frame(round(SummarizedExperiment::assay(se)))
    if (!is.null(names(get_metacell(se)))) {
        test.table <- cbind(round(SummarizedExperiment::assay(se), digits = digits), get_metacell(se))
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
