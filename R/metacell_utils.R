#' @title Quantitative metadata vocabulary for entities
#'
#' @description
#' This function gives the vocabulary used for the quantitative metadata of
#' each entity in each condition.
#'
#' @section Glossary:
#'
#' Peptide-level vocabulary
#'
#' |-- 'Any'
#' |    |
#' |    |-- 1.0 'Quantified'
#' |    |    |
#' |    |    |-- 1.1 "Quant. by direct id" (color 4, white)
#' |    |    |
#' |    |    |-- 1.2 "Quant. by recovery" (color 3, lightgrey)
#' |    |
#' |    |-- 2.0 "Missing" (no color)
#' |    |    |
#' |    |    |-- 2.1 "Missing POV" (color 1)
#' |    |    |
#' |    |    |-- 2.2 'Missing MEC' (color 2)
#' |    |
#' |    |-- 3.0 'Imputed'
#' |    |    |
#' |    |    |-- 3.1 'Imputed POV' (color 1)
#' |    |    |
#' |    |    |-- 3.2 'Imputed MEC' (color 2)
#'
#'
#'
#' Protein-level vocabulary:
#' |-- 'Any'
#' |    |
#' |    |-- 1.0 'Quantified'
#' |    |    |
#' |    |    |-- 1.1 "Quant. by direct id" (color 4, white)
#' |    |    |
#' |    |    |-- 1.2 "Quant. by recovery" (color 3, lightgrey)
#' |    |
#' |    |-- 2.0 "Missing"
#' |    |    |
#' |    |    |-- 2.1 "Missing POV" (color 1)
#' |    |    |
#' |    |    |-- 2.2 'Missing MEC' (color 2)
#' |    |
#' |    |-- 3.0 'Imputed'
#' |    |    |
#' |    |    |-- 3.1 'Imputed POV' (color 1)
#' |    |    |
#' |    |    |-- 3.2 'Imputed MEC' (color 2)
#' |    |
#' |    |-- 4.0 'Combined tags' (color 3bis, lightgrey)
#'
#'
#' @section Conversion to the glossary:
#'
#' A generic conversion
#'
#' Conversion for Proline datasets
#'
#' Conversion from Maxquant datasets
#'
#'
#' @name q_metadata
#'
#' @examples
#'
#' metacell.def("protein")
#' metacell.def("peptide")
#'
#' #-----------------------------------------------
#' # A shiny app to view color legends
#' #-----------------------------------------------
#' if (interactive()) {
#'   data(vdata)
#'   ui <- qMetacellLegend_ui("legend")
#'
#'   server <- function(input, output, session) {
#'     qMetacellLegend_server("legend",
#'       object = reactive({vdata[[1]]})
#'     )
#'   }
#'
#'   shinyApp(ui = ui, server = server)
#' }
#'
NULL



#' @param level A string designing the type of entity/pipeline.
#' Available values are: `peptide`, `protein`
#'
#' @return A data.frame containing the different tags and corresponding colors
#' for the level given in parameter
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @export
#'
#' @rdname q_metadata
#'
#' @return A list
#'
metacell.def <- function(level) {
  if (missing(level)) {
    stop("'level' is required.")
  }

  def <- switch(level,
    peptide = {
      node <- c(
        "Any",
        "Quantified",
        "Quant. by direct id",
        "Quant. by recovery",
        "Missing",
        "Missing POV",
        "Missing MEC",
        "Imputed",
        "Imputed POV",
        "Imputed MEC"
      )
      parent <- c(
        "",
        "Any",
        "Quantified",
        "Quantified",
        "Any",
        "Missing",
        "Missing",
        "Any",
        "Imputed",
        "Imputed"
      )
      data.frame(
        node = node,
        parent = parent
      )
    },
    protein = {
      node <- c(
        "Any",
        "Quantified",
        "Quant. by direct id",
        "Quant. by recovery",
        "Missing",
        "Missing POV",
        "Missing MEC",
        "Imputed",
        "Imputed POV",
        "Imputed MEC",
        "Combined tags"
      )
      parent <- c(
        "",
        "Any",
        "Quantified",
        "Quantified",
        "Any",
        "Missing",
        "Missing",
        "Any",
        "Imputed",
        "Imputed",
        "Any"
      )

      data.frame(
        node = node,
        parent = parent
      )
    }
  )


  colors <- custom_metacell_colors()

  def <- cbind(def, color = rep("white", nrow(def)))

  for (n in seq_len(nrow(def))) {
    def[n, "color"] <- colors[[def[n, "node"]]]
  }

  return(def)
}




#' @title Parent name of a node
#' @description xxx
#' @param level xxx
#' @param node xxx
#'
#' #' @examples
#' Parent('protein', 'Missing')
#' Parent('protein', 'Missing POV')
#' Parent('protein', c('Missing POV', 'Missing MEC'))
#' Parent('protein', c('Missing', 'Missing POV', 'Missing MEC'))
#'
#' @rdname q_metadata
#' @export
#'
#' @return A vector
#'
Parent <- function(level, node = NULL) {
  parents <- NULL
  tags <- metacell.def(level)

  if (!is.null(node) && length(node) > 0) {
    for (p in node) {
      ind <- match(p, tags$node)
      if (length(ind) > 0) {
        parents <- unique(c(parents, tags$parent[ind]))
      }
    }
  }


  return(parents)
}

#' @title Names of all chidren of a node
#' @description xxx
#' @param level xxx
#' @param parent xxx
#' @rdname q_metadata
#' @examples
#' Children("protein", "Missing")
#' Children("protein", "Missing POV")
#' Children("protein", c("Missing POV", "Missing MEC"))
#' Children("protein", c("Missing", "Missing POV", "Missing MEC"))
#' @export
#'
#' @return A vector
#'
Children <- function(level, parent = NULL) {
  childrens <- NULL
  tags <- metacell.def(level)
  if (!is.null(parent) && length(parent) > 0) {
    for (p in parent) {
      ind <- grepl(p, tags$parent)
      if (length(ind) > 0) {
        childrens <- unique(c(childrens, tags$node[ind]))
      }
    }
  }
  return(childrens)
}


#' @title Get metacell tags
#'
#' @description xxx
#'
#' @param object A DataFrame() representing the cell metadata
#' @param level A string corresponding to the type of object
#' @param onlyPresent A `boolean(1)`
#'
#' @examples
#' NULL
#'
#' @export
#' @rdname q_metadata
#' @author Samuel Wieczorek
#'
#' @return A vector
#'
GetMetacellTags <- function(
    object = NULL,
    level = NULL,
    onlyPresent = FALSE) {
  
  ll <- NULL
  
  
  if(is.null(object) || !inherits(object, 'DataFrame')){
    return(invisible(NULL))
  }
    

  if (is.null(level) && onlyPresent == TRUE) {
    stop("level must be defined if 'onlyPresent' equals to FALSE")
  } else {
    return(invisible(NULL))
  }


  
  if (onlyPresent) {
    # Compute unique tags
    tmp <- lapply(colnames(object), function(x) unique(object[, x]))
    ll <- unique(unlist(tmp))

    # Check if parent must be added
    test <- match(Children(level, "Any"), ll)
    if (length(test) == length(Children(level, "Any")) && !all(is.na(test))) {
      ll <- c(ll, "Any")
    }

    test <- match(Children(level, "Quantified"), ll)
    .cond <- length(Children(level, "Quantified")) && !all(is.na(test))
    if (length(test) == .cond) {
      ll <- c(ll, "Quantified")
    }

    test <- match(Children(level, "Missing"), ll)
    .cond <- length(Children(level, "Missing")) && !all(is.na(test))
    if (length(test) == .cond) {
      ll <- c(ll, "Missing")
    }

    test <- match(Children(level, "Imputed"), ll)
    if (length(test) == length(Children(level, "Imputed")) &&
      !all(is.na(test))) {
      ll <- c(ll, "Imputed")
    }

    test <- match(Children(level, "Combined tags"), ll)
    .cond <- length(Children(level, "Combined tags")) && !all(is.na(test))
    if (length(test) == .cond) {
      ll <- c(ll, "Combined tags")
    }
  } else {
    ll <- metacell.def(level)$node[-which(metacell.def(level)$node == "Any")]
  }


  return(ll)
}
