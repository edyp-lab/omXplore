#' @title Display a CC
#'
#' @param cc A cc (a list)
#' @param metadata xxx
#' @param g A cc (a list)
#' @param layout xxxxx
#' @param obj xxx
#' @param prot.tooltip xxx
#' @param pept.tooltip xxx
#' @param df xxxx
#' @param clickFunction xxxx
#'
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' data(sub_R25_pept)
#' se1 <- sub_R25_pept[[1]]
#' g <- buildGraph(get_cc(se1)[[11]])
#' display.CC.visNet(g)
#'
#'
#' @name pep_prot_CC
#'
NULL


#' @rdname pep_prot_CC
#' @return A list
#' @export
buildGraph <- function(
    cc,
    metadata = NULL) {
  
  
  nb.prot <- ncol(cc)
  subX <- cc
  #colnames(subX) <- colnames(cc)
  subX <- as.matrix(subX)
  
  nb.pep <- nrow(cc)
  nb.pep.shared <- length(which(rowSums(subX) > 1))
  nb.pep.spec <- length(which(rowSums(subX) == 1))
  nb.total <- nb.prot + nb.pep
  edge.list <- as.data.frame(which(subX == 1, arr.ind = TRUE))

  def.grp <- c(rep("shared.peptide", nb.pep), rep("protein", nb.prot))
  def.grp[which(rowSums(subX) == 1)] <- "spec.peptide"

  buildNodesInfos <- function(cc, metadata, info.indice = 1) {
    
    nodes_infos <- NULL
    if (!is.null(metadata)) {
      #nodes_infos <- rep("", nrow(cc)+ncol(cc))
      nodes_infos <- vector()
      # We add infos only on peptides nodes
      for (i in seq(nrow(cc))) {
        ind <- which(rownames(metadata) == rownames(cc)[i])
        nodes_infos <- c(nodes_infos, 
          paste0(
          "<p>", colnames(metadata)[info.indice], ":",
          metadata[ind, info.indice], "</p>")
        )
      }
    }
    nodes_infos
  }

  nodes <- data.frame(
    id = seq(nb.total),
    group = def.grp,
    label = c(rownames(subX), colnames(subX)),
    size = c(rep(10, nb.pep), rep(20, nb.prot)),
    stringsAsFactors = FALSE
  )

  #browser()
  title <- buildNodesInfos(cc, metadata)
  #if (!is.null(title)) {
  #  nodes <- cbind(nodes, title)
  #}

  edges <- data.frame(
    from = c(edge.list$row),
    to = c(edge.list$col + nb.pep),
    stringsAsFactors = FALSE
  )

  return(
    list(
      nodes = nodes,
      edges = edges
    )
  )
}



#' @import highcharter
#' @import visNetwork
#' @rdname pep_prot_CC
#' @export
#' @return A plot
#'
display.CC.visNet <- function(
    g,
    layout = "layout_with_fr",
    obj = NULL,
    prot.tooltip = NULL,
    pept.tooltip = NULL) {
  col.prot <- "#ECB57C"
  col.spec <- "#5CA3F7"
  col.shared <- "#0EA513"

  visNetwork::visNetwork(g$nodes, g$edges, width = "100%", height = "100%") %>%
    visNetwork::visNodes(shape = "dot") %>% # square for all nodes
    visNetwork::visGroups(
      groupname = "spec.peptide",
      color = col.spec
    ) %>% # darkblue for group "A"
    visNetwork::visGroups(
      groupname = "shared.peptide",
      color = col.shared
    ) %>% # darkblue for group "A"
    visNetwork::visGroups(
      groupname = "protein",
      color = col.prot, shape = "dot"
    ) %>%
    visNetwork::visOptions(highlightNearest = FALSE) %>%
    # visLegend()
    # visPhysics(stabilization = FALSE)%>%
    visNetwork::visEdges(color = "#A9A9A9", width = 2) %>%
    visNetwork::visIgraphLayout(layout)
}



#' @return A plot
#'
#' @export
#' @import highcharter
#' @rdname pep_prot_CC
#'
plotCCJitter <- function(
    df,
    clickFunction = NULL) {
  if (is.null(clickFunction)) {
    clickFunction <-
      JS("function(event){Shiny.onInputChange('eventPointClicked',
          [this.index]+'_'+ [this.series.name]);}")
  }

  # i_tooltip <- which(startsWith(colnames(df), "tooltip"))
  txt_tooltip <- NULL

  # if (length(i_tooltip) == 0){
  #  warning("There is no tooltip in the object.")
  # } else {
  # for (i in i_tooltip) {
  #   txt_tooltip <- paste(txt_tooltip, "<b>",
  #                        gsub("tooltip_", "", colnames(df)[i], fixed = TRUE),
  #                        " </b>: {point.", colnames(df)[i], "} <br> ",
  #                        sep = "")
  # }
  # }

  highcharter::highchart() %>%
    highcharter::hc_add_series(data = df, type = "scatter") %>%
    customChart(zoomType = "xy", chartType = "scatter") %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_yAxis(title = list(text = "Nb of proteins")) %>%
    highcharter::hc_xAxis(title = list(text = "Nb of peptides")) %>%
    highcharter::hc_tooltip(
      enabled = FALSE,
      headerFormat = "",
      pointFormat = txt_tooltip
    ) %>%
    highcharter::hc_plotOptions(series = list(
      animation = list(duration = 100),
      cursor = "pointer",
      point = list(events = list(click = clickFunction))
    )) %>%
    customExportMenu(fname = "plotCC")
}





#' @title xxx
#' @description xxxx
#' @param cc xxx
#' @return A `list` of three items:
#' * `One_One`: the number of cc composed of one protein and one peptide
#' * `One_Multi`: the number of cc composed of one protein and several peptides
#' * `Multi_Multi`: the number of cc composed of several proteins and
#' several (shared) peptides.
#'
#' @examples
#' data(sub_R25_pept)
#' GetCCInfos(get_cc(vdata[[1]]))
#'
#' @export
#' @rdname pep_prot_CC
#'
GetCCInfos <- function(cc) {
  #stopifnot(inherits(cc, "list"))
  cc.infos <- list(
    One_One = list(),
    One_Multi = list(),
    Multi_Multi = list()
  )
  
  
  ll.prot <- lapply(cc, function(x) { ncol(x)})
  ll.pept <- lapply(cc, function(x) {nrow(x) })
  ll.prot.one2one <- intersect(which(ll.prot == 1), which(ll.pept == 1))
  ll.prot.one2multi <- intersect(which(ll.prot == 1), which(ll.pept > 1))
  ll.prot.multi2any <- which(ll.prot > 1)
  
  cc.infos[["One_One"]] <- cc[ll.prot.one2one]
  cc.infos[["One_Multi"]] <- cc[ll.prot.one2multi]
  cc.infos[["Multi_Multi"]] <- cc[ll.prot.multi2any]
  
  cc.infos
}

