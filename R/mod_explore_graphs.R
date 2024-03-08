#'
#' #' @param id A `character(1)` which is the id of the shiny module.
#' #'
#' #' @importFrom shiny NS tagList
#' #' @importFrom DT renderDT DTOutput formatStyle %>% styleEqual datatable
#' #' @importFrom shinyjs toggle hidden
#' #' @importFrom SummarizedExperiment rowData colData assays 
#' #'
#' #' @rdname connected-components
#' #' @export
#' explore_graphs_ui <- function(id) {
#'     ns <- NS(id)
#'
#'     pkgs.require('visNetwork')
#'
#'     fluidPage(
#'         tabPanel("Peptide-Protein Graph",
#'             value = "graphTab",
#'             tabsetPanel(
#'                 id = "graphsPanel",
#'                 tabPanel(
#'                     "Settings",
#'                     tagList(
#'                         uiOutput(ns("pepInfo_ui"))
#'                     )
#'                 ),
#'                 tabPanel(
#'                     "One-One CC",
#'                     tagList(
#'                         fluidRow(
#'                             column(width = 4, tagList(
#'                                 # download_btns_ui(ns('OneOneDT_DL_btns')),
#'                                 DT::dataTableOutput(ns("OneOneDT"))
#'                             )),
#'                             column(width = 8, tagList(
#'                                 # uiOutput(ns('OneOneDTDetailed_ui')),
#'                                 DT::dataTableOutput(ns("OneOneDTDetailed"))
#'                             ))
#'                         )
#'                     )
#'                 ),
#'                 tabPanel(
#'                     "One-Multi CC",
#'                     tagList(
#'                         fluidRow(
#'                             column(width = 4,
#'                                 tagList(
#'                                 DT::dataTableOutput(ns("OneMultiDT"))
#'                             )),
#'                             column(width = 8,
#'                                 tagList(
#'                                 DT::dataTableOutput(ns("OneMultiDTDetailed"))
#'                             ))
#'                         )
#'                     )
#'                 ),
#'                 tabPanel(
#'                     "Multi-Multi CC",
#'                     tagList(
#'                         selectInput(ns("searchCC"), "Search for CC",
#'                             choices = c(
#'                                 "Tabular view" = "tabular",
#'                                 "Graphical view" = "graphical"
#'                             ),
#'                             width = "150px"
#'                         ),
#'                         fluidRow(
#'                             column(
#'                                 width = 6,
#'                                 tagList(
#'                                   highcharter::highchartOutput(
#'                                   ns("graphicalView_ui")),
#'                                   uiOutput(ns("CCMultiMulti_DL_btns_ui")),
#'                                   dataTableOutput(ns("tabularView_ui"))
#'                                 )
#'                             ),
#'                             column(
#'                                 width = 6,
#'                                 tagList(
#'                                     visNetwork::visNetworkOutput(
#'                                         ns("visNet_CC"),
#'                                         height = "600px"
#'                                     )
#'                                 )
#'                             )
#'                         ),
#'                         uiOutput(ns("CCDetailed"))
#'                     )
#'                 )
#'             )
#'         )
#'     )
#' }
#'
#'
#' #' @param id A `character(1)` which is the id of the shiny module.
#' #' @param ll.cc xxx
#' #' @param se An object of class `QFeatures`
#' #' @param settings xxx
#' #'
#' #' @rdname connected-components
#' #' @export
#' explore_graphs_server <- function(id,
#'                                       ll.cc,
#'                                       se,
#'                                       settings) {
#'
#'
#'     pkgs.require('visNetwork')
#'
#'     moduleServer(id, function(input, output, session) {
#'         ns <- session$ns
#'
#'
#'         rv.cc <- reactiveValues(
#'             ## selected CC in global CC list (tab or plot)
#'             selectedCC = NULL,
#'             selectedNode = NULL,
#'             selectedNeighbors = NULL,
#'             selectedCCgraph = NULL,
#'
#'             # when the user selects a node in the graph
#'             detailedselectedNode = list(
#'                 sharedPepLabels = NULL,
#'                 specPepLabels = NULL,
#'                 protLabels = NULL
#'             )
#'         )
#'
#'
#'
#'         #----------------------------------------------------------------
#'         # "One-One CC"
#'         #----------------------------------------------------------------
#'
#'         # Extract one-one connected components from the complete list of CC
#'         BuildOne2OneTab <- reactive({
#'             ll.cc()
#'             table <- do.call(
#'                 rbind,
#'                 lapply(
#'                     Extract_CC(ll.cc(), "OneOne"),
#'                     function(x) {
#'                         data.frame(rbind(x))
#'                     }
#'                 )
#'             )
#'
#'             if (is.null(table)) {
#'                 table <- data.frame(
#'                     proteins = character(),
#'                     peptides = character()
#'                 )
#'             }
#'
#'             table
#'         })
#'
#'
#'
#'
#'         output$OneOneDT <- DT::renderDT({
#'             req(BuildOne2OneTab())
#'             DT::datatable(BuildOne2OneTab(),
#'                 selection = "single",
#'                 rownames = FALSE,
#'                 extensions = c("Scroller", "Buttons"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "Bfrtip",
#'                     deferRender = TRUE,
#'                     bLengthChange = FALSE,
#'                     scrollX = 400,
#'                     scrollY = 200,
#'                     scroller = TRUE,
#'                     orderClasses = TRUE,
#'                     autoWidth = FALSE,
#'                     columns.searchable = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             columns.width = c("60px"),
#'                             targets = c(
#'                                 list(0),
#'                                 list(1)
#'                             )
#'                         )
#'                     )
#'                 )
#'             )
#'         })
#'
#'
#'
#'         output$OneOneDTDetailed <- DT::renderDT({
#'             req(ll.cc())
#'             req(input$OneOneDT_rows_selected)
#'             input$pepInfo
#'
#'             line <- input$OneOneDT_rows_selected
#'
#'             ind <- seq_len(ncol(se()))
#'             data <- .getDataForExprs(se())
#'             pepLine <- as.numeric(BuildOne2OneTab()[line, 2])
#'             indices <- unlist(lapply(pepLine, function(x) {
#'                 which(rownames(data) == x)
#'             }))
#'             data <- data[indices, c(ind, (ind + ncol(data) / 2))]
#'
#'             if (!is.null(input$pepInfo)) {
#'                 data <- cbind(
#'                     data,
#'                     rowData(se())[pepLine, input$pepInfo]
#'                 )
#'                 .ind <- (1 + ncol(data) - length(input$pepInfo)):ncol(data)
#'                 colnames(data)[.ind] <- input$pepInfo
#'             }
#'
#'             offset <- length(input$pepInfo)
#'             .to <- ncol(data) - offset
#'             dt <- datatable(data,
#'                 extensions = c("Scroller", "Buttons"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "Bfrtip",
#'                     blengthChange = FALSE,
#'                     pageLength = 10,
#'                     displayLength = 10,
#'                     ordering = FALSE,
#'                     header = FALSE,
#'                     server = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             targets = seq.int(
#'                              from = (.to / 2) + 1, 
#'                              to = .to),
#'                             visible = FALSE
#'                         )
#'                     )
#'                 )
#'             ) %>%
#'                 DT::formatStyle(
#'                     colnames(data)[seq_len(.to / 2)],
#'                     colnames(data)[seq.int(from = (.to / 2) + 1, to = .to)],
#'                     backgroundColor = DT::styleEqual(
#'                         c("POV", "MEC"),
#'                         c("orange", "blue"))
#'                 )
#'
#'             dt
#'         })
#'
#'
#'         #----------------------------------------------------------------
#'         # "One-Multi CC"
#'         #----------------------------------------------------------------
#'         BuildOne2MultiTab <- reactive({
#'             ll.cc()
#'             table <- do.call(
#'                 rbind,
#'                 lapply(
#'                     Extract_CC(ll.cc(), "OneMulti"),
#'                     function(x) {
#'                         data.frame(rbind(x),
#'                             nPep = length(x$peptides)
#'                         )
#'                     }
#'                 )
#'             )
#'             table <- table[c("proteins", "nPep", "peptides")]
#'
#'             if (is.null(table)) {
#'                 table <- data.frame(
#'                     proteins = character(),
#'                     nPep = integer(),
#'                     peptides = character()
#'                 )
#'             }
#'             table
#'         })
#'
#'
#'         output$OneMultiDT <- DT::renderDT({
#'             req(BuildOne2MultiTab())
#'             DT::datatable(BuildOne2MultiTab(),
#'                 selection = "single",
#'                 rownames = FALSE,
#'                 extensions = c("Scroller", "Buttons"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "Bfrtip",
#'                     deferRender = TRUE,
#'                     bLengthChange = TRUE,
#'                     displayLength = 10,
#'                     scrollX = 400,
#'                     scrollY = 400,
#'                     scroller = TRUE,
#'                     orderClasses = TRUE,
#'                     autoWidth = FALSE,
#'                     columns.searchable = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             width = c("60px"),
#'                             targets = c(list(0), list(1), list(2))
#'                         )
#'                     )
#'                 )
#'             )
#'         })
#'
#'
#'
#'         output$OneMultiDTDetailed <- DT::renderDT({
#'             input$pepInfo
#'             req(input$OneMultiDT_rows_selected)
#'
#'             line <- input$OneMultiDT_rows_selected
#'
#'             ind <- seq(ncol(se()))
#'             data <- .getDataForExprs(se())
#'             .dat <- BuildOne2MultiTab()[line, "peptides"]
#'             pepLine <- as.numeric(unlist(.dat))
#'
#'             indices <- unlist(lapply(pepLine, function(x) {
#'                 which(rownames(data) == x)
#'             }))
#'
#'             data <- data[indices, c(ind, (ind + ncol(data) / 2))]
#'
#'             if (!is.null(input$pepInfo)) {
#'                 data <- cbind(
#'                     data,
#'                     rowData(se())[pepLine, input$pepInfo]
#'                 )
#'                 .ind <- (1 + ncol(data) - length(input$pepInfo)):ncol(data)
#'                 colnames(data)[.ind] <- input$pepInfo
#'             }
#'
#'             offset <- length(input$pepInfo)
#'             .to <- ncol(data) - offset
#'
#'             dt <- datatable(data,
#'                 extensions = c("Scroller", "Buttons"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "Bfrtip",
#'                     pageLength = 10,
#'                     blengthChange = FALSE,
#'                     displayLength = 10,
#'                     ordering = FALSE,
#'                     header = FALSE,
#'                     server = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             targets = seq.int(
#'                                        from = (.to / 2) + 1, 
#'                                        to = .to),
#'                             visible = FALSE
#'                         )
#'                     )
#'                 )
#'             ) %>%
#'                 DT::formatStyle(
#'                     colnames(data)[seq_len(.to / 2)],
#'                     colnames(data)[seq.int(from = (.to / 2) + 1, to = .to)],
#'                     backgroundColor = DT::styleEqual(
#'                         c("POV", "MEC"),
#'                         c("orange", "blue"))
#'                 )
#'
#'             dt
#'         })
#'
#'
#'
#'         #----------------------------------------------------------------
#'         # "Multi-Any CC"
#'         #----------------------------------------------------------------
#'
#'         output$graphicalView_ui <- renderHighchart({
#'             req(input$searchCC == "graphical")
#'             tooltip <- NULL
#'             if (!is.null(input$pepInfo)) {
#'                 tooltip <- rowData(se())[, input$pepInfo]
#'                 tooltip <- as.data.frame(tooltip)
#'                 colnames(tooltip) <- input$pepInfo
#'             }
#'
#'             clickFun <-
#'                 JS(paste0(
#'                     "function(event) {Shiny.onInputChange('",
#'                     ns("eventPointClicked"),
#'                     "', [this.index]+'_'+ [this.series.name]);}"
#'                 ))
#'
#'             plotCC <- plotJitter_hc(
#'                 ll.cc = Extract_CC(ll.cc(), "MultiAny"),
#'                 df.tooltips = tooltip,
#'                 clickFunction = clickFun
#'             )
#'
#'
#'             plotCC
#'         })
#'
#'
#'
#'         output$tabularView_ui <- DT::renderDT({
#'             req(input$searchCC == "tabular")
#'             df <- do.call(rbind, lapply(
#'                 Extract_CC(ll.cc(), "MultiAny"),
#'                 function(x) {
#'                     data.frame(rbind(x),
#'                         nPep = length(x$peptides),
#'                         nProt = length(x$proteins)
#'                     )
#'                 }
#'             ))
#'             df <- cbind(df, id = seq(nrow(df)))
#'             df <- df[c("id", "nProt", "nPep", "proteins", "peptides")]
#'
#'             DT::datatable(df,
#'                 selection = "single",
#'                 rownames = FALSE,
#'                 extensions = c("Scroller", "Buttons"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "Bfrtip",
#'                     deferRender = TRUE,
#'                     bLengthChange = FALSE,
#'                     scrollX = 400,
#'                     scrollY = 400,
#'                     displayLength = 10,
#'                     scroller = TRUE,
#'                     orderClasses = TRUE,
#'                     autoWidth = TRUE,
#'                     columns.searchable = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             columns.width = c("60px"),
#'                             targets = c(
#'                                 list(0),
#'                                 list(1),
#'                                 list(2)
#'                             )
#'                         )
#'                     )
#'                 )
#'             )
#'         })
#'
#'
#'
#'         output$pepInfo_ui <- renderUI({
#'             se()
#'             selectInput(ns("pepInfo"),
#'                 "PepInfo",
#'                 choices = colnames(SummarizedExperiment::rowData(se())),
#'                 multiple = TRUE
#'             )
#'         })
#'
#'         observeEvent(req(input$searchCC), {
#'             #print(input$searchCC)
#'             shinyjs::toggle("graphicalView_ui",
#'                 condition = input$searchCC == "graphical")
#'             shinyjs::toggle("tabularView_ui",
#'                 condition = input$searchCC == "tabular")
#'         })
#'
#'
#'         # select a point in the grpah
#'         observeEvent(input$click, {
#'             rv.cc$selectedNode <- input$click
#'         })
#'
#'
#'         # Get the id of selected neighbors in the graph
#'         observeEvent(input$visNet_CC_highlight_color_id, {
#'             rv.cc$selectedNeighbors <- input$visNet_CC_highlight_color_id
#'         })
#'
#'
#'         # select a CC in the summary table
#'         observeEvent(input$tabularView_ui_rows_selected, {
#'             rv.cc$selectedCC <- input$tabularView_ui_rows_selected
#'         })
#'
#'         # select a CC in the jitter plot
#'         observeEvent(req(input$eventPointClicked), {
#'             # browser()
#'             .str <- strsplit(input$eventPointClicked, "_")
#'             this.index <- as.integer(.str[[1]][1])
#'             # this.index + 1
#'             rv.cc$selectedCC <- this.index + 1
#'         })
#'
#'
#'         output$visNet_CC <- visNetwork::renderVisNetwork({
#'             req(rv.cc$selectedCC)
#'             local <- Extract_CC(ll.cc(), "MultiAny")
#'             # print(local[[rv.cc$selectedCC]])
#'             rv.cc$selectedCCgraph <- convertCC2graph(
#'                 cc = local[[rv.cc$selectedCC]],
#'                 X = QFeatures::adjacencyMatrix(se())
#'             )
#'
#'             show.graph(rv.cc$selectedCCgraph) %>%
#'                 visNetwork::visEvents(click = paste0("function(nodes){
#'                 Shiny.onInputChange('", ns("click"), "', nodes.nodes[0]);
#'                 Shiny.onInputChange('",
#'                 ns("node_selected"), "', nodes.nodes.length);
#'                 ;}")) %>%
#'                 visNetwork::visOptions(highlightNearest = TRUE)
#'         })
#'
#'
#'
#'
#'         observeEvent(c(
#'             rv.cc$selectedNeighbors,
#'             input$node_selected,
#'             rv.cc$selectedCCgraph
#'         ), {
#'             local <- Extract_CC(ll.cc(), "MultiAny")
#'             rv.cc$selectedNeighbors
#'
#'             nodes <- rv.cc$selectedCCgraph$nodes
#'
#'             if (!is.null(input$node_selected) && input$node_selected == 1) {
#'                 .ensA <- which(nodes[, "group"] == "shared.peptide")
#'                 .ensB <- which(nodes[, "group"] == "spec.peptide")
#'                 .ensC <- which(nodes[, "group"] == "protein")
#'                 sharedPepIndices <- intersect(rv.cc$selectedNeighbors, .ensA)
#'                 specPepIndices <- intersect(rv.cc$selectedNeighbors, .ensB)
#'                 protIndices <- intersect(rv.cc$selectedNeighbors, .ensC)
#'             } else {
#'                 .ensA <- which(nodes[, "group"] == "shared.peptide")
#'                 sharedPepIndices <- .ensA
#'                 specPepIndices <- which(nodes[, "group"] == "spec.peptide")
#'                 protIndices <- which(nodes[, "group"] == "protein")
#'             }
#'
#'             .node1 <- nodes[sharedPepIndices, "label"]
#'             .node2 <- nodes[specPepIndices, "label"]
#'             .node3 <- nodes[protIndices, "label"]
#'             rv.cc$detailedselectedNode$sharedPepLabels <- as.numeric(.node1)
#'             rv.cc$detailedselectedNode$specPepLabels <- as.numeric(.node2)
#'             rv.cc$detailedselectedNode$protLabels <- as.numeric(.node3)
#'         })
#'
#'
#'         output$CCDetailed <- renderUI({
#'             req(rv.cc$detailedselectedNode)
#'             req(rv.cc$selectedCC)
#'
#'             tagList(
#'                 h4("Proteins"),
#'                 DT::DTOutput(ns("CCDetailedProt")),
#'                 h4("Specific peptides"),
#'                 DT::DTOutput(ns("CCDetailedSpecPep")),
#'                 h4("Shared peptides"),
#'                 DT::DTOutput(ns("CCDetailedSharedPep"))
#'             )
#'         })
#'
#'         output$CCDetailedProt <- DT::renderDT({
#'             req(rv.cc$selectedCC)
#'             rv.cc$detailedselectedNode
#'             if (is.null(rv.cc$detailedselectedNode$protLabels)) {
#'                 return(NULL)
#'             }
#'
#'             df <- data.frame(
#'                 proteinId = unlist(rv.cc$detailedselectedNode$protLabels)
#'                 .dat <- rv.cc$detailedselectedNode$protLabels
#'                 # other = rep(NA,length(.dat))
#'             )
#'             dt <- datatable(df,
#'                 extensions = c("Scroller"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "rt",
#'                     blengthChange = FALSE,
#'                     ordering = FALSE,
#'                     scrollX = 400,
#'                     scrollY = 100,
#'                     displayLength = 10,
#'                     scroller = TRUE,
#'                     header = FALSE,
#'                     server = FALSE
#'                 )
#'             )
#'             dt
#'         })
#'
#'
#'
#'         #######
#'
#'         output$CCDetailedSharedPep <- DT::renderDT({
#'             rv.cc$detailedselectedNode
#'             input$pepInfo
#'
#'             req(rv.cc$detailedselectedNode$sharedPepLabels)
#'
#'
#'             ind <- seq(ncol(se()))
#'             data <- .getDataForExprs(se())
#'             pepLine <- rv.cc$detailedselectedNode$sharedPepLabels
#'             indices <- unlist(lapply(pepLine, function(x) {
#'                 which(rownames(data) == x)
#'             }))
#'             data <- data[indices, c(ind, (ind + ncol(data) / 2))]
#'
#'             if (!is.null(input$pepInfo)) {
#'                 data <- cbind(
#'                     data,
#'                     rowData(se())[pepLine, input$pepInfo]
#'                 )
#'                 .ind <- (1 + ncol(data) - length(input$pepInfo)):ncol(data)
#'                 colnames(data)[.ind] <- input$pepInfo
#'             }
#'
#'             offset <- length(input$pepInfo)
#'             .to <- ncol(data) - offset
#'                 dt <- datatable(data,
#'                 extensions = c("Scroller"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "rt",
#'                     blengthChange = FALSE,
#'                     ordering = FALSE,
#'                     scrollX = 400,
#'                     scrollY = 150,
#'                     displayLength = 10,
#'                     scroller = TRUE,
#'                     header = FALSE,
#'                     server = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             targets = seq.int(
#'                              from = (.to / 2) + 1, 
#'                                to = .to),
#'                             visible = FALSE
#'                         )
#'                     )
#'                 )
#'             ) %>%
#'                 DT::formatStyle(
#'                     colnames(data)[seq_len(.to / 2)],
#'                     colnames(data)[seq.int(from = (.to / 2) + 1, to = .to)],
#'                     backgroundColor = DT::styleEqual(
#'                         c("POV", "MEC"),
#'                         c("orange", "blue")
#'                         )
#'                 )
#'
#'             dt
#'         })
#'
#'
#'
#'
#'
#'         ##### -----------
#'         output$CCDetailedSpecPep <- DT::renderDT({
#'             rv.cc$detailedselectedNode
#'             input$pepInfo
#'             req(rv.cc$detailedselectedNode$specPepLabels)
#'
#'             ind <- seq(ncol(se()))
#'             data <- .getDataForExprs(se())
#'             pepLine <- rv.cc$detailedselectedNode$specPepLabels
#'             indices <- unlist(lapply(pepLine, function(x) {
#'                 which(rownames(data) == x)
#'             }))
#'             data <- data[indices, c(ind, (ind + ncol(data) / 2))]
#'
#'             if (!is.null(input$pepInfo)) {
#'                 data <- cbind(
#'                     data,
#'                     rowData(se())[pepLine, input$pepInfo]
#'                 )
#'                 .ind <- (1 + ncol(data) - length(input$pepInfo)):ncol(data)
#'                 colnames(data)[.ind] <- input$pepInfo
#'             }
#'
#'             offset <- length(input$pepInfo)
#'             .to <- ncol(data) - offset
#'
#'             dt <- datatable(data,
#'                 extensions = c("Scroller"),
#'                 options = list(
#'                     initComplete = .initComplete(),
#'                     dom = "rt",
#'                     blengthChange = FALSE,
#'                     ordering = FALSE,
#'                     scrollX = 400,
#'                     scrollY = 100,
#'                     displayLength = 10,
#'                     scroller = TRUE,
#'                     header = FALSE,
#'                     server = FALSE,
#'                     columnDefs = list(
#'                         list(
#'                             targets = seq.int(
#'                                        from = (.to / 2) + 1, 
#'                                        to = .to),
#'                             visible = FALSE
#'                         )
#'                     )
#'                 )
#'             ) %>%
#'                 DT::formatStyle(
#'                     colnames(data)[seq_len(.to / 2)],
#'                     colnames(data)[seq.int(from = (.to / 2) + 1, to = .to)],
#'                     backgroundColor = DT::styleEqual(
#'                         c("POV", "MEC"),
#'                         c("orange", "blue")
#'                         )
#'                 )
#'
#'             dt
#'         })
#'
#'
#'
#'
#'
#'         BuildMulti2AnyTab <- reactive({
#'             ll.cc()
#'             table <- do.call(
#'                 rbind,
#'                 lapply(
#'                     Extract_CC(ll.cc(), "MultiAny"),
#'                     function(x) {
#'                         data.frame(rbind(x), nPep = length(x$peptides))
#'                     }
#'                 )
#'             )
#'             table <- table[c("proteins", "nPep", "peptides")]
#'             if (is.null(table)) {
#'                 table <- data.frame(
#'                     proteins = character(),
#'                     nPep = integer(),
#'                     peptides = character()
#'                 )
#'             }
#'             table
#'         })
#'     })
#' }
#'
