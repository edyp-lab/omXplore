#' @title Explore `MultiAssayExperiment` objects.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of the class `MultiAssayExperiment`
#' @param i An integer which is the index of the assay in the param obj
#' @param digits An integer for the number of digits shown in the table
#'
#' @name omXplore_tabExplorer
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     shiny::runApp(omXplore_tabExplorer(vdata, 1))
#' }
#'
#' @return NA
#'
NULL



#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom DT renderDT datatable formatStyle styleEqual renderDataTable DTOutput
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' @importFrom shinyWidgets radioGroupButtons
#'
#'
#' @rdname omXplore_tabExplorer
#'
#' @examples
#' NULL
#'
#' @export
#' @return NA
#'
omXplore_tabExplorer_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        shinyjs::useShinyjs(),
        tags$head(
            tags$style(
                HTML("
                .custom-radio-group .btn.active {
                    font-weight: bold;
                    background-color: #c0c0c0; 
                    color: black;
                }
                .custom-radio-group .btn {
                    background-color: #f0f0f0; 
                    color: black;
                }
                .custom-radio-group{
                    margin-bottom: -50px;
                    z-index: 1;
                    position: absolute;
                }")
            )
        ),
        
        shinyjs::hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        div(id = ns("div_legend"), colorLegend_ui(ns("legend"))),
       
        fluidPage(
            div(class = "custom-radio-group",
                shinyWidgets::radioGroupButtons(
                    inputId = ns("tab_controller"),
                    choices = c("Assays", "Row data", "Metacell"),
                    selected = "Assays"
                )
            ),
            
            tabsetPanel(
                id = ns("hidden_tabs"),
                type = "hidden",  # Hide the default tabs
                tabPanel(
                    title = "Assays",
                    value = "Assays",
                    DT::DTOutput(ns("qdata_ui"))
                ),
                tabPanel(
                    title = "Row data",
                    value = "Row data",
                    DT::DTOutput(ns("metadata_ui"))
                ),
                tabPanel(
                    title = "Metacell",
                    value = "Metacell",
                    DT::DTOutput(ns("qMetacell_ui"))
                )
            )
        )
    )
}


#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom DT renderDT datatable formatStyle styleEqual renderDataTable DTOutput
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' @import plotly
#'
#'
#' @return NA
#'
#' @rdname omXplore_tabExplorer
#'
#' @export
omXplore_tabExplorer_server <- function(
        id,
        dataIn = reactive({NULL}),
        i = reactive({NULL}),
        digits = reactive({3})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL,
                             i = NULL)
        
        observeEvent(input$tab_controller, {
            updateTabsetPanel(
                session,
                "hidden_tabs",
                selected = input$tab_controller
            )
        })

        observe(
            {
                is.mae <- inherits(dataIn(), "MultiAssayExperiment")

                if (isTRUE(is.mae)) {
                    rv$data <- dataIn()
                    
                    if (i() %in% names(rv$data)){
                        rv$i <- i()
                    } else {
                        rv$i <- names(rv$data)[length(rv$data)]
                    }

                    tags <- GetMetacellTags(
                        get_metacell(rv$data[[rv$i]]),
                        level = get_type(rv$data[[rv$i]]),
                        onlyPresent = TRUE
                    )

                    colorLegend_server("legend", reactive({tags}))
                }

                shinyjs::toggle("badFormatMsg", condition = !isTRUE(is.mae))
                shinyjs::toggle("div_infos", condition = !is.null(rv$data))
                shinyjs::toggle("div_legend", condition = !is.null(rv$data))
            },
            priority = 1000
        )


        output$metadata_ui <- DT::renderDT({
            req(rv$data)

            .row <- SummarizedExperiment::rowData(rv$data[[rv$i]])
            cols_to_remove <- c("adjacencyMatrix", "qMetacell")
            .row <- .row[, -grep(paste(cols_to_remove, collapse = "|"), colnames(.row))]

            dat <- DT::datatable(as.data.frame(.row),
                rownames = TRUE,
                extensions = c("Scroller", "FixedColumns"),
                options = list(
                    initComplete = .initComplete(),
                    dom = "Bfrtip",
                    pageLength = 10,
                    deferRender = TRUE,
                    bLengthChange = FALSE,
                    scrollX = 200,
                    scrollY = 600,
                    scroller = TRUE,
                    orderClasses = TRUE,
                    autoWidth = FALSE,
                    columns.searchable = FALSE,
                    fixedColumns = list(
                        leftColumns = 1
                    ),
                    columnDefs = list(
                        list(
                            columns.width = c("60px"),
                            targets = c(list(0), list(1), list(2))
                        )
                    )
                )
            )

            if ("Significant" %in% colnames(.row)) {
                dat <- dat |>
                    DT::formatStyle(
                        columns = "Significant",
                        target = "row",
                        background = DT::styleEqual(1, "lightblue")
                    )
            }

            return(dat)
        })


        output$qdata_ui <- DT::renderDataTable(server = TRUE, {
            req(rv$data)
            # .keyId <- df <- NULL
            # .row <- rowData(rv$data[[rv$i]])
            # .colId <- get_colID(rv$data[[rv$i]])
            # .metacell <- get_metacell(rv$data[[rv$i]])
            #
            # if (.colId != '' &&  ncol(.row) > 0 && nrow(.row) > 0)
            #   .keyId <- (.row)[, .colId]
            # else
            #   .keyId <- rownames(assay(rv$data[[rv$i]]))
            #
            # .qdata <- round(SummarizedExperiment::assay(rv$data[[rv$i]]),
            #     digits = digits())
            #
            # .qdata.exists <- (!is.null(.qdata) &&
            #     ncol(.qdata) > 0) &&
            #   (nrow(.qdata) > 0)
            #
            # .metacell.exists <- (!is.null(.metacell) &&
            #     ncol(.metacell) > 0) &&
            #   (nrow(.metacell) > 0)
            #
            #
            #  #if (.qdata.exists){
            #    if(.metacell.exists)
            #      df <- cbind(keyId = .keyId, .qdata, .metacell)
            #    else
            #      df <- cbind(keyId = .keyId, .qdata)
            #  #}

            df <- Build_enriched_qdata(rv$data[[rv$i]])
            .metacell.exists <- !isTRUE(all.equal(rv$data[[rv$i]], df))

            colors <- custom_metacell_colors()

            dt <- DT::datatable(as.data.frame(df),
                extensions = c("Scroller", "FixedColumns"),
                options = list(
                    initComplete = .initComplete(),
                    displayLength = 20,
                    deferRender = TRUE,
                    bLengthChange = FALSE,
                    scrollX = 200,
                    scrollY = 600,
                    scroller = TRUE,
                    ordering = FALSE,
                    server = TRUE,
                    fixedColumns = list(
                        leftColumns = 1
                    ),
                    columnDefs = if (.metacell.exists) {
                        list(
                            list(
                                targets = c(((2 + (ncol(df) - 1) / 2)):ncol(df)),
                                visible = FALSE
                            )
                        )
                    } else {
                        NULL
                    }
                )
            )

            if (.metacell.exists) {
                dt <- dt |>
                    DT::formatStyle(
                        colnames(df)[2:(1 + (ncol(df) - 1) / 2)],
                        colnames(df)[((2 + (ncol(df) - 1) / 2)):ncol(df)],
                        backgroundColor = DT::styleEqual(
                            names(colors),
                            unname(unlist(colors))
                        ),
                        backgroundSize = "98% 48%",
                        backgroundRepeat = "no-repeat",
                        backgroundPosition = "center"
                    )
            }

            dt
        })

        output$qMetacell_ui <- DT::renderDataTable(server = TRUE, {
            req(rv$data)
            df <- get_metacell(rv$data[[rv$i]])
            colors <- custom_metacell_colors()

            DT::datatable(as.data.frame(df),
                extensions = c("Scroller", "FixedColumns"),
                options = list(
                    initComplete = .initComplete(),
                    displayLength = 20,
                    deferRender = TRUE,
                    bLengthChange = FALSE,
                    scrollX = 200,
                    scrollY = 600,
                    scroller = TRUE,
                    ordering = FALSE,
                    fixedColumns = list(
                        leftColumns = 1
                    ),
                    server = TRUE
                )
            ) |>
                DT::formatStyle(
                    colnames(df),
                    colnames(df),
                    backgroundColor = DT::styleEqual(
                        names(colors),
                        unname(unlist(colors))
                    ),
                    backgroundSize = "98% 48%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })
    })
}


#' @rdname omXplore_tabExplorer
#' @export
#' @return A shiny app
#'
omXplore_tabExplorer <- function(dataIn, i) {
    ui <- fluidPage(omXplore_tabExplorer_ui("plot"))

    server <- function(input, output, session) {
        omXplore_tabExplorer_server("plot",
            dataIn = reactive({dataIn}),
            i = reactive({i})
        )
    }

    app <- shinyApp(ui = ui, server = server)
}
