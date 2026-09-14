#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' This function is a wrapper to `heatmap.2()` that displays
#' assay data in an instance of `SummarizedExperiment`. For
#' more details, see `heatmap.2()`.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of a class `MultiAssayExperiment`.
#' @param i An integer which is the index of the assay in the param obj
#'
#' @author Florence Combes, Samuel Wieczorek, Enora Fremy
#'
#' @name omXplore_heatmap
#'
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     omXplore_heatmap(vdata, 1)
#' }
#'
NULL



#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_ui <- function(id) {
    ns <- NS(id)
    tagList(
        useShinyjs(),
        hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        hidden(div(
            style = "display:inline-block; vertical-align: middle;
                  padding-right: 20px;",
            selectInput(ns("distance"), "Distance",
                choices = setNames(nm = c("euclidean", "manhattan")),
                selected = "euclidean",
                width = "150px"
            )
        )),
        hidden(div(
            style = "display:inline-block; vertical-align: middle;
                 padding-right: 20px;",
            selectInput(ns("linkage"), "Linkage",
                choices = setNames(nm = c("complete", "ward.D", "average")),
                selected = "complete",
                width = "150px"
            )
        )),
        #tags$hr(),
        uiOutput(ns("omXplore_PlotHeatmap"))
    )
}



#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment assay
#'
#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_server <- function(
        id,
        dataIn = reactive({
            NULL
        }),
        i = reactive({
            NULL
        })) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        width <- 900
        # rv <- reactiveValues(data = NULL)

        rv <- reactiveValues(
            data = NULL,
            i = NULL
        )
        
        observeEvent(dataIn(),
                      {
             # if (inherits(obj(), "SummarizedExperiment")) {
             #   rv$data <- obj()
             # }
             
             shinyjs::toggle("badFormatMsg",
                             condition = !inherits(dataIn(), "MultiAssayExperiment")
             )
             shinyjs::toggle("linkage",
                             condition = !inherits(dataIn(), "MultiAssayExperiment")
             )
             shinyjs::toggle("distance",
                             condition = !inherits(dataIn(), "MultiAssayExperiment")
             )
             
             if (i() %in% names(dataIn())){
                 rv$i <- i()
             } else {
                 rv$i <- names(dataIn())[length(dataIn())]
             }
         },
         priority = 1000
        )

        limitHeatmap <- 20000
        height <- paste0(2 * width / 3, "px")
        width <- paste0(width, "px")

        output$omXplore_PlotHeatmap <- renderUI({
            req(dataIn())
            if (nrow(SummarizedExperiment::assay(dataIn(), rv$i)) > limitHeatmap) {
                tags$p("The dataset is too large to compute the heatmap
                       in a reasonable time.")
            } else {
                plotOutput(ns("heatmap_ui"), width = width, height = height)
            }
        })

        output$heatmap_ui <- renderPlot({
            req(dataIn())
            input$linkage
            input$distance

            withProgress(message = "Making plot", value = 100, {
                heatmapD(
                    qdata = SummarizedExperiment::assay(dataIn(), rv$i),
                    conds = get_group(dataIn()),
                    distance = input$distance,
                    cluster = input$linkage
                )
            })
        })
    })
}




#' @rdname omXplore_heatmap
#' @export
#' @return A shiny app
#'
omXplore_heatmap <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- fluidPage(
        omXplore_heatmap_ui("plot")
    )

    server <- function(input, output, session) {
        omXplore_heatmap_server("plot",
            dataIn = reactive({
                dataIn
            }),
            i = reactive({
                i
            })
        )
    }

    app <- shinyApp(ui = ui, server = server)
}
