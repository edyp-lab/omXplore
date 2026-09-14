#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @name corrmatrix
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of the class `SummarizedExperiment`
#' @param i An integer which is the index of the assay in the param dataIn
#' @param rate Default value is 0.9
#' @param showValues Default is FALSE.
#'
#'
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     omXplore_corrmatrix(vdata, 1)
#' }
#'
NULL

#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @import plotly
#' @importFrom DT JS
#' @importFrom tibble tibble as_tibble
#' @importFrom stats cor
#' @import tidyr
#' @importFrom dplyr mutate left_join select 
#'
#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        div(style = "display: flex; gap: 8px;",
            uiOutput(ns("rate_ui")),
            uiOutput(ns("showValues_ui"))),
        tags$hr(),
        plotly::plotlyOutput(ns("plot"),
            width = "600px", height = "500px"
        )
    )
}



#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @import plotly
#' @importFrom DT JS
#' @importFrom tibble tibble as_tibble
#' @importFrom stats cor
#' @import tidyr
#' @importFrom dplyr mutate left_join select 
#' @importFrom SummarizedExperiment assay
#'
#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_server <- function(
        id,
        dataIn = reactive({NULL}),
        i = reactive({1})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(
            data = NULL,
            i = NULL
        )
        
        observeEvent(dataIn(), ignoreInit = FALSE, {
            shinyjs::toggle("badFormatMsg",
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

        output$rate_ui <- renderUI({
            req(inherits(dataIn(), "MultiAssayExperiment"))
            sliderInput(ns("rate"),
                "Tune to modify the color gradient",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.01
            )
        })


        output$showValues_ui <- renderUI({
            req(inherits(dataIn(), "MultiAssayExperiment"))
            checkboxInput(ns("showLabels"), "Show labels",
                value = FALSE
            )
        })

        output$plot <- plotly::renderPlotly({
            req(dataIn())
            req(rv$i)

            withProgress(message = "Making plot", value = 100, {
                tmp <- corrMatrix(
                    data = SummarizedExperiment::assay(dataIn()[[rv$i]]),
                    rate = input$rate,
                    showValues = isTRUE(input$showLabels)
                )
            })

            tmp
        })
    })
}






#' @param data An object of class 'matrix'
#'
#' @param rate The rate parameter to control the exponential law for
#' the gradient of colors
#'
#' @param showValues A boolean which indicates whether to show values in the
#' correlation plot.
#'
#' @return A plot
#' 
#' @examples
#' data(vdata)
#' corrMatrix(vdata[[1]])
#'
#' @importFrom stats cor
#' 
#' @rdname corrmatrix
#' 
#' @export
#'
corrMatrix <- function(data, 
                       rate = 0.5, 
                       showValues = FALSE) {
    cor_mat <- cor(data, use = "pairwise.complete.obs")
    vars <- colnames(cor_mat)
    
    p <- plotly::plot_ly(
        x = vars,
        y = vars,
        z = cor_mat,
        type = "heatmap",
        zmin = rate,
        zmax = 1,
        colorscale = list(
            list(0, "#2E86C1"),
            list(0.5, "#F8F5F5"),
            list(1, "#FF5733")
        ),
        hovertemplate = paste0(
            "%{x} ~ %{y}: <b>%{z:.2f}</b>",
            "<extra></extra>"
        )
    )
    
    if (showValues) {
        p <- p |> plotly::add_annotations(
            x = rep(vars, each = length(vars)), 
            y = rep(vars, times = length(vars)), 
            text = sprintf( "<span style='color:white; text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;'>%.2f</span>", as.vector(t(cor_mat)) ), 
            showarrow = FALSE, 
            font = list(color = "white", 
                        size = 12 ) 
        )
    }
    
    p |>
        plotly::layout(
            xaxis = list(title = NULL),
            yaxis = list(title = NULL, autorange = "reversed")
        )
}





#' @export
#' @rdname corrmatrix
#' @return A shiny app
#'
omXplore_corrmatrix <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- omXplore_corrmatrix_ui("plot")

    server <- function(input, output, session) {
        omXplore_corrmatrix_server("plot",
            dataIn = reactive({
                dataIn
            }),
            i = reactive({
                i
            })
        )
    }

    app <- shiny::runApp(shinyApp(ui = ui, server = server))
}
