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

#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
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
        uiOutput(ns("showValues_ui")),
        uiOutput(ns("rate_ui")),
        plotly::plotlyOutput(ns("plot"),
            width = "600px", height = "500px"
        )
    )
}



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
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

        observe(
            {
                shinyjs::toggle("badFormatMsg",
                    condition = !inherits(dataIn(), "MultiAssayExperiment")
                )
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

            withProgress(message = "Making plot", value = 100, {
                tmp <- corrMatrix(
                    data = SummarizedExperiment::assay(dataIn()[[i()]]),
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
#' @export
#'
#' @importFrom stats cor
#'
#' @return A plot
#'
#' @rdname corrmatrix
#'
#'
corrMatrix <- function(
        data,
        rate = 0.5,
        showValues = FALSE) {
    stopifnot(inherits(data, "matrix"))

    df <- cor(data, use = "pairwise.complete.obs")
    
    is.num <- vapply(df, is.numeric, FUN.VALUE = NA)
    df[is.num] <- lapply(df[is.num], round, 2)
    mat <- as.matrix(df)
    labels <- colnames(mat)
    
    text_mat <- if (showValues) {
        matrix(sprintf("%.2f", mat), nrow = nrow(mat))
    } else {
        NULL
    }
    
    plotly::plot_ly(
        x = labels,
        y = labels,
        z = mat,
        type = "heatmap",
        colorscale = list(
            list(0, "#FF5733"),
            list(0.5, "#F8F5F5"),
            list(1, "#2E86C1")
        ),
        zmin = rate,
        zmax = 1,
        text = text_mat,
        texttemplate = if (showValues) "%{text}" else NULL,
        hovertemplate = paste(
            "%{y} ~ %{x}: <b>%{z:.2f}</b><extra></extra>"
        )
    ) |>
        plotly::layout(
            xaxis = list(title = "", side = "top"),
            yaxis = list(title = ""),
            margin = list(l = 100, r = 100)
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
