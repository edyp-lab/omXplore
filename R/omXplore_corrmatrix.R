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
#' @import highcharter
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
        highcharter::highchartOutput(ns("plot"),
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
#' @import highcharter
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

        output$plot <- highcharter::renderHighchart({
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
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate left_join select
#' @importFrom tidyr gather
#' @importFrom stats cor
#' @importFrom highcharter list_parse2 highchart hc_xAxis hc_yAxis
#' hc_add_series hc_plotOptions hc_tooltip hc_legend hc_colorAxis
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

    requireNamespace('dplyr')
    res <- cor(data, use = "pairwise.complete.obs")

    df <- tibble::as_tibble(res)
    colnames(df) <- colnames(data)

    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 2)
    dist <- NULL

    x <- y <- names(df)

    df <- tibble::as_tibble(cbind(x = y, df)) |>
    
        tidyr::gather(y, dist, -x) |>
        dplyr::mutate(
            x = as.character(x),
            y = as.character(y)
        ) |>
        dplyr::left_join(
            tibble::tibble(
                x = y,
                xid = seq(length(y)) - 1
            ),
            by = "x"
        ) |>
        dplyr::left_join(
            tibble::tibble(
                y = y,
                yid = seq(length(y)) - 1
            ),
            by = "y"
        )

    ds <- df |>
        dplyr::select("xid", "yid", "dist") |>
        highcharter::list_parse2()

    fntltp <- DT::JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                  this.series.yAxis.categories[this.point.y] + ': <b>' +
                  Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
    cor_colr <- list(
        list(0, "#FF5733"),
        list(0.5, "#F8F5F5"),
        list(1, "#2E86C1")
    )


    highcharter::highchart() |>
        customChart(chartType = "heatmap") |>
        highcharter::hc_xAxis(categories = y, title = NULL) |>
        highcharter::hc_yAxis(categories = y, title = NULL) |>
        highcharter::hc_add_series(data = ds) |>
        highcharter::hc_plotOptions(
            series = list(
                boderWidth = 0,
                dataConditions = list(enabled = TRUE),
                dataLabels = list(enabled = showValues)
            )
        ) |>
        highcharter::hc_tooltip(formatter = fntltp) |>
        highcharter::hc_legend(
            align = "right", layout = "vertical",
            verticalAlign = "middle"
        ) |>
        highcharter::hc_colorAxis(stops = cor_colr, min = rate, max = 1) |>
         highcharter::hc_exporting(
        enabled = TRUE,
        filename = 'corrMatrix',
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
