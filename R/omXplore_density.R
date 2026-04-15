#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of the class `SummarizedExperiment`
#' @param i An integer which is the index of the assay in the param obj
#' @param pal.name A `character(1)` which is the name of the palette from
#' the package [RColorBrewer] from which the colors are taken. Default
#' value is 'Set1'.
#' @param data A data.frame() of quantitative data
#' @param conds A vector indicating the name of each sample.
#'
#'
#' @name density-plot
#'
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     shiny::runApp(omXplore_density(vdata, 1))
#' }
#'
NULL


#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom stats density
#' @rdname density-plot
#' @export
#' @return NA
#'
omXplore_density_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        fluidPage(
            shinyjs::hidden(div(
                id = ns("badFormatMsg"),
                h3(globals()$bad_format_txt)
            )),
            plotly::plotlyOutput(ns("plot_ui"))
        )
    )
}





#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom stats density
#' @importFrom SummarizedExperiment assay
#' @rdname density-plot
#'
#'
#' @export
#' @return NA
#'
omXplore_density_server <- function(
        id,
        dataIn = reactive({
            NULL
        }),
        i = reactive({
            1
        }),
        pal.name = reactive({
            NULL
        })) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # rv <- reactiveValues(
        #   data = NULL
        # )

        observeEvent(dataIn(),
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            {
                # if (inherits(obj(), "SummarizedExperiment")) {
                #   rv$data <- obj()
                # }

                shinyjs::toggle("badFormatMsg",
                    condition = !inherits(dataIn(), "MultiAssayExperiment")
                )
            },
            priority = 1000
        )


        output$plot_ui <- plotly::renderPlotly({
            req(dataIn())
            req(i())

            tmp <- NULL
            isolate({
                withProgress(message = "Making plot", value = 100, {
                    tmp <- densityPlot(
                        data = SummarizedExperiment::assay(dataIn(), i()),
                        conds = get_group(dataIn()),
                        pal.name = pal.name()
                    )
                })
            })
            tmp
        })
    })
}







#' @importFrom stats density
#'
#'
#' @export
#'
#' @rdname density-plot
#'
#' @return A plot
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     qdata <- SummarizedExperiment::assay(vdata[[1]])
#'     conds <- get_group(vdata)
#'     densityPlot(qdata, conds)
#' }
#'
densityPlot <- function(
        data,
        conds = NULL,
        pal.name = NULL) {
    if (missing(data)) {
        stop("'data' is missing.")
    }

    #print("data......")
    # print(head(data))
    # if (missing(conds)) {
    #   stop("'conds' is missing.")
    # }

    # if (length(conds) != ncol(data)) {
    #   stop("data and conds must have the same number of samples.")
    # }

    myColors <- NULL
    if (length(conds) > 0) {
        myColors <- SampleColors(conds, pal.name)
    } else {
        myColors <- SampleColors(seq(ncol(data)), pal.name)
    }


    p <- plotly::plot_ly()
    
    for (i in seq_len(ncol(data))) {
        
        dens <- stats::density(data[, i], na.rm = TRUE)
        
        p <- p |>
            plotly::add_trace(
                x = dens$x,
                y = dens$y,
                type = "scatter",
                mode = "lines",
                name = legend[i],
                line = list(color = myColors[i]),
                hovertemplate = paste0(
                    "<b>", legend[i], "</b>: %{y:.2f}<extra></extra>"
                )
            )
    }
    
    p <- p |>
        plotly::layout(
            title = "Density plot",
            xaxis = list(title = "log(Intensity)"),
            yaxis = list(title = "Density"),
            margin = list(t = 60, b = 60),
            legend = list(
                orientation = "h",
                x = 0,
                y = -0.15,
                xanchor = "left",
                yanchor = "top"
            )
        )
    
    return(p)
}






#' @export
#' @rdname density-plot
#' @return A shiny app
#'
omXplore_density <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- omXplore_density_ui("plot")

    server <- function(input, output, session) {
        omXplore_density_server("plot",
            dataIn = reactive({
                dataIn
            }),
            i = reactive({
                i
            })
        )
    }

    app <- shinyApp(ui, server)
}
