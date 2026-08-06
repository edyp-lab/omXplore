#' @title Variance plot
#'
#' @description
#' A shiny module which plots the variance of samples
#'
#' @name plot-variance
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of the class `MultiAssayExperiment`
#' @param i An integer which is the index of the assay in the param obj
#' @param conds A vector indicating the name of each sample.
#' @param pal.name A `character(1)` which is the name of the palette from the
#' package `RColorBrewer` from which the colors are taken.
#' Default value is 'Set1'.
#'
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     shiny::runApp(omXplore_variance(vdata, 1))
#' }
#'
NULL




#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom RColorBrewer brewer.pal
#' @import plotly
#' @importFrom DT JS
#' @importFrom stats var
#'
#'
#' @rdname plot-variance
#' @export
#' @return NA
#'
omXplore_variance_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        uiOutput(ns("helpTxt")),
        plotly::plotlyOutput(ns("viewDistCV"), width = 600, height = 600)
    )
}





#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom RColorBrewer brewer.pal
#' @import plotly
#' @importFrom DT JS
#' @importFrom stats var
#' @importFrom SummarizedExperiment assay
#'
#'
#'
#' @rdname plot-variance
#' @export
#' @return NA
#'
omXplore_variance_server <- function(
        id,
        dataIn,
        i,
        pal.name = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(data = NULL)

        observe(
            {
                is.mae <- inherits(dataIn(), "MultiAssayExperiment")
                if (is.mae) {
                    rv$data <- dataIn()
                }

                shinyjs::toggle("badFormatMsg", condition = !isTRUE(is.mae))
            },
            priority = 1000
        )

        output$viewDistCV <- renderPlotly({
            req(rv$data)
            withProgress(message = "Making plot", value = 100, {
                varDist <- CVDist(
                    dataIn = SummarizedExperiment::assay(rv$data, i()),
                    conds = get_group(dataIn()),
                    pal.name
                )
            })
        })

        output$helpTxt <- renderUI({
            req(rv$data)
            tagList(
                helpText("Display the condition-wise distributions of the
          log-intensity CV (Coefficient of Variation) of the
                   protein/peptides."),
                helpText("For better visualization, it is possible to zoom in by
            click-and-drag.")
            )
        })
    })
}




#' @importFrom stats density var
#' @import plotly
#'
#'
#' @export
#'
#' @param dataIn An matrix
#' @param pal.name A `character(1)` which is the name of the palette from
#' the package [RColorBrewer] from which the colors are taken. Default
#' value is 'Set1'.
#'
#'
#' @rdname plot-variance
#'
#' @return A plot
#'
CVDist <- function(
        dataIn,
        conds,
        pal.name = NULL) {
    stopifnot(inherits(dataIn, "matrix"))


    if (is.null(conds) || length(conds) == 0) {
        stop("conds contains no conds.")
    }

    u_conds <- unique(conds)
    myColors <- SampleColors(u_conds)
    n <- length(u_conds)

    p <- plotly::plot_ly()
    
    minX <- Inf
    maxX <- -Inf
    
    for (i in seq_len(n)) {
        
        idx <- which(conds == u_conds[i])
        
        if (length(idx) > 1) {
            t <- apply(
                dataIn[, idx, drop = FALSE], 1,
                function(x) {
                    m <- mean(x, na.rm = TRUE)
                    if (is.na(m) || m == 0) return(NA)
                    100 * stats::var(x, na.rm = TRUE) / m
                }
            )
            
            t <- t[!is.na(t)]
            
            if (length(t) > 1) {
                dens <- stats::density(t)
                
                minX <- min(minX, dens$x)
                xmaxY <- dens$x[which.max(dens$y)]
                maxX <- max(maxX, 10 * (xmaxY - minX))
                
                p <- p |>
                    plotly::add_trace(
                        x = dens$x,
                        y = dens$y,
                        type = "scatter",
                        mode = "lines",
                        name = u_conds[i],
                        line = list(color = myColors[i]),
                        hovertemplate = paste0(
                            "<b>", u_conds[i], "</b>: %{y:.2f}<extra></extra>"
                        )
                    )
            }
        }
    }
    
    if (!is.finite(minX) || !is.finite(maxX)) {
        minX <- NULL
        maxX <- NULL
    }
    
    p <- p |>
        plotly::layout(
            xaxis = list(
                title = "CV(log(Intensity))",
                range = if (!is.null(minX)) c(minX, maxX) else NULL, 
                zeroline = FALSE
            ),
            yaxis = list(title = "Density"),
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




#' @rdname plot-variance
#' @export
#' @return A shiny app
#'
omXplore_variance <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- fluidPage(
        omXplore_variance_ui("plot")
    )

    server <- function(input, output, session) {
        omXplore_variance_server("plot",
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
