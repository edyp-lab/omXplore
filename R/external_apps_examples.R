#' @title External module example
#'
#' @description
#' Example for an external shiny module, well structured to be run within a
#' workflow for `MagellanNTK`
#'
#' @name external_app
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An object of instance `MultiAssayExperiment`
#' @param i An integer which is the index of the assay in the param obj
#'
#'
#' @examples
#' if (interactive()) {
#'     data(vdata)
#'     app1 <- extFoo1(vdata, 1)
#'     app2 <- extFoo2(vdata, 1)
#'     shiny::runApp(app1)
#'     shiny::runApp(app2)
#' }
#'
#' @return NA
#'
NULL


#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        plotOutput(ns("plot"))
    )
}



#' @import shiny 
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @importFrom SummarizedExperiment assay
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_server <- function(
        id,
        dataIn = reactive({
            NULL
        }),
        i = reactive({
            NULL
        })) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(
            data = NULL
        )

        observe(
            {
                req(dataIn())
                obj.cond <- inherits(dataIn(), "MultiAssayExperiment")
                if (obj.cond) {
                    rv$data <- dataIn()
                } else {
                    shinyjs::toggle("badFormatMsg", condition = !obj.cond)
                }
            },
            priority = 1000
        )

        output$plot <- renderPlot({
            req(rv$data)
            graphics::hist(SummarizedExperiment::assay(rv$data[[i()]]))
        })
    })
}




#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo1 <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- extFoo1_ui("plot")

    server <- function(input, output, session) {
        extFoo1_server("plot",
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




#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(
            id = ns("badFormatMsg"),
            h3(globals()$bad_format_txt)
        )),
        plotOutput(ns("plot"))
    )
}



#' @import shiny
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @importFrom SummarizedExperiment assay
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_server <- function(
        id,
        dataIn = reactive({
            NULL
        }),
        i = reactive({
            NULL
        })) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv <- reactiveValues(
            data = NULL
        )

        observe(
            {
                req(dataIn())
                obj.cond <- inherits(dataIn(), "MultiAssayExperiment")
                if (obj.cond) {
                    rv$data <- dataIn()
                } else {
                    shinyjs::toggle("badFormatMsg", condition = !obj.cond)
                }
            },
            priority = 1000
        )

        output$plot <- renderPlot({
            req(rv$data)
            plot(SummarizedExperiment::assay(rv$data[[i()]]))
        })
    })
}



#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo2 <- function(dataIn, i) {
    stopifnot(inherits(dataIn, "MultiAssayExperiment"))

    ui <- extFoo2_ui("plot")

    server <- function(input, output, session) {
        extFoo2_server("plot",
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
