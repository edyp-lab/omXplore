#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' This function is a wrapper to `heatmap.2()` that displays
#' quantitative data in an object of #' class `VizData`. For
#' more details, see `heatmap.2()`.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param width xxx
#' @param obj An instance of a class `VizData`.
#' @param distance The distance used by the clustering algorithm to compute
#' the dendrogram.
#' @param cluster the clustering algorithm used to build the dendrogram.
#' @param dendro A boolean to indicate fi the dendrogram has to be displayed
#' @param x xxx
#' @param col A palette of colors
#' @param srtCol xxx
#' @param labCol xxx
#' @param labRow xxxx
#' @param key xxx
#' @param key.title xxxx
#' @param main xxx
#' @param ylab xxxx
#'
#'
#' @author Florence Combes, Samuel Wieczorek, Enor Fremy
#'
#' @name omXplore_heatmap
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_heatmap(vdata[[1]])
#' }
#'
NULL



#' @importFrom shiny NS tagList
#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
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
    tags$hr(),
    uiOutput(ns("omXplore_PlotHeatmap"))
  )
}




#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_server <- function(
    id,
    obj = reactive({
      NULL
    }),
    width = 900) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data = NULL)

    observe(
      {
        if (inherits(obj(), "VizData")) {
          rv$data <- obj()
        }

        shinyjs::toggle("badFormatMsg",
          condition = !inherits(obj(), "VizData")
        )
        shinyjs::toggle("linkage",
          condition = !inherits(obj(), "VizData")
        )
        shinyjs::toggle("distance",
          condition = !inherits(obj(), "VizData")
        )
      },
      priority = 1000
    )


    limitHeatmap <- 20000
    height <- paste0(2 * width / 3, "px")
    width <- paste0(width, "px")

    output$omXplore_PlotHeatmap <- renderUI({
      req(rv$data)
      if (nrow(rv$data@qdata) > limitHeatmap) {
        tags$p("The dataset is too large to compute the heatmap
                       in a reasonable time.")
      } else {
        plotOutput(ns("heatmap_ui"), width = width, height = height)
      }
    })



    output$heatmap_ui <- renderPlot({
      req(rv$data)
      input$linkage
      input$distance

      withProgress(message = "Making plot", value = 100, {
        heatmapD(
          obj = rv$data,
          distance = input$distance,
          cluster = input$linkage
        )
      })
    })
  })
}



#' @import shiny
#' @rdname omXplore_heatmap
#' @export
#' @return A shiny app
#'
omXplore_heatmap <- function(obj) {
  ui <- fluidPage(
    omXplore_heatmap_ui("plot")
  )

  server <- function(input, output, session) {
    omXplore_heatmap_server("plot", reactive({
      obj
    }))
  }

  shinyApp(ui = ui, server = server)
}
