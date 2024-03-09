#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' This function is a wrapper to `heatmap.2()` that displays
#' assay data in an instance of `SummarizedExperiment`. For
#' more details, see `heatmap.2()`.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param width xxx
#' @param obj An instance of a class `MultiAssayExperiment`.
#' @param i xxx
#' @param qdata xxx
#' @param conds xx
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
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' 
#' 
#' 
#' 
#' 
#' 
#' @author Florence Combes, Samuel Wieczorek, Enora Fremy
#'
#' @name omXplore_heatmap
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_heatmap(vdata, 1)
#' }
#'
NULL




#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
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
    obj = reactive({NULL}),
    i = reactive({NULL}),
    width = 900) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #rv <- reactiveValues(data = NULL)

    observe(
      {
        # if (inherits(obj(), "MultiAssayExperiment")) {
        #   rv$data <- obj()
        # }

        shinyjs::toggle("badFormatMsg",
          condition = !inherits(obj(), "MultiAssayExperiment")
        )
        shinyjs::toggle("linkage",
          condition = !inherits(obj(), "MultiAssayExperiment")
        )
        shinyjs::toggle("distance",
          condition = !inherits(obj(), "MultiAssayExperiment")
        )
      },
      priority = 1000
    )


    limitHeatmap <- 20000
    height <- paste0(2 * width / 3, "px")
    width <- paste0(width, "px")

    output$omXplore_PlotHeatmap <- renderUI({
      req(obj())
      if (nrow(assay(obj(), i())) > limitHeatmap) {
        tags$p("The dataset is too large to compute the heatmap
                       in a reasonable time.")
      } else {
        plotOutput(ns("heatmap_ui"), width = width, height = height)
      }
    })



    output$heatmap_ui <- renderPlot({
      req(obj())
      input$linkage
      input$distance

      withProgress(message = "Making plot", value = 100, {
        heatmapD(
          qdata = assay(obj(), i()),
          conds = get_group(obj()),
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
omXplore_heatmap <- function(obj, i) {
  
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- fluidPage(
    omXplore_heatmap_ui("plot")
  )

  server <- function(input, output, session) {
    omXplore_heatmap_server("plot", 
      obj = reactive({obj}),
      i = reactive({i}))
  }

  shinyApp(ui = ui, server = server)
}
