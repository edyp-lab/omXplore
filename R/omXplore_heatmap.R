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
#' @param qdata A data.frame() of quantitative data.
#' @param conds A vector indicating the name of each sample.
#' @param distance The distance used by the clustering algorithm to compute
#' the dendrogram.
#' @param cluster the clustering algorithm used to build the dendrogram.
#' @param dendro A boolean to indicate fi the dendrogram has to be displayed
#' @param x A `matrix` or `array` containing the quantitative data.
#' @param col Colors used for the image. Defaults to heat colors (heat.colors).
#' @param srtCol Angle of column conds, in degrees from horizontal
#' @param labCol Character vectors with column conds to use.
#' @param labRow Character vectors with row conds to use.
#' @param key Logical indicating whether a color-key should be shown.
#' @param key.title Main title of the color key. If set to NA no title will
#' be plotted.
#' @param main Main title; default to none.
#' @param ylab y-axis title; default to none.
#'
#' @author Florence Combes, Samuel Wieczorek, Enora Fremy
#'
#' @name omXplore_heatmap
#'
#'
#' @examples
#' \dontrun{
#'   data(vdata)
#'   omXplore_heatmap(vdata, 1)
#' }
#' 
#' 
NULL



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
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



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' 
#' @rdname omXplore_heatmap
#' @export
#' @return NA
#'
omXplore_heatmap_server <- function(
    id,
    dataIn = reactive({NULL}),
    i = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    width = 900
    #rv <- reactiveValues(data = NULL)

    observe({
        # if (inherits(obj(), "MultiAssayExperiment")) {
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
      },
      priority = 1000
    )


    limitHeatmap <- 20000
    height <- paste0(2 * width / 3, "px")
    width <- paste0(width, "px")

    output$omXplore_PlotHeatmap <- renderUI({
      req(dataIn())
      if (nrow(assay(dataIn(), i())) > limitHeatmap) {
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
          qdata = assay(dataIn(), i()),
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
        dataIn = reactive({dataIn}),
      i = reactive({i}))
  }

  app <- shinyApp(ui = ui, server = server)
}
