#' @title Color legend for DaparToolshed
#'
#' @description
#' Shows a legend based on the tags in the package 'DaparToolshed'
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param presentTags A vector of `character()` which correspond to the tags.
#' @param hide.white  A `boolean()` to indicate whether the white cells must be
#' hidden or not.
#' @param dataIn An instance of the class `SummarizedExperiment`.
#'
#' @name color-legend
#' 
#' 
#' @examples
#' \dontrun{
#' data(vdata)
#' shiny::runApp(colorLegend(vdata[[1]]))
#' }
#' 
#'
NULL


#' @export
#' @rdname color-legend
#'
#' @return A vector
#'
custom_metacell_colors <- function() {
  list(
    "Any" = "white",
    "Missing" = "#CF8205",
    "Missing POV" = "#E5A947",
    "Missing MEC" = "#F1CA8A",
    "Quantified" = "#0A31D0",
    "Quant. by recovery" = "#B9C4F2",
    "Quant. by direct id" = "#6178D9",
    "Combined tags" = "#1E8E05",
    "Imputed" = "#A40C0C",
    "Imputed POV" = "#E34343",
    "Imputed MEC" = "#F59898"
  )
}



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyBS bsCollapsePanel bsCollapse
#' @rdname color-legend
#' @export
#'
#' @return NA
#'
colorLegend_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns('legend_UI'))
}




#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyBS bsCollapsePanel bsCollapse
#' @export
#' @rdname color-legend
#' @return NA
#'
colorLegend_server <- function(id,
                               presentTags = reactive({NULL}),
                               hide.white = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$legend_UI <- renderUI({
      req(presentTags)
      
      shinyBS::bsCollapse(
        id = "collapseExample",
        open = "",
        shinyBS::bsCollapsePanel(
          title = "Legend of colors",
          uiOutput(ns("legend")),
          style = ""
        )
      )
    })
    
    
    output$legend <- renderUI({
      req(presentTags)
      mc <- custom_metacell_colors()


      tagList(
        lapply(presentTags, function(x) {
          .cond <- mc[[x]] != "white" ||
            (mc[[x]] == "white" && !isTRUE(hide.white))
          if (.cond) {
            tagList(
              tags$div(
                class = "color-box",
                style = paste0("display:inline-block; vertical-align: middle;
                    width:20px; height:20px; border:1px solid #000;
                                 background-color: ", mc[[x]], ";"),
              ),
              tags$p(style = paste0("display:inline-block;
                                      vertical-align: middle;"), x),
              br()
            )
          }
        })
      )
    })
  })
}




#' @importFrom shiny fluidPage tagList shinyApp
#' @importFrom shinyBS bsCollapsePanel bsCollapse
#' @export
#' @rdname color-legend
#' @return A shiny app
#'
colorLegend <- function(obj = SummarizedExperiment::SummarizedExperiment()) {

  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  ui <- fluidPage(
    tagList(
      colorLegend_ui("plot1"),
      colorLegend_ui("plot2"),
      colorLegend_ui("plot3")
    )
  )

  server <- function(input, output, session) {
    tags <- GetMetacellTags(
      get_metacell(obj),
      level = get_type(obj),
      onlyPresent = TRUE
    )

    # Use the default color palette
    colorLegend_server("plot1", tags)

    # Use of a user-defined color palette
    colorLegend_server("plot2", tags)

    # Use of a  palette
    colorLegend_server("plot3", tags)
  }

  app <- shinyApp(ui, server)
}
