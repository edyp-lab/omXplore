#' @title External module example
#'
#' @description
#' Example for an external shiny module, well structured to be run within a 
#' workflow for `MagellanNTK`
#'
#' @name external_app
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An object of instance `MultiAssayExperiment`
#' @param i An integer which is the index of the assay in the param obj
#'
#'
#' @examples
#' \dontrun{
#'   data(vdata)
#'   app1 <- extFoo1(vdata, 1)
#'   app2 <- extFoo2(vdata, 1)
#'   shiny::runApp(app1)
#'   shiny::runApp(app2)
#' }
#' 
#' @return NA
#'
NULL


#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
    plotOutput(ns("plot"))
  )
}



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_server <- function(
    id,
    dataIn = reactive({NULL}),
    i = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe({
        req(dataIn())
        obj.cond <- inherits(dataIn(), "MultiAssayExperiment")
        if (obj.cond) {
          rv$data <- dataIn()
        } else {
          shinyjs::toggle("badFormatMsg", condition = !obj.cond)
        }
      },  priority = 1000
    )

    output$plot <- renderPlot({
      req(rv$data)
      hist(assay(rv$data[[i()]]))
    })
  })
}




#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo1 <- function(obj, i) {
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- extFoo1_ui("plot")

  server <- function(input, output, session) {
    extFoo1_server("plot", 
        dataIn = reactive({obj}),
      i = reactive({i})
      )
  }

  app <- shinyApp(ui = ui, server = server)
}




#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
    plotOutput(ns("plot"))
  )
}



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_server <- function(
    id,
    dataIn = reactive({NULL}),
    i = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe({
        req(dataIn())
        obj.cond <- inherits(dataIn(), "MultiAssayExperiment")
        if (obj.cond) {
          rv$data <- dataIn()
        } else {
          shinyjs::toggle("badFormatMsg", condition = !obj.cond)
        }
      }, priority = 1000
    )

    output$plot <- renderPlot({
      req(rv$data)
      plot(assay(rv$data[[i()]]))
    })
  })
}



#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo2 <- function(obj, i) {
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- extFoo2_ui("plot")

  server <- function(input, output, session) {
    extFoo2_server("plot",  
        dataIn = reactive({obj}),
      i = reactive({i})
      )
  }

  app <- shinyApp(ui = ui, server = server)
}
