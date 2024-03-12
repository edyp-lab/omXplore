#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name external_app
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An object of instance `MultiAssayExperiment`
#' @param i xxx
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   extFoo1(vdata, 1)
#'   extFoo2(vdata, 1)
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
    obj = reactive({NULL}),
    i = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe({
        req(obj())
        obj.cond <- inherits(obj(), "MultiAssayExperiment")
        if (obj.cond) {
          rv$data <- obj()
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
      obj = reactive({obj}),
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
    obj = reactive({NULL}),
    i = reactive({1})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe(
      {
        req(obj())
        obj.cond <- inherits(obj(), "MultiAssayExperiment")
        if (obj.cond) {
          rv$data <- obj()
        } else {
          shinyjs::toggle("badFormatMsg", condition = !obj.cond)
        }
      },
      priority = 1000
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
      obj = reactive({obj}),
      i = reactive({i})
      )
  }

  app <- shinyApp(ui = ui, server = server)
}
