#' @title plots_tracking_ui and plots_tracking_server
#'
#' @description This shiny module provides a tool to select
#'
#' @param id shiny id
#' @param obj An instance of the class `MultiAssayExperiment`
#' @param remoteReset A `boolean(1)` which indicates whether to show the 'Reset'
#' button or not.
#' @param is.enabled xxx
#'
#' 
#' @examples
#' \dontrun{
#'   data(vdata)
#'   shiny::runApp(plots_tracking(vdata[[1]]))
#'   }
#'
#' @name plots_tracking
#'
NULL



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' 
#' 
#' @rdname plots_tracking
#' @export
#' @return NA
#'
plots_tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("typeSelect_UI")),
    uiOutput(ns("listSelect_UI")),
    uiOutput(ns("randSelect_UI")),
    uiOutput(ns("colSelect_UI"))
  )
}



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' 
#' 
#' @rdname plots_tracking
#'
#' @export
#' @keywords internal
#' @return A `list` (same structure as the parameter `params`)
#'
plots_tracking_server <- function(
    id,
    obj = reactive({NULL}),
    remoteReset = reactive({NULL}),
    is.enabled = reactive({TRUE})
  ) {
  
  widgets.default.values <- list(
    typeSelect = "None",
    listSelect = NULL,
    randSelect = NULL,
    colSelect = 'None'
  )
  
  # rv.custom.default.values <- list(
  #   #indices = NULL
  # )
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    rv.widgets <- reactiveValues(
      typeSelect = widgets.default.values$typeSelect,
      listSelect = widgets.default.values$listSelect,
      randSelect = widgets.default.values$randSelect,
      colSelect = widgets.default.values$colSelect
    )
    
    rv <- reactiveValues(
      dataIn = NULL
    )
    
    dataOut <- reactiveValues(
      trigger = NULL,
      indices = NULL
    )
    
   
    observeEvent(obj(), ignoreNULL = TRUE, {
      
      stopifnot(inherits(obj(), 'SummarizedExperiment'))
          rv$dataIn <- obj()
          #dataOut$trigger <- as.numeric(Sys.time())
          #dataOut$indices <- NULL
      }, priority = 1000)


    observeEvent(remoteReset(), ignoreNULL = TRUE, ignoreInit = TRUE, {
      print('omXplore : in observeEvent(remoteReset()')
      
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
      
      rv$dataIn <- obj()
      updateSelectInput(session, "typeSelect", selected = widgets.default.values$typeSelect)
      updateSelectInput(session, ns("listSelect"), selected = widgets.default.values$listSelect)
      updateTextInput(session, ns("randSelect"), value = widgets.default.values$randSelect)
      updateSelectInput(session, ns("colSelect"), selected = widgets.default.values$colSelect)


      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$indices <- NULL
      
    })
    

    
    Get_LogicalCols_in_Dataset <- reactive({
      req(rv$dataIn)
      .row <- SummarizedExperiment::rowData(rv$dataIn)
      logical.cols <- lapply(colnames(.row),
        function(x) is.logical(.row[, x]))
      
      logical.cols <- which(unlist(logical.cols))
      logical.cols
    })
    
    output$typeSelect_UI <- renderUI({
      req(rv$dataIn)
      rv.widgets$typeSelect
      .choices <- c("None" = "None", 
        "List" = "List", 
        "Random" = "Random")
      
      if (length(Get_LogicalCols_in_Dataset()) > 0)
        .choices <- c(.choices, "Column" = "Column")
      
      selectInput(ns("typeSelect"), 
        "Type of selection",
        choices = .choices,
        selected = rv.widgets$typeSelect,
        width = "130px")
    })
    
    observeEvent(input$typeSelect, {rv.widgets$typeSelect <- input$typeSelect})
    observeEvent(input$listSelect, {rv.widgets$listSelect <- input$listSelect})
    observeEvent(input$randSelect, {rv.widgets$randSelect <- input$randSelect})
    observeEvent(input$colSelect, {rv.widgets$colSelect <- input$colSelect})
    
   
  output$listSelect_UI <- renderUI({
    req(rv$dataIn)
    req(rv.widgets$typeSelect == "List")
    
    .row <- SummarizedExperiment::rowData(rv$dataIn)
    
    .choices <- seq(nrow(.row))
    .colID <- get_colID(rv$dataIn)
    if (!is.null(.colID) && .colID != "" && length(.colID) > 0) {
      .choices <- .row[, .colID]
    }
    
    selectInput(ns("listSelect"),
      label = "Select protein",
      choices = .choices, # use server side option
      width = "400px",
      selected = rv.widgets$listSelect,
      multiple = TRUE
      #options = list(maxOptions = 10000)
    )
  })
  
  output$colSelect_UI <- renderUI({
    req(rv$dataIn)
    req(rv.widgets$typeSelect == "Column")
    req(length(Get_LogicalCols_in_Dataset()) > 0)
    
    selectInput(ns("colSelect"),
      "Column of rowData",
      choices = setNames(nm = c("None", colnames(rowData(rv$dataIn)))),
      selected = rv.widgets$colSelect
    )
  })
  
  
  output$randSelect_UI <- renderUI({
    req(rv.widgets$typeSelect == "Random")
    
    textInput(ns("randSelect"),
      "Random",
      value = rv.widgets$randSelect,
      width = ("120px")
    )
  })
  
  
  
    observeEvent(req(rv.widgets$listSelect), ignoreNULL = TRUE, {
req(rv$dataIn)
      .row <- SummarizedExperiment::rowData(rv$dataIn)
      .id <- get_colID(rv$dataIn)
      dataOut$trigger <- as.numeric(Sys.time())
      if(is.null(.id))
        dataOut$indices <- rv.widgets$listSelect
      else
        dataOut$indices <- match(rv.widgets$listSelect, .row[, .id])
    })


    observeEvent(req(rv.widgets$randSelect), ignoreNULL = TRUE, {
      req(rv$dataIn)
      
        cond <- is.null(rv.widgets$randSelect)
        cond <- cond || rv.widgets$randSelect == ""
        cond <- cond || (as.numeric(rv.widgets$randSelect) < 0)
        cond <- cond || (as.numeric(rv.widgets$randSelect) > nrow(rv$dataIn))
        if (!cond) {
          .row <- SummarizedExperiment::rowData(rv$dataIn)
          dataOut$trigger <- as.numeric(Sys.time())
        dataOut$indices <- sample(seq_len(nrow(.row)),
          as.numeric(rv.widgets$randSelect),
          replace = FALSE
        )
        }
      })




    observeEvent(req(rv.widgets$colSelect), {
      req(rv$dataIn)
      req(rv.widgets$colSelect != "None")
      dataOut$trigger <- as.numeric(Sys.time())
      .op1 <- rowData(rv$dataIn)[, rv.widgets$colSelect]
      dataOut$indices <- which(.op1 == TRUE)
    })


    return(reactive({dataOut}))
  })
}




#' @export
#' @rdname plots_tracking
#' @return A shiny app
#'
plots_tracking <- function(obj) {
  stopifnot(inherits(obj, "SummarizedExperiment"))
  
  ui <- fluidPage(
    actionButton('reset', 'Reset'),
      plots_tracking_ui("tracker1")
  )

  server <- function(input, output, session) {
    indices <- plots_tracking_server(
      id = "tracker1", 
      obj = reactive({obj}),
      remoteReset = reactive({input$reset})
    )
    
    observeEvent(req(indices()$indices), {
      print(indices()$indices)
    })
  }

  app <- shiny::shinyApp(ui, server)
}
