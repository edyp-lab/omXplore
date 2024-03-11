#' @title plots_tracking_ui and plots_tracking_server
#'
#' @description This shiny module provides a tool to select
#'
#' @param id shiny id
#' @param obj An instance of the class `MultiAssayExperiment`
#' @param resetBtn A `boolean(1)` which indicates whether to show the 'Reset'
#' button or not.
#'
#' 
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   plots_tracking(vdata, 1)
#' }
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
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
    shinyjs::hidden(actionButton(ns("reset"), "Reset")),
    shinyjs::hidden(selectInput(ns("typeSelect"), "Type of selection",
      choices = character(0),
      selected = character(0),
      width = "130px"
    )),
    shinyjs::hidden(
      selectizeInput(
        inputId = ns("listSelect"),
        label = "Select protein",
        choices = character(0),
        width = "400px",
        multiple = TRUE,
        options = list(maxOptions = 10000)
      )
    ),
    shinyjs::hidden(
      textInput(ns("randSelect"), "Random", width = "120px")
    ),
    shinyjs::hidden(
      selectInput(ns("colSelect"), "Column", choices = character(0))
    )
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
  i = reactive({1}),
    resetBtn = reactive({
      FALSE
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv.track <- reactiveValues(
      type = character(0),
      data = NULL
    )


    dataOut <- reactiveValues(
      indices = NULL
    )


    observe(
      {
        if (inherits(obj(), "MultiAssayExperiment")) {
          rv.track$data <- obj()
        }

        shinyjs::toggle("badFormatMsg", condition = is.null(rv.track$data))
        shinyjs::toggle("typeSelect", condition = !is.null(rv.track$data))
      },
      priority = 1000
    )



    # observe({
    #   req(rv.track$data)
    #   shinyjs::toggle('reset', condition = isTRUE(resetBtn()))
    #
    #
    # })


    observe({
      req(rv.track$data)

      shinyjs::toggle("reset", condition = isTRUE(resetBtn()))


      if (length(Get_LogicalCols_in_Dataset()) > 0) {
        updateSelectInput(session, "typeSelect",
          choices = c(
            "None" = "None", "Protein list" = "List",
            "Random" = "Random", "Column" = "Column"
          )
        )
      } else {
        updateSelectInput(session, "typeSelect",
          choices = c(
            "None" = "None",
            "Protein list" = "List",
            "Random" = "Random"
          )
        )
      }

      
      .colID <- get_colID(rv.track$data[[i()]])
      .row <- rowData(rv.track$data[[i()]])
      if (!is.null(.colID) && .colID != "" && length(.colID) > 0) {
        .choices <- .row[, .colID]
        updateSelectizeInput(session, "listSelect",
          choices = .choices,
          selected = character(0),
          server = TRUE
        )
      }

      updateSelectInput(session, "randSelect",
        selected = character(0)
      )


      updateSelectInput(session, "colSelect",
        choices = Get_LogicalCols_in_Dataset(),
        selected = character(0)
      )
    })



    Get_LogicalCols_in_Dataset <- reactive({
      .row <- rowData(rv.track$data[[i()]])
      
      logical.cols <- lapply(
        colnames(.row),
        function(x) {
          is.logical(.row[, x])
        }
      )
      logical.cols <- which(unlist(logical.cols))
      logical.cols
    })


    observeEvent(input$typeSelect, {
      rv.track$type <- input$typeSelect
      shinyjs::toggle("listSelect", condition = input$typeSelect == "List")
      shinyjs::toggle("randSelect", condition = input$typeSelect == "Random")
      shinyjs::toggle("colSelect", condition = input$typeSelect == "Column" &&
        length(Get_LogicalCols_in_Dataset()) > 0)

      updateSelectInput(session, "listSelect", selected = "")
      updateSelectInput(session, "randSelect", selected = "")
      updateSelectInput(session, "colSelect", selected = NULL)

      if (input$typeSelect == "None") {
        RunReset()
      }
    })


    RunReset <- function() {
      updateSelectInput(session, "typeSelect", selected = "None")
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected = "")
      updateSelectInput(session, "colSelect", selected = NULL)

      rv.track$type <- character(0)
      dataOut$indices <- NULL
    }

    observeEvent(input$reset, ignoreInit = TRUE, ignoreNULL = TRUE, {
      RunReset()
    })


    observeEvent(req(length(input$listSelect) > 0), ignoreNULL = FALSE, {
      .row <- rowData(assay(rv.track$data[[i()]]))
      .id <- get_colID(rv.track$data[[i()]])
      dataOut$indices <- match(input$listSelect, .row[, .id])
    })


    observeEvent(req(!is.null(input$randSelect) && input$randSelect != ""),
      ignoreNULL = FALSE,
      {
        .row <- rowData(rv.track$data[[i()]])
        dataOut$indices <- sample(seq_len(nrow(.row)),
          as.numeric(input$randSelect),
          replace = FALSE
        )
      }
    )




    observeEvent(req(input$colSelect), {
      .row <- rowData(rv.track$data[[i()]])
      .arg <- .row[, input$colSelect]
      dataOut$indices <- which(.arg == TRUE)
    })


    return(reactive({
      dataOut$indices
    }))
  })
}




#' @export
#' @rdname plots_tracking
#' @return A shiny app
#'
plots_tracking <- function(obj, i) {
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- fluidPage(
    plots_tracking_ui("tracker")
  )

  server <- function(input, output, session) {
    indices <- plots_tracking_server("tracker", 
      obj = reactive({obj}),
      i = reactive({i})
    )
  }

  shinyApp(ui, server)
}
