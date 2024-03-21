#' @title Explore `MultiAssayExperiment` objects.
#'
#' @description xxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `MultiAssayExperiment`
#' @param i xxx
#' @param digits xxx
#'
#' 
#' 
#' @name omXplore_tabExplorer
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_tabExplorer(vdata, 1)
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
#' @importFrom DT DTOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom DT renderDT datatable formatStyle styleEqual renderDataTable
#' DTOutput
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' @importFrom shinyBS bsCollapsePanel bsCollapse
#' 
#' 
#' @rdname omXplore_tabExplorer
#' @import shinyBS
#'
#' @export
#' @return NA
#'
omXplore_tabExplorer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
    shinyjs::hidden(div(id = ns("div_legend"), colorLegend_ui(ns("legend")))),
    shinyjs::hidden(
      div(
        id = ns("div_infos"),
        shinyBS::bsCollapse(
          id = "infos", open = "", multiple = TRUE,
          shinyBS::bsCollapsePanel("Assays",
            DT::DTOutput(ns("qdata_ui")),
            style = "info"
          ),
          shinyBS::bsCollapsePanel("Row data",
            DT::DTOutput(ns("metadata_ui")),
            style = "info"
          ),
          shinyBS::bsCollapsePanel("Metacell",
            DT::DTOutput(ns("qMetacell_ui")),
            style = "info"
          )
        )
      )
    )
  )
}




#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom DT renderDT datatable formatStyle styleEqual renderDataTable
#' DTOutput
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom SummarizedExperiment rowData colData assays
#' @importFrom shinyBS bsCollapsePanel bsCollapse
#' @import highcharter
#' 
#' 
#' @return NA
#'
#' @rdname omXplore_tabExplorer
#'
#' @export
omXplore_tabExplorer_server <- function(
    id,
    obj = reactive({NULL}),
    i = reactive({NULL}),
    digits = reactive({3})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data = NULL)

    observe(
      {
        is.mae <- inherits(obj(), "MultiAssayExperiment")
        
        if (isTRUE(is.mae)){
          rv$data <- obj()

          tags <- GetMetacellTags(
            get_metacell(rv$data[[i()]]),
            level = get_type(rv$data[[i()]]),
            onlyPresent = TRUE
          )

          colorLegend_server("legend", tags)
        }

        shinyjs::toggle("badFormatMsg", condition = !isTRUE(is.mae))
        shinyjs::toggle("div_infos", condition = !is.null(rv$data))
        shinyjs::toggle("div_legend", condition = !is.null(rv$data))
      },
      priority = 1000
    )


    #
    #     output$viewDesign <- DT::renderDT({
    #       req(rv$data)
    #
    #       data <- tibble::as_tibble(SummarizedExperiment::colData(se()))
    #
    #       pal <- unique(RColorBrewer::brewer.pal(8, "Dark2"))
    #
    #       dt <- DT::datatable(  data,
    #       extensions = c('Scroller', 'Buttons'),
    #       rownames=  FALSE,
    #       options=list(initComplete = .initComplete(),
    #       dom = 'Brtip',
    #       pageLength=10,
    #       orderClasses = TRUE,
    #       autoWidth=TRUE,
    #       deferRender = TRUE,
    #       bLengthChange = FALSE,
    #       scrollX = 200,
    #       scrollY = 500,
    #       scroller = TRUE,
    #       columnDefs = list(list(width='60px',targets= "_all"))
    #       )) %>%
    #         DT::formatStyle(
    #           columns = colnames(data)[seq_len(2)],
    #           valueColumns = colnames(data)[2],
    #           backgroundColor = DT::styleEqual(
    #           unique(data$Condition),
    #           pal[seq_len(length(unique(data$Condition)))])
    #         )
    #
    #     })


    output$metadata_ui <- DT::renderDT({
      req(rv$data)

      .row <- SummarizedExperiment::rowData(rv$data[[i()]])
      
      tryCatch({# remove columns that are instances of DataFrame
      .row <- .row[, -match('adjacencyMatrix', colnames(.row))]
      .row <- .row[, -match('metacell', colnames(.row))]
      },
        warning = function(w) NULL,
        error = function(e) NULL
        )
      
      dat <- DT::datatable(as.data.frame(.row),
        rownames = TRUE,
        extensions = c("Scroller", "Buttons", "FixedColumns"),
        options = list(
          initComplete = .initComplete(),
          dom = "Bfrtip",
          pageLength = 10,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          orderClasses = TRUE,
          autoWidth = FALSE,
          columns.searchable = FALSE,
          fixedColumns = list(
            leftColumns = 1
          ),
          columnDefs = list(
            list(
              columns.width = c("60px"),
              targets = c(list(0), list(1), list(2))
            )
          )
        )
      )

      if ("Significant" %in% colnames(.row)) {
        dat <- dat %>%
          DT::formatStyle(
            columns = "Significant",
            target = "row",
            background = DT::styleEqual(1, "lightblue")
          )
      }

      return(dat)
    })


    output$qdata_ui <- DT::renderDataTable(server = TRUE, {
      req(rv$data)
      .keyId <- df <- NULL
      .row <- rowData(rv$data[[i()]])
      .colId <- get_colID(rv$data[[i()]])
      .metacell <- get_metacell(rv$data[[i()]])

      if (.colId != '' &&  ncol(.row) > 0 && nrow(.row) > 0) 
        .keyId <- (.row)[, .colId] 
      else 
        .keyId <- rownames(assay(rv$data[[i()]]))
      
      .qdata <- round(SummarizedExperiment::assay(rv$data[[i()]]), 
          digits = digits())
      
      .qdata.exists <- (!is.null(.qdata) && 
          ncol(.qdata) > 0) && 
        (nrow(.qdata) > 0)
      
      .metacell.exists <- (!is.null(.metacell) && 
          ncol(.metacell) > 0) && 
        (nrow(.metacell) > 0)
      

       #if (.qdata.exists){
         if(.metacell.exists)
           df <- cbind(keyId = .keyId, .qdata, .metacell)
         else
           df <- cbind(keyId = .keyId, .qdata)
       #}

      colors <- custom_metacell_colors()

      dt <- DT::datatable(as.data.frame(df),
        extensions = c("Scroller"),
        options = list(
          initComplete = .initComplete(),
          displayLength = 20,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          ordering = FALSE,
          server = TRUE,
          columnDefs = if (.metacell.exists){
            list(
              list(
              targets = c(((2 + (ncol(df) - 1) / 2)):ncol(df)),
              visible = FALSE
            )) } else NULL
          )
        )
      
      if (.metacell.exists){
        dt <- dt %>%
        DT::formatStyle(
          colnames(df)[2:(1 + (ncol(df) - 1) / 2)],
          colnames(df)[((2 + (ncol(df) - 1) / 2)):ncol(df)],
          backgroundColor = DT::styleEqual(
            names(colors),
            unname(unlist(colors))
          ),
          backgroundSize = "98% 48%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
      }
      
      dt
    })

    output$qMetacell_ui <- DT::renderDataTable(server = TRUE, {
      req(rv$data)
      df <- get_metacell(rv$data[[i()]])
      colors <- custom_metacell_colors()

      DT::datatable(as.data.frame(df),
        extensions = c("Scroller"),
        options = list(
          initComplete = .initComplete(),
          displayLength = 20,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          ordering = FALSE,
          server = TRUE
        )
      ) %>%
        DT::formatStyle(
          colnames(df),
          colnames(df),
          backgroundColor = DT::styleEqual(
            names(colors),
            unname(unlist(colors))
          ),
          backgroundSize = "98% 48%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
  })
}


#' @rdname omXplore_tabExplorer
#' @export
#' @return A shiny app
#'
omXplore_tabExplorer <- function(obj, i) {
  ui <- fluidPage(omXplore_tabExplorer_ui("plot"))

  server <- function(input, output, session) {
    omXplore_tabExplorer_server("plot", 
      obj = reactive({obj}),
      i = reactive({i}))
  }

  app <- shinyApp(ui = ui, server = server)
}
