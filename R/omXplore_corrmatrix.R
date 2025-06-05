#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @name corrmatrix
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param dataIn An instance of the class `SummarizedExperiment`
#' @param i An integer which is the index of the assay in the param obj
#' @param rate Default value is 0.9
#' @param showValues Default is FALSE.
#'
#' 
#' 
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_corrmatrix(vdata, 1)
#' }
#'
NULL

#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @import highcharter
#' @importFrom DT JS
#' @importFrom tibble tibble as_tibble
#' @importFrom stats cor
#' @import tidyr
#' @importFrom dplyr mutate left_join select
#' @import bs4Dash
#' @import thematic
#' @import waiter
#' 
#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        
        bs4Card(
            title = "Correlation matrix", 
            closable = FALSE, 
            width = 6,
            status = "info", 
            icon = img(src='images/corrmatrix.png', width = 30),
            solidHeader = TRUE, 
            collapsible = TRUE,
            collapsed = TRUE,
            dropdownMenu = boxDropdown(
                boxDropdownItem("Link to google", href = "https://www.google.com"),
                boxDropdownItem("Item with inputId", id = "dropdown_item2"),
                dropdownDivider(),
                boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
            ),
            sidebar = boxSidebar(
                startOpen = FALSE,
                id = "mycardsidebar",
                background = "#7f7f7f",
                uiOutput(ns("showValues_ui")),
                uiOutput(ns("rate_ui"))
            ),
            shinyjs::hidden(div(id = ns("badFormatMsg"), 
                h3(globals()$bad_format_txt))),
            highcharter::highchartOutput(ns("plot"),
                width = "600px", height = "500px")
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
#' @import highcharter
#' @importFrom DT JS
#' @importFrom tibble tibble as_tibble
#' @importFrom stats cor
#' @import tidyr
#' @importFrom dplyr mutate left_join select
#' 
#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_server <- function(
    id,
    dataIn = reactive({ NULL}),
  i = reactive({NULL})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

    observe({
        shinyjs::toggle("badFormatMsg",
            condition = !inherits(dataIn(), "MultiAssayExperiment")
        )
      }, priority = 1000)

    output$rate_ui <- renderUI({
      req(inherits(dataIn(), "MultiAssayExperiment"))
        sliderInput(ns("rate"),
            "Tune to modify the color gradient",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
        )
    })


    output$showValues_ui <- renderUI({
      req(inherits(dataIn(), "MultiAssayExperiment"))
      checkboxInput(ns("showLabels"), "Show labels",
        value = FALSE
      )
    })

    output$plot <- renderHighchart({
      req(dataIn())

      withProgress(message = "Making plot", value = 100, {
        tmp <- corrMatrix(
          data = assay(dataIn()[[i()]]),
          rate = input$rate,
          showValues = isTRUE(input$showLabels)
        )
      })

      tmp
    })
  })
}






#' @param data An object of class 'matrix'
#'
#' @param rate The rate parameter to control the exponential law for
#' the gradient of colors
#'
#' @param showValues A boolean which indicates whether to show values in the
#' correlation plot.
#'
#' @export
#' 
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate left_join select
#' @importFrom tidyr gather
#' @importFrom stats cor
#' @importFrom highcharter list_parse2 highchart hc_xAxis hc_yAxis
#' hc_add_series hc_plotOptions hc_tooltip hc_legend hc_colorAxis
#'
#'
#' @return A plot
#'
#' @rdname corrmatrix
#'
#'
corrMatrix <- function(
    data,
    rate = 0.5,
    showValues = FALSE) {
  

  stopifnot(inherits(data, "matrix"))

  res <- cor(data, use = "pairwise.complete.obs")

  df <- tibble::as_tibble(res)
  colnames(df) <- colnames(data)

  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 2)
  dist <- NULL

  x <- y <- names(df)

  df <- tibble::as_tibble(cbind(x = y, df)) %>%
    tidyr::gather(y, dist, -x) %>%
    dplyr::mutate(
      x = as.character(x),
      y = as.character(y)
    ) %>%
    dplyr::left_join(
      tibble::tibble(
        x = y,
        xid = seq(length(y)) - 1
      ),
      by = "x"
    ) %>%
    dplyr::left_join(
      tibble::tibble(
        y = y,
        yid = seq(length(y)) - 1
      ),
      by = "y"
    )

  ds <- df %>%
    dplyr::select("xid", "yid", "dist") %>%
    highcharter::list_parse2()

  fntltp <- DT::JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                  this.series.yAxis.categories[this.point.y] + ': <b>' +
                  Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
  cor_colr <- list(
    list(0, "#FF5733"),
    list(0.5, "#F8F5F5"),
    list(1, "#2E86C1")
  )


  highcharter::highchart() %>%
    customChart(chartType = "heatmap") %>%
    hc_xAxis(categories = y, title = NULL) %>%
    hc_yAxis(categories = y, title = NULL) %>%
    hc_add_series(data = ds) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataConditions = list(enabled = TRUE),
        dataLabels = list(enabled = showValues)
      )
    ) %>%
    hc_tooltip(formatter = fntltp) %>%
    hc_legend(
      align = "right", layout = "vertical",
      verticalAlign = "middle"
    ) %>%
    hc_colorAxis(stops = cor_colr, min = rate, max = 1) %>%
    customExportMenu(fname = "corrMatrix")
}





#' @export
#' @rdname corrmatrix
#' @return A shiny app
#'
omXplore_corrmatrix <- function(dataIn, i) {
  
  stopifnot(inherits(dataIn, "MultiAssayExperiment"))
  
  ui = dashboardPage(
      preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
      dark = NULL,
      help = NULL,
      header = dashboardHeader(disable = TRUE),
      sidebar = dashboardSidebar(disable = TRUE),
      body = dashboardBody(
          
          tags$script(HTML(
              'document.getElementsByClassName("main-header navbar navbar-expand navbar-light navbar-white")[0].style.visibility = "hidden";'
          )),
          omXplore_corrmatrix_ui("plot")
      )
  )
  
  
  server = function(input, output, session) {
      useAutoColor()
      omXplore_corrmatrix_server("plot", 
          dataIn = reactive({dataIn}),
          i = reactive({i}))
  }
  
  shiny::shinyApp(ui, server)
}
