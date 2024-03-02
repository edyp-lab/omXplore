#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name corrmatrix
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `VizData`
#' @param rate xxx. Default value is 0.9
#' @param showValues Default is FALSE.
#'
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_corrmatrix(vdata[[1]])
#' }
#'
NULL

#' @importFrom shiny NS tagList
#' @import shinyjs
#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
        uiOutput(ns("showValues_ui")),
        uiOutput(ns("rate_ui")),
        highcharter::highchartOutput(ns("plot"),
            width = "600px", height = "500px")
    )
}

#' @rdname corrmatrix
#' @export
#' @return NA
#'
omXplore_corrmatrix_server <- function(
    id,
    obj = reactive({ NULL}),
    rate = reactive({0.5}),
    showValues = reactive({FALSE})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        rv.corr <- reactiveValues(
            data = NULL,
            rate = NULL,
           showValues = NULL
        )

    observeEvent(id, {
        rv.corr$rate <- rate()
        rv.corr$showValues <- showValues()
    })


    observe({
        if (inherits(obj(), "VizData")) {rv.corr$data <- obj()}

        shinyjs::toggle("badFormatMsg",
            condition = !inherits(obj(), "VizData")
        )
      }, priority = 1000)

    output$rate_ui <- renderUI({
        req(rv.corr$rate)
        sliderInput(ns("rate"),
            "Tune to modify the color gradient",
            min = 0,
            max = 1,
            value = rv.corr$rate,
            step = 0.01
        )
    })


    output$showValues_ui <- renderUI({
      req(rv.corr$data)
      rv.corr$showValues
      checkboxInput(ns("showLabels"), "Show labels",
        value = rv.corr$showValues
      )
    })


    observeEvent(req(input$showLabels), {
      input$showLabels
      rv.corr$showValues <- input$showLabels
    })

    observeEvent(req(input$rate), {
      rv.corr$rate <- input$rate
    })

    output$plot <- renderHighchart({
      req(rv.corr$data)
      rv.corr$rate
      rv.corr$showValues

      withProgress(message = "Making plot", value = 100, {
        tmp <- corrMatrix(
          data = rv.corr$data@qdata,
          rate = rv.corr$rate,
          showValues = rv.corr$showValues
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
#' @param showValues xxx
#'
#' @import highcharter
#' @importFrom DT JS
#' @importFrom tibble tibble as_tibble
#' @importFrom stats cor
#'
#' @export
#'
#' @examples
#' data(vdata)
#' qdata <- GetSlotQdata(vdata[[1]])
#' corrMatrix(qdata)
#'
#' @return A plot
#'
#' @rdname corrmatrix
#'
#' @import tidyr
#' @import dplyr
#' @importFrom DT JS
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
omXplore_corrmatrix <- function(obj) {
  ui <- omXplore_corrmatrix_ui("plot")

  server <- function(input, output, session) {
    omXplore_corrmatrix_server("plot", reactive({
      obj
    }))
  }

  shinyApp(ui = ui, server = server)
}
