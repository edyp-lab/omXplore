#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `VizData`.
#' @param pal.name It is a reactive value.
#'
#' @name density-plot
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vData_ft)
#'   omXplore_density(vData_ft[[1]])
#' }
#'
NULL


#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @rdname density-plot
#' @export
#' @return NA
#'
omXplore_density_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidPage(
      shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
      highcharter::highchartOutput(ns("plot_ui"))
    )
  )
}


#' @rdname density-plot
#'
#' @importFrom highcharter renderHighchart
#'
#' @export
#' @return NA
#'
omXplore_density_server <- function(
    id,
    obj = reactive({
      NULL
    }),
    pal.name = reactive({
      NULL
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe(
      {
        if (inherits(obj(), "VizData")) {
          rv$data <- obj()
        }

        shinyjs::toggle("badFormatMsg",
          condition = !inherits(obj(), "VizData")
        )
      },
      priority = 1000
    )


    output$plot_ui <- renderHighchart({
      req(rv$data)
      tmp <- NULL
      isolate({
        withProgress(message = "Making plot", value = 100, {
          tmp <- densityPlot(
            data = rv$data@qdata,
            conds = rv$data@conds,
            pal.name = pal.name()
          )
        })
      })
      tmp
    })
  })
}





#' @param data A `data.frame` or `matrix` of `numeric()` which is the
#' quantitative data to plot.
#'
#' @param conds A `character()` of condition name for each sample. The
#' length of 'conds' must be equal to the number of columns of 'data'.
#'
#' @param pal.name A `character(1)` which is the name of the palette from
#' the package [RColorBrewer] from which the colors are taken. Default
#' value is 'Set1'.
#'
#' @import highcharter
#' @importFrom stats density
#'
#' @export
#'
#' @rdname density-plot
#'
#' @return A plot
#'
#' @examples
#' data(vData_ft)
#' qdata <- GetSlotQdata(vData_ft[[1]])
#' conds <- GetSlotConds(vData_ft[[1]])
#' densityPlot(qdata, conds)
#'
densityPlot <- function(
    data,
    conds,
    pal.name = NULL) {
  if (missing(data)) {
    stop("'data' is missing.")
  }

  if (missing(conds)) {
    stop("'conds' is missing.")
  }

  if (length(conds) != ncol(data)) {
    stop("data and conds must have the same number of samples.")
  }

  myColors <- SampleColors(conds, pal.name)


  h1 <- highcharter::highchart() %>%
    hc_title(text = "Density plot") %>%
    customChart(chartType = "spline", zoomType = "x") %>%
    hc_legend(enabled = TRUE) %>%
    hc_xAxis(title = list(text = "log(Intensity)")) %>%
    hc_yAxis(title = list(text = "Density")) %>%
    hc_tooltip(
      headerFormat = "",
      pointFormat = "<b> {series.name} </b>: {point.y} ",
      valueDecimals = 2
    ) %>%
    customExportMenu(fname = "densityplot") %>%
    hc_plotOptions(
      series = list(
        animation = list(
          duration = 100
        ),
        connectNulls = TRUE,
        marker = list(
          enabled = FALSE
        )
      )
    ) %>%
    hc_colors(myColors)



  for (i in seq_len(ncol(data))) {
    tmp <- data.frame(
      x = stats::density(data[, i], na.rm = TRUE)$x,
      y = stats::density(data[, i], na.rm = TRUE)$y
    )

    h1 <- h1 %>%
      hc_add_series(
        data = list_parse(tmp),
        name = colnames(data)[i]
      )
  }

  h1
}






#' @import shiny
#' @export
#' @rdname density-plot
#' @return A shiny app
#'
omXplore_density <- function(obj) {
  ui <- omXplore_density_ui("plot")

  server <- function(input, output, session) {
    omXplore_density_server("plot", obj = reactive({
      obj
    }))
  }

  shinyApp(ui, server)
}
