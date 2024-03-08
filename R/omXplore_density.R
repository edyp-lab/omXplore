#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id xxx
#' @param obj An instance of the class `SummarizedExperiment`
#' @param i xxx
#' @param data xxx
#' @param conds xxx
#' @param pal.name A `character(1)` which is the name of the palette from
#' the package [RColorBrewer] from which the colors are taken. Default
#' value is 'Set1'.
#'
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' @importFrom highcharter highchartOutput renderHighchart
#' @importFrom stats density
#' 
#' @name density-plot
#'
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_density(vdata, 1)
#' }
#'
NULL


#' @rdname density-plot
#' @export
#' @return NA
#'
omXplore_density_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidPage(
      shinyjs::hidden(div(id = ns("badFormatMsg"), 
        h3(globals()$bad_format_txt))),
      highcharter::highchartOutput(ns("plot_ui"))
    )
  )
}


#' @rdname density-plot
#'
#'
#' @export
#' @return NA
#'
omXplore_density_server <- function(
    id,
    obj = reactive({NULL}),
    i = reactive({NULL}),
    pal.name = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # rv <- reactiveValues(
    #   data = NULL
    # )

    observe(
      {
        # if (inherits(obj(), "SummarizedExperiment")) {
        #   rv$data <- obj()
        # }

        shinyjs::toggle("badFormatMsg",
          condition = !inherits(obj(), "MultiAssayExperiment")
        )
      },
      priority = 1000
    )


    output$plot_ui <- renderHighchart({
      req(obj())
      tmp <- NULL
      isolate({
        withProgress(message = "Making plot", value = 100, {
          tmp <- densityPlot(
            data = assay(obj()[[i()]]),
            conds = get_group(obj()),
            pal.name = pal.name()
          )
        })
      })
      tmp
    })
  })
}







#'
#' @export
#'
#' @rdname density-plot
#'
#' @return A plot
#'
#' @examples
#' data(vdata)
#' qdata <- SummarizedExperiment::assay(vdata[[1]])
#' conds <- get_group(vdata)
#' densityPlot(qdata, conds)
#'
densityPlot <- function(
    data,
    conds = NULL,
    pal.name = NULL) {
  
  if (missing(data)) {
    stop("'data' is missing.")
  }

  # if (missing(conds)) {
  #   stop("'conds' is missing.")
  # }

  # if (length(conds) != ncol(data)) {
  #   stop("data and conds must have the same number of samples.")
  # }

  myColors <- NULL
  if (length(conds) > 0)
    myColors <- SampleColors(conds, pal.name)
  else
    myColors <- SampleColors(seq(ncol(data)), pal.name)


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






#' @export
#' @rdname density-plot
#' @return A shiny app
#'
omXplore_density <- function(obj, i) {
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- omXplore_density_ui("plot")

  server <- function(input, output, session) {
    omXplore_density_server("plot", 
      obj = reactive({obj}),
      i = reactive({i})
    )
    
  }

  shinyApp(ui, server)
}
