#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj A instance of the class `VizData`
#' @param track.indices xxx
#' @param withTracking xxx
#' @param data Numeric matrix
#' @param conds A `character()` of the name of conditions
#' (one condition per sample). The number of conditions must be equal to
#' the number of samples (number of columns) of the parameter 'data'.
#' @param pal.name A `character(1)` which is the name of the palette from the
#' package [RColorBrewer] from which the colors are taken. Default value
#' is 'Set1'.
#' @param subset A `integer()` vector of index indicating the indices
#' of rows in the dataset to highlight
#'
#' @name intensity-plots
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_intensity(vdata[[1]])
#' }
#'
#' data(vdata)
#' qdata <- GetSlotQdata(vdata[[1]])
#' conds <- GetSlotConds(vdata[[1]])
#' boxPlot(qdata, conds)
#'
NULL


#' @import shiny
#' @import shinyjs
#' @importFrom stats setNames
#' @export
#' @rdname intensity-plots
#' @return NA
#'
omXplore_intensity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
    hidden(radioButtons(ns("choosePlot"), "",
      choices = setNames(nm = c("violin", "box"))
    )),
    highchartOutput(ns("box")),
    shinyjs::hidden(imageOutput(ns("violin")))
  )
}



#' @rdname intensity-plots
#'
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom shinyjs toggle hidden
#' @import highcharter
#'
#' @return NA
#'
omXplore_intensity_server <- function(
    id,
    obj,
    track.indices = reactive({
      NULL
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data = NULL)

    observe(
      {
        if (inherits(obj(), "VizData")) {
          rv$data <- obj()
        }

        shinyjs::toggle("badFormatMsg", condition = is.null(rv$data))
        shinyjs::toggle("choosePlot", condition = !is.null(rv$data))
      },
      priority = 1000
    )


    observeEvent(input$choosePlot, {
      req(rv$data)
      shinyjs::toggle("violin", condition = input$choosePlot == "violin")
      shinyjs::toggle("box", condition = input$choosePlot == "box")
    })

    output$box <- renderHighchart({
      req(rv$data)
      # withProgress(message = "Making plot", value = 100, {
      boxPlot(
        data = rv$data@qdata,
        conds = rv$data@conds,
        subset = track.indices()
      )

      # })
    })

    output$violin <- renderImage(
      {
        req(rv$data)
        # A temp file to save the output. It will be deleted after
        # renderImage sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext = ".png")
        # Generate a png
        withProgress(message = "Making plot", value = 100, {
          png(outfile)
          pattern <- paste0("test", ".violinplot")
          tmp <- violinPlot(
            data = as.matrix(rv$data@qdata),
            conds = rv$data@conds,
            subset = track.indices()
          )
          # future(createPNGFromWidget(tmp,pattern))
          dev.off()
        })
        tmp
        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )
  })
}


#' @import shiny
#' @rdname intensity-plots
#' @export
#' @return A shiny app
#'
omXplore_intensity <- function(
    obj,
    withTracking = FALSE) {
  ui <- fluidPage(
    tagList(
      plots_tracking_ui("tracker"),
      omXplore_intensity_ui("iplot")
    )
  )

  server <- function(input, output, session) {
    indices <- plots_tracking_server("tracker",
      obj = reactive({obj})
    )

    omXplore_intensity_server("iplot",
      obj = reactive({obj}),
      track.indices = reactive({indices()})
    )
  }


  shinyApp(ui = ui, server = server)
}
