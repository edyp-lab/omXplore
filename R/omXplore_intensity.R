#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj A instance of the class `MultiAssayExperiment`
#' @param i xxx
#' @param track.indices xxx
#' @param withTracking xxx
#' @param pal.name A `character(1)` which is the name of the palette from the
#' package [RColorBrewer] from which the colors are taken. Default value
#' is 'Set1'.
#' @param subset A `integer()` vector of index indicating the indices
#' of rows in the dataset to highlight
#'
#' @name intensity-plots
#' 
#' @importFrom grDevices png dev.off
#' @importFrom shinyjs useShinyjs hidden toggle
#' @import highcharter
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom stats setNames
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   omXplore_intensity(vdata, 1)
#' }
#'
NULL



#' @export
#' @rdname intensity-plots
#' @return NA
#'
omXplore_intensity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), 
      h3(globals()$bad_format_txt))),
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
#'
#' @return NA
#'
omXplore_intensity_server <- function(
    id,
    obj,
  i,
    track.indices = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data = NULL)

    observe(
      {
        if (inherits(obj(), "MultiAssayExperiment")) {
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
        data = assay(rv$data, i()),
        conds = get_group(rv$data),
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
            data = as.matrix(assay(rv$data, i())),
            conds = get_group(rv$data),
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



#' @rdname intensity-plots
#' @export
#' @return A shiny app
#'
omXplore_intensity <- function(
    obj,
  i,
    withTracking = FALSE) {
  
  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- fluidPage(
    tagList(
      plots_tracking_ui("tracker"),
      omXplore_intensity_ui("iplot")
    )
  )

  server <- function(input, output, session) {
    indices <- plots_tracking_server("tracker",
      obj = reactive({obj}),
      i = reactive({i})
    )

    omXplore_intensity_server("iplot",
      obj = reactive({obj}),
      i = reactive({i}),
      track.indices = reactive({indices()})
    )
  }


  shinyApp(ui = ui, server = server)
}
