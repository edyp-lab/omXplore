#' @title Displays different intensity plots.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj A instance of the class `MultiAssayExperiment`
#' @param i An integer which is the index of the assay in the param obj
#' @param track.indices A vector of integers which are the indices of
#' lines to track.
#' @param data A data.frame() of quantitaive data
#' @param withTracking A `boolean(1)` indicating whether the tracking option is
#' activated or not.
#' @param pal.name A `character(1)` which is the name of the palette from the
#' package [RColorBrewer] from which the colors are taken. Default value
#' is 'Set1'.
#' @param subset A `integer()` vector of index indicating the indices
#' of rows in the dataset to highlight
#' 
#' @param conds A vector indicating the name of each sample.
#' @param legend A vector of the conditions (one condition per sample).
#' @param pal A basis palette for the boxes which length must be equal
#' to the number of unique conditions in the dataset.
#'
#' @name intensity-plots
#'
#' @examplesIf interactive()
#'   data(vdata)
#'   shiny::runApp(omXplore_intensity(vdata, 1))
#'   
#' data(sub_R25)
#' conds <- legend <- SummarizedExperiment::colData(sub_R25)$group
#' pal <- ExtendPalette(length(unique(conds)))
#' boxPlot(sub_R25[[1]], conds, legend, pal, seq_len(10))
#' 
#' shiny::runApp(omXplore_intensity(sub_R25, 1, withTracking = TRUE))
#' 
#' 
NULL




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
#' @export
#' @rdname intensity-plots
#' @return NA
#'
omXplore_intensity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    #shinyjs::hidden(div(id = ns("badFormatMsg"), 
      # h3(globals()$bad_format_txt))),
    radioButtons(ns("choosePlot"), "",
      choices = setNames(nm = c("violin", "box"))
    ),
    highchartOutput(ns("box")),
    shinyjs::hidden(imageOutput(ns("violin")))
  )
}




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
#' @rdname intensity-plots
#'
#' @export
#'
#' @return NA
#'
omXplore_intensity_server <- function(
    id,
    obj = reactive({NULL}),
    i = reactive({1}),
    track.indices = reactive({NULL}),
    remoteReset = reactive({NULL}),
    is.enabled = reactive({TRUE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL,
      conds = NULL
      )
    
    observeEvent(req(remoteReset()), {
      updateSelectInput(session, "choosePlot", selected = "violin")
    })

    observeEvent(obj(),{
      #browser()
        stopifnot(inherits(obj(), "MultiAssayExperiment"))
      
          rv$data <- obj()[[i()]]
          rv$conds <- get_group(obj())
          
        #shinyjs::toggle("badFormatMsg", condition = is.null(rv$data))
        shinyjs::toggle("choosePlot", condition = !is.null(rv$data))
      })


    observeEvent(input$choosePlot, {
      shinyjs::toggle("violin", condition = input$choosePlot == "violin")
      shinyjs::toggle("box", condition = input$choosePlot == "box")
    })

    output$box <- renderHighchart({
      req(rv$data)
      req(input$choosePlot == "box")
      # withProgress(message = "Making plot", value = 100, {

      boxPlot(
        obj = rv$data,
        conds = rv$conds,
        subset = track.indices()
      )

      # })
    })

    output$violin <- renderImage({
        req(rv$data)
      req(rv$conds)
      req(input$choosePlot == "violin")

        # A temp file to save the output. It will be deleted after
        # renderImage sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext = ".png")
        # Generate a png
        withProgress(message = "Making plot", value = 100, {
          png(outfile)
          pattern <- paste0("test", ".violinplot")
          tmp <- violinPlot(
            data = as.matrix(SummarizedExperiment::assay(rv$data)),
            conds = rv$conds,
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
    i = NULL,
    withTracking = FALSE) {
  
  ui <- fluidPage(
    tagList(
      actionButton('reset', "Reset"),
      plots_tracking_ui("tracker"),
      omXplore_intensity_ui("iplot")
    )
  )

  server <- function(input, output, session) {
    
     rv <- reactiveValues(
       indices = reactive({NULL})
     )
     
    indices <- plots_tracking_server("tracker",
      obj = reactive({obj[[i]]}),
      remoteReset = reactive({input$reset})
    )

    
    omXplore_intensity_server("iplot",
      obj = reactive({obj}),
      i = reactive({i}),
      track.indices = reactive({indices()$indices}),
      remoteReset = reactive({input$reset}),
      is.enabled = reactive({TRUE})
    )
  }


  app <- shinyApp(ui = ui, server = server)
}
