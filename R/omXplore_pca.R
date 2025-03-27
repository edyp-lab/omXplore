#' @title Bar plot of missing values per lines using highcharter.
#'
#' @description
#'
#' This method plots a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins).
#'
#' * `wrapper_pca()`
#' * `plotPCA_Eigen_hc()`: plots the eigen values of PCA with the highcharts
#'    library
#' * `plotPCA_Eigen()`: plots the eigen values of PCA
#' * `plotPCA_Var()`
#' * `plotPCA_Ind()`
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `MultiAssayExperiment`.
#' @param i An integer which is the index of the assay in the param obj
#' @param var.scaling The dimensions to plot
#' @param ncp A `integer(1)` which represents the umber of dimensions kept in
#' the results.
#' @param res.pca Result of FactoMineR::PCA
#' @param chosen.axes The dimensions to plot
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @name ds-pca
#' 
#' @importFrom SummarizedExperiment assay
#'
#' @examples
#' \dontrun{
#'   data(vdata)
#'   library(shinyWidgets)
#'   library(dplyr)
#'   # Replace missing values for the example
#'   sel <- is.na(SummarizedExperiment::assay(vdata, 1))
#'   SummarizedExperiment::assay(vdata[[1]])[sel] <- 0
#'   SummarizedExperiment::assay(vdata[[1]])[1,1] <- NA
#'   shiny::runApp(omXplore_pca(vdata, 1))
#' }
#' 
#' 
NULL



#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList checkboxInput fluidPage div
#'  p numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom RColorBrewer brewer.pal
#' @importFrom highcharter renderHighchart
#' @importFrom shinyjs useShinyjs hidden toggle
#' 
#' @rdname ds-pca
#' @export
#' @return NA
#'
omXplore_pca_ui <- function(id) {

  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(
        div(
            id = ns("badFormatMsg"), 
            h3(globals()$bad_format_txt))
        ),
    uiOutput(ns("WarningNA_PCA")),
    uiOutput(ns("pcaOptions")),
    uiOutput(ns("pcaPlots"))
  )
}


#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList checkboxInput fluidPage div
#'  p numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle
#' @importFrom RColorBrewer brewer.pal
#' @importFrom highcharter renderHighchart
#' @importFrom shinyjs useShinyjs hidden toggle
#'
#' @rdname ds-pca
#'
#' @import factoextra
#'
#' @export
#' @return NA
#'
omXplore_pca_server <- function(
        id,
    obj = reactive({NULL}),
    i = reactive({NULL})) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv.pca <- reactiveValues(
      data = NULL,
      PCA_axes = NULL,
      res.pca = NULL,
      PCA_varScale = TRUE,
      gramschmidt_PCA = TRUE,
      method_PCA = "FM",
        obj = NULL,
        i = NULL
    )
    
    
    observe({
        is.mae <- inherits(obj(), "MultiAssayExperiment")
        stopifnot(is.mae)
        rv.pca$data <- SummarizedExperiment::assay(obj(), i())
        rv.pca$obj <- obj()
        rv.pca$i <- i()
        shinyjs::toggle("badFormatMsg", condition = !is.mae)
      }, priority = 1000)
    
    
    output$WarningNA_PCA <- renderUI({

      req(rv.pca$data)
      req(length(which(is.na(rv.pca$data))) > 0)
      req(rv.pca$method_PCA == "FM")
      
        tagList(
          tags$p(
            style = "color:red;font-size: 20px",
            "Warning: As your dataset contains missing values,
            the PCA cannot be computed. Please impute them first or use Nipals."
            )
        )
    })
    
    
    output$pcaOptions <- renderUI({
      req(rv.pca$data)
      req(length(which(is.na(rv.pca$data))) == 0)
      
      tagList(
        tags$div(
          tags$div(
            style = "display:inline-block;
                             vertical-align: middle; padding-right: 20px;",
            numericInput(ns("pca_axe1"), "Dimension 1",
                         min = 1,
                         max = Compute_PCA_dim(),
                         value = 1,
                         width = "100px"
            )
          ),
          tags$div(
            style = "display:inline-block; vertical-align: middle;",
            numericInput(ns("pca_axe2"), "Dimension 2",
                         min = 1,
                         max = Compute_PCA_dim(),
                         value = 2,
                         width = "100px"
            )
          ),
          tags$div(
            selectInput(ns("method_PCA"), label = "Method used for PCA",
                        choices = c("FactoMineR" = "FM", "Nipals" = "NIPALS"),
                        width = "150px"
            )
          ),
            tags$div(
                shinyjs::hidden(checkboxInput(ns("gramschmidt_PCA"), "gramschmidt in Nipals", value = rv.pca$gramschmidt_PCA)),
                shinyjs::hidden(checkboxInput(ns("PCA_varScale"), "VarScale in FactoMineR", value = rv.pca$PCA_varScale)),
                
            )
        ))
        })
        
        
        
        
        observeEvent(c(input$pca_axe1, input$pca_axe2), {
            rv.pca$PCA_axes <- c(input$pca_axe1, input$pca_axe2)
        })
        
        observeEvent(input$PCA_varScale,{
            rv.pca$PCA_varScale <- input$PCA_varScale})
        observeEvent(input$gramschmidt_PCA, {rv.pca$gramschmidt_PCA <- input$gramschmidt_PCA})
        observeEvent(c(input$pca_axe1, input$pca_axe2), {
            rv.pca$PCA_axes <- c(input$pca_axe1, input$pca_axe2)
        })
        
        observeEvent(input$method_PCA, {
            rv.pca$method_PCA <- input$method_PCA
            
            shinyjs::toggle('PCA_varScale', condition = rv.pca$method_PCA == 'FM')
            shinyjs::toggle('gramschmidt_PCA', condition = rv.pca$method_PCA == 'NIPALS')
        })
        
        

        Compute_PCA_dim <- reactive({
            
            print('IN Compute_PCA_dim()')
            req(rv.pca$data)
            nmax <- 12 # ncp should not be greater than...
            # for info, ncp = number of components or dimensions in PCA results
            
            y <- rv.pca$data
            nprot <- dim(y)[1]
            n <- dim(y)[2] # If too big, take the number of conditions.
            
            if (n > nmax) {
                n <- length(unique(get_group(rv.pca$obj)))
            }
            
            ncp <- min(n, nmax)
            ncp
        })
   # })

    
    

    
    observeEvent(c(rv.pca$method_PCA, rv.pca$gramschmidt_PCA, rv.pca$PCA_varScale), 
        ignoreInit = FALSE,{
      req(rv.pca$method_PCA)
      
      print(paste("gramschmidt omXplore pca :", rv.pca$gramschmidt_PCA))
      print(paste("method omXplore pca :", rv.pca$method_PCA))
      print(paste("var scale :", rv.pca$PCA_varScale))
      
      withProgress(message = "Performing PCA", value = 100, {
          
        # rv.pca$res.pca <- wrapper_pca_with_nipals(
        #   qdata = rv.pca$data,
        #   group = get_group(rv.pca$obj),
        #   var.scaling = rv.pca$PCA_varScale,
        #   ncp = Compute_PCA_dim(),
        #   method = rv.pca$method_PCA,
        #   gramschmidt = rv.pca$gramschmidt_PCA
        # )
        
        
        rv.pca$res.pca <- wrapper_pca(
            qdata = rv.pca$data,
            group = get_group(rv.pca$obj),
            var.scaling = rv.pca$PCA_varScale,
            ncp = Compute_PCA_dim()
        )
        

         # })
      })
    })
    
    # 
    # observeEvent(rv.pca$res.pca, {
    #     print('change in rv.pca$res.pca')
    # })
    # 
    # observe({
    #     
    #     print(head(rv.pca$res.pca$var$coord))
    #     
    #     
    # })
    # 
    
    output$pcaPlots <- renderUI({
      req(rv.pca$data)
      #req(length(which(is.na(rv.pca$data))) == 0)
      req(rv.pca$res.pca$var$coord)
      
      #df <- as.data.frame(rv.pca$res.pca$var$coord)
      formatDT_server("PCAvarCoord",
          data = reactive({round(as.data.frame(rv.pca$res.pca$var$coord), digits = 2)}),
          showRownames = TRUE
      )
      .width <- '500px'
      tagList(
        plotOutput(ns("pcaPlotVar"), width = .width)
        ,plotOutput(ns("pcaPlotInd"), width = .width)
        ,formatDT_ui(ns("PCAvarCoord"))
        ,highcharter::highchartOutput(ns("pcaPlotEigen"), width = .width)
      )
      
      
    })
    

    
    
    output$pcaPlotVar <- renderPlot({
      req(c(rv.pca$PCA_axes, rv.pca$res.pca))
      withProgress(message = "Making variables plot", value = 100, {
        factoextra::fviz_pca_var(rv.pca$res.pca,
            axes = rv.pca$PCA_axes,
            col.var = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE
        )
      })
    })

    output$pcaPlotInd <- renderPlot({
      req(c(rv.pca$PCA_axes, rv.pca$res.pca))
      withProgress(message = "Making individuals plot", value = 100, {
        factoextra::fviz_pca_ind(rv.pca$res.pca,
                                 axes = rv.pca$PCA_axes,
                                 geom = "point"
        )
      })
    })
    
    output$pcaPlotEigen <- highcharter::renderHighchart({
      req(rv.pca$res.pca)
    
      withProgress(message = "Making eigen plot", value = 100, {
        plotPCA_Eigen_hc(rv.pca$res.pca)
      })
    })
    

  })
}


#' @export
#' @rdname ds-pca
#' @return A shiny app
#'
omXplore_pca <- function(obj, i) {

  stopifnot(inherits(obj, "MultiAssayExperiment"))
  
  ui <- omXplore_pca_ui("plot")
  
  server <- function(input, output, session) {
    omXplore_pca_server("plot", 
                        obj = reactive({obj}),
                        i = reactive({i}))
  }
  app <- shinyApp(ui = ui, server = server)

}