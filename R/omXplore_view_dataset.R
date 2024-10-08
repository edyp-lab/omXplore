#' @title Bar plot of missing values per lines using `highcharter`.
#'
#' @description
#'
#' This method plots a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (i.e. proteins).
#'
#'
#' @details
#'
#' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (i.e. proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#'
#' - Variance : Builds a densityplot of the CV of entities in numeric matrix.
#' The CV is calculated for each condition present in the dataset
#' (see the slot \code{'Condition'} in the \code{colData()} DataFrame)
#'
#' - Heatmap:
#'
#'
#' The function [heatmapD()]
#'
#'
#' The function `[]` is inspired from the function 'heatmap.2'
#' that displays a numeric matrix. For more information, please refer to the
#' help of the heatmap.2 function.
#'
#'
#' @section Missing values:
#'
#' #' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#' @name ds-view
#'
#' @param id A `character(1)` for the 'id' of the shiny module. It must be
#' the same as for the '*_ui' function.
#' @param obj An instance of the class `MultiAssayExperiment`.
#' @param addons A `list` to configure the other shiny apps to integrate.
#' Each item correspond to one package:
#' * the name of the slot is the name of the package
#' * the content of the slot is a vector composed of the generic name of the
#' shiny app. Each of the apps listed here must be an exported app of the
#' package.
#' For example, given the value addons = list(testPkg = c('foo', 'foo2')). That
#' means that the package called "testPkg" must provide the four functions:
#' foo1_ui(), foo1_server() and foo2_ui(), foo2_server())
#' @param useModal A `boolean(1)` that indicates whether to open plot modules 
#' in a modal window or not. Default is TRUE.
#' @param verbose A boolean for verbose mode. Default is FALSE.
#' 
#' 
#' 
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' \dontrun{
#'   data(vdata)
#'   addons <- list(omXplore = c("extFoo1", "extFoo2"))
#'   runApp(view_dataset(vdata, addons))
#'   
#'   shiny::runApp(view_dataset(vdata))
#' }
#' 
#' 
#' @return NA
#'
NULL





#'
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @importFrom shinyjqui jqui_resizable
#' 
#' @rdname ds-view
#' @export
#' @return NA
#'
view_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidPage(
      h3('View dataset'),
      shinyjs::hidden(
        div(id = ns("badFormatMsg"), p("Dataset in not in correct format."))
      ),
      fluidRow(
        column(3, div(style = globals()$general_style,
          wellPanel(
            uiOutput(ns("chooseDataset_ui"))
          )
        )),
        column(9, 
          div(style = globals()$general_style,
          uiOutput(ns("Show_ui")),
          uiOutput(ns("ShowVignettesNoModal_ui")),
          uiOutput(ns("ShowPlotsNoModal_ui"))
          )
        )
      )
    )
  )
}


#'
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow 
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent 
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput 
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel 
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @importFrom shinyjqui jqui_resizable
#' 
#' @rdname ds-view
#' @export
#' @return NA
#'
view_dataset_server <- function(
    id,
    obj = reactive({NULL}),
    addons = list(),
    useModal = TRUE,
    verbose = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    shiny::addResourcePath(
      prefix = "images",
      directoryPath = system.file("images", package = "omXplore")
    )
    
    width <- 40
    height <- 40
    
    rv <- reactiveValues(
      data = NULL,
      conds = NULL,
      current.se = NULL,
      btns.history = NULL,
      ll.mods = NULL
    )
    
   
    is.addon <- function(x)
      (length(grep('addon_', x)) == 1)
    
    Name2show <- function(x) {
      # indice for builtin module
      ind <- 2
      # Check and update if the module is  an external one
      if(is.addon(x))
        ind <- 3
      
      unlist(strsplit(x, split='_'))[ind]
    }
    
    GetPackageName <- function(x){
      # indice for builtin module
      ind <- 1
      # Check and update if the module is  an external one
      if(is.addon(x))
        ind <- 2
      
      unlist(strsplit(x, split='_'))[ind]
    }
    
    GetFuncName <- function(x){
      # indice for builtin module
      ind <- 2
      # Check and update if the module is  an external one
      if(is.addon(x))
        ind <- 3
      
      unlist(strsplit(x, split='_'))[ind]
    }
    
    FindImgSrc <- function(x) {
      
      # By default, search image from the images directory of the omXplore
      # package. This works for built-in plot modules. For external modules,
      # then load customized resource path
      
      #img_path <- system.file("images", paste0(GetFuncName(x), ".png"), 
      #package = GetPackageName(x))
      if (!is.addon(x))
        paste0("images/", GetFuncName(x), ".png")
      else
        paste0(GetPackageName(x), "_images/", GetFuncName(x), ".png")
    }
    
    observeEvent(req(obj()), {

        #inherits_mae <- inherits(obj(), "MultiAssayExperiment")
        #if (!inherits_mae){
        tryCatch({
          rv$data <- convert_to_mae(obj())
        },
          warning = function(w) {
            print(w)
            rv$data <- NULL
            shinyjs::toggle("badFormatMsg", condition = TRUE)
            },
          error = function(e) {
            print(e)
            rv$data <- NULL
            shinyjs::toggle("badFormatMsg", condition = TRUE)
            }
          )
        # } else {
        #   rv$data <- obj()
        # }
        
        if(!is.null(rv$data)){
          
          conds <- get_group(rv$data[1])
          # Load external modules
          addModules(addons)
          
          rv$ll.mods <- listPlotModules()
        }
      },
      priority = 1000
    )

    
    observeEvent(GetVignettesBtns(), ignoreInit = TRUE, {
      req(rv$ll.mods)
      req(!isTRUE(useModal))

      # Which vignette has been clicked
      clicked <- which(rv$btns.history != GetVignettesBtns())

      # Show the corresponding plot
      shinyjs::show(paste0("div_", rv$ll.mods[clicked], "_large"))
      shinyjs::runjs(paste0('document.getElementById("',
        ns(rv$ll.mods[clicked]), '").style.backgroundColor = "lightgrey";'))


      # hide the other ones
      lapply(rv$ll.mods[-clicked], function(y) {
        shinyjs::hide(paste0("div_", y, "_large"))
        shinyjs::runjs(paste0('document.getElementById("',
          ns(y), '").style.backgroundColor = "white";'))
        })

      rv$btns.history <- GetVignettesBtns()
      })



    GetVignettesBtns <- reactive({
      req(rv$ll.mods)
      req(!isTRUE(useModal))

        unlist(lapply(rv$ll.mods, function(x) input[[x]]))
      })



    output$ShowPlotsNoModal_ui <- renderUI({
      req(rv$ll.mods)
      req(!isTRUE(useModal))

      lapply(rv$ll.mods, function(x) {
        shinyjs::hidden(
          div(id = ns(paste0("div_", x, "_large")),
            do.call(paste0(x, "_ui"), list(ns(paste0(x, "_large"))))
            )
          )
        })
      })
    
    # output$ShowVignettesNoModal_ui <- renderUI({
    #   #req(rv$data)
    #   print('In ShowVignettesNoModal_ui')
    #   
    #   req(rv$ll.mods)
    #   req(!isTRUE(useModal))
    #   
    #   
    #   #wellPanel(style = "height: 120px; overflow-y: scroll;",
    #   lapply(rv$ll.mods, function(x) {
    #     actionButton(ns(x),
    #       label = tagList(
    #         p(Name2show(x)),
    #         tags$img(src = FindImgSrc(x), height = "50px")
    #         ),
    #         style = "padding: 0px; border: none;
    #       background-size: cover; background-position: center;
    #       background-color: white;"
    #       )
    #     })
    #   #)
    #   })



    output$Show_ui <- renderUI({

       req(rv$ll.mods)

      if (useModal){
      #wellPanel(style = "height: 120px; overflow-y: scroll;",
        lapply(rv$ll.mods, function(x) {
        shinyjqui::jqui_resizable(
          paste0("#", ns(paste0("window_", x)), " .modal-content")
          )
        
        tagList(
          actionButton(
            ns(x),
            label = tagList(
              p(Name2show(x)),
              tags$img(src = FindImgSrc(x), height = "50px")
            ),
            style = "padding: 5px; border: none;
              background-size: cover; background-position: center;
            background-color: white;"
          ),
          shinyBS::bsModal(ns(paste0("window_", x)),
            title = x,
            trigger = ns(x),
            footer = NULL,
            do.call(
              paste0(x, "_ui"),
              list(id = ns(paste0(x, "_large")))
            )
            
            # Here, we could put the global function that calls shinyApp with
            # the module but it takes a longer time to display than if the
            # server is lrleady launched elsewhere
            #do.call(x, list(obj = rv$current.se))
          )
        )
      })
        
      } else {
        lapply(rv$ll.mods, function(x) {
          actionButton(ns(x),
            label = tagList(
              p(Name2show(x)),
              tags$img(src = FindImgSrc(x), height = "50px")
            ),
            style = "padding: 0px; border: none;
          background-size: cover; background-position: center;
          background-color: white;"
          )
        })
      }
     # )
    })


    
    observe({
      req(input$chooseDataset)
      req(rv$ll.mods)
      req(rv$data)

      for (x in rv$ll.mods) {
        do.call(
          paste0(x, "_server"),
          list(
            id = paste0(x, "_large"),
            obj = reactive({rv$data}),
            i = reactive({input$chooseDataset})
          )
        )
      }
    })



    output$chooseDataset_ui <- renderUI({
      req(rv$data)

      if (length(rv$data) == 0) {
        choices <- list(" " = character(0))
      } else {
        choices <- names(rv$data)
      }

      radioButtons(ns("chooseDataset"), "Dataset",
        choices = choices,
        selected = names(rv$data)[length(rv$data)],
        width = 200
      )
    })
  })
}


#' @export
#' @rdname ds-view
#'
#' @return A shiny application which wraps the functions view_dataset_ui()
#' and the view_dataset_server()
#' 
#'
#' @examples
#' if (interactive()) {
#'   data(vdata)
#'   view_dataset(vdata)
#' }
#'
view_dataset <- function(
    obj = NULL,
    addons = NULL,
  useModal = TRUE) {
  
  # if (!inherits(obj, "MultiAssayExperiment"))
  #   obj <- convert_to_mae(obj)
  # 
  ui <- fluidPage(
    view_dataset_ui("dataset")
  )

  server <- function(input, output, session) {
    view_dataset_server("dataset",
      obj = reactive({obj}),
      addons = addons,
      useModal = useModal
    )
  }

  app <- shinyApp(ui, server)
}
