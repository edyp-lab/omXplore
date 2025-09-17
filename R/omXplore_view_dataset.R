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
#' The function [] is inspired from the function 'heatmap.2'
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
#' @param dataIn An instance of the class `MultiAssayExperiment`.
#' @param addons A `list` to configure the other shiny apps to integrate.
#' Each item correspond to one package:
#' * the name of the slot is the name of the package
#' * the content of the slot is a vector composed of the generic name of the
#' shiny app. Each of the apps listed here must be an exported app of the
#' package.
#' For example, given the value addons = list(testPkg = c('foo', 'foo2')). That
#' means that the package called "testPkg" must provide the four functions:
#' foo1_ui(), foo1_server() and foo2_ui(), foo2_server())
#' @param verbose A boolean for verbose mode. Default is FALSE.
#'
#'
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#' library(omXplore)
#'     data(vdata)
#'     addons <- list(omXplore = c("extFoo1", "extFoo2"))
#'     runApp(omXplore::view_dataset(vdata, addons))
#'
#'     omXplore::view_dataset(vdata)
#' }
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
         
         fluidRow(
             
            column(3, uiOutput(ns("chooseDataset_ui"))),
            column(9,
                uiOutput(ns("ShowVignettesNoModal_ui"))
                )),
                uiOutput(ns("ShowPlotsNoModal_ui"))
            )
}


#'
#' @importFrom shiny shinyApp reactive NS tagList tabsetPanel tabPanel fluidRow
#' column uiOutput radioButtons reactive moduleServer reactiveValues observeEvent
#' renderUI req selectInput isolate uiOutput tagList fluidPage div p
#' numericInput observe plotOutput renderImage renderPlot selectizeInput
#' sliderInput textInput updateSelectInput updateSelectizeInput wellPanel
#' withProgress h3 br actionButton addResourcePath h4 helpText imageOutput
#' @importFrom shinyjs useShinyjs hidden toggle show hide
#' @importFrom shinyjqui jqui_resizable
#'
#' @rdname ds-view
#' @export
#' @return NA
#'
view_dataset_server <- function(
        id,
        dataIn = reactive({NULL}),
        addons = list(),
        verbose = FALSE) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        width <- 40
        height <- 40

        rv <- reactiveValues(
            data = NULL,
            conds = NULL,
            current.se = NULL,
            btns.history.old = NULL,
            btns.history.new = NULL,
            clicked = NULL,
            ll.mods = NULL
        )


        is.addon <- function(x) {(length(grep("addon_", x)) == 1)  }

        Name2show <- function(x) {
            # indice for builtin module
            ind <- 2
            # Check and update if the module is  an external one
            if (is.addon(x)) {
                ind <- 3
            }

            unlist(strsplit(x, split = "_"))[ind]
        }

        GetPackageName <- function(x) {
            # indice for builtin module
            ind <- 1
            # Check and update if the module is  an external one
            if (is.addon(x)) {
                ind <- 2
            }

            unlist(strsplit(x, split = "_"))[ind]
        }

        GetFuncName <- function(x) {
            # indice for builtin module
            ind <- 2
            # Check and update if the module is  an external one
            if (is.addon(x)) {
                ind <- 3
            }

            unlist(strsplit(x, split = "_"))[ind]
        }

        FindImgSrc <- function(x) {
            # By default, search image from the images directory of the omXplore
            # package. This works for built-in plot modules. For external modules,
            # then load customized resource path

            if (!is.addon(x)) {
                paste0("images/", GetFuncName(x), ".png")
            } else {
                paste0(GetPackageName(x), "_images/", GetFuncName(x), ".png")
            }
        }


        observeEvent(req(dataIn()),
            {
                # inherits_mae <- inherits(dataIn(), "MultiAssayExperiment")
                # if (!inherits_mae){
                tryCatch(
                    {
                        rv$data <- convert_to_mae(dataIn())
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
                #   rv$data <- dataIn()
                # }

                if (!is.null(rv$data)) {
                    conds <- get_group(rv$data[1])
                    # Load external modules
                    addModules(addons)

                    rv$ll.mods <- listPlotModules()
                    rv$btns.history.old <- rep(0, length(rv$ll.mods))
                }
            },
            priority = 1000
        )

        
        
        observe({
            req(input$chooseDataset)
            req(rv$ll.mods)
            req(rv$data)
            
            for (x in rv$ll.mods) {
                do.call(
                    paste0(x, "_server"),
                    list(
                        id = paste0(x, "_large"),
                        dataIn = reactive({rv$data}),
                        i = reactive({input$chooseDataset})
                    )
                )
            }
        })
        

        
        
        
        # 
        # output$test <- renderUI({
        #     lapply(1:3, function(val) {
        #         fluidRow(column(12,textOutput(session$ns(paste0("line_", val)))))
        #     })
        # })
        
        # observe({
        #     lapply(1:3, function(val) {
        #         output[[paste0("line_", val)]] <- renderText(paste("Line", val))
        #     })
        # })
        
        
        output$ShowVignettesNoModal_ui <- renderUI({
            req(rv$ll.mods)
            print(rv$ll.mods)
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

        })
        

        # GetCliked <- reactive({
        #     req(rv$ll.mods)
        #     unlist(lapply(rv$ll.mods, function(x) input[[x]]))
        # })
        
        observeEvent(unlist(lapply(rv$ll.mods, function(x) input[[x]])), {
            new <- unlist(lapply(rv$ll.mods, function(x) input[[x]]))
            rv$clicked <- which(new != rv$btns.history.new)
            rv$btns.history.new <- new
        })

        output$ShowPlotsNoModal_ui <- renderUI({
            req(rv$ll.mods)
            req(rv$clicked > 0)
            mod2show <- rv$ll.mods[rv$clicked]
            do.call(paste0(mod2show, "_ui"), list(ns(paste0(mod2show, "_large"))))

})


        output$chooseDataset_ui <- renderUI({
            req(rv$data)

            .choices <- if (length(rv$data) == 0) 
                list(" " = character(0))
                    else
                names(rv$data)

            radioButtons(ns("chooseDataset"), "Dataset",
                choices = .choices,
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
#'     data(vdata)
#'     view_dataset(vdata)
#' }
#'
view_dataset <- function(
        dataIn = NULL,
        addons = NULL) {
    # if (!inherits(dataIn, "MultiAssayExperiment"))
    #   dataIn <- convert_to_mae(dataIn)
    #
    ui <- fluidPage(
        omXplore::view_dataset_ui("dataset")
    )

    server <- function(input, output, session) {
        omXplore::view_dataset_server("dataset",
            dataIn = reactive({dataIn}),
            addons = addons
        )
    }

    app <- shiny::runApp(shinyApp(ui, server))
}
