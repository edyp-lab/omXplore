library(shiny)
library(shinyjs)
library(shinyBS)
library(bs4Dash)
library(omXplore)
library(visNetwork)
library(SummarizedExperiment)
library(highcharter)
library(tibble)


    ui <- function(id, session) {
        bs4Dash::dashboardPage(
            
            title = "Basic Dashboard",
        header = bs4Dash::dashboardHeader(),
        sidebar = bs4Dash::dashboardSidebar(),
        body = bs4Dash::dashboardBody(
            useShinyjs(),
            omXplore::view_dataset_ui("eda1")
        )
    )
    }
    
    server <- function(input, output) {
        
        data(Exp1_R25_pept, package = 'DaparToolshedData')
        obj <- Exp1_R25_prot
       # observe({  
            omXplore::view_dataset_server("eda1",
            dataIn = reactive({Exp1_R25_prot}))
          #  })

    }


shinyApp(ui, server)