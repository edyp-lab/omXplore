



library(shiny)
library(shinyjs)
library(bs4Dash)
library(omXplore)
library(visNetwork)
library(SummarizedExperiment)
#library(highcharter)
library(tibble)
library(shinyBS)


ui <- function(id, session) {
    bs4Dash::dashboardPage(
        
        title = "Basic Dashboard",
        header = bs4Dash::dashboardHeader(),
        sidebar = bs4Dash::dashboardSidebar(),
        body = bs4Dash::bs4DashBody  (
            tags$head(tags$style(paste0(".modal-dialog {width:large; }"))),
            tags$head(tags$style(".modal-dialog {z-index: 9999999;}")),
            tags$head(tags$style(".modal-dialog {width: fit-content !important;}")),
            
            actionButton("btn_test", label = "Click Me")
        )
    )
}

server <- function(session, input, output) {
    
    data(Exp1_R25_prot, package = 'DaparToolshedData')
    obj <- Exp1_R25_prot
    
    
    observeEvent(input$btn_test, {
        showModal(
            modalDialog(
            omXplore::view_dataset_ui("eda1"),
            title = "Test Modal", 
            size = "l"
            
        ))
    })
    
        omXplore::view_dataset_server("eda1", dataIn = reactive({obj}))

    
}


shinyApp(ui, server)