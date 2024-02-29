#' @title Explore `VizData` objects.
#'
#' @description xxx
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `VizData`
#' @param digits xxx
#'
#'
#' @name omXplore_tabExplorer
#'
#' @examples
#' if (interactive()) {
#'   data(vData_ft)
#'   omXplore_tabExplorer(vData_ft[[1]])
#' }
#' 
#' @return NA
#'
NULL



#' @import shiny
#' @import DT
#' @rdname omXplore_tabExplorer
#' @import shinyBS
#'
#' @export
#' @return NA
#'
omXplore_tabExplorer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
    shinyjs::hidden(div(id = ns("div_legend"), colorLegend_ui(ns("legend")))),
    shinyjs::hidden(
      div(
        id = ns("div_infos"),
        shinyBS::bsCollapse(
          id = "infos", open = "", multiple = TRUE,
          shinyBS::bsCollapsePanel("Quantitative data",
            DT::DTOutput(ns("qdata_ui")),
            style = "info"
          ),
          shinyBS::bsCollapsePanel("Metadata",
            DT::DTOutput(ns("metadata_ui")),
            style = "info"
          ),
          shinyBS::bsCollapsePanel("quantitative Metadata",
            DT::DTOutput(ns("qMetacell_ui")),
            style = "info"
          )
        )
      )
    )
  )
}


#' @return NA
#' @import DT
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#'
#' @rdname omXplore_tabExplorer
#'
#' @export
omXplore_tabExplorer_server <- function(
    id,
    obj = reactive({
      NULL
    }),
    digits = reactive({
      3
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data = NULL)

    observe(
      {
        if (inherits(obj(), "VizData")) {
          rv$data <- obj()

          tags <- GetMetacellTags(rv$data@metacell,
            level = rv$data@type,
            onlyPresent = TRUE
          )

          colorLegend_server("legend", tags)
        }

        shinyjs::toggle("badFormatMsg", condition = is.null(rv$data))
        shinyjs::toggle("div_infos", condition = !is.null(rv$data))
        shinyjs::toggle("div_legend", condition = !is.null(rv$data))
      },
      priority = 1000
    )


    #
    #     output$viewDesign <- DT::renderDT({
    #       req(rv$data)
    #
    #       data <- tibble::as_tibble(colData(se()))
    #
    #       pal <- unique(RColorBrewer::brewer.pal(8, "Dark2"))
    #
    #       dt <- DT::datatable(  data,
    #       extensions = c('Scroller', 'Buttons'),
    #       rownames=  FALSE,
    #       options=list(initComplete = .initComplete(),
    #       dom = 'Brtip',
    #       pageLength=10,
    #       orderClasses = TRUE,
    #       autoWidth=TRUE,
    #       deferRender = TRUE,
    #       bLengthChange = FALSE,
    #       scrollX = 200,
    #       scrollY = 500,
    #       scroller = TRUE,
    #       columnDefs = list(list(width='60px',targets= "_all"))
    #       )) %>%
    #         DT::formatStyle(
    #           columns = colnames(data)[seq_len(2)],
    #           valueColumns = colnames(data)[2],
    #           backgroundColor = DT::styleEqual(
    #           unique(data$Condition),
    #           pal[seq_len(length(unique(data$Condition)))])
    #         )
    #
    #     })


    output$metadata_ui <- DT::renderDT({
      req(rv$data)

      dat <- DT::datatable(tibble::as_tibble(rv$data@metadata),
        rownames = TRUE,
        extensions = c("Scroller", "Buttons", "FixedColumns"),
        options = list(
          initComplete = .initComplete(),
          dom = "Bfrtip",
          pageLength = 10,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          orderClasses = TRUE,
          autoWidth = FALSE,
          columns.searchable = FALSE,
          fixedColumns = list(
            leftColumns = 1
          ),
          columnDefs = list(
            list(
              columns.width = c("60px"),
              targets = c(list(0), list(1), list(2))
            )
          )
        )
      )

      if ("Significant" %in% colnames(rv$data@metadata)) {
        dat <- dat %>%
          DT::formatStyle(
            columns = "Significant",
            target = "row",
            background = DT::styleEqual(1, "lightblue")
          )
      }

      return(dat)
    })


    output$qdata_ui <- DT::renderDataTable(server = TRUE, {
      req(rv$data)

      df <- cbind(
        keyId = (rv$data@metadata)[, rv$data@colID],
        round(rv$data@qdata, digits = digits()),
        rv$data@metacell
      )

      colors <- custom_metacell_colors()

      DT::datatable(df,
        extensions = c("Scroller"),
        options = list(
          initComplete = .initComplete(),
          displayLength = 20,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          ordering = FALSE,
          server = TRUE,
          columnDefs = list(
            list(
              targets = c(((2 + (ncol(df) - 1) / 2)):ncol(df)),
              visible = FALSE
            )
          )
        )
      ) %>%
        DT::formatStyle(
          colnames(df)[2:(1 + (ncol(df) - 1) / 2)],
          colnames(df)[((2 + (ncol(df) - 1) / 2)):ncol(df)],
          backgroundColor = DT::styleEqual(
            names(colors),
            unname(unlist(colors))
          ),
          backgroundSize = "98% 48%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    output$qMetacell_ui <- DT::renderDataTable(server = TRUE, {
      req(rv$data)
      df <- rv$data@metacell
      colors <- custom_metacell_colors()

      DT::datatable(df,
        extensions = c("Scroller"),
        options = list(
          initComplete = .initComplete(),
          displayLength = 20,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 600,
          scroller = TRUE,
          ordering = FALSE,
          server = TRUE
        )
      ) %>%
        DT::formatStyle(
          colnames(df),
          colnames(df),
          backgroundColor = DT::styleEqual(
            names(colors),
            unname(unlist(colors))
          ),
          backgroundSize = "98% 48%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
  })
}


#' @rdname omXplore_tabExplorer
#' @export
#' @import shiny
#' @return A shiny app
#'
omXplore_tabExplorer <- function(obj) {
  ui <- fluidPage(omXplore_tabExplorer_ui("plot"))

  server <- function(input, output, session) {
    omXplore_tabExplorer_server("plot", reactive({
      obj
    }))
  }

  shinyApp(ui = ui, server = server)
}
