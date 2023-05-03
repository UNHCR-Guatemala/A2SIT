input_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "upload",
    column(
      4,
      box(title = "Data upload",
          width = NULL,
          collapsible = TRUE,
          status = "primary",
          "Browse to the location of your input data, which must be formatted according to the template.",
          br(),
          tags$a(href="https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/data_module-input.xlsx", "For testing purpose, you can download & use this file"),

          br(),br(),
          fileInput(NS(id, "xlsx_file"), "Load Data", buttonLabel = "Browse...", accept = c("xls", "xlsx")),
          actionButton(NS(id, "load_click"), "Load")),
      fluidRow(
        shinydashboard::infoBoxOutput(NS(id, "n_indicators_box"), width = 6),
        shinydashboard::infoBoxOutput(NS(id, "n_units_box"), width = 6)
      ),

      box(title = "Messages", width = NULL, status = "info",
          verbatimTextOutput(NS(id, "data_message")))

    ),
    column(
      8,
      box(title = "Index Framework", width = NULL, collapsible = TRUE, status = "success",
          plotly::plotlyOutput(NS(id, "framework"), height = "80vh"))
    )
  )

}

input_server <- function(id, coin, coin_full) {

  moduleServer(id, function(input, output, session) {

    observeEvent(input$load_click, {

      req(input$xlsx_file)

      data_message <- utils::capture.output({
        coin(f_data_input(input$xlsx_file$datapath))
      }, type = "message")

      # copy of full coin for plotting later
      coin_full(coin())

      # Outputs
      output$data_message <- renderText(data_message, sep = "\n")

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Data uploaded",
        text = "Please check the messages for more info.",
        type = "success"
      )

    })

    # coin print output
    output$coin_print <- renderPrint({
      req(coin())
      f_print_coin(coin())
    })

    # plot framework
    output$framework <- plotly::renderPlotly({
      req(coin())
      iCOINr::iplot_framework(coin())
    })

    # value boxes
    output$n_indicators_box <- shinydashboard::renderInfoBox({
      req(coin())
      shinydashboard::infoBox(
        title = "Indicators",
        value = get_n_indicators(coin()),
        icon = icon("list"),
        color = "blue"
      )
    })
    # value boxes
    output$n_units_box <- shinydashboard::renderInfoBox({
      req(coin())
      shinydashboard::infoBox(
        title = "Regions",
        value = get_n_units(coin()),
        icon = icon("location-dot"),
        color = "green"
      )
    })


  })

}
