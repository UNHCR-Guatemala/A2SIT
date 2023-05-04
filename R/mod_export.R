export_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "export",
    fluidRow(
      box(title = "Export to Excel", width = 6, status = "info",
          verbatimTextOutput(NS(id, "data_message")))
    )

  )

}

export_server <- function(id, coin, coin_full, input) {

  moduleServer(id, function(input, output, session) {


  })

}
