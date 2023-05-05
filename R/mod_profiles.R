profiles_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "profiles",
    column(6,
      box(title = "Select a region", width = 12, status = "info",
          selectInput(NS(id, "selected_unit"), label = NULL,
                      choices = NULL, width = "100%")
      ),
      shinydashboard::valueBox(12, "Rank"),
      shinydashboard::valueBox(50.9, "Score"),
      box(title = "Summary", width = 12, status = "info",
          "Text")
    ),
    column(6,
           box(title = "Top-ranked indicators", width = 12, status = "info",
               "Text"),
           box(title = "Bottom-ranked indicators", width = 12, status = "info",
               "Text"))

  )

}

profiles_server <- function(id, coin, coin_full, input) {

  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectInput(inputId = "selected_unit", choices = get_unit_list(coin()))
    })

  })

}
