scenarios_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "scenarios",
    "test"

  )

}

scenarios_server <- function(id, coin, input, shared_reactives) {

  moduleServer(id, function(input, output, session) {

    #

  })

}
