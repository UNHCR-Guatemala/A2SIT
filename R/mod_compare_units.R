compare_units_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "compare_units",

    shinydashboardPlus::box(
      title = "Compare",
      width = 4,
      selectInput(NS(id, "selected_unit1"), label = "Region 1",
                  choices = NULL, width = "100%"),
      selectInput(NS(id, "selected_unit2"), label = "Region 2",
                  choices = NULL, width = "100%")
    ),

    shinydashboardPlus::box(
      title = NULL,
      width = 8,
      DT::DTOutput(NS(id, "df_indicators"))
    ),

  )

}

compare_units_server <- function(id, coin, input, shared_reactives) {

  moduleServer(id, function(input, output, session) {

    # populate dropdown for selecting unit
    observe({
      req(shared_reactives$results_built)
      updateSelectInput(inputId = "selected_unit1", choices = get_unit_list(coin()))
      updateSelectInput(inputId = "selected_unit2", choices = get_unit_list(coin()))
    })


    # summary table of scores/ranks for each indicator
    output$df_indicators <- DT::renderDataTable({
      req(shared_reactives$results_built)
      f_compare_units_table(coin(), input$selected_unit1, input$selected_unit2)
    })

  })

}
