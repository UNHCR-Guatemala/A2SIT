compare_units_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "compare_units",
    "test"

  )

}

compare_units_server <- function(id, coin, input, shared_reactives) {

  moduleServer(id, function(input, output, session) {

    # populate dropdown for selecting unit
    observe({
      req(shared_reactives$results_built)
      updateSelectInput(inputId = "selected_unit", choices = get_unit_list(coin()))
    })

    # update selected unit (link from results tab)
    observeEvent(shared_reactives$profile_unit, {
      req(shared_reactives$results_built)
      req(shared_reactives$profile_unit)
      updateSelectInput(inputId = "selected_unit", selected = shared_reactives$profile_unit)
    })

    # title
    output$unit_name <- renderText({
      req(input$selected_unit)
      req(shared_reactives$results_built)
      COINr::ucodes_to_unames(coin(), input$selected_unit)
    })



    # summary table of scores/ranks for each indicator
    output$df_indicators <- DT::renderDataTable({
      req(shared_reactives$results_built)
      f_indicator_table(coin(), input$selected_unit)
    })

  })

}
