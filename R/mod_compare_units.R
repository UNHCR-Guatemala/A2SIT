compare_units_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "compare_units",

    shinydashboardPlus::box(
      title = "Compare",
      width = 12,
      col_4(selectInput(NS(id, "selected_unit1"), label = NULL,
                        choices = NULL, width = "100%")),
      col_1(p("with", style = "text-align: center;")),
      col_4(selectInput(NS(id, "selected_unit2"), label = NULL,
                        choices = NULL, width = "100%")),
      col_1(p("using", style = "text-align: center;")),
      col_2(selectInput(NS(id, "compare_using"), label = NULL,
                        choices = c("Ranks", "Scores", "Severity"), width = "100%"))
    ),

    shinydashboardPlus::box(
      title = box_pop_title(
        title = "Comparison",
        popover_text = "For each row, the region with the most severe value is highlighted.",
        placement = "top"
      ),
      width = 12,
      DT::DTOutput(NS(id, "df_indicators"))
    ),

  )

}

compare_units_server <- function(id, coin, input, r_shared) {

  moduleServer(id, function(input, output, session) {

    # populate dropdown for selecting unit
    observe({
      req(r_shared$results_built)
      unit_list <- get_unit_list(coin())
      updateSelectInput(inputId = "selected_unit1", choices = unit_list, selected = unit_list[[1]])
      updateSelectInput(inputId = "selected_unit2", choices = unit_list, selected = unit_list[[2]])
    })


    # summary table of scores/ranks for each indicator
    output$df_indicators <- DT::renderDataTable({
      req(r_shared$results_built)
      f_compare_units_table(
        coin(),
        input$selected_unit1,
        input$selected_unit2,
        using = input$compare_using)
    })

  })

}
