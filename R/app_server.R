#' @import shiny
app_server <- function(input, output, session) {


  # Shared variables --------------------------------------------------------

  # initialise reactives to be shared between modules. These can be modified
  # within any module and don't need to be passed back out.

  coin <- reactiveVal(NULL)
  coin_full <- reactiveVal(NULL)
  shared_reactives <- reactiveValues(
    ISO3 = NULL, # country of the data
    profile_unit = NULL, # unit to view in profiles
    results_built = FALSE, # whether results (up to aggregation) built
    scenarios = NULL # list of saved scenarios for comparison
  )

  # Modules -----------------------------------------------------------------

  welcome_server("id_welcome", session)
  input_server("id_input", coin, coin_full, shared_reactives)
  analysis_server("id_analysis", coin, coin_full, input)
  results_server("id_results", coin, coin_full, input, session, shared_reactives)
  profiles_server("id_profiles", coin, coin_full, input, shared_reactives)
  scenarios_server("id_scenarios", coin, input, shared_reactives)

  # Extras (not modules) ----------------------------------------------------

  # # close sidebar for welcome screen
  # observeEvent(input$tab_selected, {
  #
  #   browser()
  #   if(input$tab_selected != "welcome"){
  #     shinydashboardPlus::updateSidebar("tab_selected")
  #   }
  # })

  # Export to Excel
  output$export_button_excel <- downloadHandler(
    filename = function() {
      "index_export.xlsx"
    },
    content = function(file) {
      f_export_to_excel(coin(), file)
    }
  )

  # Export to R
  output$export_button_R <- downloadHandler(
    filename = function() {
      "index_export.Rdata"
    },
    content = function(file) {
      coin_export <- coin()
      save("coin_export", file = file)
    }
  )

  # help icon
  output$header_help <- renderUI({
    selected_modal <- paste0(input$tab_selected, "_modal")
    header_help_icon(selected_modal)
  })


}
