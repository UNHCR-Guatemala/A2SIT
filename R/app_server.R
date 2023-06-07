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
  compare_units_server("id_compare_units", coin, input, shared_reactives)

  # Exports -----------------------------------------------------------------

  # Export to Excel
  output$export_button_excel <- downloadHandler(
    filename = function() {
      "index_export.xlsx"
    },
    content = function(file) {
      f_export_to_excel(coin(), shared_reactives$scenarios, file)
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

  # Help --------------------------------------------------------------------

  # help icon
  output$header_help <- renderUI({
    selected_modal <- paste0(input$tab_selected, "_modal")
    header_help_icon(selected_modal)
  })

  # Bookmarking -------------------------------------------------------------

  # things NOT to bookmark otherwise they trigger recreating the coin
  setBookmarkExclude(c("id_input-load_click"))

  # The coin and other things have to be added to the bookmark state$value
  onBookmark(function(state){
    state$values$coin_saved <- coin()
    state$values$r_shared <- shared_reactives
  })

  # When session is restored we have to:
  # - restore the coin from the state$values
  # - extract analysis tables again
  # - make a copy of the coin with no indicators removed, for plotting
  onRestored(function(state){

    coin(state$values$coin_saved)
    shared_reactives <- state$values$r_shared

    # # extract analysis tables
    # if(!is.null(coin()$Analysis$Raw)){
    #   l_analysis(
    #     coin()$Analysis$Raw[c("FlaggedStats", "Flags")]
    #   )
    #   l_analysis_f(
    #     filter_to_flagged(l_analysis())
    #   )
    # }
    #
    # # make "full" coin with all indicators in (for plotting)
    # coin_full(reset_coin(coin()))

  })

}
