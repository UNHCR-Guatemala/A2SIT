#' App server function
#'
#' This is called by `run_app()`.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session SHiny session
#'
#' @import shiny
app_server <- function(input, output, session) {


  # Shared variables --------------------------------------------------------

  # initialise reactives to be shared between modules. These can be modified
  # within any module and don't need to be passed back out.

  coin <- reactiveVal(NULL)
  coin_full <- reactiveVal(NULL)
  r_shared <- reactiveValues(
    ISO3 = NULL, # country of the data
    profile_unit = NULL, # unit to view in profiles
    results_built = FALSE, # whether results (up to aggregation) built
    scenarios = NULL, # list of saved scenarios for comparison
    l_analysis = NULL,
    l_analysis_f = NULL
  )

  # Modules -----------------------------------------------------------------

  welcome_server("id_welcome", session)
  input_server("id_input", coin, coin_full, r_shared)
  analysis_server("id_analysis", coin, coin_full, input, r_shared)
  results_server("id_results", coin, coin_full, input, session, r_shared)
  profiles_server("id_profiles", coin, coin_full, input, r_shared)
  scenarios_server("id_scenarios", coin, input, r_shared)
  compare_units_server("id_compare_units", coin, input, r_shared)

  # Exports -----------------------------------------------------------------

  # Export to Excel
  output$export_button_excel <- downloadHandler(
    filename = function() {
      "index_export.xlsx"
    },
    content = function(file) {
      f_export_to_excel(coin(), r_shared$scenarios, file)
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
    state$values$r_shared <- reactiveValuesToList(r_shared)
  })

  # When session is restored we have to:
  # - restore the coin from the state$values
  # - extract analysis tables again
  # - make a copy of the coin with no indicators removed, for plotting
  onRestored(function(state){

    # restore coin
    coin(state$values$coin_saved)
    # restore reactive values list (has to be done in loop unfortunately)
    for(rname in names(state$values$r_shared)){
      r_shared[[rname]] <- state$values$r_shared[[rname]]
    }

    # make "full" coin with all indicators in (for plotting)
    coin_full(reset_coin(coin()))

  })

}
