#' @import shiny
app_server <- function(input, output, session) {


  # Shared variables --------------------------------------------------------

  # initialise reactives to be shared between modules. These can be modified
  # within any module and don't need to be passed back out.

  coin <- reactiveVal(NULL)
  coin_full <- reactiveVal(NULL)
  shared_reactives <- reactiveValues(
    profile_unit = NULL, # unit to view in profiles
    results_built = FALSE # whether results (up to aggregation) built
  )

  # Modules -----------------------------------------------------------------

  input_server("id_input", coin, coin_full)
  analysis_server("id_analysis", coin, coin_full, input)
  results_server("id_results", coin, coin_full, input, session, shared_reactives)
  profiles_server("id_profiles", coin, coin_full, input, shared_reactives)

  # Extras (not modules) ----------------------------------------------------

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


}
