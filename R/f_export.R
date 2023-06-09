#' Export to excel
#'
#' Simplified export to Excel with some formatting
#' Note if we change the index structure, the conditional formatting will need
#' to be adjusted.
#'
#'
#' @param coin The coin, with results present.
#' @param l_scen List of scenario data frames
#' @param fname file name to write the results to.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom openxlsx createWorkbook addWorksheet writeData writeDataTable
#'               conditionalFormatting saveWorkbook
#'
#' @return Writes an Excel spreadsheet.
#'
#' @export
f_export_to_excel <- function(coin, l_scen, fname = "index_export.xlsx"){

  # scenarios
  if(is.null(l_scen)){
    l <- list()
  } else {
    # scenarios
    l <- l_scen
    names(l) <- paste0("Scen_", gsub(" ", "_", names(l_scen)))
  }

  # # Results
  # l$Scores <- coin$Results$FullScore
  # l$Ranks <- coin$Results$FullRank

  # Structure
  l$Structure <- coin$Meta$Lineage

  # Analysis
  l$Analysis <- coin$Analysis$Raw$FlaggedStats

  # Weights
  l$Weights <- f_get_last_weights(coin)

  # Data sets
  l <- c(l, coin$Data)

  # colours
  tab_colours <- list(
    Results = "green",
    Structure = "orange",
    Analysis = "blue",
    Weights = "yellow",
    Other = "grey"
  )

  openxlsx::write.xlsx(l, file = fname)

}
