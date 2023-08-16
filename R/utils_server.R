# UTILITY FUNCTIONS FOR SERVER

# named list of units, for dropdown menus
get_unit_list <- function(coin){
  l_units <- as.list(coin$Meta$Unit$uCode)
  names(l_units) <- coin$Meta$Unit$uName
  l_units
}

# check for indicator analysis
# returns TRUE if analysis is present
analysis_exists <- function(coin){
  !is.null(coin$Analysis$Raw$FlaggedStats)
}

# check for results
# returns TRUE if results are present
results_exist <- function(coin){
  !is.null(coin$Data$Aggregated)
}

# whether an indicator is currently included or not
get_inclusion_status <- function(l_analysis, isel){
  l_analysis$FlaggedStats$Status[l_analysis$FlaggedStats$iCode == isel]
}

#' HTML summary of a selected indicator
#'
#' Used in the analysis tab.
#'
#' @param coin_full The full coin with no indicators removed
#' @param coin The coin
#' @param isel A selected indicator (iCode)
#'
#' @return A text summary, as html.
#' @export
#'
get_indicator_info <- function(coin_full, coin, isel){

  df_stats <- coin$Analysis$Raw$Stats
  isel_row <- df_stats$iCode == isel

  ind_status <- get_inclusion_status(coin$Analysis$Raw, isel)
  stat_string <- paste0("<b>Status:</b> ", ind_status)

  cat_string <- paste0("<b>Category (level 2):</b> ", get_parent(coin_full, isel, 2))
  dim_string <- paste0("<b>Dimension (level 3):</b> ", get_parent(coin_full, isel, 3))

  xmin <- df_stats$Min[isel_row]
  xmax <- df_stats$Max[isel_row]
  minmax_string <- paste0("<b>Min/max:</b> ", xmin, "/", xmax)

  xmean <- df_stats$Mean[isel_row]
  xmed<- df_stats$Median[isel_row]
  meanmed_string <- paste0("<b>Mean/median:</b> ", xmean, "/", xmed)

  n_avail <- df_stats$N.Avail[isel_row]
  prc_avail <- df_stats$Frc.Avail[isel_row]*100
  avail_string <- paste0("<b>Availabilty:</b> ", n_avail, " obs (", prc_avail, "%)")


  paste0(cat_string, "<br>",
         dim_string, "<br>",
         minmax_string, "<br>",
         meanmed_string, "<br>",
         avail_string, "<br>",
         stat_string)

}

#' Get list of available indicator/aggregate codes
#'
#' This is for use in dropdown menus, e.g. in plotting.
#'
#' @param coin The coin
#' @param code_types Either `"Aggregate"` (for just aggregate codes), `"Indicator"` (for
#' just indicator codes) or `"all"` for both.
#' @param with_levels If `TRUE`, outputs a named list where the list names are of the format
#' "Level: iCode" or "Level: iName". These names are what users will see in the dropdown menus.
#' Else if `FALSE` just outputs a character vector of iCodes.
#' @param use_names if `TRUE` uses iNames in the named list, else uses iCodes.
#'
#' @return A list or character vector, for input into e.g. `selectInput()`.
get_indicator_codes <- function(coin, code_types = "all",
                                with_levels = TRUE, use_names = TRUE) {

  stopifnot(code_types %in% c("all", "Aggregate", "Indicator"))

  if(code_types == "all"){
    code_types <- c("Indicator", "Aggregate")
  }

  # Filter iMeta to code types of interest, then
  # sort iMeta by highest level downwards (better for display in dropdown menus)
  imeta <- coin[["Meta"]][["Ind"]]
  imeta <- imeta[imeta[["Type"]] %in% code_types, ]
  imeta <- imeta[order(-imeta[["Level"]]), ]

  if (with_levels) {

    imeta_split <- split(imeta, imeta$Level)

    l_out <- lapply(imeta_split, function(df_level){
      l <- as.list(df_level$iCode)
      names(l) <- df_level$iName
      l
    })
    names(l_out) <- paste0("Level ", names(l_out))

    # codes_to_display <- if(use_names) imeta[['iName']] else imeta[['iCode']]
    #
    # l_out <- as.list(imeta[["iCode"]])
    # names(l_out) <- glue::glue("{imeta[['Level']]}: {codes_to_display}")
    rev(l_out)


  } else {
    imeta[["iCode"]]
  }

}

# function that returns the present weights in the sliders in the UI
# This must be used in a reactive environment.
get_slider_weights <- function(input, icodes){

  l_input <- isolate(reactiveValuesToList(input))[icodes]
  if(all(sapply(l_input, is.null))){
    return(NULL)
  }
  w <- as.numeric(l_input)
  names(w) <-  names(l_input)
  w

}

# function to rescale a numeric vector x to 1-5
# if direction is -1, the scale is reversed
to_discrete_scale <- function(x, direction = 1){
  stopifnot(direction %in% c(1, -1), is.numeric(x))
  x <-  x*direction
  cut(x, 5, labels = FALSE)
}

# get correct data set to plot results: Raw for indicators, otherwise Aggregated
get_plot_dset <- function(coin, iCode){
  icode_level <- get_level_of_icode(coin, iCode)
  if (icode_level == 1) "Raw" else "Aggregated"
}

#' Shortcut function for building GTM coin, for debugging
#'
#' @return coin
#' @export
f_build_GTM_coin <- function(){
  coin <- f_data_input("./inst/A2SIT_data_input_template_GTM.xlsx", "GTM")
  f_build_index(coin)
}
