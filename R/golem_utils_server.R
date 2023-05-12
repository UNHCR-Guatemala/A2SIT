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

get_number_of_children <- function(coin, iCode){
  sum(coin$Meta$Ind$Parent == iCode, na.rm = TRUE)
}

# get the highest level code
get_index_code <- function(coin){
  index_code <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == coin$Meta$maxlev]
  index_code[!is.na(index_code)] |>
    unique()
}

get_parent <- function(coin, iCode, at_level = 2, as_name = TRUE){
  pCode <- coin$Meta$Lineage[[at_level]][coin$Meta$Lineage[[1]] == iCode]
  if(as_name){
    COINr::icodes_to_inames(coin, pCode)
  } else {
    pCode
  }
}

get_inclusion_status <- function(l_analysis, isel){
  l_analysis$FlaggedStats$Status[l_analysis$FlaggedStats$iCode == isel]
}

get_indicator_info <- function(coin_full, coin, isel){

  df_stats <- coin$Analysis$Raw$Stats
  isel_row <- df_stats$iCode == isel

  ind_status <- get_inclusion_status(coin$Analysis$Raw, isel)
  stat_string <- paste0("<b>Status:</b> ", ind_status)

  cat_string <- paste0("<b>Category:</b> ", get_parent(coin, isel, 2))
  dim_string <- paste0("<b>Dimension:</b> ", get_parent(coin_full, isel, 3))

  xmin <- df_stats$Min[isel_row]
  xmax <- df_stats$Max[isel_row]
  minmax_string <- paste0("<b>Min/max:</b> ", xmin, "/", xmax)

  n_avail <- df_stats$N.Avail[isel_row]
  prc_avail <- df_stats$Frc.Avail[isel_row]*100
  avail_string <- paste0("<b>Availabilty:</b> ", n_avail, " obs (", prc_avail, "%)")


  paste0(cat_string, "<br>", dim_string, "<br>", stat_string, "<br>",
         minmax_string, "<br>", avail_string)

}

get_codes_at_level <- function(coin, Level){
  codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == Level]
  codes[!is.na(codes)] |>
    unique()
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

get_n_indicators <- function(coin){
  coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"] |>
    length()
}

get_n_units <- function(coin){
  nrow(coin$Data$Raw)
}

get_index_rank <- function(coin, usel){
  index_code <- get_index_code(coin)
  coin$Results$FullRank[[index_code]][
    coin$Results$FullRank$uCode == usel
  ]
}

get_index_score <- function(coin, usel){
  index_code <- get_index_code(coin)
  coin$Data$Aggregated[[index_code]][
    coin$Data$Aggregated$uCode == usel
  ]
}

get_level_of_icode <- function(coin, iCode){
  coin$Meta$Ind$Level[coin$Meta$Ind$iCode == iCode]
}

# Not in operator
#
# For convenience, rather than always `!(x, %in% y)`
#
# @param x A scalar or vector
# @param y A scalar or vector
#
# @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}
