# Utility functions for COINr
# These are general-purpose utility functions for COINr, which are useful
# within the app.


#' Find number of children
#'
#' Returns number of children of specified indicator/aggregate
#'
#' @param coin The coin
#' @param iCode An indicator/aggregate
#'
#' @return Number
#' @export
#'
get_number_of_children <- function(coin, iCode){
  sum(coin$Meta$Ind$Parent == iCode, na.rm = TRUE)
}


#' Get the highest level iCode
#'
#' Returns the iCode at the highest level, i.e. the index code.
#'
#' @param coin The coin
#'
#' @return An iCode (string)
#' @export
#'
get_index_code <- function(coin){
  index_code <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == coin$Meta$maxlev]
  index_code[!is.na(index_code)] |>
    unique()
}


#' Get parent at specified level
#'
#' Returns the parent group of an iCode at a specified level, either as iCode or
#' iName.
#'
#' @param coin The coin
#' @param iCode iCode to check for parent
#' @param at_level Level to check for parent
#' @param as_name Logical: if `TRUE` returns iName
#'
#' @return iCode or iName of parent
#' @export
#'
get_parent <- function(coin, iCode, at_level = 2, as_name = TRUE){

  code_level <- get_level_of_icode(coin, iCode)

  if(at_level %nin% code_level:coin$Meta$maxlev) stop("at_level is out of range...")

  pCode <- coin$Meta$Lineage[[at_level]][coin$Meta$Lineage[[code_level]] == iCode] |>
    unique()
  if(as_name){
    COINr::icodes_to_inames(coin, pCode)
  } else {
    pCode
  }
}

#' Get iCodes at specified level
#'
#' Returns all iCodes at a specified level
#'
#' @param coin The coin
#' @param Level Index level
#'
#' @return Character vector of iCodes
#' @export
#'
get_codes_at_level <- function(coin, Level){

  if(Level %nin% 1:coin$Meta$maxlev) stop("Level is out of range...")
  codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == Level]
  codes[!is.na(codes)] |>
    unique()
}

#' Get total number of indicators
#'
#' @param coin The coin
#'
#' @return Number
#' @export
#'
get_n_indicators <- function(coin){
  coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"] |>
    length()
}

#' Get total number of units
#'
#' @param coin The coin
#'
#' @return Number
#' @export
#'
get_n_units <- function(coin){
  nrow(coin$Data$Raw)
}

#' Get rank of selected unit at index level
#'
#' @param coin The coin
#' @param usel A unit (uCode)
#'
#' @return A rank
#' @export
#'
get_index_rank <- function(coin, usel){
  index_code <- get_index_code(coin)
  coin$Results$FullRank[[index_code]][
    coin$Results$FullRank$uCode == usel
  ]
}

#' Get score of selected unit at index level
#'
#' @param coin The coin
#' @param usel A unit (uCode)
#'
#' @return A score
#' @export
#'
get_index_score <- function(coin, usel){
  index_code <- get_index_code(coin)
  coin$Data$Aggregated[[index_code]][
    coin$Data$Aggregated$uCode == usel
  ]
}

#' Get level of iCode
#'
#' Returns the level of iCode in the framework
#'
#' @param coin The coin
#' @param iCode An iCode
#'
#' @return Level, as integer
#' @export
#'
get_level_of_icode <- function(coin, iCode){
  coin$Meta$Ind$Level[coin$Meta$Ind$iCode == iCode]
}

#' Get direction of iCode
#'
#' Returns the direction of iCode
#'
#' @param coin The coin
#' @param iCode An iCode
#'
#' @return Either -1 or 1
#' @export
#'
get_indicator_direction <- function(coin, iCode){
  stopifnot(iCode %in% coin$Meta$Ind$iCode)
  coin$Meta$Ind$Direction[coin$Meta$Ind$iCode == iCode]
}

#' Data frame of equal weights
#'
#' Copies the original weights in the coin and sets them all equal
#'
#' @param coin The coin
#'
#' @return Data frame of weights
#' @export
#'
f_get_equal_weights <- function(coin){

  w <- coin$Meta$Weights$Original
  stopifnot(!is.null(w))

  w$Weight <- 1

  w
}


#' Find last weights used
#'
#' Returns the last set of weights used when aggregating the index, by checking
#' the log. Or `NULL` if not aggregated yet.
#'
#' @param coin The coin
#'
#' @return Data frame of weights.
#' @export
#'
f_get_last_weights <- function(coin){

  if(is.null(coin$Log$Aggregate)){
    return(NULL)
  }

  w_log <- coin$Log$Aggregate$w

  if(is.null(w_log)){

    w_new <- coin$Meta$Weights$Original

  } else if (is.character(w_log)){

    w_new <- coin$Meta$Weights[[w_log]]

  } else if (is.data.frame(w_log)){

    w_new <- w_log
  } else {
    stop("Weights not recognised at coin$Log$Aggregate$w", call. = FALSE)
  }

  stopifnot(is.data.frame(w_new))

  w_new

}


#' Replace all indicators
#'
#' Reset coin so that all indicators are included, and regenerate results.
#' Note that any analysis tables etc will be lost.
#'
#' @param coin The coin
#'
#' @return Updated coin
#' @export
#'
reset_coin <- function(coin){
  stopifnot(is.coin(coin))
  coin$Log$new_coin$exclude <- NULL
  COINr::Regen(coin, quietly = TRUE)
}
