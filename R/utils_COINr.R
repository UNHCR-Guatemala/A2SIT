# Utility functions for COINr
# These are general-purpose utility functions for COINr

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

get_codes_at_level <- function(coin, Level){
  codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Level == Level]
  codes[!is.na(codes)] |>
    unique()
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

# returns the direction of an indicator as 1 or -1
get_indicator_direction <- function(coin, iCode){
  stopifnot(iCode %in% coin$Meta$Ind$iCode)
  coin$Meta$Ind$Direction[coin$Meta$Ind$iCode == iCode]
}

# returns a data frame of equal weights
f_get_equal_weights <- function(coin){

  w <- coin$Meta$Weights$Original
  stopifnot(!is.null(w))

  w$Weight <- 1

  w
}

# Returns the last set of weights used when aggregating the index.
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
