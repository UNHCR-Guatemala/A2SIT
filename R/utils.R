# General purpose utility functions for programming

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

# can a vector be coerced to numeric?
possibly_numeric <- function(x){
  suppressWarnings(all(!is.na(as.numeric(as.character(x)))))
}

# function for adjusting figures on different scales
# outputs a character vector...
round_values <- function(x, as_character = TRUE){

  if(as_character){
    ifelse(
      (is.wholenumber(x)) | (x > 1000),
      as.character(round(x)),
      as.character(signif(x, 3))
    )
  } else {
    ifelse(
      (is.wholenumber(x)) | (x > 1000),
      round(x),
      signif(x, 3)
    )
  }

}

# function to check if whole number (as opposed to integer)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}
