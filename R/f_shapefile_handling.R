# reads a geojson and outputs as sf
geojson_to_sf <- function(file_path){

  tryCatch({
      sf::st_read(file_path)
    },
    error = function(e){
      message("Geometry file not successfully read/converted by the app.")
      print(e)
    }
  )

}

# A series of checks and possible fixes for geometry data frame uploaded by user
check_and_fix_sf <- function(df_geom, uCode_col, uName_col){

  stopifnot(inherits(df_geom, "sf"))

  if(uCode_col %nin% names(df_geom)){
    stop("String specified as 'uCode' is not found among column names of uploaded geometry table.")
  }
  if(uName_col %nin% names(df_geom)){
    stop("String specified as 'uName' is not found among column names of uploaded geometry table.")
  }

  uCodes <- df_geom[[uCode_col]]
  uNames <- df_geom[[uName_col]]

  stopifnot(is.character(uCodes),
            is.character(uNames),
            !anyNA(uCodes),
            !anyNA(uNames))

  # check duplicates and rename
  if(anyDuplicated(uCodes) > 1){
    # rename duplicates
    uCodes <-  make.unique(uCodes)
    warning("Duplicate codes detected in column '", uCode_col, "' - these have been renamed to be unique.")
  }

  # check numbers at beginning
  # should not contain spaces
  spaces <- grepl(" ", uCodes)
  if(any(spaces)){
    uCodes <- gsub(" ", "", uCodes)
    warning("Spaces detected in entries of column '", uCode_col, "' - these are not allowed and have been removed.")
  }

  # should not start with a number
  num_start <- substring(uCodes, 1,1) %in% 0:9
  if(any(num_start)){
    uCodes <- paste0("u_", uCodes)
    warning("Some entries in column '", uCode_col, "' begin with a number. These have been adjusted to avoid processing difficulties.")
  }

  df_geom[[uCode_col]] <- uCodes

  df_geom

}
