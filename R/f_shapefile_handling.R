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
check_and_fix_sf <- function(df_geom, uCode_col = NULL, uName_col = NULL){

  stopifnot(inherits(df_geom, "sf"))

  if(is.null(uCode_col)){
    uCode_col <- "adm2_source_code"
  }
  if(is.null(uName_col)){
    uName_col <- "gis_name"
  }

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
    uCodes <- gsub(" ", "_", uCodes)
    warning("Spaces detected in entries of column '", uCode_col, "' - these have been substituted with '_'.")
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


sample_geom_column <- function(df_geom, col_selected, max_length = 30){

  stopifnot(col_selected %in% names(df_geom))

  str_out <- df_geom[[col_selected]]

  if(!(is.character(str_out) || is.numeric(str_out))){
    return("Invalid column selected (not numeric/character)")
  }
  if(all(str_out == "") || all(is.na(str_out))){
    return("Empty column selected")
  }

  str_out <- head(str_out, 3) |> toString(max_length)
  str_out <- paste0("Sample: ", str_out)

  str_out
}

# adds a column 'AppGeneratedCode'
add_uCode_col <- function(df_geom){
  df_geom$AppGeneratedCode <- paste0("r", sprintf("%04d", 1:nrow(df_geom)))
  df_geom
}
