# BACK END FUNCTIONS FOR DATA INPUT

#' Data input
#'
#' Reads a formatted Excel file found at `file_path` and outputs a constructed coin.
#'
#' On reading the Excel file, this function does the following:
#'
#' - Data is split into data and metadata and tidied
#' - Metadata is read from structure tab
#' - Any indicators with no data at all are removed
#' - Any indicators with only one unique value are also removed (these cannot be normalised as they
#' have a range of zero)
#' - Any resulting aggregation groups with no "children" are removed
#' - A coin is assembled using COINr and this is the function output
#'
#' If indicators/groups are removed, a message is sent to the console.
#'
#' The Excel file is required to be in a fairly strict format: templates are downloaded
#' through the app or using [f_generate_input_template()].
#'
#' User errors are trapped as much as possible with hopefully-helpful error messages.
#'
#' @param file_path path to the excel file where we have the raw data
#' @param ISO3 ISO3 code of country to which the data belongs, e.g. `"GTM"`
#'
#' @return coin-class object
#'
#' @export
f_data_input <- function(file_path, ISO3, df_geom, uCode_col){

  # Checks ----

  if(!is.null(ISO3)){
    valid_ISOs <- get_cached_countries()
    stopifnot(ISO3 %in% valid_ISOs)
  }

  # Settings ----

  # anchor points in spreadsheet
  idata_topleft <- c(5, 1)
  imeta_topleft <- c(1, 3)
  imeta_botleft <- c(5, 3)

  # col names to look for
  ucode_name <- "admin2Pcode"
  uname_name <- "Name"


  # Read in iData and validate ----

  iData <- readxl::read_excel(
    path = file_path, sheet = "Data",
    range = readxl::cell_limits(ul = idata_topleft, lr = c(NA, NA))
  )

  iData_message <- validate_iData(iData)

  if(!is.null(iData_message)){
    message("Problem in Data tab: ", iData_message)
    return(NULL)
  }

  # Read in iMeta and validate (only indicator level) ----

  iMeta <- readxl::read_excel(
    path = file_path, sheet = "Data",
    range = readxl::cell_limits(ul = imeta_topleft,
                                lr = c(imeta_botleft[1], NA)),
    col_names = FALSE
  ) |> suppressMessages()

  # Tidy iData ----

  names(iData)[names(iData) == ucode_name] <- "uCode"
  names(iData)[names(iData) == uname_name] <- "uName"

  # check that uCodes correspond to admin2 codes in the geometry

  rogue_ucodes <- iData$uCode[iData$uCode %nin% df_geom[[uCode_col]]]
  if(length(rogue_ucodes) > 1){
    message("One or more codes in 'adm2_source_code' column in Data tab not valid codes: ",
            toString(utils::head(rogue_ucodes)))
    message("Did you select the correct country?")
    return(NULL)
  }

  # Tidy and merge metadata ----

  # tidy existing
  iMeta <- as.data.frame(t(iMeta))

  iMeta_message <- validate_iMeta(iMeta)

  if(!is.null(iMeta_message)){
    message("Problem in Data tab (with metadata rows): ", iMeta_message)
    return(NULL)
  }

  names(iMeta) <- c("Weight", "Direction", "Parent", "iName", "iCode")
  iMeta$Weight <- as.numeric(iMeta$Weight)
  iMeta$Direction <- as.numeric(iMeta$Direction)
  row.names(iMeta) <- NULL

  # add cols (ready for merge)
  iMeta$Level <- 1
  iMeta$Type <- "Indicator"

  # merge with aggregate levels
  iMeta_aggs <- readxl::read_excel(path = file_path, sheet = "Structure")

  # check all groups specified in Data tab exist in meta
  rogue_groups <- iMeta$Parent[iMeta$Parent %nin% iMeta_aggs$iCode]
  if(length(rogue_groups) > 1){
    message("One or more groups specified in the Data tab not found in the Structure tab: ", toString(rogue_groups))
    return(NULL)
  }

  # add missing cols
  iMeta_aggs$Direction <- 1
  iMeta_aggs$Type <- "Aggregate"

  iMeta <- rbind(iMeta, iMeta_aggs)

  # Further tidying ----

  # remove indicators with no data

  i_nodata <- names(iData)[colSums(!is.na(iData)) == 0]
  iData <- iData[!(names(iData) %in% i_nodata)]
  iMeta <- iMeta[!(iMeta$iCode %in% i_nodata), ]

  if(length(i_nodata) > 0){
    message("Removed indicators with no data points: \n --> ", toString(i_nodata))
  }

  # remove units with no data
  iData_ <- iData[names(iData) %nin% c("uCode", "uName")]
  empty_rows <- rowSums(!is.na(iData_)) == 0

  if(sum(empty_rows) > 0){
    message("Removed rows with no data points for admin2 codes: ", toString(iData$admin2Pcode[empty_rows]))
    iData <- iData[!empty_rows, ]
  }

  # remove any indicators with same values for all indicators
  iData_ <- iData[names(iData) %nin% c("uCode", "uName")]
  n_unique <- sapply(iData_, function(x){length(unique(x))})
  to_remove <- names(iData_)[n_unique < 2]
  if(length(to_remove) > 0){
    message("Removed indicators with only one unique value: ", toString(to_remove))
    iData <- iData[names(iData) %nin% to_remove]
    iMeta <- iMeta[iMeta$iCode %nin% to_remove, ]
  }

  # iteratively remove groups with no children, working upwards
  for (Level in 2 : max(iMeta$Level, na.rm = TRUE)){

    no_children <- iMeta$iCode[iMeta$Level == Level & !(iMeta$iCode %in% iMeta$Parent)]
    iMeta <- iMeta[!(iMeta$iCode %in% no_children), ]

    if(length(no_children) > 0){
      message("Removed ", length(no_children), " aggregate groups with no children or data at Level ", Level, ": \n --> ", toString(no_children))
    }

  }

  # Build coin and output ----

  COINr::new_coin(iData, iMeta, quietly = TRUE)

}

#' Print Coin
#'
#' This is a print-style text output function for summarising the contents of the coin.
#'
#' It is intended to be used immediately on loading data, to show the user what they have
#' input, so they can check for any unexpected things.
#'
#' @param coin The coin
#'
#' @return Text
#'
#' @export
f_print_coin <- function(coin){

  cat("----------\n")
  cat("Your data:\n")
  cat("----------\n")
  # Input
  # Units
  firstunits <- paste0(utils::head(coin$Data$Raw$uCode, 3), collapse = ", ")
  if(length(coin$Data$Raw$uCode)>3){
    firstunits <- paste0(firstunits, ", ...")
  }

  # Indicators
  iCodes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"]
  firstinds <- paste0(utils::head(iCodes, 3), collapse = ", ")
  if(length(iCodes)>3){
    firstinds <- paste0(firstinds, ", ...")
  }

  cat("Input:\n")
  cat("  Units: ", nrow(coin$Data$Raw), " (", firstunits, ")\n", sep = "")
  cat(paste0("  Indicators: ", length(iCodes), " (", firstinds, ")\n\n"))

  # Structure
  fwk <- coin$Meta$Lineage

  cat("Structure:\n")

  for(ii in 1:ncol(fwk)){

    codes <- unique(fwk[[ii]])
    nuniq <- length(codes)
    first3 <- utils::head(codes, 3)
    if(length(codes)>3){
      first3 <- paste0(first3, collapse = ", ")
      first3 <- paste0(first3, ", ...")
    } else {
      first3 <- paste0(first3, collapse = ", ")
    }

    # colnames are level names
    levnames <- colnames(fwk)
    # check if auto-generated, if so we don't additionally print.
    if(levnames[1] == "Level_1"){
      levnames <- NULL
    }

    if(ii==1){
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " indicators (", first3,") \n"))
    } else {
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " groups (", first3,") \n"))
    }

  }
  cat("\n")

}


#' Generate Excel template for input data, for specified country
#'
#' Writes codes and names of Admin2 regions to an Excel template, for a specified
#' country. Takes an sf-class data frame and extracts codes and names from it.
#'
#' @param df_geom Data frame of "sf" class which contains geometry for mapping the data,
#' and associated codes and names of each geographical area (region/country).
#' @param uCode_col Column name in `df_geom` to use as uCodes in COINr. Must contain
#' unique codes, not starting with number.
#' @param uName_col Column name in `df_geom` to use as uNames in COINr (could also
#' be same as `uCode_col`).
#' @param to_file_name Where to write the template file to.
#'
#' @export
f_generate_input_template <- function(df_geom = NULL, to_file_name = NULL,
                                      uCode_col = NULL, uName_col = NULL,
                                      with_fake_data = FALSE){


  # Prep ------------------------------------------------------------------

  stopifnot(inherits(df_geom, "sf"))

  if(is.null(uCode_col) || uCode_col == ""){
    uCode_col <- "adm2_source_code"
  }
  if(is.null(uName_col) || uName_col == ""){
    uName_col <- "gis_name"
  }

  if(is.null(to_file_name)){
    to_file_name <- paste0("A2SIT_data_input_template.xlsx")
  }


  # Make template -----------------------------------------------------------

  # get cols of interest and sort
  df_write <- data.frame(
    admin2Pcode = df_geom[[uCode_col]],
    Name = df_geom[[uName_col]]
  )
  df_write <- df_write[order(df_write$admin2Pcode), ]

  if(with_fake_data){
    l <- populate_with_fake_data(df_write)
    df_write <- l$iData
  }

  names(df_write)[1:2] <- c("admin2Pcode",	"Name")

  # load template
  wb <- system.file("A2SIT_data_input_template.xlsx", package = "A2SIT") |>
    openxlsx::loadWorkbook()

  # write codes/names to wb
  openxlsx::writeData(
    wb, sheet = "Data", x = df_write, colNames = TRUE, startCol = 1, startRow = 5)

  if(with_fake_data){
    # also write fake metadata
    openxlsx::writeData(
      wb, sheet = "Data", x = l$iMeta_numeric, colNames = FALSE, startCol = 3, startRow = 1)
    openxlsx::writeData(
      wb, sheet = "Data", x = l$iMeta_text, colNames = FALSE, startCol = 3, startRow = 3)
  }

  # save
  openxlsx::saveWorkbook(wb, to_file_name, overwrite = T)
}

populate_with_fake_data <- function(df_write){

  iMeta <- readxl::read_excel(
    system.file("A2SIT_data_input_template.xlsx", package = "A2SIT"),
    sheet = "Structure"
  )

  iCodes_l2 <- iMeta$iCode[iMeta$Level == 2]
  n_unit <- nrow(df_write)

  iMeta_rows <- data.frame(Init_weight = 1, Direction = 1, Group = iCodes_l2[1], iName = "Indicator 1")
  iMeta_rows <- iMeta_rows[FALSE, ]

  i_index <- 0

  for(parent_code in iCodes_l2){

    n_ind <- sample(3, 1)
    i_indexes <- 1:n_ind + i_index

    cols2add <- matrix(runif(n_ind*n_unit), nrow = n_unit, ncol = n_ind) |>
      as.data.frame()
    names(cols2add) <- paste0("ind_", i_indexes)

    df_write <- cbind(df_write, cols2add)

    iMeta_add <- data.frame(
      Init_weight = 1,
      Direction = sample(c(-1, 1, 1, 1), n_ind),
      Group = rep(parent_code, n_ind),
      iName = paste0("Indicator ", i_indexes)
    )

    iMeta_rows <- rbind(iMeta_rows, iMeta_add)

    i_index <- i_indexes[length(i_indexes)]
  }

  iMeta_numeric <- t(iMeta_rows[1:2]) |> as.data.frame()
  iMeta_text <- t(iMeta_rows[3:4]) |> as.data.frame()

  list(iData = df_write,
       iMeta_numeric = iMeta_numeric,
       iMeta_text = iMeta_text)

}


#' Verbose check on iData tab
#'
#' Checks and messages to pass to the user for basic errors in the iData tab of
#' the input spreadsheet. This is *not* intended to be used on the iData input
#' to COINr, but rather the iData tab of the template, which is a bit different.
#' Note COINr does many of these checks anyway, but the intention is to generate
#' some user-friendly versions here. Called in [f_data_input()].
#'
#' @param iData Table imported from user input spreadsheet.
#'
#' @return Message as text, or `NULL` if no errors.
#' @export
#'
validate_iData <- function(iData){


  # ID cols -----------------------------------------------------------------

  if(!is.data.frame(iData)){
    return("Cannot recognise your data as valid table - something very wrong!")
  }

  if("admin2Pcode" %nin% names(iData)){
    return("Expected column 'admin2Pcode' not found in your data - did you change the column name or delete it?")
  }

  if("Name" %nin% names(iData)){
    return("Expected column 'Name' not found in your data - did you change the column name or delete it?")
  }

  if(!is.character(iData$admin2Pcode)){
    return("The 'admin2Pcode' column is expected to be formatted as text but it is not (did you enter numbers here?)")
  }

  if(!is.character(iData$Name)){
    return("The 'Name' column is expected to be formatted as text but it is not (did you enter numbers here?)")
  }

  if(any(is.na(iData$admin2Pcode))){
    return("Missing values found in 'admin2Pcode' column - please correct.")
  }

  if(any(is.na(iData$Name))){
    return("Missing values found in 'Name' column - please correct.")
  }

  if("Rank" %in% names(iData)){
    return("Reserved column name 'Rank' found. Please rename this indicator code.")
  }


  # Data cols ---------------------------------------------------------------

  if(ncol(iData) < 3){
    return("No data columns found in your data?")
  }

  # get only data cols (exclude those with all NAs which are removed later)
  iData_ <- iData[(names(iData) %nin% c("admin2Pcode","Name")) &
                    !(colSums(!is.na(iData)) == 0)]

  not_numeric <- names(iData_)[!sapply(iData_, is.numeric)]
  if(length(not_numeric) > 0){
    return(paste0("One or more of your data columns have non-numeric entries: ", toString(not_numeric)))
  }

  duplicate_icodes <- names(iData)[duplicated(names(iData))]
  if(length(duplicate_icodes) > 0){
    return(paste0("Duplicate indicator codes detected: ", toString(duplicate_icodes)))
  }

  duplicate_ucodes <- iData$admin2Pcode[duplicated(iData$admin2Pcode)]
  if(length(duplicate_ucodes) > 0){
    return(paste0("Duplicate admin2 codes detected: ", toString(duplicate_ucodes)))
  }

  NULL

}


#' Verbose check on indicator metadata tab
#'
#' Checks and messages to pass to the user for basic errors in metadata rows of
#' the Data tab in the input template. Called in [f_data_input()].
#'
#' @param iMeta Table imported from user input spreadsheet.
#'
#' @return Message as text, or `NULL` if no errors.
#' @export
#'
validate_iMeta <- function(iMeta){

  if(!is.data.frame(iMeta)){
    return("Cannot recognise indicator data as valid table - something very wrong!")
  }

  # weights
  w_col <- iMeta[[1]]

  if(any(is.na(w_col))){
    return("One or more missing values in the 'weights' row - please fill in.")
  }

  if(!possibly_numeric(w_col)){
    return("One or more entries in the 'weights' row are not numbers.")
  }

  if(any(as.numeric(w_col) < 0)){
    return("Negative weights detected?")
  }

  # directions
  d_col <- iMeta[[2]]

  if(any(is.na(d_col))){
    return("One or more missing values in the 'directions' row - please fill in.")
  }

  if(!possibly_numeric(d_col)){
    return("One or more entries in the 'directions' row are not numbers.")
  }

  if(any(as.numeric(d_col) %nin% c(-1, 1))){
    return("Values in the 'directions' row found that are not -1 or 1. Please fix.")
  }

  # parents
  p_col <- iMeta[[3]]

  if(any(is.na(p_col))){
    return("One or more missing values in the 'Group' row - please fill in.")
  }

  if(possibly_numeric(p_col)){
    return("One or more entries in the 'Group' row look like numbers - please ensure codes start with a letter.")
  }

  # names
  n_col <- iMeta[[4]]

  if(any(is.na(n_col))){
    return("One or more missing values in the 'Indicator name' row - please fill in.")
  }

  if(possibly_numeric(n_col)){
    return("One or more entries in the 'Indicator' row look like numbers - please ensure names start with a letter.")
  }

  NULL


}
