# BACK END FUNCTIONS FOR DATA INPUT

#' Data input
#'
#' Reads a formatted Excel file found at `file_path` and outputs a constructed coin.
#'
#' On reading the Excel file, this function does the following:
#'
#' - Data is split into data and metadata and tidied
#' - Metadata is merged with hard-coded index structure
#' - Any indicators with no data at all are removed
#' - Any resulting aggregation groups with no "children" are removed
#' - A coin is assembled using COINr and this is the function output
#'
#' If indicators/groups are removed, a message is sent to the console.
#'
#' The Excel file is required to be in a fairly strict format: an example is given at
#' `inst/data_module-input.xlsx`. This template is still a work in progress
#' and can be modified in the app phase following further feedback.
#'
#' @param file_path path to the excel file where we have the raw data
#' @param ISO3 ISO3 code of country to which the data belongs, e.g. `"GTM"`
#'
#' @return coin-class object
#'
#' @export
f_data_input <- function(file_path, ISO3){

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

  # check that uCodes correspond to admin2 codes in the geometry file
  # get geom
  admin2_geom <- system.file("geom", paste0(ISO3,".RDS"), package = "A2SIT") |>
    readRDS()

  rogue_ucodes <- iData$uCode[iData$uCode %nin% admin2_geom$adm2_source_code]
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
#' @return A coin
#'
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
#' country.
#'
#' @param ISO3 Valid ISO3 code with geometry available in inst/geom
#' @param to_file_name Where to write to
#' @export
f_generate_input_template <- function(ISO3, to_file_name = NULL){

  stopifnot(ISO3 %in% get_cached_countries())

  if(is.null(to_file_name)){
    to_file_name <- paste0("A2SIT_data_input_template_", ISO3, ".xlsx")
  }

  # load table for selected country
  df_ISO3 <- system.file("geom", paste0(ISO3,".RDS"), package = "A2SIT") |>
    readRDS()

  # get cols of interest and sort
  df_write <- data.frame(
    admin2Pcode = df_ISO3$adm2_source_code,
    Name = df_ISO3$gis_name
  )
  df_write <- df_write[order(df_write$admin2Pcode), ]

  # load template
  wb <- system.file("A2SIT_data_input_template.xlsx", package = "A2SIT") |>
    openxlsx::loadWorkbook()

  # write codes/names to wb
  openxlsx::writeData(
    wb, sheet = "Data", x = df_write, colNames = F, startCol = 1, startRow = 6)

  # save
  openxlsx::saveWorkbook(wb, to_file_name, overwrite = T)
}

# checks and messages to pass to the user for basic errors in iData
# Note COINr does many of these checks anyway, but the intention is to generate
# some user-friendly versions here
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


# checks and messages to pass to the user for basic errors in iMeta
# Note COINr does many of these checks anyway, but the intention is to generate
# some user-friendly versions here
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
