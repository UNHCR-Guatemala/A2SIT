# formatter for strengths and weaknesses
format_sw <-  function(X){

  # round values depending on size of number
  X$Value <- round_values(X$Value)

  X$Rank <- as.integer(X$Rank)

  names(X) <- c("Indicator", "Group", "Rank", "Value")

  X

}

# display formatted indicator table for a selected unit
f_indicator_table <- function(coin, usel){

  # get indicator table for selected unit
  df_out <- COINr::get_unit_summary(
    coin,
    usel = usel,
    Levels = coin$Meta$maxlev:1
  )

  # add level column
  imeta <- coin$Meta$Ind
  df_out$Level <- imeta$Level[match(df_out$Code, imeta$iCode)]
  #df_out$Level <- paste0("Level ", df_out$Level)

  # add severity column
  df_out$Severity <- coin$Data$Severity[
    coin$Data$Severity$uCode == usel,
    match(df_out$Code, names(coin$Data$Severity))
  ] |> as.integer()

  df_out <- df_out[c("Level", "Name", "Rank", "Score", "Severity")]

  # find min and max of ranks ----
  min_rank <- min(df_out$Rank, na.rm = TRUE)
  max_rank <- max(df_out$Rank, na.rm = TRUE)

  # generate colours ----
  breaks <- seq(min_rank, max_rank, length.out = 12)[2:11]
  colour_func <- grDevices::colorRampPalette(c("#0072BC", "#DCE9FF"))
  colour_palette <- colour_func(length(breaks) + 1)

  # same now for severity
  min_sev <- 1
  max_sev <- 5

  # generate colours ----
  breaks_sev <- seq(min_sev, max_sev, length.out = 5)
  colour_func <- grDevices::colorRampPalette(c("#DCE9FF", "#0072BC"))
  colour_palette_sev <- colour_func(length(breaks_sev) + 1)

  # Create table
  df_out |>
    DT::datatable(
      rownames = FALSE,
      selection = "none",
      filter = 'top'
      # extensions = 'RowGroup',
      # options = list(
      #   rowGroup = list(dataSrc = 0),
      #   columnDefs = list(
      #     list(
      #       visible=FALSE,
      #       targets=0)
      #   )
      # )
    ) |>
    DT::formatStyle(
      columns = "Rank",
      backgroundColor = DT::styleInterval(breaks, colour_palette)
    ) |>
    DT::formatStyle(
      columns = "Severity",
      backgroundColor = DT::styleInterval(breaks_sev, colour_palette_sev)
    )

}

f_plot_radar <- function(coin, usel, plot_group){

  icode_level <- get_level_of_icode(coin, plot_group)
  stopifnot(icode_level > 1) # otherwise can't plot children
  plot_level <- icode_level - 1

  if(get_number_of_children(coin, plot_group) < 3){
    return(NULL)
  }

  iCOINr::iplot_radar(
    coin,
    dset = "Aggregated",
    usel = usel,
    Level = plot_level,
    iCodes = plot_group,
    addstat = "median"
  )  |>
    plotly::layout(
      title = "",
      colorway = c("#0072BC", "#FAEB00"))

}

# output a DT table for a group of indicators, for specified unit
f_display_group_table <- function(coin, usel, plot_group){

  icode_level <- get_level_of_icode(coin, plot_group)
  stopifnot(icode_level > 1) # otherwise can't plot children
  plot_level <- icode_level - 1

  dset <- if(plot_level == 1) "Raw" else "Aggregated"

  iData <- COINr::get_data(coin, dset = dset, iCode = plot_group, Level = plot_level)
  iData_usel <- iData[iData$uCode == usel, ]

  iCodes <- names(iData)[names(iData) != "uCode"]

  iranks <- coin$Results$FullRank[coin$Results$FullRank$uCode == usel, iCodes] |>
    as.integer()

  ivalues <- iData_usel[iCodes] |>
    as.numeric() |>
    round_values(as_character = FALSE)

  imedians <- sapply(iData[iCodes], stats::median) |>
    round_values(as_character = FALSE)

  df_out <- data.frame(
    Indicator = COINr::icodes_to_inames(coin, iCodes),
    Rank = iranks,
    Value = ivalues,
    Median = imedians
  )

  DT::datatable(
    df_out,
    rownames = FALSE,
    selection = "none",
    options = list(dom = 't')
  )

}
