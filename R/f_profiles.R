#' Formatter for strengths and weaknesses
#'
#' Tidies up formatting of numbers
#'
#' @param X Output from [COINr::get_str_weak()]
#'
#' @return Tidied table
#' @export
#'
format_sw <-  function(X){

  # round values depending on size of number
  X$Value <- round_values(X$Value)

  X$Rank <- as.integer(X$Rank)

  names(X) <- c("Indicator", "Group", "Rank", "Value")

  X

}

#

#' Data frame of score, ranks etc for specific unit
#'
#' This is like [COINr::get_unit_summary()] but with some additions.
#'
#' @param coin The coin
#' @param usel A uCode found within the coin
#'
#' @return Data frame
#' @export
#'
f_indicator_df <- function(coin, usel){

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
  ] |> as.numeric()

  df_out <- df_out[c("Level", "Name", "Rank", "Score", "Severity")]

  df_out

}

# display

#' Interactive indicator table for a selected unit
#'
#' A DT table for a selected unit
#'
#' @param coin The coin
#' @param usel A uCode found within the coin
#'
#' @return A DT table
#' @export
#'
f_indicator_table <- function(coin, usel){

  df_out <- f_indicator_df(coin, usel)

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

  # convert level to factor so filters display properly
  df_out$Level <- as.factor(df_out$Level)

  # Create table
  df_out |>
    DT::datatable(
      rownames = FALSE,
      selection = "none",
      filter = 'top'
      # extensions = 'RowGroup', # this was for grouping rows, see DT documentation...
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

#' Comparison table for two units
#'
#' A data frame with scores, ranks, severity, for all levels.
#'
#' @param coin The coin
#' @param usel1 A unit, referenced by its uCode
#' @param usel2 A unit, referenced by its uCode
#'
#' @return A data frame
#' @export
#'
f_compare_units_df <- function(coin, usel1, usel2){

  df1 <- f_indicator_df(coin, usel1)
  df2 <- f_indicator_df(coin, usel2)

  uname1 <- COINr::ucodes_to_unames(coin, usel1)
  uname2 <- COINr::ucodes_to_unames(coin, usel2)

  # rename
  names(df1)[3:5] <- paste0(names(df1)[3:5], " (", uname1,")")
  names(df2)[3:5] <- paste0(names(df2)[3:5], " (", uname2,")")

  # merge
  df12 <- base::merge(df1, df2, by = c("Level", "Name"), sort = FALSE)

  # parent
  df12$Parent <- coin$Meta$Ind$Parent[match(df12$Name, coin$Meta$Ind$iName)]

  # reorder
  df12 <- df12[c("Level", "Name", "Parent",
                 names(df12)[startsWith(names(df12), "Rank")],
                 names(df12)[startsWith(names(df12), "Score")],
                 names(df12)[startsWith(names(df12), "Severity")])]

}

#' Interactive unit comparison table
#'
#' Calls [f_compare_units_df()], then just shows the columns specified by `using`.
#' This is in order not to have too much info in the table. Returns DT table.
#'
#' @param coin The coin
#' @param usel1 A unit, referenced by its uCode
#' @param usel2 A unit, referenced by its uCode
#' @param using One of `c("Ranks", "Scores", "Severity")`
#'
#' @return A DT table with highlighting
#' @export
#'
f_compare_units_table <- function(coin, usel1, usel2, using){

  stopifnot(using %in% c("Ranks", "Scores", "Severity"))

  dfc <- f_compare_units_df(coin, usel1, usel2)
  dfc$Level <- as.factor(dfc$Level) # DT filter works better this way

  # delete columns we don't need
  if (using == "Ranks"){
    dfc <- dfc[!startsWith(names(dfc), "Score")]
    dfc <- dfc[!startsWith(names(dfc), "Severity")]
    compare_cols <- data.frame(
      r1 = as.numeric(dfc[[4]] < dfc[[5]]),
      r2 = as.numeric(dfc[[5]] < dfc[[4]])
    )
  } else if (using == "Scores"){
    dfc <- dfc[!startsWith(names(dfc), "Rank")]
    dfc <- dfc[!startsWith(names(dfc), "Severity")]
    compare_cols <- data.frame(
      sc1 = as.numeric(dfc[[4]] > dfc[[5]]),
      sc2 = as.numeric(dfc[[5]] > dfc[[4]])
    )
  } else if (using == "Severity"){
    dfc <- dfc[!startsWith(names(dfc), "Rank")]
    dfc <- dfc[!startsWith(names(dfc), "Score")]
    compare_cols <- data.frame(
      sv1 = as.numeric(dfc[[4]] > dfc[[5]]),
      sv2 = as.numeric(dfc[[5]] > dfc[[4]])
    )
  }

  ncol_display <- ncol(dfc)

  # compare_cols will be hidden inside the DT call next
  dfc <- cbind(dfc, compare_cols)

  # highlight colours
  styles <- c("white", "#8EBEFF")

  DT::datatable(
    dfc,
    options = list(
      scrollX = TRUE,
      columnDefs = list(
        list(
          visible=FALSE,
          targets=ncol_display:(ncol(dfc)-1)
        )
      )),
    rownames = FALSE,
    selection = "none",
    filter = 'top'
  ) |>
    DT::formatStyle(
      columns = 4:ncol_display,
      valueColumns = (ncol_display + 1):ncol(dfc),
      backgroundColor = DT::styleEqual(c(0,1), styles)
    )

}

#' Radar plot for a group
#'
#' Thin wrapper for [iCOINr::iplot_radar()]: just makes sure to return `NULL` if
#' less than 3 indicators in group.
#'
#' @param coin The coin
#' @param usel Selected unit (uCode)
#' @param plot_group iCode of group to plot
#'
#' @return Radar plot, using plotly
#' @export
#'
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

#' Interactive table of group scores
#'
#' This is the counterpart table to the radar plot. Returns a table of scores or
#' values for a given aggregation group, including ranks and medians.
#'
#' @param coin The coin
#' @param usel Selected unit (uCode)
#' @param plot_group iCode of group to display
#'
#' @return A DT table
#' @export
#'
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
