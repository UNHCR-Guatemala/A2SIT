
#' Compare scenarios
#'
#' Compares a list of scenarios as a data frame. The list `l` is a named list which is generated
#' by the Results tab, and includes a data frame of results for each tab.
#'
#' @param coin The coin
#' @param l List of scenarios to compare
#' @param base_scen Name of scenario to use as the base scenario (for rank diffs)
#' @param comp_with Either "Ranks" or "Scores"
#' @param tab_type Either "Values", "Differences" or "Absolute differences"
#'
#' @return A data frame
f_make_comparison_table <- function(coin, l, base_scen, comp_with, tab_type){

  # change order of list: put ibase first
  l <- l[c(base_scen, setdiff(names(l), base_scen))]

  n_scen <- length(l)
  names_scen <- names(l)

  icode_compare <- get_index_code(coin)


  # Merging -----------------------------------------------------------------

  # create base df
  df_merged <- l[[1]][c("uCode", icode_compare)]
  # add names
  df_merged <- base::merge(df_merged, coin$Meta$Unit[c("uCode", "uName")], by = "uCode")
  # reorder
  df_merged <- df_merged[c("uCode", "uName", icode_compare)]

  # merge other dfs
  if(n_scen > 1){
    for(ii in 2:n_scen){

      dfi <- l[[ii]][c("uCode", icode_compare)]
      df_merged <- base::merge(df_merged, dfi, by = "uCode")

    }
  }

  # rename cols
  names(df_merged) <- c("Code", "Name", names_scen)


  # Convert to ranks etc ----------------------------------------------------

  # convert to ranks if required
  if(comp_with == "Ranks"){
    df_merged <- COINr::rank_df(df_merged)
    df_merged <- df_merged[order(df_merged[[base_scen]], decreasing = FALSE), ]
  } else {
    saved_names <- names(df_merged)
    df_merged <- df_merged[order(df_merged[[base_scen]], decreasing = TRUE), ] |>
      COINr::round_df(2)
    names(df_merged) <- saved_names
  }

  # convert to diffs or abs diffs if required

  # col to sort on in case of diffs: the next along from base_col if it exists
  diff_order_col <- if(ncol(df_merged) > 3) 4 else 3

  if (tab_type != "Values"){
    # DIFFS: subtract each col from the first (numeric) col
    df_merged[names_scen] <- df_merged[names_scen] - data.frame(rep(df_merged[names_scen[1]], length(names_scen)) )
    df_merged <- df_merged[order(df_merged[[diff_order_col]], decreasing = TRUE), ]
  }

  if (tab_type == "Absolute differences"){
    df_merged[names_scen] <- abs(df_merged[names_scen])
    df_merged <- df_merged[order(df_merged[[diff_order_col]], decreasing = TRUE), ]
  }

  df_merged

}

f_style_comparison_table <- function(df_merged, comp_with){

  # find min and max of score ranges
  numeric_columns <- 3:ncol(df_merged)
  df_numeric <- as.matrix(df_merged[numeric_columns])
  min_all <- min(df_numeric, na.rm = TRUE)
  max_all <- max(df_numeric, na.rm = TRUE)

  # generate colours
  breaks <- seq(min_all, max_all, length.out = 12)[2:11]
  main_colours <- c("#FFFFFF", "#8EBEFF")
  # reverse for ranks
  if(comp_with == "Ranks") main_colours <- rev(main_colours)
  colour_func <- grDevices::colorRampPalette(main_colours)

  colour_palette <- colour_func(length(breaks) + 1)

  df_merged |>
    DT::datatable(
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 20, 50)
      ),
      rownames = FALSE,
      #selection = list(mode = "single", target = "column", selected = 3, selectable = c(-1, -2))
      selection = list(mode = "none")
    ) |>
    DT::formatStyle(
      numeric_columns,
      backgroundColor = DT::styleInterval(breaks, colour_palette)
    )
}



#' Simple comparison table for scatter plot
#'
#' Either merged table of scores or ranks. This is independent from the
#' `f_make_comparison_table()` function to allow some flexibility.
#'
#' @param coin The coin
#' @param l List of scenarios to compare
#' @param comp_with Either "Ranks" or "Scores"
#'
#' @return A data frame
f_df_for_scat <- function(coin, l, comp_with){

  n_scen <- length(l)
  names_scen <- names(l)

  icode_compare <- get_index_code(coin)

  # Merging -----------------------------------------------------------------

  # create base df
  df_merged <- l[[1]][c("uCode", icode_compare)]
  # add names
  df_merged <- base::merge(df_merged, coin$Meta$Unit[c("uCode", "uName")], by = "uCode")
  # reorder
  df_merged <- df_merged[c("uCode", "uName", icode_compare)]

  # merge other dfs
  if(n_scen > 1){
    for(ii in 2:n_scen){

      dfi <- l[[ii]][c("uCode", icode_compare)]
      df_merged <- base::merge(df_merged, dfi, by = "uCode")

    }
  }

  # rename cols
  names(df_merged) <- c("Code", "Name", names_scen)


  # Convert to ranks etc ----------------------------------------------------

  # convert to ranks if required
  if(comp_with == "Ranks"){
    df_merged <- COINr::rank_df(df_merged)
  } else {
    saved_names <- names(df_merged)
    df_merged <- COINr::round_df(df_merged, 2)
    names(df_merged) <- saved_names
  }

  df_merged

}
