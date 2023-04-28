# Results back end functions

# this function builds the MVI. Assumes that at this point you have imported
# your data and built the MVI coin. Also optionally you have analysed and
# possibly removed indicators, but taken no further steps.
#
f_build_index <- function(coin, agg_method = "a_amean", only_aggregate = FALSE){

  stopifnot(is.coin(coin))

  # Settings ----

  max_winsorisation <- 5
  skew_thresh <- 2
  kurt_thresh <- 3.5

  # skip this if only_aggregate
  if(!only_aggregate){

    # treat outliers
    coin <- COINr::qTreat(
      coin, dset = "Raw",
      winmax = max_winsorisation,
      skew_thresh = skew_thresh,
      kurt_thresh = kurt_thresh,
      f2 = "log_CT_plus")

    # normalise to [1, 100]: otherwise if we have zeros can't use geometric mean
    coin <- COINr::Normalise(
      coin,
      dset = "Treated",
      global_specs = list(f_n = "n_minmax",
                          f_n_para = list(l_u = c(1, 100)))
    )

  }

  # aggregate using weighted arithmetic mean
  coin <- COINr::Aggregate(coin, dset = "Normalised", f_ag = agg_method)

  # generate results tables
  coin <- f_generate_results(coin)

  coin
}

# Outputs an interactive results table suitable for HTML documents and the app.
# set type = "scores" or "ranks".
#
f_display_results_table <- function(coin, type = "scores"){

  if(type == "scores"){
    df_results <- coin$Results$FullScore
  } else if (type == "ranks"){
    df_results <- coin$Results$FullRank
  }

  if(is.null(df_results)){
    abort("Can't find results in the coin. Did you forget to build the index first?")
  }

  # find min and max of score ranges ----
  all_columns <- names(df_results)
  factor_columns <-  c("uCode", "uName", "Rank")
  numeric_columns <- setdiff(all_columns, factor_columns)
  df_numeric <- as.matrix(df_results[numeric_columns])
  min_all <- min(df_numeric, na.rm = TRUE)
  max_all <- max(df_numeric, na.rm = TRUE)

  # generate colours ----
  breaks <- seq(min_all, max_all, length.out = 12)[2:11]
  colour_func <- grDevices::colorRampPalette(c("white", "aquamarine3"))
  colour_palette <- colour_func(length(breaks) + 1)

  # Create table
  df_results |>
    DT::datatable(
      options = list(scrollX = TRUE),
      rownames = FALSE
    ) |>
    DT::formatStyle(
      numeric_columns,
      backgroundColor = DT::styleInterval(breaks, colour_palette)
    )

}


# NOTE to plot bar charts just use the dedicated COINr function.



# Plots an interactive choropleth map of the index or any indicator, using supplied shape
# files.
#
# shp_path is currently at "shp/gtm_admbnda_adm2_ocha_conred_20190207.shp"
#
#
f_plot_map <- function(coin, dset = "Aggregated", iCode = "MVI", shp_path){

  shp <- sf::read_sf(shp_path)

  # get data first
  df_plot <- COINr::get_data(coin, dset = dset, iCodes = iCode)

  # merge into shape df
  shp$Indicator <- df_plot[[iCode]][match(shp$ADM2_PCODE, df_plot$uCode)]

  # colorBin is a leaflet function
  pal <- leaflet::colorBin("YlOrRd", domain = shp$Indicator, bins = 7)

  # labels
  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    shp$ADM2_ES, round(shp$Indicator, 1)
  ) |>
    lapply(htmltools::HTML)


  # now we can make the map

  mp <- leaflet::leaflet(shp) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(layerId = ~ADM2_PCODE,
                         fillColor = ~pal(Indicator),
                         weight = 2,
                         opacity = 1,
                         color = "white",
                         dashArray = "3",
                         fillOpacity = 0.7,
                         highlightOptions = leaflet::highlightOptions(
                           weight = 5,
                           color = "#666",
                           dashArray = "",
                           fillOpacity = 0.7,
                           bringToFront = TRUE),
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto")) |>
    leaflet::addLegend(pal = pal, values = ~Indicator, opacity = 0.7, title = NULL,
                       position = "bottomright")

  mp

}

# generates sorted results tables and attaches back to the coin
#' Add results table to coin
#'
#' @param coin Coin
#'
#' @importFrom COINr is.coin
#'
#' @noRd
f_generate_results <- function(coin){

  stopifnot(is.coin(coin),
            !is.null(coin$Data$Aggregated))

  # generate results tables (attached to coin, so will appear when exported to Excel)
  coin <- COINr::get_results(coin, dset = "Aggregated", tab_type = "Full",
                             also_get = "uName", nround = 1, out2 = "coin")
  coin <- COINr::get_results(coin, dset = "Aggregated", tab_type = "Full", use = "ranks",
                             also_get = "uName", nround = 1, out2 = "coin")

  coin

}


# Function that takes some new weights and regenerates the coin.
#
# The weights `w` can either be a named list with names as iCodes and values
# the new weights, OR as a data frame with columns "iCode" and "Weight" with
# codes and corresponding weights.
# In both cases a subset of weight-code pairs can be specified.
# E.g. `list("Salud" = 0.5, Amenazas = 0.8)`.
#
# OR set w = "equal" for equal weights everywhere, or w = "original" to use the
# weights that were input with the input data.
#
# Remember that weights are relative within aggregation groups.
#
# Outputs a coin.
#
f_change_weights <- function(coin, w){

  stopifnot(is.coin(coin))

  if(is.null(coin$Data$Aggregated)){
    abort("Can't find results in the coin. Did you forget to build the index first?")
  }

  # Get weights that were last used to aggregate ----

  # special case: reset or make equal
  if(is.character(w)){

    stopifnot(length(w) == 1)

    if(w == "equal"){
      w_new <- f_get_equal_weights(coin)
    } else if (w == "original"){
      w_new <- coin$Meta$Weights$Original
    }

    coin$Log$Aggregate$w <- w_new
    return(Regen(coin, from = "Aggregate"))
  }

  w_new <- f_get_last_weights(coin)

  # Alter weights based on input type ----

  if(is.data.frame(w)){

    stopifnot(all(c("iCode", "Weight") %in% names(w)),
              all(w$iCode %in% w_new$iCode),
              is.numeric(w$Weight))

    # subst new weights in
    w_new$Weight[match(w$iCode, w_new$iCode)] <- w$Weight

  } else if (is.list(w)){

    stopifnot(all(names(w) %in% w_new$iCode),
              all(sapply(w, is.numeric)),
              all(lengths(w) == 1))

    # subst new weights in
    w_new$Weight[match(names(w), w_new$iCode)] <- as.numeric(w)

  }

  # Regen with new weights ----

  coin$Log$Aggregate$w <- w_new

  # extract analysis
  ind_analysis <- coin$Analysis$Raw
  analysis_exists <- !is.null(ind_analysis)

  coin <- COINr::Regen(coin, from = "Aggregate")

  if(analysis_exists){
    coin$Analysis$Raw <- ind_analysis
  }

  # generate results tables again
  coin <- f_generate_results(coin)

  coin

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
    abort("Weights not recognised at coin$Log$Aggregate$w")
  }

  stopifnot(is.data.frame(w_new))

  w_new

}
