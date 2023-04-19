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

  df_results |>
    DT::datatable() |>
    DT::formatStyle(columns = ncol(df_results),
                    fontSize = '50%')
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
