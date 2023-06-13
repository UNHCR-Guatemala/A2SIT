# Results back end functions

f_get_scenarios <- function(coin){

  current_agg_method <- coin$Log$Aggregate$f_ag
  agg_methods <- c("a_amean", "a_gmean")
  stopifnot(!is.null(current_agg_method),
            current_agg_method %in% agg_methods)

  # get current scenario (should be arithmetic mean)
  l <- list(
    #coin$Data$Aggregated
    COINr::get_results(coin, dset = "Aggregated", tab_type = "Full",
                               also_get = "uName", nround = 2, out2 = "df")
  )
  names(l) <- get_aggregation_name(coin$Log$Aggregate$f_ag)

  # run remaining scenarios
  agg_methods <- setdiff(agg_methods, current_agg_method)

  for(agg_method in agg_methods){
    coin_new <- f_build_index(coin, agg_method = agg_method, only_aggregate = TRUE) |>
      suppressMessages()
    agg_name <- get_aggregation_name(agg_method)
    l[[agg_name]] <- COINr::get_results(coin_new, dset = "Aggregated", tab_type = "Full",
                                        also_get = "uName", nround = 2, out2 = "df")
  }

  l

}

# translates from aggregation function name to display name
get_aggregation_name <- function(f_ag){
  switch(f_ag,
         "a_amean" = "Arithmetic mean",
         "a_gmean" = "Geometric mean",
         stop("Aggregation method type not recognised")
  )
}

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

  # add severity level df
  coin <- f_make_severity_level_dset(coin)

  coin
}

#
f_make_severity_level_dset <- function(coin){

  iData <- COINr::get_dset(coin, "Aggregated")
  agg_codes <- get_indicator_codes(coin, "Aggregate", with_levels = FALSE,
                                   use_names = FALSE)

  agg_cols <- lapply(agg_codes, function(iCode){
    direction <- get_indicator_direction(coin, iCode)
    to_discrete_scale(iData[[iCode]], direction)
  }) |> as.data.frame()
  names(agg_cols) <- agg_codes

  iData[agg_codes] <- agg_cols

  coin$Data$Severity <- iData

  df_results <- COINr::get_results(coin, "Severity", tab_type = "Full",
                                   also_get = "uName", nround = 1, out2 = "df")
  df_results <- df_results[names(df_results) != "Rank"]

  coin$Results$Severity <- df_results

  coin

}

# Outputs an interactive results table suitable for HTML documents and the app.
# set type = "scores" or "ranks".
#
f_display_results_table <- function(coin, type = "scores", as_discrete = FALSE){

  if(as_discrete){
    df_results <- coin$Results$Severity
  } else {
    if(type == "scores"){
      df_results <- coin$Results$FullScore
    } else if (type == "ranks"){
      df_results <- coin$Results$FullRank
    }
  }

  if(is.null(df_results)){
    stop("Can't find results in the coin. Did you forget to build the index first?", call. = FALSE)
  }

  # find min and max of score ranges ----
  if(!as_discrete){
    all_columns <- names(df_results)
    factor_columns <-  c("uCode", "uName", "Rank")
    numeric_columns <- setdiff(all_columns, factor_columns)
    df_numeric <- as.matrix(df_results[numeric_columns])
    min_all <- min(df_numeric, na.rm = TRUE)
    max_all <- max(df_numeric, na.rm = TRUE)
  } else {
    numeric_columns <- get_indicator_codes(coin, "Aggregate", with_levels = FALSE,
                                           use_names = FALSE)
    min_all <- 1
    max_all <- 5
  }

  # generate colours ----
  breaks <- seq(min_all, max_all, length.out = 12)[2:11]
  colour_func <- grDevices::colorRampPalette(c("#DCE9FF", "#0072BC"))
  colour_palette <- colour_func(length(breaks) + 1)

  # Create table
  df_results |>
    DT::datatable(
      options = list(scrollX = TRUE),
      rownames = FALSE,
      selection = "single",
    ) |>
    DT::formatStyle(
      numeric_columns,
      backgroundColor = DT::styleInterval(breaks, colour_palette)
    )

}





# Plots an interactive choropleth map of the index or any indicator, using supplied shape
# files.
#
# shp_path is currently at "shp/gtm_admbnda_adm2_ocha_conred_20190207.shp"
#
#
f_plot_map <- function(coin, iCode, ISO3, as_discrete = TRUE){

  available_ISO3s <- get_cached_countries()

  if(ISO3 %nin% available_ISO3s){
    stop("Cannot render map because geometry for country ", ISO3,
         " not available in inst/geom. Run f_get_admin2_boundaries() to get",
         " and store the required file.")
  }

  # get geom
  admin2_geom <- system.file("geom", paste0(ISO3,".RDS"), package = "A2SIT") |>
    readRDS()

  # find dset
  dset_plot <- get_plot_dset(coin, iCode)

  # get data first
  df_plot <- COINr::get_data(coin, dset = dset_plot, iCodes = iCode)
  iValues <- df_plot[[2]]

  # add rank
  df_plot$Rank <- rank(-1*df_plot[[iCode]], na.last = "keep", ties.method = "min")

  if(as_discrete){
    icode_direction <- get_indicator_direction(coin, iCode)
    df_plot[["Indicator"]] <- to_discrete_scale(iValues, icode_direction)
  } else {
    names(df_plot)[2] <- "Indicator" # just for convenience later
  }

  # merge into shape df
  admin2_geom <- base::merge(admin2_geom, df_plot, by.x = "adm2_source_code", by.y = "uCode")

  # Colours and labels ------------------------------------------------------

  # colorBin is a leaflet function
  palette <- c("#FFE7E8", "#B41C37")

  pal <- if(as_discrete){
    leaflet::colorFactor(palette, levels = 1:5)
  } else {
    leaflet::colorNumeric(palette, domain = iValues)
  }

  labels <- paste0(
    "<strong>", admin2_geom$gis_name, "</strong><br/>",
    round(admin2_geom$Indicator, 1), " (rank ", admin2_geom$Rank,")"
  ) |>
    lapply(htmltools::HTML)


  # Plot --------------------------------------------------------------------

  mp <- leaflet::leaflet(admin2_geom) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(layerId = ~adm2_source_code,
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


#' Regenerate results
#'
#' Regenerates results based on specified weights and aggregation method.
#'
#' @param coin The coin
#' @param w can either be a named list with names as iCodes and values
#' as the new weights, OR as a data frame with columns "iCode" and "Weight" with
#' codes and corresponding weights.
#' @param agg_method One of `c("a_amean", "a_gmean")` currently supported.
#'
#' @return Updated coin
#' @export
f_rebuild_index <- function(coin, w = NULL, agg_method){

  stopifnot(is.coin(coin),
            agg_method %in% c("a_amean", "a_gmean"))

  if(is.null(coin$Data$Aggregated)){
    stop("Can't find results in the coin. Did you forget to build the index first?", call. = FALSE)
  }

  # Get weights that were last used to aggregate ----

  if (is.null(w)){

    # use same weights
    w_new <- f_get_last_weights(coin)

  } else {

    # special case: reset or make equal
    if(is.character(w)){

      stopifnot(length(w) == 1)

      if(w == "equal"){
        w_new <- f_get_equal_weights(coin)
      } else if (w == "original"){
        w_new <- coin$Meta$Weights$Original
      }

      coin$Log$Aggregate$w <- w_new
      COINr::Regen(coin, from = "Aggregate")
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

  }

  # Regen with new weights + agg method ----

  coin$Log$Aggregate$w <- w_new
  coin$Log$Aggregate$f_ag <- agg_method

  # extract analysis
  ind_analysis <- coin$Analysis$Raw
  analysis_exists <- !is.null(ind_analysis)

  coin <- COINr::Regen(coin, from = "Aggregate")

  if(analysis_exists){
    coin$Analysis$Raw <- ind_analysis
  }

  # generate results tables again
  coin <- f_generate_results(coin)

  # add severity level results
  coin <- f_make_severity_level_dset(coin)


  coin

}

#' Get admin2 polygons from API
#'
#' Queries the gis.unhcr.org sever to return admin2 shape files for a specific
#' country. Some of the files returned are heavy, and the server also seems to
#' fail or time out every now and then. Also, some admin2 codes are not in the
#' expected format.
#'
#' @param ISO3 ISO3 code of country
#' @param simplify Logical: whether to simplify or not
#' @param dTolerance parameter passed to [sf::st_simplify()]
#'
#' @return sf object
#' @export
f_get_admin2_boundaries <- function(ISO3, simplify = TRUE, dTolerance = 500){

  stopifnot(ISO3 %in% A2SIT::country_codes$ISO3)

  # generate query string
  # from: https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_adm2_a_unhcr/MapServer/0/query
  api_query <- paste0(
    "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_adm2_a_unhcr/",
    "MapServer/0/query?where=ISO3+%3D+%27", ISO3,
    "%27&text=&objectIds=&time=&geometry=",
    "&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects",
    "&distance=&units=esriSRUnit_Foot&relationParam=",
    "&outFields=pcode%2C+adm2_source_code%2C+gis_name&returnGeometry=true&",
    "returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&",
    "havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&",
    "groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&",
    "gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=",
    "&resultRecordCount=&returnExtentOnly=false&datumTransformation=&",
    "parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")

  # read and create feature table
  df_geom <- sf::st_read(api_query)

  if(any(is.na(df_geom$adm2_source_code))){
    warning("NAs found in Admin2 codes...")
  }

  if(simplify){
    sf::st_simplify(df_geom, preserveTopology = TRUE, dTolerance = dTolerance)
  } else {
    df_geom
  }



}

#' Retrieve and store geometry for countries
#'
#' Queries the UNHCR API to get Admin2 layer for the specified countries, then
#' simplifies the geometry and stores in inst/geom. To enable quick retrieval
#' of maps. This is intended to just be run occasionally.
#'
#' @param ISO3s Character vector of ISO3s to get maps for
#' @param overwrite if TRUE overwrites any existing files
#'
#' @export
cache_admin2_geometry <- function(ISO3s, overwrite = FALSE){

  existing_ISO3s <- get_cached_countries()

  for(ISO3 in ISO3s){

    if(!overwrite && (ISO3 %in% existing_ISO3s)){
      message(ISO3, " already cached - skipping this (set overwrite = TRUE to change overwrite next time if needed)")
      next
    }

    message("Fetching geometry for ", ISO3, "........")

    tryCatch(
      expr = {
        geom_simplified <- f_get_admin2_boundaries(ISO3)
        if(nrow(geom_simplified) == 0){
          warning("No rows returned for ", ISO3, " - can't save anything here...")
        } else {
          saveRDS(geom_simplified, file = paste0("./inst/geom/", ISO3, ".RDS"))
        }
      },
      error = function(e){
        message("Cannot get geometry for some reason for: ", ISO3)
        print(e)
      }
    )

  }

}

#' Get countries with cached maps
#'
#' Returns ISO3 codes of all countries for which simplified maps have been
#' stored in inst/geom. Countries not in this list will have to have their
#' maps downloaded from the server.
#'
#' @return Character vector of ISO3 codes
#' @export
get_cached_countries <- function(){

  ISO3s <- list.files(system.file("geom", package = "A2SIT")) |>
    substring(1,3)

  if(!all(ISO3s %in% A2SIT::country_codes$ISO3)){
    stop("One or more files in inst/geom with unrecognised ISO3 code?")
  }

  ISO3s
}

# Codes cached so far (or tried to...!)
#ISO3s_2get <- c("ARG", "BLZ", "BRA", "BOL", "CHL", "COL", "CRI", "DOM", "ECU", "SLV", "GTM", "GUY", "HND", "MEX", "PAN", "PRY", "PER", "URY", "VEN")
#Argentina,  Belize, Brazil, Bolivia, Chile, Colombia, Costa Rica, Dominican Republic, Ecuador, El Salvador, Guatemala, Guyana, Honduras, Mexico, Panama, Paraguay, Peru, Uruguay, Venezuela.
