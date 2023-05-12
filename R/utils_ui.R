# slider for weights
weights_slider <- function(id, label = NULL) {
  if(is.null(label)) label <- id
  sliderInput(id, label = label, min = 0, max = 1, value = 1, step = 0.1, ticks = FALSE)
}

# generates a dropdown menu of countries where we have available admin2 geometry
# stored in inst/geom. Adds flags.
country_dropdown <- function(id, label){

  # country_codes already has codes plus flag URLs for all countries, we just
  # filter to what we have available
  df_countries <- country_codes[country_codes$ISO3 %in% get_cached_countries(),]

  l_countries <- as.list(df_countries$ISO3)
  names(l_countries) <- df_countries$CountryName

  shinyWidgets::pickerInput(
    inputId = id,
    label = label,
    choices = l_countries,
    choicesOpt = list(
      content =
        mapply(df_countries$CountryName, df_countries$FlagLink, FUN = function(country, flagUrl) {
          HTML(paste(
            tags$img(src=flagUrl, width=20, height=15),
            country
          ))
        }, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    ))

}
