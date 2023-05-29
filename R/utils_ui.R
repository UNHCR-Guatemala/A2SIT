# slider for weights
weights_slider <- function(id, label = NULL, value = 1) {
  if(is.null(label)) label <- id
  sliderInput(id, label = label, min = 0, max = 1, value = value, step = NULL, ticks = FALSE)
}

# generates a dropdown menu of countries where we have available admin2 geometry
# stored in inst/geom. Adds flags.
country_dropdown <- function(id, label){

  # country_codes already has codes plus flag URLs for all countries, we just
  # filter to what we have available
  df_countries <- A2SIT::country_codes[A2SIT::country_codes$ISO3 %in% get_cached_countries(),]

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

# shortcut for adding tooltip to a shiny input
add_input_pop <- function(tag, popover_text, placement = "top"){

  tag |>
    bsplus::shinyInput_label_embed(
      icon("info-circle", class = "input-pop",
           style = "color: #66c2ff;") |>
        bsplus::bs_embed_tooltip(title = popover_text, placement = placement)
    )

}

# shortcut for adding an info icon to a box title
box_pop_title <- function(title, popover_text, placement = "top", px_from_right = 10){

  p(title,
    icon("info-circle", class = "box-pop",
         style = paste0("position: absolute; right: ", px_from_right, "px; color: #66c2ff;")) |>
      bsplus::bs_embed_tooltip(title = popover_text, placement = placement))

}

# TO FINISH
add_modal <- function(tag, message_to_add){
  bsplus::shiny_iconlink() |>
    bsplus::bs_attach_modal(id_modal = "modal_equation")
}


# function that creates a help pop up window for each page
header_help_icon <- function(id_modal){

  tags$li(class = "dropdown",
          htmltools::tags$a(shiny::icon("info-circle", id = "header_help_icon"),
                            style = "color: #FFFFFF; position: absolute; right: 40px; top: 5px; font-size: 30px;",
                            href = "#")
  ) |>
    bsplus::bs_attach_modal(id_modal = id_modal)

}


# Modal help --------------------------------------------------------------

welcome_modal <- function(){
  bsplus::bs_modal(
    id = "welcome_modal",
    title = "Welcome",
    body = includeMarkdown(system.file("md-help", "welcome.md", package = "A2SIT")),
    size = "medium"
  )
}

upload_modal <- function(){
  bsplus::bs_modal(
    id = "upload_modal",
    title = "Data input",
    body = includeMarkdown(system.file("md-help", "input.md", package = "A2SIT")),
    size = "medium"
  )
}

analyse_modal <- function(){
  bsplus::bs_modal(
    id = "analyse_modal",
    title = "Indicator analysis",
    body = includeMarkdown(system.file("md-help", "analysis.md", package = "A2SIT")),
    size = "medium"
  )
}

results_modal <- function(){
  bsplus::bs_modal(
    id = "results_modal",
    title = "Results explorer",
    body = includeMarkdown(system.file("md-help", "results.md", package = "A2SIT")),
    size = "medium"
  )
}

profiles_modal <- function(){
  bsplus::bs_modal(
    id = "profiles_modal",
    title = "Admin2 Profiles",
    body = includeMarkdown(system.file("md-help", "profiles.md", package = "A2SIT")),
    size = "medium"
  )
}
