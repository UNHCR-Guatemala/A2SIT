
#' Weights slider
#'
#' Thin wrapper for sliderinput.
#'
#' @param id ID for slider
#' @param label Label
#' @param value initial value
#'
#' @return HTML code for slider
#' @export
#'
weights_slider <- function(id, label = NULL, value = 1) {
  if(is.null(label)) label <- id
  sliderInput(id, label = label, min = 0, max = 1, value = value, step = NULL, ticks = FALSE)
}

#' Generate country dropdown menu
#'
#' Generates a dropdown menu of countries where we have available admin2 geometry
#' stored in inst/geom. Adds flags.
#'
#' @param id ID to assign
#' @param label Label
#'
#' @return HTML
#' @export
#'
country_dropdown <- function(id, label){

  # A2SIT::country_codes already has codes plus flag URLs for all countries, we just
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

#' Shortcut for adding tooltip to a shiny input
#'
#' This can be piped onto the end of a Shiny input. See examples in UI functions.
#'
#' @param tag The object to add the tooltip to
#' @param popover_text Text to display
#' @param placement Where to display
#'
#' @return Input, now with popover
#' @export
#'
add_input_pop <- function(tag, popover_text, placement = "top"){

  tag |>
    bsplus::shinyInput_label_embed(
      icon("info-circle", class = "input-pop",
           style = "color: #66c2ff;") |>
        bsplus::bs_embed_tooltip(title = popover_text, placement = placement)
    )

}


#' Add help icon to box title
#'
#' Shortcut for adding an info icon to a box title. This is used in the "title"
#' argument of a Shinydashboard box.
#'
#' @param title The box title text
#' @param popover_text Text to display in the popover
#' @param placement Where to show the popover text
#' @param px_from_right Position from the right side of the box. This will need to
#' be adjusted depending on the other content in the box, e.g. if there is a collapsible
#' icon, sidebar icon, etc. See examples in app.
#'
#' @return HTML to be inserted in box title
#' @export
#'
box_pop_title <- function(title, popover_text, placement = "top", px_from_right = 10){

  p(title,
    icon("info-circle", class = "box-pop",
         style = paste0("position: absolute; right: ", px_from_right, "px; color: #66c2ff;")) |>
      bsplus::bs_embed_tooltip(title = popover_text, placement = placement))

}

# # TO FINISH
# add_modal <- function(tag, message_to_add){
#   bsplus::shiny_iconlink() |>
#     bsplus::bs_attach_modal(id_modal = "modal_equation")
# }


#' Help pop up window
#'
#' Function that creates a help pop up window for each page. This function
#' generates the icon, the thing that is displayed is controlled by `id_modal`.
#' This is called in the app server function.
#'
#' @param id_modal ID of modal to display
#'
#' @return HTML
#' @export
#'
header_help_icon <- function(id_modal){

  tags$li(class = "dropdown",
          htmltools::tags$a(shiny::icon("info-circle", id = "header_help_icon"),
                            style = "color: #FFFFFF; position: absolute; right: 40px; top: 5px; font-size: 30px;",
                            href = "#")
  ) |>
    bsplus::bs_attach_modal(id_modal = id_modal)

}


# Modal help --------------------------------------------------------------
# Each function here is used to to display a single modal help page, which are
# stored in ./inst/md-help. These are called in the main server function.

welcome_modal <- function(){
  bsplus::bs_modal(
    id = "welcome_modal",
    title = "Welcome",
    body = includeMarkdown(system.file("md-help", "welcome.md", package = "A2SIT")),
    size = "large"
  )
}

upload_modal <- function(){
  bsplus::bs_modal(
    id = "upload_modal",
    title = "Data input",
    body = includeMarkdown(system.file("md-help", "input.md", package = "A2SIT")),
    size = "large"
  )
}

analyse_modal <- function(){
  bsplus::bs_modal(
    id = "analyse_modal",
    title = "Indicator analysis",
    body = includeMarkdown(system.file("md-help", "analysis.md", package = "A2SIT")),
    size = "large"
  )
}

results_modal <- function(){
  bsplus::bs_modal(
    id = "results_modal",
    title = "Results explorer",
    body = includeMarkdown(system.file("md-help", "results.md", package = "A2SIT")),
    size = "large"
  )
}

profiles_modal <- function(){
  bsplus::bs_modal(
    id = "profiles_modal",
    title = "Admin2 Profiles",
    body = includeMarkdown(system.file("md-help", "profiles.md", package = "A2SIT")),
    size = "large"
  )
}

scenarios_modal <- function(){
  bsplus::bs_modal(
    id = "scenarios_modal",
    title = "Compare scenarios",
    body = includeMarkdown(system.file("md-help", "scenarios.md", package = "A2SIT")),
    size = "large"
  )
}

compare_units_modal <- function(){
  bsplus::bs_modal(
    id = "compare_units_modal",
    title = "Compare units",
    body = includeMarkdown(system.file("md-help", "compare_units.md", package = "A2SIT")),
    size = "large"
  )
}
