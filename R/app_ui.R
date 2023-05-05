#' Assemble UI modules
#'
#' @return Front end of app
#'
#' @importFrom shinydashboardPlus box dashboardPage
#' @importFrom shinydashboard menuItem menuSubItem tabItem
#'
#'
#' @export
app_ui <- function() {


  # enable alert messages
  shinyWidgets::useSweetAlert()


  # Sidebar -----------------------------------------------------------------

  db_sidebar <- shinydashboardPlus::dashboardSidebar(
    minified = FALSE, collapsed = FALSE, width = "15vw",
    shinydashboard::sidebarMenu(
      id = "tab_selected",
      shinydashboard::menuItem("Upload", tabName = "upload", icon = icon("upload")),
      shinydashboard::menuItem("Analyse", tabName = "analyse", icon = icon("magnifying-glass-chart")),
      shinydashboard::menuItem("Results", tabName = "results", icon = icon("square-poll-vertical")),
      shinydashboard::menuItem("Profiles", tabName = "profiles", icon = icon("location-dot"))
    )
  )


  # Dashboard body ----------------------------------------------------------

  db_body <- shinydashboard::dashboardBody(

    theme_dashboard(),
    shinydashboard::tabItems(
      input_UI("id_input"),
      analysis_UI("id_analysis"),
      results_UI("id_results"),
      profiles_UI("id_profiles")
    )
  )


  # Assemble ----------------------------------------------------------------

  # define UNHCR logo
  title_logo <- tags$div(tags$img(src="https://raw.githubusercontent.com/UNHCR-Guatemala/A2SIT/main/inst/app/www/logo.svg", height ='30vh'), "  A2SIT")

  shinydashboardPlus::dashboardPage(
    md = FALSE,
    #skin = "blue",
    options = list(sidebarExpandOnHover = TRUE),
    header = shinydashboardPlus::dashboardHeader(
      title = title_logo,
      titleWidth = "15vw",
      controlbarIcon = icon("gears"),
      leftUi = tagList(
        shinydashboardPlus::dropdownBlock(
          id = "save_session",
          title = "Save session",
          icon = icon("floppy-disk"), badgeStatus = NULL,
          "To add"
        ),
        shinydashboardPlus::dropdownBlock(
          id = "load_session",
          title = "Load session",
          icon = icon("folder-open"), badgeStatus = NULL,
          "To add"
        ),
        shinydashboardPlus::dropdownBlock(
          id = "export_to_excel",
          title = "Export",
          icon = icon("file-export"), badgeStatus = NULL,
          "To add"
        )
      )
    ),
    footer = shinydashboardPlus::dashboardFooter(right = "2023 UNHCR"),
    sidebar = db_sidebar,
    body = db_body,
    controlbar = shinydashboardPlus::dashboardControlbar(disable = TRUE),
    title = "Admin2 Severity Index Tool"
  )

}


#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = "A2SIT")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("A2SIT")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")

  )
}
