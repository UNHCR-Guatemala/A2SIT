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
    #tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
    minified = TRUE, collapsed = FALSE, width = "30vw",
    shinydashboard::sidebarMenu(
      id = "tab_selected",
      shinydashboard::menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      shinydashboard::menuItem("Upload", tabName = "upload", icon = icon("upload")),
      shinydashboard::menuItem("Analyse", tabName = "analyse", icon = icon("magnifying-glass-chart")),
      shinydashboard::menuItem("Results", tabName = "results", icon = icon("square-poll-vertical")),
      shinydashboard::menuItem("Profiles", tabName = "profiles", icon = icon("location-dot"))
    )
  )


  # Dashboard body ----------------------------------------------------------

  db_body <- shinydashboard::dashboardBody(

    # add modals (longer help in md format)
    welcome_modal(),
    upload_modal(),
    analyse_modal(),
    results_modal(),
    profiles_modal(),

    # some themeing (to improve)
    includeCSS(system.file("app", "www", "custom.css", package = "A2SIT")),
    fresh::use_theme(theme_UNHCR),

    # increase width of dropdown menus
    tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:200px;
  }
  '))),

    shinydashboard::tabItems(
      welcome_UI("id_welcome"),
      input_UI("id_input"),
      analysis_UI("id_analysis"),
      results_UI("id_results"),
      profiles_UI("id_profiles")
    )
  )


  # Assemble ----------------------------------------------------------------

  # define UNHCR logo
  title_logo <- tags$div(tags$img(src="https://raw.githubusercontent.com/UNHCR-Guatemala/A2SIT/main/inst/app/www/logo.svg", height ='30vh'), "  A2SIT")
  title_beta <- tags$div(tags$span("A2SIT", style = "font-size: 30px"), tags$span("beta", style = "font-size: 12px"))

  shinydashboardPlus::dashboardPage(

    md = FALSE,
    options = list(sidebarExpandOnHover = TRUE),

    header = shinydashboardPlus::dashboardHeader(

      #header_help_icon("modal_help"),
      uiOutput("header_help", inline = TRUE),

      title = title_beta,
      titleWidth = "30vw",
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
          h5("Export to Excel"),
          downloadButton("export_button_excel", "Excel"),
          br(),
          h5("Export to R"),
          downloadButton("export_button_R", "R")
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
  addResourcePath('img', system.file('app/img', package = 'A2SIT') )

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
