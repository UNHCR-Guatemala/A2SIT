#' Assemble UI modules
#'
#' This is called by `run_app()`.
#'
#' @return Front end of app
#'
#' @importFrom shinydashboardPlus box dashboardPage
#' @importFrom shinydashboard menuItem menuSubItem tabItem
#' @importFrom unhcrshiny theme_shinydashboard_unhcr
#'
#'
#' @export
app_ui <- function() {


  # enable alert messages
  shinyWidgets::useSweetAlert()

  # Sidebar -----------------------------------------------------------------

  db_sidebar <- shinydashboardPlus::dashboardSidebar(
    id = "db_sidebar",
    #tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
    minified = TRUE, collapsed = FALSE, width = "17%",
    shinydashboard::sidebarMenu(
      id = "tab_selected",
      shinydashboard::menuItem(span("Welcome", id = "welcome_sb_link"), tabName = "welcome", icon = icon("house")),
      shinydashboard::menuItem(span("Upload", id = "upload_sb_link"), tabName = "upload", icon = icon("upload")),
      shinydashboard::menuItem(span("Analyse", id = "analyse_sb_link"), tabName = "analyse", icon = icon("magnifying-glass-chart")),
      shinydashboard::menuItem(span("Results", id = "results_sb_link"), tabName = "results", icon = icon("square-poll-vertical")),
      shinydashboard::menuItem(span("Profiles", id = "profiles_sb_link"), tabName = "profiles", icon = icon("location-dot")),
      shinydashboard::menuItem(span("Compare scenarios", id = "scenarios_sb_link"), tabName = "scenarios", icon = icon("circle-half-stroke")),
      shinydashboard::menuItem(span("Compare regions", id = "compare_sb_link"), tabName = "compare_units", icon = icon("code-compare"))
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
    scenarios_modal(),
    compare_units_modal(),

    # some themeing (to improve)
    #includeCSS(system.file("app", "www", "custom.css", package = "A2SIT")),
    # fresh::use_theme(theme_UNHCR),
    unhcrshiny::theme_shinydashboard_unhcr(),

    # increase width of dropdown menus
    tags$head(tags$style(HTML('
        .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
        width:200px;
      }
    }'
    )),
    tags$script(HTML("
      var screenWidth = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
      console.log(screenWidth)
      if (screenWidth <= 768) {
      document.body.classList.add('sidebar-collapse');
      }"))
    ),

    shinydashboard::tabItems(
      welcome_UI("id_welcome"),
      input_UI("id_input"),
      analysis_UI("id_analysis"),
      results_UI("id_results"),
      profiles_UI("id_profiles"),
      compare_units_UI("id_compare_units"),
      scenarios_UI("id_scenarios")
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
      titleWidth = "17%",
      controlbarIcon = icon("gears"),
      leftUi = tagList(
        shinydashboardPlus::dropdownBlock(
          id = "save_session",
          title = "Save progress",
          icon = icon("floppy-disk"), badgeStatus = NULL,
          bookmarkButton()
        ),

        # Old position of export to Excel/R
        # shinydashboardPlus::dropdownBlock(
        #   id = "export_to_excel",
        #   title = "Export",
        #   icon = icon("file-export"), badgeStatus = NULL,
        #   "stuff"
        #   # h5("Export to Excel"),
        #   # downloadButton("export_button_excel", "Excel"),
        #   # br(),
        #   # h5("Export to R"),
        #   # downloadButton("export_button_R", "R")
        # )
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
