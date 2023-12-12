welcome_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "welcome",
    tags$style("
        #splash_panel {
          background-color: #0072BC;
          opacity: 1;
          text-align: center;
          padding-top: 15%;
          padding-right: 20%;
          padding-left: 30%;
          color:white;
        }"),
    cicerone::use_cicerone(),
    absolutePanel(
      id = "splash_panel", top = 0, left = 0, right = 0, bottom = 0,
      #img(src='https://raw.githubusercontent.com/UNHCR-Guatemala/A2SIT/main/inst/app/www/logo.svg', height ='70vh', align = "center"),
      #br(),br(),
      p(
        tags$span("Admin2 Severity Index Tool", style = "font-size: 60px", id = "app_title"),
        tags$span("beta", style = "font-size: 24px")
      ),
      br(),
      p(paste("Welcome to the Admin2 Severity Index Tool (A2SIT).",
      "The A2SIT builds a composite indicator from your admin2 data to analyse",
      "severity in your country. Use the navigation menu on the left or click",
      "the options below to get started!"), style = "font-size: 20px"),
      br(),
      br(),

      fluidRow(
        column(3),
        column(
          3,
          actionButton(NS(id, "go_to_tour"), label = "Tour", width = "150px", style='font-size: 16px; color: #18375F', icon = icon("route")),
          style = "font-size: 18px; text-align: right;"
        ),
        column(6, "Take a guided tour of the app", style = "font-size: 18px; text-align: left;")
      ),
      br(),

      fluidRow(
        column(3),
        column(
          3,
          actionButton(NS(id, "go_to_datainput"), label = "Data input", width = "150px", style='font-size: 16px; color: #18375F', icon = icon("upload")),
          style = "font-size: 18px; text-align: right;"
        ),
        column(6, "Go straight to data input", style = "font-size: 18px; text-align: left;")
      ),
      br(),

      fluidRow(
        column(3),
        column(
          3,
          actionButton(NS(id, "go_to_doc"), label = "Know more", width = "150px",
                       style='font-size: 16px; color: #18375F', icon = icon("circle-info"),
                       onclick ="window.open('https://unhcr-guatemala.github.io/A2SIT/book/index.html', '_blank')"),
          style = "font-size: 18px; text-align: right;"
        ),
        column(6, "Read the A2SIT documentation", style = "font-size: 18px; text-align: left;")
      )

    )
  )

}

welcome_server <- function(id, parent_session, parent_input, r_shared) {

  moduleServer(id, function(input, output, session) {

    # auto-launch tour the first time a user visits this tab
    observeEvent(parent_input$tab_selected, {
      req(parent_input$tab_selected == "welcome")
      if(r_shared$new_to_welcome_tab){

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Welcome", html = TRUE,

          text = tags$div(
            "Welcome to the A2SIT app. If this is your first time here, please take the ",
            actionLink(NS(id, "launch_tour"), label = "quick tour.")
          ),
          btn_labels = "Close",
          type = "info"
        )
        # set so that tour not launched if return to this tab
        r_shared$new_to_welcome_tab <- FALSE

      }
    })

    # launch tour on button click (from welcome sweetalert)
    observeEvent(input$launch_tour, {
      shinyWidgets::closeSweetAlert()
      guide <- create_guide(id)
      guide$init()$start()
    })

    # launch tour on button click (from button in welcome window)
    observeEvent(input$go_to_tour, {
      guide <- create_guide(id)
      guide$init()$start()
    })

    # go to data input tab
    observeEvent(input$go_to_datainput, {
      shinydashboard::updateTabItems(
        session = parent_session,
        inputId = "tab_selected",
        selected = "upload"
      )
    })


  })

}
