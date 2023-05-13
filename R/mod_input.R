input_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "upload",
    tags$style(type='text/css', '#id_input-data_message {white-space: pre-wrap;}'),
    column(
      3,
      box(title = "Data upload",
          width = NULL,
          collapsible = TRUE,
          status = "primary",

          "Select the country you want to analyse, then upload your Admin2 data.",
          "Use the link below to download a template for your selected country, or",
          tags$a(href="https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/data_module-input.xlsx",
                 "download example data set"),
          "for testing/demo.",
          br(),br(),

          country_dropdown(NS(id, "ISO3"), "Select country:"),

          downloadLink(NS(id, "download_country_template"), "Download country template"),

          br(),br(),
          fileInput(NS(id, "xlsx_file"), "Load Data", buttonLabel = "Browse...", accept = c("xls", "xlsx")),
          actionButton(NS(id, "load_click"), "Load")),

      box(title = "Messages", width = NULL, status = "info",
          verbatimTextOutput(NS(id, "data_message")))

    ),

    column(
      9,

      fluidRow(
        column(1, uiOutput(NS(id, "flag"))),
        column(5, h1(textOutput(NS(id, "country_name")))),
        column(3, shinydashboard::infoBoxOutput(NS(id, "n_indicators_box"), width = 12)),
        column(3, shinydashboard::infoBoxOutput(NS(id, "n_units_box"), width = 12))
      ),

      box(title = "Index Framework", width = NULL, collapsible = TRUE, status = "success",
          plotly::plotlyOutput(NS(id, "framework"), height = "80vh"))
    )
  )

}

input_server <- function(id, coin, coin_full, shared_reactives) {

  moduleServer(id, function(input, output, session) {

    # update shared reactive for other modules
    observeEvent(input$ISO3, {
      shared_reactives$ISO3 <- input$ISO3
    })

    # Download template
    output$download_country_template <- downloadHandler(
      filename = function() {
        paste0("A2SIT_data_input_template_", input$ISO3, ".xlsx")
      },
      content = function(file) {
        f_generate_input_template(input$ISO3, file)
      }
    )

    # load data
    observeEvent(input$load_click, {

      req(input$xlsx_file)

      data_message <- utils::capture.output({
        coin(f_data_input(input$xlsx_file$datapath, input$ISO3))
      }, type = "message")

      if(is.null(coin())){

        # not successful
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Problem with data input",
          text = "Please check the messages box for more info.",
          type = "warning"
        )

      } else {

        # copy of full coin for plotting later
        coin_full(coin())

        # render flag
        output$flag <- renderUI({
          tags$img(
            src = country_codes$FlagLink[country_codes$ISO3 == input$ISO3],
            width = 100,
            height = 75
          )
        })

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Data uploaded",
          text = "Please check the messages box for any messages.",
          type = "success"
        )

      }

      # Outputs
      output$data_message <- renderText(data_message, sep = "\n")

    })

    # coin print output
    output$coin_print <- renderPrint({
      req(coin())
      f_print_coin(coin())
    })

    # country title
    output$country_name <- renderText({
      req(coin())
      c_name <- country_codes$CountryName[country_codes$ISO3 == input$ISO3]
      paste0(" ", c_name)
    })

    # plot framework
    output$framework <- plotly::renderPlotly({
      req(coin())
      iCOINr::iplot_framework(coin())
    })

    # value boxes
    output$n_indicators_box <- shinydashboard::renderInfoBox({
      req(coin())
      shinydashboard::infoBox(
        title = "Indicators",
        value = get_n_indicators(coin()),
        icon = icon("list"),
        color = "blue"
      )
    })
    # value boxes
    output$n_units_box <- shinydashboard::renderInfoBox({
      req(coin())
      shinydashboard::infoBox(
        title = "Regions",
        value = get_n_units(coin()),
        icon = icon("location-dot"),
        color = "green"
      )
    })


  })

}
