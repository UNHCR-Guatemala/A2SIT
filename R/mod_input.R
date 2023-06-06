input_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "upload",
    tags$style(type='text/css', '#id_input-data_message {white-space: pre-wrap;}'),
    column(
      4,
      box(title = "Data upload",
          width = NULL,
          collapsible = TRUE,
          status = "primary",

          h4("1. Select your country"),
          country_dropdown(NS(id, "ISO3"), "Country") |>
            add_input_pop("If your country is not on this list please contact us."),
          hr(),

          h4("2. Download country template"),
          p("Download the template for the selected country:"),
          downloadButton(NS(id, "download_country_template"), "Download country template"),
          hr(),

          h4("3. Upload your data"),
          p("Upload your compiled template here and click the 'Load' button."),
          fileInput(NS(id, "xlsx_file"), NULL, buttonLabel = "Browse...", accept = c("xls", "xlsx")) |>
            add_input_pop("Please upload the template spreadsheet compiled with your data."),
          actionButton(NS(id, "load_click"), "Load"),
          hr(),

          p("An example data set is available to download ",
            tags$a(href="https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/A2SIT_data_input_template_GTM.xlsx",
                   "here."))
      ),

      box(title = box_pop_title("Messages", "Any messages from the data import process."), width = NULL, status = "info",
          verbatimTextOutput(NS(id, "data_message")))

    ),

    column(
      8,

      fluidRow(
        column(6, uiOutput(NS(id, "flag"))),
        column(3, shinydashboard::infoBoxOutput(NS(id, "n_indicators_box"), width = 12)),
        column(3, shinydashboard::infoBoxOutput(NS(id, "n_units_box"), width = 12))
      ),

      box(title = box_pop_title(
        "Index Framework",
        "The framework plot shows the structure of the index. The outer ring shows the indicators, and each succesive ring inwards shows the groupings in the levels above. The size of each chunk is the relative weighing of each component in the index.",
        placement = "bottom"),
        width = NULL, collapsible = FALSE, status = "success",
        plotly::plotlyOutput(NS(id, "framework"), height = "70vh"))
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

        c_name <- A2SIT::country_codes$CountryName[A2SIT::country_codes$ISO3 == input$ISO3]

        # render flag + country name
        output$flag <- renderUI({

          tagList(
            column(3, tags$img(
              src = A2SIT::country_codes$FlagLink[A2SIT::country_codes$ISO3 == input$ISO3],
              width = 100,
              height = 75
            )),
            column(9, h1(c_name))
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

    # plot framework (use UNHCR colours)
    output$framework <- plotly::renderPlotly({
      req(coin())
      iCOINr::iplot_framework(
        coin(),
        plotly_colorway = c("#18375F", "#0072BC", "#8EBEFF", "#00B398",  "#666666"))
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
