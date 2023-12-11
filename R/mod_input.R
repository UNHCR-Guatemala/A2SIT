input_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "upload",
    tags$style(type='text/css', '#id_input-data_message {white-space: pre-wrap;}'),
    column(
      4,
      box(title = "Data upload",
          width = NULL,
          collapsible = TRUE,
          status = "primary",

          p(
            "Follow the steps below to download a country template, fill in your data and upload. You can also download the ",
            tags$a(href="https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/A2SIT_data_input_template_GTM.xlsx",
                   "example data set for Guatemala "),
            "to use as an example, or to run through the app as a demo."

          ),
          hr(),

          h4("1. Select your country"),
          country_dropdown(NS(id, "ISO3"), "Country") |>
            add_input_pop("If your country is not on this list please contact us."),
          shinyWidgets::prettySwitch(NS(id, "enable_map_upload"), label = "Shape file upload (advanced)", value = FALSE),

          # map upload: this bit is hidden unless switch is on
          p("Upload a valid GEOJSON or JSON file specifying the regions you wish to map.", id = NS(id, "shape_upload_text")),
          fluidRow(
            col_8(
              fileInput(NS(id, "shape_file"), NULL, buttonLabel = "Browse...", accept = c(".json", ".geojson"))
            ),
            col_4(actionButton(NS(id, "load_shape_click"), "Load"))
          ),
          p("Select fields to use to generate input template.", id = NS(id, "shape_upload_text_2")),
          selectInput(NS(id, "uCode_col"), "Field name of region codes (entries must be unique)", choices = NULL),
          verbatimTextOutput(NS(id, "uCode_text")),
          selectInput(NS(id, "uName_col"), "Field name of region names", choices = NULL),
          verbatimTextOutput(NS(id, "uName_text")),

          col_12(
            hr(),
            h4("2. Download country template"),
            p("Download the template for the selected country:"),
            downloadButton(NS(id, "download_country_template"), "Download country template"),
            shinyWidgets::prettySwitch(NS(id, "gen_fake_data"), label = "With fake data", value = FALSE, inline = TRUE),
            hr(),

            h4("3. Upload your data"),
            p("Upload your compiled template here and click the 'Load' button."),
            style='padding:0px;'
          ),

          col_8(
            fileInput(NS(id, "xlsx_file"), NULL, buttonLabel = "Browse...", accept = c("xls", "xlsx")) |>
              add_input_pop("Please upload the template spreadsheet compiled with your data."),
            style='padding:0px;'
          ),
          col_4(actionButton(NS(id, "load_click"), "Load")),

          # col here otherwise seems to attach to the col-4 previously
          col_12(
            p(icon("triangle-exclamation"), em("Only data formatted using the input template can be uploaded.")),
            style='padding:0px;'
          )

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

input_server <- function(id, coin, coin_full, r_shared, df_geom) {

  moduleServer(id, function(input, output, session) {


    # App geom ----------------------------------------------------------------

    # update ISO3 for other modules
    observeEvent(input$ISO3, {
      req(!r_shared$user_geom)
      stopifnot(input$ISO3 %in% get_cached_countries())
      r_shared$ISO3 <- input$ISO3

      # load table for selected country
      df_ISO3 <- system.file("geom", paste0(input$ISO3,".RDS"), package = "A2SIT") |>
        readRDS()
      # check
      df_ISO3 <- check_and_fix_sf(df_ISO3)
      # save to shared reactive
      df_geom(df_ISO3)
    })


    # User geom ---------------------------------------------------------------

    # show/hide map upload controls
    observeEvent(input$enable_map_upload, {
      shinyjs::toggle("shape_file")
      shinyjs::toggle("load_shape_click")
      shinyjs::toggle("uCode_col")
      shinyjs::toggle("uCode_text")
      shinyjs::toggle("uName_col")
      shinyjs::toggle("shape_upload_text")
      shinyjs::toggle("shape_upload_text_2")

      if(input$enable_map_upload){
        shinyjs::hide("ISO3")
      } else {
        shinyjs::show("ISO3")
      }
    })

    # upload shape files
    observeEvent(input$load_shape_click, {
      req(input$shape_file)
      # load
      dfg <- geojson_to_sf(input$shape_file$datapath) |>
        add_uCode_col() # add app-generated unit codes
      df_geom(dfg)

      r_shared$ISO3 <- NULL
      r_shared$user_geom <- TRUE
    })

    # update geom column dropdowns
    # These are both NULL if user-input geometry
    observe({
      req(r_shared$user_geom)
      updateSelectInput(inputId = "uCode_col", choices = names(df_geom()))
      updateSelectInput(inputId = "uName_col", choices = names(df_geom()))
    })

    # samples of df columns
    output$uCode_text <- renderPrint({
      req(r_shared$user_geom)
      req((input$uCode_col != ""))
      r_shared$uCode_col <- input$uCode_col
      txt_out <- sample_geom_column(df_geom(), input$uCode_col)
      cat(txt_out)
    })
    output$uName_text <- renderPrint({
      req(r_shared$user_geom)
      req((input$uName_col != ""))
      r_shared$uName_col <- input$uName_col
      txt_out <- sample_geom_column(df_geom(), input$uName_col)
      cat(txt_out)
    })


    # Template upload/download ------------------------------------------------

    # Download template
    output$download_country_template <- downloadHandler(
      filename = function() {
        tmpname <- if(is.null(r_shared$ISO3)) "USER" else r_shared$ISO3
        paste0("A2SIT_data_input_template_", tmpname, ".xlsx")
      },
      content = function(file) {
        # fix columns first
        if(input$uCode_col == input$uName_col){
          df_geom(cbind(df_geom(), uName_app = df_geom()[[input$uCode_col]]))
          r_shared$uName_col <- "uName_app"
        }
        # will rename
        df_geom(check_and_fix_sf(df_geom(), r_shared$uCode_col, r_shared$uName_col))
        # make template
        f_generate_input_template(
          df_geom = df_geom(),
          uCode_col = r_shared$uCode_col, uName_col = r_shared$uName_col,
          to_file_name = file, with_fake_data = input$gen_fake_data)
      }
    )

    # load data
    observeEvent(input$load_click, {

      req(input$xlsx_file)
      req(df_geom())

      # fix columns first (if not already done on template download)
      df_geom(check_and_fix_sf(df_geom(), r_shared$uCode_col, r_shared$uName_col))

      # # record column names used in df_geom if user geometry
      # if(r_shared$user_geom){
      #   r_shared$uCode_col <- input$uCode_col
      #   r_shared$uName_col <- input$uName_col
      # }

      # load data and capture any messages
      data_message <- utils::capture.output({
        coin(f_data_input(input$xlsx_file$datapath, r_shared$ISO3,
                          df_geom = df_geom(), uCode_col = r_shared$uCode_col))
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


    # Outputs -----------------------------------------------------------------

    # render flag + country name
    output$flag <- renderUI({

      req(coin())

      c_name <- A2SIT::country_codes$CountryName[
        A2SIT::country_codes$ISO3 == input$ISO3]

      tagList(
        column(3, tags$img(
          src = A2SIT::country_codes$FlagLink[A2SIT::country_codes$ISO3 == input$ISO3],
          width = 100,
          height = 75
        )),
        column(9, h1(c_name))
      )

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
