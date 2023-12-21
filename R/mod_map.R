map_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "full_map",

    shinydashboardPlus::box(
      title = "Map",
      width = 12,
      sidebar = shinydashboardPlus::boxSidebar(
        id = "map_sidebar",
        icon = icon("gear"),
        width = 25,
        startOpen = TRUE,
        background = "#80808060",

        h3("Map controls"),
        p("Click the gear icon above to close/open this panel"),
        selectInput(NS(id, "plot_icode"), label = "Indicator to plot",
                    choices = NULL, width = "95%"),
        #h4("Colours"),
        numericInput(NS(id, "n_colours"), label = "Number of colours", 4, min = 2, max = 10, step = 1, width = "50%"),
        "Note: the first colour corresponds to the lowest scores.",
        uiOutput(NS(id, "mapcolour_dropdowns")),

        #h4("Styling"),
        numericInput(NS(id, "map_opacity"), label = "Opacity", value = 0.7, min = 0.1, max = 1, step = 0.1, width = "50%"),

        fluidRow(
          col_6(textInput(NS(id, "map_linecolour"), label = "Line colour", placeholder = "HEX code", width = "90%", value = "white")),
          col_6(numericInput(NS(id, "map_lineweight"), label = "Line weight", value = 2, min = 0.5, max = 5, step = 0.5, width = "90%"))
        ),

        selectInput(NS(id, "map_linetype"), label = "Line type",
                          choices = list(Solid = 1, Dot = 2, Dash = 4),  width = "50%"),
        selectInput(NS(id, "map_base"), label = "Map base tiles", choices = get_leaflet_map_providers(), width = "70%"),

        actionButton(NS(id, "plot_map_button"), label = "Plot"),
        hr(),

        fluidRow(
          col_6(downloadButton(NS(id, "download_map"), label = "Download map")),
          col_6(selectInput(NS(id, "download_map_filetype"), label = NULL,
                            choices = c("png", "jpeg", "html"),
                            width = "80%"))
        ),

        tags$style("z-index: 2000")

      ),
      leaflet::leafletOutput(NS(id, "map"), height = "95vh", width = "100%")
    )
  )

}

map_server <- function(id, coin, parent_input, r_shared, df_geom) {

  moduleServer(id, function(input, output, session) {

    # populate map dropdown
    observe({
      req(coin())
      updateSelectInput(inputId = "plot_icode",
                        choices = get_indicator_codes(coin(), with_levels = TRUE))
    })

    # reactive for triggering map build
    make_map <- reactiveVal(NULL)

    # update from button
    observeEvent(input$plot_map_button,{
      make_map(input$plot_map_button)
    })

    # plot map
    current_map <- reactive({
      req(coin())
      req(results_exist(coin()))
      req(input$plot_icode)
      req(df_geom())

      l_input <- reactiveValuesToList(input)
      colour_ids <- paste0("colour_", 1:input$n_colours)
      bin_colours <- l_input[colour_ids] |> as.character()
      req(bin_colours)

      f_plot_map(
        coin = coin(),
        df_geom = df_geom(),
        uCode_col = r_shared$uCode_col,
        uName_col = r_shared$uName_col,
        iCode = input$plot_icode,
        as_discrete = FALSE,
        bin_colours = bin_colours,
        poly_opacity = input$map_opacity,
        line_colour = input$map_linecolour,
        line_weight = input$map_lineweight,
        line_type = as.character(input$map_linetype),
        legendposition = "bottomleft",
        map_base = input$map_base
      )
    }) |>
      bindEvent(make_map())

    # Plot map
    output$map <- leaflet::renderLeaflet({
      current_map()
    })

    # # reactive which tells that ready to build map
    # colours_ready <- reactiveVal(FALSE)

    # render the colour inputs. has to be done server-side because depends on
    # user input.
    output$mapcolour_dropdowns <- renderUI({

      req(coin())
      req(input$n_colours)

      colour_ids <- paste0("colour_", 1:input$n_colours)
      initial_colours <- RColorBrewer::brewer.pal(input$n_colours, "Paired")

      div(
        lapply(1:input$n_colours, function(ii){
          colour_input(NS(id, colour_ids[ii]), initial_colours[ii])
        })
      )

    })

    # user map (for download)
    user_map <- reactive({
      leaflet::setView(
        current_map(),
        lng = input$map_center$lng,
        lat = input$map_center$lat,
        zoom = input$map_zoom
      )
    })

    # download map
    output$download_map <- downloadHandler(
      filename = function(){paste0("A2SIT_map.", input$download_map_filetype)},
      content = function(file) {
        f_save_map(plt = user_map(), file_name = file)
      }
    )

  })

}
