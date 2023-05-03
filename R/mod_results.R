results_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "results",

    tags$style(HTML(
      "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 50%;          /* Target width for slider */
    }
    "
    )),

    fluidRow(

      tags$style("
        #controls {
          background-color: #ffffff;
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 0.9;
        }
        #unit_summary {
          background-color: #ffffff;
          font-size: 12px;
          opacity: 0.7;
        }
        #unit_summary:hover{
          opacity: 0.9;
        }"),

      shinydashboard::tabBox(
        title = "Results",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = NS(id, "tabset1"), width = 10, side = "right",
        tabPanel(
          "Map",
          leaflet::leafletOutput(NS(id, "map"), height = "60vh"),
          absolutePanel(
            id = "controls",
            class = "panel panel-default",
            top = "6vh", right = "2vw", left = "auto", bottom = "auto",
            width = "25%",
            draggable = TRUE, height = "auto",
            selectInput(NS(id, "plot_icode"), label = "Plot indicator:",
                        choices = NULL, width = "100%"),
            style = "z-index: 20; padding-top: 10px; padding-left: 10px; padding-right: 10px;",
          ),
          absolutePanel(
            id = "unit_summary",
            class = "panel panel-default",
            bottom = "2vh", left = "2vw", right = "auto", top = "auto",
            width = "25%",
            draggable = FALSE, height = "auto",
            h4(textOutput(NS(id, "unit_name"))),
            h5(textOutput(NS(id, "index_rank"))),
            tableOutput(NS(id, "scores_table")),
            style = "z-index: 20; padding-top: 10px; padding-left: 10px; padding-right: 10px;",
          ),
        ),
        tabPanel("Table", DT::dataTableOutput(NS(id, "results_table")))
      ),

      shinydashboardPlus::box(
        title = "Adjust",
        width = 2, status = "warning",
        strong("Weights"),
        br(),br(),
        div(
          class = "label-left",
          weights_slider(NS(id,"w1"), "Human Mobility"),
          weights_slider(NS(id,"w2"), "Threats"),
          weights_slider(NS(id,"w3"), "Socioec. Situation"),
          weights_slider(NS(id,"w4"), "Response Capacity")
        ),
        selectInput(NS(id, "agg_method"), label = "Aggregate using:",
                    choices = list("Arithmetic mean" = "a_amean",
                                   "Geometric mean" = "a_gmean"),
                    width = "100%"),
        shinyWidgets::actionBttn(
          inputId = NS(id, "regen"),
          label = "Recalculate",
          style = "jelly",
          color = "success", icon = icon("calculator"), size = "sm"
        )
      )
    ),

    fluidRow(
      shinydashboardPlus::box(
        id = NS(id, "bar_box"),
        width = 12, headerBorder = FALSE,
        plotly::plotlyOutput(NS(id, "bar_plot"), height = "20vh")
      ),
      # remove header space in box
      tags$head(tags$style(paste0("#", NS(id, "bar_box"), " .box-header{ display: none}")))
    )

  )

}

results_server <- function(id, coin, coin_full, parent_input) {

  moduleServer(id, function(input, output, session) {

    # create reactive value for selected unit
    # (updated by map and table clicks)
    unit_selected <- reactiveVal(NULL)

    # when user selects results tab, if not done so already, generate results
    observeEvent(parent_input$tab_selected, {

      req(coin())
      if(parent_input$tab_selected == "results"){

        if(!results_exist(coin())){
          coin(f_build_index(coin()))
        }
      }

      updateSelectInput(inputId = "plot_icode",
                        choices = get_indicator_codes(coin(), with_levels = TRUE))

    })

    # Plot map
    output$map <- leaflet::renderLeaflet({

      req(coin())
      req(results_exist(coin()))
      req(input$plot_icode)

      shapefile_path <- system.file(
        "shp",
        "gtm_admbnda_adm2_ocha_conred_20190207.shp",
        package = "A2SIT"
      )

      f_plot_map(coin(), dset = "Aggregated", iCode = input$plot_icode,
                 shp_path = shapefile_path)

    })

    # Results table
    output$results_table <- DT::renderDataTable({
      req(coin())
      req(results_exist(coin()))

      f_display_results_table(coin(), type = "scores")
    })

    # update selected unit for table
    observeEvent(input$results_table_rows_selected, {
      df_results <- coin()$Results$FullScore
      unit_selected(
        df_results$uCode[input$results_table_rows_selected]
      )
    })
    # update selected unit for map
    observeEvent(input$map_shape_click$id, {
      unit_selected(input$map_shape_click$id)
    })
    # update selected unit for bar
    observeEvent(eventbar(),{
      unit_selected(eventbar()$key)
    })

    # bar chart
    output$bar_plot <- plotly::renderPlotly({

      req(coin())
      req(results_exist(coin()))
      req(input$plot_icode)

      # plot underlying dimensions only if have more than 1 child
      stack_children <- get_number_of_children(coin(), input$plot_icode) > 1

      # get iName
      iName <- COINr::icodes_to_inames(coin(), input$plot_icode)

      iCOINr::iplot_bar(
        coin(), dset = "Aggregated",
        iCode = input$plot_icode,
        orientation = "horizontal",
        usel = unit_selected(),
        stack_children = stack_children
      ) |>
        plotly::layout(
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          yaxis = list(title = ""),
          xaxis = list(showticklabels = FALSE),
          title = list(text = iName, y = 0.9, x = 0.5, xanchor = 'center', yanchor =  'top'),
          legend = list(x = 1, y = 1.1, orientation = 'h', xanchor = "right", yanchor = "top"),
          showlegend = TRUE,
          margin = list(b = 0, l = 0)
        ) |>
        plotly::config(displayModeBar = FALSE)

    })

    # extract click data from bar chart
    eventbar <- reactive({
      req()
      plotly::event_data(
        event = "plotly_click",
        source = "bar_chart",
        priority = "event")
    })

    # # radar chart
    # output$radar_chart <- plotly::renderPlotly({
    #   req(coin())
    #   req(results_exist(coin()))
    #   req(unit_selected())
    #
    #   iCOINr::iplot_radar(
    #     coin(),
    #     dset = "Aggregated",
    #     usel = unit_selected(),
    #     Level = coin()$Meta$maxlev - 1,
    #     iCodes = get_index_code(coin()),
    #     addstat = "median"
    #   )
    # })

    # change weights or aggregation method
    observeEvent(input$regen, {

      req(results_exist(coin()))
      # assemble weights into list
      w <-  list(
        Amenazas = input$w1,
        Mov_Hum = input$w2,
        Sit_SocEc = input$w3,
        Cap_Resp = input$w4
      )
      # update with new weights
      coin(f_rebuild_index(coin(), w, input$agg_method))

    })

    # unit info: header
    output$unit_name <- renderText({
      if(is.null(unit_selected())){
        "Select a region..."
      } else {
        COINr::ucodes_to_unames(coin(), unit_selected())
      }
    })

    # unit info: index rank
    output$index_rank <- renderText({

      req(unit_selected())

      index_code <- get_index_code(coin())
      index_rank <- coin()$Results$FullRank[[index_code]][
        coin()$Results$FullRank$uCode == unit_selected()
      ]

      paste0("Index rank = ", index_rank)
    })

    # unit info: scores and ranks
    output$scores_table <- renderTable({

      req(coin())
      req(results_exist(coin()))
      req(unit_selected())

      df_info <- COINr::get_unit_summary(coin(), unit_selected(), Levels = 3, dset = "Aggregated")[-1]
      df_info$Rank <- as.integer(df_info$Rank)
      names(df_info)[1] <- "Dimension"
      df_info
    })

    #
    # output$strengths_table <- renderTable({
    #
    #   req(coin())
    #   req(results_exist(coin()))
    #   req(input$map_shape_click$id)
    #
    #   X <- COINr::get_str_weak(coin(), usel = input$map_shape_click$id, dset = "Raw", topN = 3,
    #                            withcodes = FALSE, unq_discard = 0.2, with_units = FALSE, sig_figs = NULL)$Strengths |>
    #     format_sw()
    #   names(X) <- c("Indicator", "Value (rank)")
    #   X
    # })
    # output$weaks_table <- renderTable({
    #
    #   req(coin())
    #   req(results_exist(coin()))
    #   req(input$map_shape_click$id)
    #
    #   X <- COINr::get_str_weak(coin(), usel = input$map_shape_click$id, dset = "Raw", topN = 3,
    #                            withcodes = FALSE, unq_discard = 0.2, with_units = FALSE, sig_figs = NULL)$Weaknesses |>
    #     format_sw()
    #   names(X) <- c("Indicator", "Value (rank)")
    #   X
    # })

  })



}
