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

      shinydashboard::tabBox(
        title = "Results",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", width = 10, side = "right",
        tabPanel("Map", leaflet::leafletOutput(NS(id, "map"), height = "60vh")),
        tabPanel("Table", DT::dataTableOutput(NS(id, "results_table")))
      ),

      shinydashboardPlus::box(
        title = "Adjust",
        width = 2, status = "warning",
        selectInput(NS(id, "map_plot_icode"), label = "Plot on map:",
                    choices = NULL, width = "80%"),
        h5("Weights"),
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
                    width = "80%"),
        shinyWidgets::actionBttn(
          inputId = NS(id, "regen"),
          label = "Recalculate",
          style = "jelly",
          color = "success", icon = icon("calculator"), size = "sm"
        )
      )

      # shinydashboardPlus::box(
      #   id = NS(id, "map_box"),
      #   title = "Results",
      #   width = 12,
      #   sidebar = shinydashboardPlus::boxSidebar(
      #     id = NS(id, "map_sidebar"),
      #     startOpen = TRUE,
      #     icon = icon("gear"),
      #     width = 25,
      #     selectInput(NS(id, "map_plot_type"), label = "Show results as:", choices = c("Map", "Table"), width = "80%"),
      #     selectInput(NS(id, "map_plot_icode"), label = "Plot on map:", choices = NULL, width = "80%"),
      #     h4("Weights"),
      #     div(
      #       class = "label-left",
      #       weights_slider(NS(id,"w1"), "Human Mobility"),
      #       weights_slider(NS(id,"w2"), "Threats"),
      #       weights_slider(NS(id,"w3"), "Socioec. Situation"),
      #       weights_slider(NS(id,"w4"), "Response Capacity")
      #     ),
      #     selectInput(NS(id, "agg_method"), label = "Aggregate using:",
      #                 choices = list("Arithmetic mean" = "a_amean",
      #                                "Geometric mean" = "a_gmean"),
      #                 width = "80%"),
      #     shinyWidgets::actionBttn(
      #       inputId = NS(id, "regen"),
      #       label = "Recalculate",
      #       style = "jelly",
      #       color = "success", icon = icon("calculator"), size = "sm"
      #     )
      #   ),
      #   leaflet::leafletOutput(NS(id, "map"), height = "60vh")
      # )
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

    # when user selects results tab, if not done so already, generate results
    observeEvent(parent_input$tab_selected, {

      req(coin())
      if(parent_input$tab_selected == "results"){

        if(!results_exist(coin())){
          coin(f_build_index(coin()))
        }
      }

      updateSelectInput(inputId = "map_plot_icode",
                        choices = get_indicator_codes(coin(), with_levels = TRUE))

    })

    # Plot map
    output$map <- leaflet::renderLeaflet({

      req(coin())
      req(results_exist(coin()))

      shapefile_path <- system.file(
        "shp",
        "gtm_admbnda_adm2_ocha_conred_20190207.shp",
        package = "A2SIT"
      )

      f_plot_map(coin(), dset = "Aggregated", iCode = "MVI",
                 shp_path = shapefile_path)

    })

    # Results table
    output$results_table <- DT::renderDataTable({
      req(coin())
      req(results_exist(coin()))

      f_display_results_table(coin(), type = "scores")
    })

    # bar chart
    output$bar_plot <- plotly::renderPlotly({

      req(coin())
      req(results_exist(coin()))

      iCOINr::iplot_bar(
        coin(), dset = "Aggregated", iCode = "MVI", orientation = "horizontal", usel = input$map_shape_click$id, stack_children = TRUE
      ) |>
        plotly::layout(
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          yaxis = list(title = ""),
          xaxis = list(showticklabels = FALSE),
          showlegend = FALSE,
          margin = list(b = 0, l = 0)
        ) |>
        plotly::config(displayModeBar = FALSE)

    })


    # change aggregation method
    observeEvent(input$agg_method, {
      req(coin())
      req(results_exist(coin()))
      coin(f_build_index(coin(), input$agg_method, only_aggregate = TRUE))
    })

    # change weights
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
      coin(f_change_weights(coin(), w))

    })
    #
    # # info panel
    # output$unit_name <- renderText({
    #   req(input$map_shape_click$id)
    #   COINr::ucodes_to_unames(coin(), input$map_shape_click$id)
    # })
    #
    # output$scores_table <- renderTable({
    #
    #   req(coin())
    #   req(results_exist(coin()))
    #   req(input$map_shape_click$id)
    #
    #   df_info <- COINr::get_unit_summary(coin(), input$map_shape_click$id, Levels = c(4,3), dset = "Aggregated")[-1]
    #   df_info$Rank <- as.integer(df_info$Rank)
    #   names(df_info)[1] <- "Aggregate"
    #   df_info
    # })
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
