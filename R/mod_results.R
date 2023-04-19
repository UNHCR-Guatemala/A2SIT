results_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "results",

    fluidRow(
      shinydashboardPlus::box(
        id = NS(id, "map_box"),
        title = "Map", width = 12,
        leaflet::leafletOutput(NS(id, "map"), height = "60vh")
      )
    ),
    fluidRow(
      shinydashboardPlus::box(
        id = NS(id, "bar_box"),
        title = "Bar", width = 12,
        plotly::plotlyOutput(NS(id, "bar_plot"), height = "20vh")
      )
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
    #
    #
    # # change aggregation method
    # observeEvent(input$agg_method, {
    #   req(coin())
    #   req(results_exist(coin()))
    #   coin(f_build_index(coin(), input$agg_method, only_aggregate = TRUE))
    # })
    #
    # # change weights
    # observeEvent(input$recalculate, {
    #
    #   req(results_exist(coin()))
    #   # assemble weights into list
    #   w <-  list(
    #     Amenazas = input$w1,
    #     Mov_Hum = input$w2,
    #     Sit_SocEc = input$w3,
    #     Cap_Resp = input$w4
    #   )
    #   # update with new weights
    #   coin(f_change_weights(coin(), w))
    #
    # })
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
