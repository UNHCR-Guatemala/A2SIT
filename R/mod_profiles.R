profiles_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "profiles",
    column(
      6,

      box(
        id = "title_box",
        title = NULL,
        width = 12,
        height = "25px",
        headerBorder = FALSE,
        h1(textOutput(NS(id, "unit_name"))),
        selectInput(NS(id, "selected_unit"), label = "Select a region",
                    choices = NULL, width = "100%") |>
          add_input_pop("Regions can also be selected from the map.")
      ),
      tags$head(tags$style('#title_box .box-header{ display: none}')),

      shinydashboard::valueBoxOutput(NS(id, "rank_box")),
      shinydashboard::valueBoxOutput(NS(id, "score_box")),

      box(
        title = box_pop_title(
          title = "Details",
          popover_text = "The scores and ranks of each indicator and aggregate for the selected region. The table is ordered from the highest level downwards.",
          placement = "top"
        ),
        width = 12, status = "info",
        DT::dataTableOutput(NS(id, "df_indicators")))
    ),
    column(
      6,
      box(
        id = NS(id, "radar_box"),
        title = box_pop_title(
          title = "Dimensions",
          popover_text = "The dimension scores of the selected unit, compared to the median. Click the 'gear' icon to change the group to plot and to toggle to a table.",
          placement = "bottom",
          px_from_right = 70
        ),
        width = 12, status = "info", collapsible = TRUE,
        sidebar = shinydashboardPlus::boxSidebar(
          id = "radar_sidebar",
          icon = icon("gear"),
          width = 40,
          selectInput(NS(id, "radar_group"), label = "Show group", choices = NULL),
          shinyWidgets::prettySwitch(NS(id, "plot_radar"), label = "Toggle radar/table", value = TRUE)
        ),
        plotly::plotlyOutput(NS(id, "radar_chart")),
        DT::dataTableOutput(NS(id, "radar_table"))
      ),

      box(
        title = box_pop_title(
          title = "Highest-ranked indicators",
          popover_text = "The five indicators with the highest ranks (after direction adjustment).",
          placement = "top"
        ),
        width = 12, status = "info",
        tableOutput(NS(id, "df_strengths"))),

      box(
        title = box_pop_title(
          title = "Lowest-ranked indicators",
          popover_text = "The five indicators with the lowest ranks (after direction adjustment).",
          placement = "top"
        ),
        width = 12, status = "info",
        tableOutput(NS(id, "df_weaknesses"))))

  )

}

profiles_server <- function(id, coin, coin_full, input, r_shared) {

  moduleServer(id, function(input, output, session) {

    # populate dropdown for selecting unit
    observe({
      req(r_shared$results_built)
      updateSelectInput(inputId = "selected_unit", choices = get_unit_list(coin()))
    })

    # update selected unit (link from results tab)
    observeEvent(r_shared$profile_unit, {
      req(r_shared$results_built)
      req(r_shared$profile_unit)
      updateSelectInput(inputId = "selected_unit", selected = r_shared$profile_unit)
    })

    # title
    output$unit_name <- renderText({
      req(input$selected_unit)
      req(r_shared$results_built)
      COINr::ucodes_to_unames(coin(), input$selected_unit)
    })

    output$rank_box <- shinydashboard::renderValueBox({
      req(coin())
      req(r_shared$results_built)
      shinydashboard::valueBox(
        get_index_rank(coin(), input$selected_unit),
        "Rank",
        #icon = icon("ranking-star"),
        color = "light-blue"
      )
    })

    output$score_box <- shinydashboard::renderValueBox({
      req(coin())
      req(r_shared$results_built)
      shinydashboard::valueBox(
        get_index_score(coin(), input$selected_unit) |>
          round(1),
        "Score",
        #icon = icon("star"),
        color = "green"
      )
    })

    # summary table of scores/ranks for each indicator
    output$df_indicators <- DT::renderDataTable({
      req(r_shared$results_built)
      f_indicator_table(coin(), input$selected_unit)
    })

    # radar chart
    output$radar_chart <- plotly::renderPlotly({

      req(coin())
      req(input$selected_unit)
      req(r_shared$results_built)

      f_plot_radar(
        coin(),
        usel =  input$selected_unit,
        plot_group = input$radar_group
      )

    })

    # radar table
    output$radar_table <- DT::renderDataTable({

      req(coin())
      req(input$selected_unit)
      req(r_shared$results_built)

      f_display_group_table(
        coin(),
        usel = input$selected_unit,
        plot_group = input$radar_group
      )

    })

    # toggle between radar and table
    observeEvent(input$plot_radar,{
      if (input$plot_radar) {
        shinyjs::hide("radar_table")
        shinyjs::show("radar_chart")
      } else {
        shinyjs::hide("radar_chart")
        shinyjs::show("radar_table")
      }
    })

    observeEvent(input$radar_group, {

      n_children <- get_number_of_children(coin(), input$radar_group)
      if(n_children < 3){
        shinyWidgets::updatePrettySwitch(inputId = "plot_radar", value = FALSE,
                                         label = "Too few indicators for radar chart")
        shinyjs::disable("plot_radar")
      } else {
        shinyjs::enable("plot_radar")
        shinyWidgets::updatePrettySwitch(inputId = "plot_radar", value = TRUE,
                                         label = "Toggle radar/table")
      }
    })

    # populate dropdown for radar chart
    observeEvent(r_shared$results_built, {
      req(r_shared$results_built)
      updateSelectInput(
        inputId = "radar_group",
        choices = get_indicator_codes(coin(), code_types = "Aggregate"),
        selected = get_index_code(coin())
      )
    })

    # title of radar chart box (and size)
    observe({

      req(coin())
      req(input$radar_group)
      req(r_shared$results_built)

      n_children <- get_number_of_children(coin(), input$radar_group)

      if((n_children < 5) & !input$plot_radar){
        box_height <- "300px"
      } else {
        box_height <- NULL
      }

      shinydashboardPlus::updateBox(
        id = "radar_box",
        action = "update",
        options = list(
          #title = COINr::icodes_to_inames(coin(), input$radar_group),
          height = box_height
        )
      )
    })


    output$df_strengths <- renderTable({
      req(r_shared$results_built)
      X <- COINr::get_str_weak(
        coin(),
        usel = input$selected_unit,
        dset = "Raw",
        topN = 5,
        withcodes = FALSE,
        unq_discard = 0.2,
        with_units = FALSE,
        sig_figs = NULL)$Strengths |>
        format_sw()
      X
    })

    output$df_weaknesses <- renderTable({
      req(r_shared$results_built)
      X <- COINr::get_str_weak(
        coin(),
        usel = input$selected_unit,
        dset = "Raw",
        topN = 5,
        withcodes = FALSE,
        unq_discard = 0.2,
        with_units = FALSE,
        sig_figs = NULL)$Weaknesses |>
        format_sw()
      X
    })

  })

}
