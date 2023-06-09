analysis_UI <- function(id) {

  shinydashboard::tabItem(
    tabName = "analyse",
    column(7,

           shinydashboardPlus::box(
             title = box_pop_title(
               title = "Indicator analysis",
               popover_text = "The table shows details about each indicator, with possible issues highlighted in yellow. Please click the main help icon in the upper right for more information.",
               placement = "bottom", px_from_right = 40
             ),
             collapsible = FALSE, width = 12,
             status = "info",
             sidebar = shinydashboardPlus::boxSidebar(
               id = "analysis_table_sidebar",
               icon = icon("gear"),
               width = 25,
               checkboxInput(
                 NS(id, "filter_table"),
                 label = "Filter to flagged indicators",
                 value = FALSE)
             ),
             DT::DTOutput(NS(id, "analysis_table"))
           ),

           shinydashboardPlus::box(
             id = NS(id, "indbox"),
             title = box_pop_title(
               "Indicator information",
               popover_text = "Info about the selected indicator. Optionally use the 'Remove' button to remove the indicator, and the 'Restore' button to restore it.",
               placement = "top", px_from_right = 70),
             collapsible = TRUE, width = 12, closable = TRUE,
             status = "info",
               column(
                 7,
                 htmlOutput(NS(id, "indicator_summary"))
               ),
               column(
                 4,
                 shinyWidgets::actionBttn(
                   inputId = NS(id, "remove_indicator"),
                   label = "Remove",
                   style = "jelly",
                   color = "danger", icon = icon("minus"), size = "sm"
                 ),
                 shinyWidgets::actionBttn(
                   inputId = NS(id, "add_indicator"),
                   label = "Restore",
                   style = "jelly",
                   color = "success", icon = icon("plus"), size = "sm"
                 )
               )

           )
    ),
    column(5,

           shinydashboardPlus::box(
             title = box_pop_title(
               "Distribution",
               popover_text = "Distribution plot of the indicator selected in the analysis table. Click the 'gear' icon to the right to change the plot type.",
               placement = "bottom", px_from_right = 70),
             collapsible = TRUE,
             status = "info", width = 12,
             sidebar = shinydashboardPlus::boxSidebar(
               id = "violin_sidebar",
               icon = icon("gear"),
               width = 40,
               selectInput(NS(id, "dist_plottype"), label = "Plot type", choices = c("Violin", "Histogram"))
             ),
             plotly::plotlyOutput(NS(id, "violin_plot"))
           ),

           shinydashboardPlus::box(
             title = box_pop_title(
               "Scatter plot",
               popover_text = "The x axis is the indicator selected in the table. Click on the 'gear' icon to the right to select the indicator to plot on the y axis and to optionally use log axes.",
               placement = "top", px_from_right = 70),
             collapsible = TRUE,
             status = "info", width = 12,
             sidebar = shinydashboardPlus::boxSidebar(
               id = "scatter_sidebar",
               icon = icon("gear"),
               width = 40,
               selectInput(NS(id, "scat_v2"), label = "Plot against", choices = c("tmp_999")),
               shinyWidgets::prettySwitch(NS(id, "scat_trend"), label = "Trendline"),
               shinyWidgets::prettySwitch(NS(id, "scat_logx"), label = "Log X"),
               shinyWidgets::prettySwitch(NS(id, "scat_logy"), label = "Log Y")
             ),
             plotly::plotlyOutput(NS(id, "scatter_plot"))
           )
    )
  )

}

analysis_server <- function(id, coin, coin_full, parent_input, r_shared) {

  moduleServer(id, function(input, output, session) {

    # if stats table doesn't exist yet, this will be null
    # they will be re-accessed every time the coin changes
    #l_analysis <- reactiveVal(NULL)
    #l_analysis_f <- reactiveVal(NULL) # filtered version

    # when user comes to this tab the analysis is calculated
    # Note: if user enters new data this might not update in the same sesh.
    # could add a refresh button maybe.
    observeEvent(parent_input$tab_selected,{

      req(coin())

      if((parent_input$tab_selected == "analyse") && !analysis_exists(coin())){

        # analyse indicators and update coin
        coin(f_analyse_indicators(coin()))

        # extract analysis tables
        r_shared$l_analysis <- coin()$Analysis$Raw[c("FlaggedStats", "Flags")]
        r_shared$l_analysis_f <- filter_to_flagged(r_shared$l_analysis)

      }

      r_shared$isel <- coin_full()$Meta$Lineage[[1]][1]

    })

    # use coin to update scatter plot dropdown of variables
    observe({
      updateSelectInput(
        inputId = "scat_v2",
        choices = coin_full()$Meta$Lineage[[1]]
      )
    })

    # Generate and display results table
    output$analysis_table <- DT::renderDT({
      req(r_shared$l_analysis)
      f_display_indicator_analysis(r_shared$l_analysis, filter_to_flagged = input$filter_table)

    })

    # text (html) summary of selected indicator
    output$indicator_summary <- renderText({
      req(r_shared$isel)

      get_indicator_info(coin_full(), coin(), r_shared$isel)
    })

    # update selected row variable
    observeEvent(input$analysis_table_rows_selected, {
      if(input$filter_table){
        r_shared$isel <- r_shared$l_analysis_f$FlaggedStats$iCode[input$analysis_table_rows_selected]
      } else {
        r_shared$isel <- r_shared$l_analysis$FlaggedStats$iCode[input$analysis_table_rows_selected]
      }
    })

    # update indicator info box
    observeEvent(input$analysis_table_rows_selected, {
      req(r_shared$isel)
      shinydashboardPlus::updateBox(
        "indbox",
        action = "update",
        options = list(
          title = h2(COINr::icodes_to_inames(coin_full(), r_shared$isel))
        )
      )
      shinydashboardPlus::updateBox("indbox", action = "restore")
    })

    # Remove indicators
    observeEvent(input$remove_indicator, {

      if(get_inclusion_status(r_shared$l_analysis, r_shared$isel) == "OUT"){

        shinyWidgets::show_toast(
          title = "Already removed",
          text = paste0("Indicator ", r_shared$isel, " has already been removed."),
          type = "error",
          timer = 5000,
          position = "bottom-end"
        )

      } else {

        # remove indicators and update coin
        coin(f_remove_indicators(coin(), r_shared$isel))

        shinyWidgets::show_toast(
          title = "Indicator removed",
          text = paste0("Indicator ", r_shared$isel, " was successfully removed."),
          type = "info",
          timer = 5000,
          position = "bottom-end"
        )

        # update analysis tables
        r_shared$l_analysis <- coin()$Analysis$Raw[c("FlaggedStats", "Flags")]

      }

    })

    # Add indicators
    observeEvent(input$add_indicator, {

      if(get_inclusion_status(r_shared$l_analysis, r_shared$isel) == "In"){

        shinyWidgets::show_toast(
          title = "Already in",
          text = paste0("Indicator ", r_shared$isel, " is already included in the framework."),
          type = "error",
          timer = 5000,
          position = "bottom-end"
        )

      } else {

        coin(f_add_indicators(coin(), r_shared$isel))

        shinyWidgets::show_toast(
          title = "Indicator restored",
          text = paste0("Indicator ", r_shared$isel, " was successfully restored"),
          type = "info",
          timer = 5000,
          position = "bottom-end"
        )

        # update analysis tables
        r_shared$l_analysis <- coin()$Analysis$Raw[c("FlaggedStats", "Flags")]

      }
    })

    # violin plot
    output$violin_plot <- plotly::renderPlotly({
      req(r_shared$isel)
      iCOINr::iplot_dist(coin_full(), dset = "Raw", iCode = r_shared$isel, ptype = input$dist_plottype)
    })

    # scatter plot
    output$scatter_plot <- plotly::renderPlotly({
      req(r_shared$isel)
      req(input$scat_v2 != "tmp_999")

      browser
      iCOINr::iplot_scatter(
        coin_full(),
        dsets = "Raw",
        iCodes = c(r_shared$isel, input$scat_v2),
        Levels = 1,
        log_axes = c(input$scat_logx, input$scat_logy),
        trendline = input$scat_trend
      )
    })

  })

}
