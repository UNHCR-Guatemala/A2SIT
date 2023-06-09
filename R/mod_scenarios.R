scenarios_UI <- function(id) {

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    tabName = "scenarios",

      shinydashboardPlus::box(
        id = NS(id, "comp_box"),
        title = box_pop_title(
          title = "Compare scenarios",
          popover_text = "Scenarios are columns in the table. Click the 'gear' icon to adjust the table.",
          placement = "bottom", px_from_right = 40
        ),
        width = 8,
        sidebar = shinydashboardPlus::boxSidebar(
          id = "comp_sidebar",
          icon = icon("gear"),
          width = 25,
          selectInput(
            NS(id, "comp_with"), "Compare using:",
            choices = c("Ranks", "Scores")),
          selectInput(
            NS(id, "tab_type"), "How to compare:",
            choices = c("Values", "Differences", "Absolute differences")),
          selectInput(
            NS(id, "base_scen"), "Use as base scenario:",
            choices = NULL)
        ),

        textOutput(NS(id, "no_scen_text")),

        DT::DTOutput(NS(id, "comp_table"))
      ),

      shinydashboardPlus::box(
        id = NS(id, "scat_box"),
        title = box_pop_title(
          title = "Scatter",
          popover_text = "Scatter plot of one scenario against the other.",
          placement = "top", px_from_right = 40
        ),
        sidebar = shinydashboardPlus::boxSidebar(
          id = "scat_sidebar",
          icon = icon("gear"),
          width = 50,
          selectInput(
            NS(id, "scat_x"), "Plot on x-axis:",
            choices = NULL),
          selectInput(
            NS(id, "scat_y"), "Plot on y-axis:",
            choices = NULL)
        ),
        width = 4,
        plotly::plotlyOutput(NS(id, "scenario_scatter"))
      )

      # shinydashboardPlus::box(
      #   id = NS(id, "meta_box"),
      #   title = box_pop_title(
      #     title = "Metadata",
      #     popover_text = "The weights and aggregation methods of your saved scenarios.",
      #     placement = "top", px_from_right = 40
      #   ),
      #   width = 7,
      #   DT::DTOutput(NS(id, "meta_table"))
      # ),


  )

}

scenarios_server <- function(id, coin, input, r_shared) {

  moduleServer(id, function(input, output, session) {

    # update the sidebar dropdown and hide message if we have scenarios
    observe({
      req(r_shared$scenarios)
      updateSelectInput(inputId = "base_scen",
                        choices = names(r_shared$scenarios))
      shinyjs::hide("no_scen_text")
    })

    # comparison data frame
    df_comp <- reactive({

      req(coin())
      req(req(results_exist(coin())))
      req(input$base_scen)
      req(input$comp_with)

      # gather scenarios
      l <- r_shared$scenarios
      if(is.null(l)){
        return(NULL)
      }

      f_make_comparison_table(
        coin = coin(),
        l = l,
        base_scen = input$base_scen,
        comp_with = input$comp_with,
        tab_type = input$tab_type
      )

    })

    # comparison table
    output$comp_table <- DT::renderDT({
      req(df_comp())
      f_style_comparison_table(df_comp(), input$comp_with)
    })

    # if no scenarios some text
    output$no_scen_text <- renderText({
      "No scenarios have been saved yet. Go to the Results tab and use the 'save scenario' dialogue to save and name scenarios first."
    })

    # metadata table
    output$meta_table <- DT::renderDT({

      req(coin())
      req(req(results_exist(coin())))

      # gather scenarios
      l <- r_shared$scenarios
      if(is.null(l)){
        return(NULL)
      }

      # weights
      df_w <- lapply(l, `[[`, "w")
      df_w <- Reduce(rbind, df_w) |>
        as.data.frame()
      names(df_w) <- names(l[[1]]$w)

      agg_methods <- sapply(l, `[[`, "agg_method")

      data.frame(
        Scenario = names(l),
        "Agg. Method" = agg_methods,
        df_w
      )

    }, rownames = FALSE, options = list(scrollX = TRUE, dom = 't'))

    # update scatter plot dropdown selectors
    observe({

      req(df_comp())

      scens <- names(df_comp())[3:ncol(df_comp())]

      if(ncol(df_comp()) >= 4){
        updateSelectInput(inputId = "scat_x", choices = scens, selected = scens[1])
        updateSelectInput(inputId = "scat_y", choices = scens, selected = scens[2])
      }

    })

    # scenario scatter plot
    output$scenario_scatter <- plotly::renderPlotly({

      req(df_comp())
      req(input$scat_y)
      req(input$scat_x)

      if(ncol(df_comp()) < 4){
        return(NULL)
      }

      fig <- plotly::plot_ly(data = df_comp(), type = 'scatter', mode = 'markers') |>
        plotly::add_trace(
          x = ~get(input$scat_x),
          y = ~get(input$scat_y),
          text = ~Name,
          hoverinfo = 'text',
          marker = list(size = 12, color = "rgba(10,77,104,0.5)"),
          showlegend = F
        ) |>
        plotly::layout(
          xaxis = list(title = paste0(input$scat_x, " ", input$comp_with, " (", input$tab_type, ")")),
          yaxis = list(title = paste0(input$scat_y, " ", input$comp_with, " (", input$tab_type, ")")))

      fig

    })

  })

}
