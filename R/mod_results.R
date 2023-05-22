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
        id = NS(id, "tabset1"), width = 9, side = "right",
        tabPanel(
          title = "Map",
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
            h5(textOutput(NS(id, "index_score"))),
            h5(textOutput(NS(id, "index_rank"))),
            tableOutput(NS(id, "scores_table")),
            actionLink(NS(id, "go_to_profile"), label = ""),
            style = "z-index: 20; padding-top: 10px; padding-left: 10px; padding-right: 10px;",
          ),
        ),
        tabPanel("Table", DT::dataTableOutput(NS(id, "results_table")))
      ),

      shinydashboardPlus::box(
        title = box_pop_title(
          title = "Adjust",
          popover_text = "Use the sliders to adjust dimension weights, and click 'Recalculate' to recalculate the results. Note that weights are constrained to sum to 1.",
          placement = "bottom"
        ),
        width = 3, status = "warning",
        strong("Weights"),
        br(),br(),
        uiOutput(NS(id, "weight_sliders")),
        selectInput(NS(id, "agg_method"), label = "Aggregate using:",
                    choices = list("Arithmetic mean" = "a_amean",
                                   "Geometric mean" = "a_gmean"),
                    width = "100%") |>
          add_input_pop("The formula used to aggregate indicators at each level. Please click the main help icon in the upper right for more information."),
        shinyWidgets::actionBttn(
          inputId = NS(id, "regen"),
          label = "Recalculate",
          style = "jelly",
          color = "success", icon = icon("calculator"), size = "sm"
        ),

        hr(),

        textInput(inputId = NS(id,"scenario_name"), label = "Save scenario for comparison", placeholder = "Enter a name"),
        shinyWidgets::actionBttn(
          inputId = NS(id, "scenario_save"),
          label = "Save",
          style = "jelly",
          color = "success", icon = icon("floppy-disk"), size = "sm"
        )
      )
    ),

    fluidRow(
      shinydashboardPlus::box(
        id = NS(id, "bar_box"), title = "",
        width = 12,
        sidebar = shinydashboardPlus::boxSidebar(
          id = "bar_sidebar",
          icon = icon("gear"),
          width = 25,
          selectInput(
            NS(id, "bar_subset"), "Regions to plot:",
            choices = c("Top 50", "Bottom 50", "All")),
          checkboxInput(
            NS(id, "stack_children"),
            label = "Show components of scores",
            value = TRUE)
        ),
        plotly::plotlyOutput(NS(id, "bar_plot"), height = "25vh")
      ),
      # remove header space in box
      #tags$head(tags$style(paste0("#", NS(id, "bar_box"), " .box-header{ display: none}")))
    )

  )

}

results_server <- function(id, coin, coin_full, parent_input, parent_session, shared_reactives) {

  moduleServer(id, function(input, output, session) {


    # Initialise tab ----------------------------------------------------------

    # create reactive value for selected unit
    # (updated by map and table clicks)
    unit_selected <- reactiveVal(NULL)

    # initial vector of slider weights: equal and sum to 1
    slider_weights <- reactiveVal(NULL)

    # when user selects results tab, if not done so already, generate results
    observeEvent(parent_input$tab_selected, {

      req(coin())
      if(parent_input$tab_selected == "results"){

        if(!results_exist(coin())){
          shared_reactives$results_built <- TRUE
          coin(f_build_index(coin()))
        }
      }

      updateSelectInput(inputId = "plot_icode",
                        choices = get_indicator_codes(coin(), with_levels = TRUE))

    })

    top_icodes <- reactive({
      maxlevel <- coin()$Meta$maxlev
      get_codes_at_level(coin(), maxlevel - 1)
    })

    # render the weight sliders. has to be done server-side because depends on
    # user input.
    output$weight_sliders <- renderUI({

      req(coin())

      n_sliders <- length(top_icodes())
      weights <- rep(1/n_sliders, n_sliders)
      names(weights) <- top_icodes()

      slider_weights(weights)

      div(
        class = "label-left",
        lapply(names(weights), function(icode){
          weights_slider(NS(id, icode), icode, weights[[icode]])
        })
      )

    }) |> bindEvent(shared_reactives$results_built)

    # Plots -------------------------------------------------------------------

    # Plot map
    output$map <- leaflet::renderLeaflet({

      req(coin())
      req(results_exist(coin()))
      req(input$plot_icode)

      icode_level <- get_level_of_icode(coin(), input$plot_icode)

      dset_plot <- if (icode_level == 1) "Raw" else "Aggregated"

      f_plot_map(coin(), dset = dset_plot, ISO3 = shared_reactives$ISO3,
                 iCode = input$plot_icode)

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
      can_stack <- get_number_of_children(coin(), input$plot_icode) > 1
      stack_children <- input$stack_children && can_stack

      # bar subset
      if(input$bar_subset == "All"){
        plot_subset <- NULL
        showticks <- FALSE
      } else if (input$bar_subset == "Top 50"){
        plot_subset <- 50
        showticks <- TRUE
      } else if (input$bar_subset == "Bottom 50"){
        plot_subset <- -50
        showticks <- TRUE
      }

      # get iName
      iName <- COINr::icodes_to_inames(coin(), input$plot_icode)

      iCOINr::iplot_bar(
        coin(), dset = "Aggregated",
        iCode = input$plot_icode,
        orientation = "horizontal",
        usel = unit_selected(),
        stack_children = stack_children,
        plot_subset = plot_subset
      ) |>
        plotly::layout(
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          yaxis = list(title = ""),
          xaxis = list(showticklabels = showticks),
          title = list(text = iName, y = 1, x = 0.5, xanchor = 'center', yanchor =  'top'),
          legend = list(x = 1, y = 1.1, orientation = 'h', xanchor = "right", yanchor = "top"),
          showlegend = TRUE,
          margin = list(b = 0, l = 0),
          colorway = c("#18375F", "#0072BC", "#8EBEFF", "#00B398",  "#666666")
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


    # Unit info ---------------------------------------------------------------

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

      index_rank <- get_index_rank(coin(), unit_selected())
      n_units <- get_n_units(coin())

      paste0("Overall rank = ", index_rank, "/", n_units)
    })

    # mean score (avoid calculating repeatedly)
    mean_index_score <- reactive({
      coin()$Data$Aggregated[[
        get_index_code(coin())]] |>
        mean(na.rm = TRUE) |>
        round(1)
    })

    # unit info: index score
    output$index_score <- renderText({

      req(unit_selected())

      index_score <- get_index_score(coin(), unit_selected()) |>
        round(1)
      n_units <- get_n_units(coin())

      paste0("Overall score = ", index_score, " (mean = ", mean_index_score(), ")")
    })

    # unit info: scores and ranks
    output$scores_table <- renderTable({

      req(coin())
      req(results_exist(coin()))
      req(unit_selected())

      df_info <- COINr::get_unit_summary(
        coin(),
        unit_selected(),
        Levels = coin()$Meta$maxlev - 1,
        dset = "Aggregated")[-1]
      df_info$Rank <- as.integer(df_info$Rank)
      names(df_info)[1] <- "Dimension"
      df_info
    })

    # profile link - update text
    observeEvent(unit_selected(), {
      req(unit_selected())
      updateActionLink(inputId = "go_to_profile", label = "Go to profile", icon = icon("location-dot"))
    })

    # profile link: go to profile
    observeEvent(input$go_to_profile, {

      # update selected unit reactive
      shared_reactives$profile_unit <-  unit_selected()

      # go to profile tab
      shinydashboard::updateTabItems(
        session = parent_session,
        inputId = "tab_selected",
        selected = "profiles"
      )
    })


    # Constrain weight sliders ------------------------------------------------
    # To add to 1. Note this was VERY tricky to implement due to the web of
    # reactive dependencies, hence the slightly hacky-looking code.

    w_old <- reactiveVal(NULL)
    w_new <- reactiveVal(NULL)

    observeEvent(reactiveValuesToList(input), {
      w_new(get_slider_weights(input, top_icodes()))
    })

    observeEvent(w_new(), {

      req(w_new())

      # first time we see this - have to set w_old to w_new
      if(is.null(w_old())) w_old(w_new())

      # only update if there were changes
      if(!identical(w_old(), w_new())){

        # find which weight is different
        i_diff <- which(w_old() != w_new())

        # if more than one changed presume changed as update not by user, so skip
        if(length(i_diff) == 1){

          icodes <- names(w_new())

          # the one that changed
          icode_changed <- icodes[[i_diff]]

          # factor to scale other weights by
          w_fac <- (1 - w_new()[i_diff])/sum(w_old()[-i_diff])

          # make copy because need to adjust one element at a time
          w <- w_old()

          # loop over weights that weren't changed
          for (icode in icodes[-i_diff]) {

            # update sliders
            updateSliderInput(inputId = icode, value = as.numeric(w_old()[[icode]]*w_fac))

            # update old weights
            w[icode] <- as.numeric(w_old()[[icode]]*w_fac)

          }

          # update final value
          w[i_diff] <- as.numeric(w_new()[i_diff])
          w_old(w)

        } else {
          # if there was not a manual update, ensure old = new
          w_old(w_new())
        }

      }

    })



    # Regenerate --------------------------------------------------------------

    # change weights or aggregation method
    observeEvent(input$regen, {

      req(results_exist(coin()))

      # since I have to access multiple values of input, I have to convert
      # to a list - not sure if there is another way
      l_input <- isolate(reactiveValuesToList(input))
      # assemble weights into list
      w <- l_input[top_icodes()]

      # update with new weights
      coin(f_rebuild_index(coin(), w, input$agg_method))

    })

    # Save scenario -----------------------------------------------------------

    observeEvent(input$scenario_save, {

      req(coin())
      req(req(results_exist(coin())))
      req(input$scenario_name)

      # extract things of interest
      df_results <- coin()$Data$Aggregated
      w <- get_slider_weights(input, top_icodes())
      agg_method <- if(input$agg_method == "a_amean"){
        "Arithmetic mean"
      } else {
        "Geometric mean"
      }

      # save to list
      shared_reactives$scenarios[[input$scenario_name]] <- list(
        df_results = df_results,
        w = w,
        agg_method = agg_method
      )

      # notify user
      shinyWidgets::show_toast(
        title = "Scenario saved",
        text = paste0("Scenario '", input$scenario_name, "' was saved. Go to the 'Scenarios' tab to compare."),
        type = "info",
        timer = 5000,
        position = "bottom-end"
      )

    })


  })

}
