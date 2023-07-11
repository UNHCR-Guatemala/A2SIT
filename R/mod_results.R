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
          opacity: 0.9;
        }
        #unit_summary {
          background-color: #ffffff;
          font-size: 12px;
          opacity: 0.9;
        }
        #download_block {
          display: inline-block;
          padding: 1rem 1rem;
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
            selectInput(NS(id, "plot_icode"), label = NULL,
                        choices = NULL, width = "100%"),
            selectInput(NS(id, "as_discrete"), label = NULL,
                        choices = list("1-5 scale" = TRUE, "1-100 scale" = FALSE), width = "100%"),

            div(style = "display: flex; justify-content: space-between;",
                downloadLink(NS(id, "download_map"), label = "Download map", style = "text-align: right;"),
                selectInput(NS(id, "download_map_filetype"), label = NULL,
                            choices = c("png", "pdf", "jpeg"), width = "40%")
            ),

            style = "z-index: 20; padding: 10px; font-size: 0.8em;",
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
          )
        ),
        tabPanel("Table", DT::dataTableOutput(NS(id, "results_table")))
      ),

      shinydashboardPlus::box(
        title = box_pop_title(
          title = "Adjust",
          popover_text = "Select a scenario and click 'Recalculate'. See 'gear' icon in the top right for advanced options.",
          placement = "bottom", px_from_right = 40
        ),
        width = 3, status = "warning",
        sidebar = shinydashboardPlus::boxSidebar(
          id = "adjust_sidebar",
          icon = icon("gear"),
          width = 40,
          checkboxInput(
            NS(id, "show_weights"),
            label = "Enable weight adjustment",
            value = FALSE)
        ),
        uiOutput(NS(id, "weight_sliders")),
        selectInput(NS(id, "agg_method"), label = "Scenarios",
                    choices = list("Scenario 1: Arithmetic mean" = "a_amean",
                                   "Scenario 2: Geometric mean" = "a_gmean"),
                    width = "100%") |>
          add_input_pop("The formula used to aggregate indicators at each level. Please click the main help icon in the upper right for more information."),
        shinyWidgets::actionBttn(
          inputId = NS(id, "regen"),
          label = "Recalculate",
          style = "jelly",
          color = "success", icon = icon("calculator"), size = "sm"
        )#,
        # hr(),
        #
        # textInput(inputId = NS(id,"scenario_name"), label = "Save scenario for comparison", placeholder = "Enter a name"),
        # shinyWidgets::actionBttn(
        #   inputId = NS(id, "scenario_save"),
        #   label = "Save",
        #   style = "jelly",
        #   color = "success", icon = icon("floppy-disk"), size = "sm"
        # )
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

results_server <- function(id, coin, coin_full, parent_input, parent_session, r_shared) {

  moduleServer(id, function(input, output, session) {


    # Initialise tab ----------------------------------------------------------

    # create reactive value for selected unit
    # (updated by map and table clicks)
    #unit_selected <- reactiveVal(NULL)

    # initial vector of slider weights: equal and sum to 1
    slider_weights <- reactiveVal(NULL)

    # when user selects results tab, if not done so already, generate results
    # and generate scenarios (alt. aggregation methods)
    observeEvent(parent_input$tab_selected, {

      req(coin())
      if(parent_input$tab_selected == "results"){

        if(!results_exist(coin())){
          r_shared$results_built <- TRUE
          coin(f_build_index(coin()))
          r_shared$scenarios <- f_get_scenarios(coin())
        }
      }

    })

    # populate map dropdown
    observe({
      req(coin())
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

      if(!input$show_weights){
        return(NULL)
      }

      w_full <- f_get_last_weights(coin())
      weights <- w_full$Weight[match(top_icodes(), w_full$iCode)]
      weights <- weights/sum(weights)
      names(weights) <- top_icodes()

      slider_weights(weights)

      div(
        class = "label-left",
        lapply(names(weights), function(icode){
          weights_slider(NS(id, icode), icode, weights[[icode]])
        })
      )

    })

    # Plots -------------------------------------------------------------------

    # create map
    current_map <- reactive({
      req(coin())
      req(results_exist(coin()))
      req(input$plot_icode)

      f_plot_map(coin(), ISO3 = r_shared$ISO3,
                 iCode = input$plot_icode, as_discrete = as.logical(input$as_discrete))
    })

    # Plot map
    output$map <- leaflet::renderLeaflet({
      current_map()
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
        mapview::mapshot(
          x = user_map(),
          file = file,
          cliprect = "viewport",
          selfcontained = FALSE
        )
      }
    )

    # Results table
    output$results_table <- DT::renderDataTable({
      req(coin())
      req(results_exist(coin()))

      f_display_results_table(coin(), type = "scores", as_discrete = as.logical(input$as_discrete))
    })

    # update selected unit for table
    observeEvent(input$results_table_rows_selected, {
      df_results <- coin()$Results$FullScore
      r_shared$usel <- df_results$uCode[input$results_table_rows_selected]
    })
    # update selected unit for map
    observeEvent(input$map_shape_click$id, {
      r_shared$usel <- input$map_shape_click$id
    })
    # update selected unit for bar
    observeEvent(eventbar(),{
      r_shared$usel <- eventbar()$key
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

      dset_plot <- get_plot_dset(coin(), input$plot_icode)

      plot_title <- paste0(iName, " (1-100 scale)")

      iCOINr::iplot_bar(
        coin(), dset = dset_plot,
        iCode = input$plot_icode,
        orientation = "horizontal",
        usel = r_shared$usel,
        stack_children = stack_children,
        plot_subset = plot_subset
      ) |>
        plotly::layout(
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          yaxis = list(title = ""),
          xaxis = list(showticklabels = showticks),
          title = list(text = plot_title, y = 1, x = 0.5, xanchor = 'center', yanchor =  'top'),
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
      if(is.null(r_shared$usel)){
        "Click on a region on the map to see the details about it."
      } else {
        COINr::ucodes_to_unames(coin(), r_shared$usel)
      }
    })

    # unit info: index rank
    output$index_rank <- renderText({

      req(r_shared$usel)

      index_rank <- get_index_rank(coin(), r_shared$usel)
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

      req(r_shared$usel)

      if (as.logical(input$as_discrete)) {
        index_sev <- coin()$Results$Severity[
          coin()$Results$Severity$uCode == r_shared$usel,
          get_index_code(coin())
        ]
        paste0("Overall severity = ", index_sev)
      } else {
        index_score <- get_index_score(coin(), r_shared$usel) |>
          round(1)
        paste0("Overall score = ", index_score, " (mean ", mean_index_score(), ")")
      }


    })

    # unit info: scores and ranks
    output$scores_table <- renderTable({

      req(coin())
      req(results_exist(coin()))
      req(r_shared$usel)

      if (as.logical(input$as_discrete)) {

        df_info <- get_unit_summary_sev(coin(), r_shared$usel,
                                        Level = coin()$Meta$maxlev - 1)
      } else {

        df_info <- COINr::get_unit_summary(
          coin(),
          r_shared$usel,
          Levels = coin()$Meta$maxlev - 1,
          dset = "Aggregated")[-1]

      }

      df_info$Rank <- as.integer(df_info$Rank)
      names(df_info)[1] <- "Dimension"
      df_info
    })

    # profile link - update text
    observeEvent(r_shared$usel, {
      req(r_shared$usel)
      updateActionLink(inputId = "go_to_profile", label = "Go to profile", icon = icon("location-dot"))
    })

    # profile link: go to profile
    observeEvent(input$go_to_profile, {

      # update selected unit reactive
      r_shared$profile_unit <-  r_shared$usel

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

      if (input$show_weights) {

        # if weights are enabled, collect weight values
        # since I have to access multiple values of input, I have to convert
        # to a list - not sure if there is another way
        l_input <- isolate(reactiveValuesToList(input))
        # assemble weights into list
        w <- l_input[top_icodes()]

      } else {
        # this means reuse current weights
        w <- NULL
      }

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
      r_shared$scenarios[[input$scenario_name]] <- list(
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
