library(shiny)

weltonrovers <- function(...) {

  results_df <- vroom::vroom("./data/results.csv")
  scorers_df <- vroom::vroom("./data/scorers.csv")

  ui <- shinydashboard::dashboardPage(
    skin = "green",
    title = "Welton Rovers",

    shinydashboard::dashboardHeader(title = "Welton Rovers F.C."),

    shinydashboard::dashboardSidebar(
      # Drop-down where a season is selected
      selectInput(
        "season", "Select season(s):", get_season_list(),
        selected = "2022/23",
        multiple = TRUE
        )
      ),

    shinydashboard::dashboardBody(
      htmltools::tags$head(
        includeCSS(path = "www/style.css")
        ),
      htmltools::h1("Quick Facts"),

      # First row of Quick Facts
      fluidRow(
        shinydashboard::valueBoxOutput("win_pc"),
        shinydashboard::valueBoxOutput("most_goals"),
        shinydashboard::valueBoxOutput("winning_streak")
        ),

      # Second row of Quick Facts
      fluidRow(
        shinydashboard::valueBoxOutput("top_scorer"),
        shinydashboard::valueBoxOutput("biggest_win"),
        shinydashboard::valueBoxOutput("av_league_pts")
        ),
      htmltools::hr(),

      # First chart: Point accumulation over season
      htmltools::h1("Point Accumulation"),
      plotly::plotlyOutput("pts_plot"),
      hr(),

      # Second chart: PPG over season(s)
      h1("Points-per-Game"),
      plotly::plotlyOutput("ppg_plot"),
      hr(),

      # Three tables: League records (overall, home, away)
      h1("League Records"),
      DT::dataTableOutput("ssn_records_all"),
      br(),
      h3("Home"),
      DT::dataTableOutput("ssn_records_home"),
      br(),
      h3("Away"),
      DT::dataTableOutput("ssn_records_away"),
      hr(),

      # Table showing streaks during season(s)
      htmltools::h1("Streaks"),
      DT::dataTableOutput("streaks_table"),
      hr(),

      # Tabs containing results for each season
      htmltools::h1("Results by Season"),
      uiOutput("ssn_tabs"),
      hr(),

      # **IN-PROGRESS** Charts showing three top scorers per season
      htmltools::h1("Top Scorers"),
      plotly::plotlyOutput("scorers_plot"),
      hr()
    )
  )

  server <- function(input, output, session) {

    # Row 1, Box 1
    output$win_pc <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("win_pc", input$season)[[1]],
        fill_value_box("win_pc", input$season)[[2]],
        icon = icon("futbol"),
        color = "green"
      )
    })

    # Row 1, Box 2
    output$most_goals <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("most_goals", input$season)[[1]],
        fill_value_box("most_goals", input$season)[[2]],
        icon = icon("futbol"),
        color = "olive"
      )
    })

    # Row 1, Box 3
    output$winning_streak <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("winning_streak", input$season)[[1]],
        fill_value_box("winning_streak", input$season)[[2]],
        icon = icon("thumbs-up", lib = "glyphicon"),
        color = "green"
      )
    })

    # Row 2, Box 1
    output$top_scorer <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("top_scorer", input$season)[[1]],
        fill_value_box("top_scorer", input$season)[[2]],
        icon = icon("user", lib = "glyphicon"),
        color = "olive"
      )
    })

    # Row 2, Box 2
    output$biggest_win <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("biggest_win", input$season)[[1]],
        fill_value_box("biggest_win", input$season)[[2]],
        icon = icon("futbol", lib = "glyphicon"),
        color = "green"
      )
    })

    # Row 2, Box 3
    output$av_league_pts <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        fill_value_box("av_league_pts", input$season)[[1]],
        fill_value_box("av_league_pts", input$season)[[2]],
        icon = icon("futbol", lib = "glyphicon"),
        color = "olive"
      )
    })

    output$pts_plot <- plotly::renderPlotly(
      plot_ssn_pts(input$season)
    )

    output$ppg_plot <- plotly::renderPlotly(
      plot_ssn_ppg(input$season)
    )

    output$ssn_records_all <- DT::renderDataTable(
      get_ssn_records(input$season),
      rownames = FALSE,
      # container = get_table_headers(),
      options = list(
        paging = FALSE,
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = FALSE,
        server = FALSE,
        dom = 'rtp'
      )
    )

    output$ssn_records_home <- DT::renderDataTable(
      get_ssn_records(input$season, "H"),
      rownames = FALSE,
      # container = get_table_headers(),
      options = list(
        paging = FALSE,
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = FALSE,
        server = FALSE,
        dom = 'rtp'
      )
    )

    output$ssn_records_away <- DT::renderDataTable(
      get_ssn_records(input$season, "A"),
      rownames = FALSE,
      # container = get_table_headers(),
      options = list(
        paging = FALSE,
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = FALSE,
        server = FALSE,
        dom = 'rtp'
      )
    )

    output$streaks_table <- DT::renderDataTable(
      get_streaks(input$season),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'tip',
        info = FALSE,
        paging = FALSE
      )
    )

    output_ssn_results <- function(season) {
      DT::renderDataTable(
        filter_results(season) %>% dplyr::mutate(date = format(date, format = "%d %b %Y")),
        options = list(
          paging = TRUE,
          pageLength = 10,
          info = TRUE,
          scrollX = TRUE,
          scrollY = TRUE,
          autoWidth = FALSE,
          server = FALSE,
          dom = 'frtip',
          columnDefs = list(
            list(targets = c(0, 2, 3, 6, 7, 8, 10, 11), className = 'dt-left'),
            list(targets = c(1, 4, 5), className = 'dt-center'),
            list(targets = c(9), className = 'dt-right')
          )
        ),
        extensions = 'Buttons',
        selection = 'single',
        filter = 'bottom',
        rownames = FALSE
      )
    }

    # Dynamically render the tab panels based on user input
    output$ssn_tabs <- renderUI({
      if (!is.null(input$season)) {
        # Get selected seasons
        selected_seasons <- sort(input$season, decreasing = TRUE)

        # Create a tab panel for each selected season
        ssn_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            fluidRow(
              output_ssn_results(season)
            )
          )
        })

        # Return the tabsetPanel containing season results
        do.call(tabsetPanel, ssn_tabs)
      } else {
        htmltools::p("Please select one or more seasons from the dropdown menu.")
      }
    })

    output$scorers_plot <- plotly::renderPlotly(
      plot_ssn_scorers(input$season)
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server, ...)
}
