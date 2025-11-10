# Instruqt Hours Reporting
# A modern Shiny dashboard for tracking Instruqt infrastructure hours consumed

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)

# Enable console logging for errors
options(shiny.error = function() {
  cat("\n=== SHINY ERROR ===\n")
  traceback()
})

# Print startup message
cat("=== Instruqt Hours Reporting ===\n")
cat("Starting application...\n")
cat("Debug mode:", Sys.getenv("INSTRUQT_DEBUG", "FALSE"), "\n")
cat("API key set:", Sys.getenv("INSTRUQT_API_KEY") != "", "\n\n")

# Source API functions
source("instruqt_api.R")

# UI Definition
ui <- page_sidebar(
  title = "Instruqt Reporting",
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#0d6efd",
    bg = "#ffffff",
    fg = "#212529"
  ),

  # Sidebar
  sidebar = sidebar(
    width = 280,

    dateRangeInput(
      "date_range",
      "Date Range:",
      start = "2025-05-10",
      end = Sys.Date(),
      width = "100%"
    ),

    actionButton(
      "refresh_data",
      "Refresh Data",
      icon = icon("refresh"),
      width = "100%",
      class = "btn-primary mt-3"
    ),

    hr(),

    div(
      class = "text-muted small",
      textOutput("last_updated")
    )
  ),

  # Custom CSS to ensure all value boxes are the same height and fix card width
  tags$head(
    tags$style(HTML("
      .bslib-value-box {
        height: 120px !important;
        min-height: 120px !important;
        max-height: 120px !important;
      }
      /* Fix card width and remove scrollbars */
      .bslib-card {
        width: 100% !important;
        max-width: 100% !important;
        overflow: hidden !important;
        margin-top: 1rem !important;
      }
      .bslib-card .card-body {
        width: 100% !important;
        max-width: 100% !important;
        overflow: hidden !important;
        box-sizing: border-box !important;
        padding: 1rem !important;
      }
      /* Ensure plotly charts fit within their container */
      .plotly {
        width: 100% !important;
        max-width: 100% !important;
      }
      .js-plotly-plot {
        width: 100% !important;
      }
      /* Fixed layout container */
      .main-content-grid {
        display: grid;
        grid-template-rows: auto auto auto;
        row-gap: 0rem;
        padding: 0rem;
      }
      .main-content-grid > :nth-child(2) {
        margin-top: 0.5rem;
      }
      .main-content-grid > :nth-child(3) {
        margin-top: 2rem;
      }
      .plot-card-container {
        min-height: 450px;
        max-height: 450px;
        overflow: visible;
      }
      .plot-card-container .bslib-card {
        overflow: hidden;
      }
      .table-card-container {
        min-height: 400px;
        max-height: 400px;
        overflow: visible;
      }
      .table-card-container .bslib-card {
        overflow: auto;
        max-height: 400px;
      }
    "))
  ),

  # Main content with fixed grid layout
  div(
    class = "main-content-grid",

    # Row 1: Value boxes
    layout_columns(
      col_widths = c(4, 4, 4),

      value_box(
        title = "Hours Consumed (Selected Range)",
        value = textOutput("total_hours_value"),
        showcase = icon("calendar-days"),
        theme = "primary",
        height = "120px"
      ),

      value_box(
        title = "Total Since License Start",
        value = textOutput("license_total_hours_value"),
        showcase = icon("clock"),
        theme = "info",
        height = "120px"
      ),

      uiOutput("license_usage_box")
    ),

    # Row 2: Plot card
    div(
      class = "plot-card-container",
      card(
        card_header(
          "Hours Consumed Over Time",
          class = "d-flex justify-content-between align-items-center"
        ),
        card_body(
          navset_pill(
            id = "time_granularity",
            nav_panel("Daily", plotlyOutput("hours_timeline_daily", height = "350px")),
            nav_panel("Weekly", plotlyOutput("hours_timeline_weekly", height = "350px")),
            nav_panel("Monthly", plotlyOutput("hours_timeline_monthly", height = "350px"))
          )
        ),
        full_screen = TRUE
      )
    ),

    # Row 3: Track breakdown card
    div(
      class = "table-card-container",
      uiOutput("track_breakdown_card")
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    consumption_data = NULL,
    license_total_data = NULL,
    play_reports_data = NULL,
    last_update = NULL
  )

  # Load data on startup and when refresh is clicked
  observeEvent(c(input$refresh_data, TRUE), {
    withProgress(message = 'Loading hours data...', {

      cat("\n[APP] Fetching consumption data...\n")

      # Convert date range to ISO 8601 format
      start_iso <- format(as.POSIXct(input$date_range[1]), "%Y-%m-%dT00:00:00Z")
      end_iso <- format(as.POSIXct(input$date_range[2]), "%Y-%m-%dT23:59:59Z")

      rv$consumption_data <- tryCatch({
        result <- get_consumption_details(start_iso, end_iso, play_type = "ALL")
        cat("[APP] Successfully loaded consumption data\n")
        result
      }, error = function(e) {
        cat("[APP] ERROR loading consumption data:\n")
        cat("  Message:", e$message, "\n")
        showNotification(paste("Error loading data:", e$message), type = "error", duration = NULL)
        NULL
      })

      # Always fetch total since license start (May 10, 2025)
      cat("\n[APP] Fetching license total consumption data...\n")
      rv$license_total_data <- tryCatch({
        license_start <- "2025-05-10T00:00:00Z"
        today <- format(Sys.Date(), "%Y-%m-%dT23:59:59Z")
        result <- get_consumption_details(license_start, today, play_type = "ALL")
        cat("[APP] Successfully loaded license total data\n")
        result
      }, error = function(e) {
        cat("[APP] ERROR loading license total data:\n")
        cat("  Message:", e$message, "\n")
        NULL
      })

      # Fetch play reports for track breakdown
      cat("\n[APP] Fetching play reports for track breakdown...\n")
      rv$play_reports_data <- tryCatch({
        result <- get_all_play_reports(max_records = 5000, page_size = 100)
        cat("[APP] Successfully loaded play reports\n")
        result
      }, error = function(e) {
        cat("[APP] ERROR loading play reports:\n")
        cat("  Message:", e$message, "\n")
        NULL
      })

      rv$last_update <- Sys.time()

      if (!is.null(rv$consumption_data)) {
        showNotification("Data loaded successfully!", type = "message")
      }
    })
  }, ignoreNULL = FALSE, once = FALSE)

  # Last updated text
  output$last_updated <- renderText({
    if (!is.null(rv$last_update)) {
      paste("Updated:", format(rv$last_update, "%b %d, %Y %H:%M:%S"))
    } else {
      "Not loaded"
    }
  })

  # Total hours value (for selected date range)
  output$total_hours_value <- renderText({
    req(rv$consumption_data)

    total_hours <- sum(rv$consumption_data$sandboxHours, na.rm = TRUE)
    sprintf("%.1f hrs", total_hours)
  })

  # License total hours value (since May 10, 2025)
  output$license_total_hours_value <- renderText({
    req(rv$license_total_data)

    total_hours <- sum(rv$license_total_data$sandboxHours, na.rm = TRUE)
    sprintf("%.1f hrs", total_hours)
  })

  # License usage percentage value
  output$license_usage_value <- renderText({
    req(rv$license_total_data)

    LICENSE_LIMIT <- 1000
    total_hours <- sum(rv$license_total_data$sandboxHours, na.rm = TRUE)
    usage_percent <- (total_hours / LICENSE_LIMIT) * 100
    sprintf("%.1f%%", usage_percent)
  })

  # License usage percentage box with dynamic color
  output$license_usage_box <- renderUI({
    req(rv$license_total_data)

    LICENSE_LIMIT <- 1000
    total_hours <- sum(rv$license_total_data$sandboxHours, na.rm = TRUE)
    usage_percent <- (total_hours / LICENSE_LIMIT) * 100

    # Determine theme color based on usage percentage
    # 0-70%: success (green)
    # 70-90%: warning (yellow/orange)
    # 90-100%: danger (orange/red)
    # >100%: danger (red)
    theme_color <- if (usage_percent <= 70) {
      "success"
    } else if (usage_percent <= 90) {
      "warning"
    } else {
      "danger"
    }

    # Create custom CSS for gradient effect if over 100%
    custom_bg <- if (usage_percent > 100) {
      "background: linear-gradient(135deg, #dc3545 0%, #c82333 100%) !important;"
    } else {
      ""
    }

    div(
      style = "height: 120px;",
      value_box(
        title = "License Usage",
        value = sprintf("%.1f%%", usage_percent),
        showcase = icon("gauge-high"),
        theme = theme_color,
        height = "120px",
        max_height = "120px",
        min_height = "120px",
        tags$style(HTML(sprintf(".bg-%s { %s }", theme_color, custom_bg)))
      )
    )
  })

  # Hours timeline chart - Daily
  output$hours_timeline_daily <- renderPlotly({
    req(rv$consumption_data)

    data <- rv$consumption_data %>%
      mutate(date = as.Date(date))

    plot_ly(data, x = ~date, y = ~sandboxHours,
            type = "bar",
            marker = list(color = "#3c8dbc"),
            source = "daily_plot") %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE,
        autosize = TRUE,
        margin = list(l = 50, r = 20, t = 20, b = 50)
      )
  })

  # Unified track breakdown card that shows below the chart
  output$track_breakdown_card <- renderUI({
    req(rv$play_reports_data)

    # Get click data from all sources
    click_data_daily <- event_data("plotly_click", source = "daily_plot")
    click_data_weekly <- event_data("plotly_click", source = "weekly_plot")
    click_data_monthly <- event_data("plotly_click", source = "monthly_plot")

    # Check which tab is active
    active_tab <- input$time_granularity

    # Process based on active tab and corresponding click data
    if (active_tab == "Daily" && !is.null(click_data_daily)) {
      clicked_date <- as.Date(click_data_daily$x)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started)) %>%
        filter(date == clicked_date) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for", format(clicked_date, "%B %d, %Y"))

    } else if (active_tab == "Weekly" && !is.null(click_data_weekly)) {
      clicked_week <- as.Date(click_data_weekly$x)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started), week = floor_date(date, "week")) %>%
        filter(week == clicked_week) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for Week of", format(clicked_week, "%B %d, %Y"))

    } else if (active_tab == "Monthly" && !is.null(click_data_monthly)) {
      clicked_month <- as.Date(click_data_monthly$x)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started), month = floor_date(date, "month")) %>%
        filter(month == clicked_month) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for", format(clicked_month, "%B %Y"))

    } else {
      # No bar clicked yet or wrong tab active
      return(NULL)
    }

    # Return card with the track data
    if (nrow(track_data) == 0) {
      return(
        card(
          card_header("Track Breakdown"),
          card_body(
            p("No track data available for this period", class = "text-muted text-center")
          )
        )
      )
    }

    card(
      card_header(title),
      card_body(
        renderTable({
          track_data %>%
            mutate(hours = sprintf("%.2f", hours)) %>%
            select(Track = trackTitle, Plays = plays, Hours = hours)
        }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
      )
    )
  })

  # Hours timeline chart - Weekly
  output$hours_timeline_weekly <- renderPlotly({
    req(rv$consumption_data)

    data <- rv$consumption_data %>%
      mutate(date = as.Date(date)) %>%
      mutate(week = floor_date(date, "week")) %>%
      group_by(week) %>%
      summarise(sandboxHours = sum(sandboxHours, na.rm = TRUE), .groups = "drop")

    plot_ly(data, x = ~week, y = ~sandboxHours,
            type = "bar",
            marker = list(color = "#3c8dbc"),
            source = "weekly_plot") %>%
      layout(
        xaxis = list(title = "Week"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE,
        autosize = TRUE,
        margin = list(l = 50, r = 20, t = 20, b = 50)
      )
  })

  # Hours timeline chart - Monthly
  output$hours_timeline_monthly <- renderPlotly({
    req(rv$consumption_data)

    data <- rv$consumption_data %>%
      mutate(date = as.Date(date)) %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(sandboxHours = sum(sandboxHours, na.rm = TRUE), .groups = "drop")

    plot_ly(data, x = ~month, y = ~sandboxHours,
            type = "bar",
            marker = list(color = "#3c8dbc"),
            source = "monthly_plot") %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE,
        autosize = TRUE,
        margin = list(l = 50, r = 20, t = 20, b = 50)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
