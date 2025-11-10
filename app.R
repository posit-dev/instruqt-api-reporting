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
    #title = "Filters",
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

  # Custom CSS to ensure all value boxes are the same height
  tags$head(
    tags$style(HTML("
      .bslib-value-box {
        height: 150px !important;
        min-height: 150px !important;
        max-height: 150px !important;
      }
    "))
  ),

  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),

    value_box(
      title = "Hours Consumed (Selected Range)",
      value = textOutput("total_hours_value"),
      showcase = icon("calendar-days"),
      theme = "primary",
      height = "150px"
    ),

    value_box(
      title = "Total Since License Start",
      value = textOutput("license_total_hours_value"),
      showcase = icon("clock"),
      theme = "info",
      height = "150px"
    ),

    uiOutput("license_usage_box")
  ),

  card(
    card_header(
      "Hours Consumed Over Time",
      class = "d-flex justify-content-between align-items-center"
    ),
    card_body(
      navset_pill(
        id = "time_granularity",
        nav_panel("Daily", plotlyOutput("hours_timeline_daily", height = "450px")),
        nav_panel("Weekly", plotlyOutput("hours_timeline_weekly", height = "450px")),
        nav_panel("Monthly", plotlyOutput("hours_timeline_monthly", height = "450px"))
      )
    ),
    full_screen = TRUE
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    consumption_data = NULL,
    license_total_data = NULL,
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
      style = "height: 150px;",
      value_box(
        title = "License Usage",
        value = sprintf("%.1f%%", usage_percent),
        showcase = icon("gauge-high"),
        theme = theme_color,
        height = "150px",
        max_height = "150px",
        min_height = "150px",
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
            marker = list(color = "#3c8dbc")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE
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
            marker = list(color = "#3c8dbc")) %>%
      layout(
        xaxis = list(title = "Week"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE
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
            marker = list(color = "#3c8dbc")) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Hours"),
        showlegend = FALSE
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
