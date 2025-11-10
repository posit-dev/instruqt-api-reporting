# Instruqt Hours Reporting
# A simple Shiny dashboard for tracking Instruqt infrastructure hours consumed

library(shiny)
library(shinydashboard)
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
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(title = "Instruqt Hours"),

  # Sidebar
  dashboardSidebar(
    div(style = "padding: 15px;",
        h4("Date Range"),
        dateRangeInput(
          "date_range",
          NULL,
          start = "2025-05-10",
          end = Sys.Date()
        ),
        br(),
        actionButton(
          "refresh_data",
          "Refresh Data",
          icon = icon("refresh"),
          width = "100%",
          class = "btn-primary"
        ),
        br(),
        br(),
        textOutput("last_updated")
    )
  ),

  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box h3 { font-size: 48px; font-weight: bold; }
        .small-box p { font-size: 16px; }
        .content-wrapper { background-color: #f4f6f9; }
      "))
    ),

    fluidRow(
      valueBoxOutput("total_hours_box", width = 6),
      valueBoxOutput("license_total_hours_box", width = 6)
    ),

    fluidRow(
      box(
        title = "Hours Consumed Over Time",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput("hours_timeline", height = "400px")
      )
    )
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
      paste("Updated:", format(rv$last_update, "%H:%M:%S"))
    } else {
      "Not loaded"
    }
  })

  # Total hours value box (for selected date range)
  output$total_hours_box <- renderValueBox({
    req(rv$consumption_data)

    total_hours <- sum(rv$consumption_data$sandboxHours, na.rm = TRUE)

    valueBox(
      value = sprintf("%.1f", total_hours),
      subtitle = "Hours Consumed (Selected Range)",
      icon = icon("calendar"),
      color = "blue"
    )
  })

  # License total hours value box (since May 10, 2025)
  output$license_total_hours_box <- renderValueBox({
    req(rv$license_total_data)

    total_hours <- sum(rv$license_total_data$sandboxHours, na.rm = TRUE)

    valueBox(
      value = sprintf("%.1f", total_hours),
      subtitle = "Total Hours Since License Start (May 10, 2025)",
      icon = icon("clock"),
      color = "green"
    )
  })

  # Hours timeline chart (bar chart)
  output$hours_timeline <- renderPlotly({
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
}

# Run the application
shinyApp(ui = ui, server = server)
