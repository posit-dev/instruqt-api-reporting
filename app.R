# Instruqt Hours Reporting
# A modern Shiny dashboard for tracking Instruqt infrastructure hours consumed

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)

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
ui <- page_navbar(
  title = "Instruqt Reporting",
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#0d6efd",
    bg = "#ffffff",
    fg = "#212529"
  ),

  header = tags$head(
    tags$style(HTML("
      .bslib-value-box {
        height: 95px !important;
        min-height: 95px !important;
        max-height: 95px !important;
      }
      .bslib-value-box .value-box-title {
        font-size: 0.85rem !important;
      }
      .bslib-value-box .value-box-value {
        font-size: 1.25rem !important;
      }
      .bslib-value-box .value-box-showcase {
        font-size: 1.5rem !important;
      }
      .bslib-value-box .value-box-showcase i {
        font-size: 1.5rem !important;
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
        grid-template-rows: auto auto auto auto;
        row-gap: 0rem;
        padding: 0rem;
        padding-bottom: 2rem;
      }
      .main-content-grid > :nth-child(2) {
        margin-top: 0rem;
      }
      .main-content-grid > :nth-child(3) {
        margin-top: 0rem;
      }
      .main-content-grid > :nth-child(4) {
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
        max-height: 400px;
        display: flex;
        flex-direction: column;
      }
      .table-card-container .card-body {
        overflow-y: auto !important;
        overflow-x: hidden !important;
        flex: 1;
        min-height: 0;
      }
    "))
  ),

  # Overview Page
  nav_panel(
    title = "Overview",
    icon = icon("chart-line"),

    layout_sidebar(
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

      # Main content with fixed grid layout
      div(
    class = "main-content-grid",

    # Row 1: Value boxes (first row)
    layout_columns(
      col_widths = c(4, 4, 4),

      value_box(
        title = "Hours Consumed (Selected Range)",
        value = textOutput("total_hours_value"),
        showcase = icon("calendar-days"),
        theme = "primary",
        height = "95px"
      ),

      value_box(
        title = "Total Since License Start",
        value = textOutput("license_total_hours_value"),
        showcase = icon("clock"),
        theme = "info",
        height = "95px"
      ),

      uiOutput("license_usage_box")
    ),

    # Row 1b: Additional value boxes (second row)
    layout_columns(
      col_widths = c(4, 4, 4),

      value_box(
        title = "Total Active Time (Selected Range)",
        value = textOutput("total_active_hours_value"),
        showcase = icon("play-circle"),
        theme = "success",
        height = "95px"
      ),

      value_box(
        title = "Developer Hours (Selected Range)",
        value = textOutput("developer_hours_value"),
        showcase = icon("code"),
        theme = "warning",
        height = "95px"
      ),

      value_box(
        title = "Overhead Time (Selected Range)",
        value = textOutput("total_overhead_hours_value"),
        showcase = icon("server"),
        theme = "secondary",
        height = "95px"
      )
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
  ),

  # Tracks Page
  nav_panel(
    title = "Tracks",
    icon = icon("list-check"),

    layout_sidebar(
      sidebar = sidebar(
        width = 280,

        dateRangeInput(
          "tracks_date_range",
          "Date Range:",
          start = "2025-05-10",
          end = Sys.Date(),
          width = "100%"
        ),

        selectInput(
          "track_tag_filter",
          "Filter by Tag:",
          choices = c("All Tags" = "all"),
          width = "100%"
        ),

        actionButton(
          "refresh_tracks",
          "Refresh Data",
          icon = icon("refresh"),
          width = "100%",
          class = "btn-primary mt-3"
        ),

        hr(),

        div(
          class = "text-muted small",
          textOutput("tracks_last_updated")
        )
      ),

      # Main content
      div(
        class = "p-3",

        # Summary cards
        layout_columns(
          col_widths = c(6, 6),

          value_box(
            title = "Total Tracks",
            value = textOutput("total_tracks_value"),
            showcase = icon("list"),
            theme = "primary",
            height = "95px"
          ),

          value_box(
            title = "Total Plays",
            value = textOutput("total_plays_value"),
            showcase = icon("play"),
            theme = "info",
            height = "95px"
          )
        ),

        # Tracks table
        div(
          class = "mt-3",
          h4("Track Details"),
          DT::dataTableOutput("tracks_table")
        )
      )
    )
  ),

  # Invites Page
  nav_panel(
    title = "Invites",
    icon = icon("envelope"),

    layout_sidebar(
      sidebar = sidebar(
        width = 280,

        selectInput(
          "invite_track_filter",
          "Filter by Track:",
          choices = c("All Tracks" = "all"),
          width = "100%"
        ),

        selectInput(
          "invite_status_filter",
          "Filter by Status:",
          choices = c("All" = "all", "Active" = "active", "Expired" = "expired"),
          selected = "all",
          width = "100%"
        ),

        actionButton(
          "refresh_invites",
          "Refresh Data",
          icon = icon("refresh"),
          width = "100%",
          class = "btn-primary mt-3"
        ),

        hr(),

        div(
          class = "text-muted small",
          textOutput("invites_last_updated")
        )
      ),

      # Main content
      div(
        class = "p-3",

        # Summary cards
        layout_columns(
          col_widths = c(4, 4, 4),

          value_box(
            title = "Total Invites",
            value = textOutput("total_invites_value"),
            showcase = icon("envelope"),
            theme = "primary",
            height = "95px"
          ),

          value_box(
            title = "Total Claims",
            value = textOutput("total_claims_value"),
            showcase = icon("user-check"),
            theme = "info",
            height = "95px"
          ),

          value_box(
            title = "Avg Claims per Invite",
            value = textOutput("avg_claims_per_invite_value"),
            showcase = icon("chart-line"),
            theme = "success",
            height = "95px"
          )
        ),

        # Invites table
        div(
          class = "mt-3",
          h4("Invite Details"),
          DT::dataTableOutput("invites_table")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values (shared between all pages)
  rv <- reactiveValues(
    consumption_data = NULL,
    developer_consumption_data = NULL,
    license_total_data = NULL,
    play_reports_data = NULL,
    tracks_data = NULL,
    invites_data = NULL,
    last_update = NULL
  )

  # Load data on startup and when refresh is clicked from any page
  observeEvent(c(input$refresh_data, input$refresh_tracks, input$refresh_invites, TRUE), {
    # Show loading notification with spinner
    loading_id <- showNotification(
      ui = tagList(
        icon("spinner", class = "fa-spin"),
        " Loading data..."
      ),
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )

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

      # Fetch developer hours for the selected date range
      cat("\n[APP] Fetching developer consumption data...\n")
      rv$developer_consumption_data <- tryCatch({
        result <- get_consumption_details(start_iso, end_iso, play_type = "DEVELOPER")
        cat("[APP] Successfully loaded developer consumption data\n")
        result
      }, error = function(e) {
        cat("[APP] ERROR loading developer consumption data:\n")
        cat("  Message:", e$message, "\n")
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

      # Fetch current tracks list
      cat("\n[APP] Fetching tracks list...\n")
      rv$tracks_data <- tryCatch({
        result <- get_all_tracks()
        cat("[APP] Successfully loaded tracks list\n")

        # Update tag filter choices for Tracks page based on active tracks
        if (!is.null(result) && nrow(result) > 0) {
          all_tags <- unique(unlist(strsplit(result$tags, ", ")))
          all_tags <- all_tags[all_tags != ""]
          all_tags <- sort(all_tags)

          updateSelectInput(session, "track_tag_filter",
                           choices = c("All Tags" = "all", setNames(all_tags, all_tags)))
        }

        result
      }, error = function(e) {
        cat("[APP] ERROR loading tracks list:\n")
        cat("  Message:", e$message, "\n")
        NULL
      })

      # Fetch invites
      cat("\n[APP] Fetching invites...\n")
      rv$invites_data <- tryCatch({
        result <- get_all_invites()
        cat("[APP] Successfully loaded invites\n")

        # Update track filter choices for Invites page
        if (!is.null(result) && nrow(result) > 0) {
          all_tracks <- unique(result$track_title)
          all_tracks <- all_tracks[all_tracks != ""]
          all_tracks <- sort(all_tracks)

          updateSelectInput(session, "invite_track_filter",
                           choices = c("All Tracks" = "all", setNames(all_tracks, all_tracks)))
        }

        result
      }, error = function(e) {
        cat("[APP] ERROR loading invites:\n")
        cat("  Message:", e$message, "\n")
        NULL
      })

      rv$last_update <- Sys.time()

      # Remove loading notification
      removeNotification(loading_id)

      # Show success notification
      if (!is.null(rv$consumption_data)) {
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }
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

  # Total active hours value (for selected date range)
  output$total_active_hours_value <- renderText({
    req(rv$play_reports_data)
    req(rv$consumption_data)

    # Filter play reports to selected date range
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    total_active <- rv$play_reports_data %>%
      mutate(date = as.Date(started)) %>%
      filter(date >= start_date & date <= end_date) %>%
      pull(hoursConsumed) %>%
      sum(na.rm = TRUE)

    sprintf("%.1f hrs", total_active)
  })

  # Developer hours value (for selected date range)
  output$developer_hours_value <- renderText({
    req(rv$developer_consumption_data)

    total_dev_hours <- sum(rv$developer_consumption_data$sandboxHours, na.rm = TRUE)
    sprintf("%.1f hrs", total_dev_hours)
  })

  # Total overhead hours value (for selected date range)
  output$total_overhead_hours_value <- renderText({
    req(rv$play_reports_data)
    req(rv$consumption_data)

    # Total consumption hours
    total_consumption <- sum(rv$consumption_data$sandboxHours, na.rm = TRUE)

    # Filter play reports to selected date range
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    total_active <- rv$play_reports_data %>%
      mutate(date = as.Date(started)) %>%
      filter(date >= start_date & date <= end_date) %>%
      pull(hoursConsumed) %>%
      sum(na.rm = TRUE)

    overhead <- total_consumption - total_active

    sprintf("%.1f hrs", overhead)
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
      style = "height: 95px;",
      value_box(
        title = "License Usage",
        value = sprintf("%.1f%%", usage_percent),
        showcase = icon("gauge-high"),
        theme = theme_color,
        height = "95px",
        max_height = "95px",
        min_height = "95px",
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
    req(rv$consumption_data)

    # Get click data from all sources
    click_data_daily <- event_data("plotly_click", source = "daily_plot")
    click_data_weekly <- event_data("plotly_click", source = "weekly_plot")
    click_data_monthly <- event_data("plotly_click", source = "monthly_plot")

    # Check which tab is active
    active_tab <- input$time_granularity

    # Process based on active tab and corresponding click data
    if (active_tab == "Daily" && !is.null(click_data_daily)) {
      clicked_date <- as.Date(click_data_daily$x)

      # Get sandbox hours from consumption data
      sandbox_hours <- rv$consumption_data %>%
        mutate(date = as.Date(date)) %>%
        filter(date == clicked_date) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      # Get developer hours from developer consumption data
      developer_hours <- rv$developer_consumption_data %>%
        mutate(date = as.Date(date)) %>%
        filter(date == clicked_date) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started)) %>%
        filter(date == clicked_date) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          tags = paste(unique(trackTags[trackTags != ""]), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for", format(clicked_date, "%B %d, %Y"))
      total_track_hours <- sum(track_data$hours, na.rm = TRUE)

    } else if (active_tab == "Weekly" && !is.null(click_data_weekly)) {
      clicked_week <- as.Date(click_data_weekly$x)

      # Get sandbox hours from consumption data for the week
      sandbox_hours <- rv$consumption_data %>%
        mutate(date = as.Date(date), week = floor_date(date, "week")) %>%
        filter(week == clicked_week) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      # Get developer hours from developer consumption data for the week
      developer_hours <- rv$developer_consumption_data %>%
        mutate(date = as.Date(date), week = floor_date(date, "week")) %>%
        filter(week == clicked_week) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started), week = floor_date(date, "week")) %>%
        filter(week == clicked_week) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          tags = paste(unique(trackTags[trackTags != ""]), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for Week of", format(clicked_week, "%B %d, %Y"))
      total_track_hours <- sum(track_data$hours, na.rm = TRUE)

    } else if (active_tab == "Monthly" && !is.null(click_data_monthly)) {
      clicked_month <- as.Date(click_data_monthly$x)

      # Get sandbox hours from consumption data for the month
      sandbox_hours <- rv$consumption_data %>%
        mutate(date = as.Date(date), month = floor_date(date, "month")) %>%
        filter(month == clicked_month) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      # Get developer hours from developer consumption data for the month
      developer_hours <- rv$developer_consumption_data %>%
        mutate(date = as.Date(date), month = floor_date(date, "month")) %>%
        filter(month == clicked_month) %>%
        pull(sandboxHours) %>%
        sum(na.rm = TRUE)

      track_data <- rv$play_reports_data %>%
        mutate(date = as.Date(started), month = floor_date(date, "month")) %>%
        filter(month == clicked_month) %>%
        group_by(trackTitle) %>%
        summarise(
          plays = n(),
          hours = sum(hoursConsumed, na.rm = TRUE),
          tags = paste(unique(trackTags[trackTags != ""]), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(hours))

      title <- paste("Track Breakdown for", format(clicked_month, "%B %Y"))
      total_track_hours <- sum(track_data$hours, na.rm = TRUE)

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

    # Calculate overhead (total minus active and developer time)
    overhead_hours <- sandbox_hours - total_track_hours - developer_hours

    card(
      card_header(title),
      card_body(
        # Summary section
        div(
          class = "mb-3 p-3",
          style = "background-color: #f8f9fa; border-radius: 5px;",
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            div(
              strong("Total Compute Hours:"),
              br(),
              sprintf("%.2f hrs", sandbox_hours)
            ),
            div(
              strong("Total Active Time:"),
              br(),
              sprintf("%.2f hrs", total_track_hours),
              br(),
              span(class = "text-muted small", "(Active track play time)")
            ),
            div(
              strong("Total Developer Time:"),
              br(),
              sprintf("%.2f hrs", developer_hours)
            ),
            div(
              strong("Overhead Time:"),
              br(),
              sprintf("%.2f hrs", overhead_hours),
              br(),
              span(class = "text-muted small", "(startup, idle, etc.)")
            )
          )
        ),
        # Track breakdown table
        h5("Track Details"),
        renderTable({
          track_data %>%
            mutate(hours = sprintf("%.2f", hours)) %>%
            select(Track = trackTitle, Plays = plays, Hours = hours, Tags = tags)
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

  # ===== TRACKS PAGE LOGIC =====

  # Last updated text for tracks page (uses shared rv$last_update)
  output$tracks_last_updated <- renderText({
    if (!is.null(rv$last_update)) {
      paste("Updated:", format(rv$last_update, "%b %d, %Y %H:%M:%S"))
    } else {
      "Not loaded"
    }
  })

  # Filtered tracks data based on date range and tag
  filtered_tracks <- reactive({
    req(rv$play_reports_data)
    req(rv$tracks_data)

    # Get list of active track IDs
    active_track_ids <- rv$tracks_data$id

    # Filter play reports to only include active tracks and date range
    data <- rv$play_reports_data %>%
      filter(trackId %in% active_track_ids) %>%
      mutate(date = as.Date(started)) %>%
      filter(date >= input$tracks_date_range[1] & date <= input$tracks_date_range[2])

    # Apply tag filter
    if (input$track_tag_filter != "all") {
      data <- data %>%
        filter(grepl(input$track_tag_filter, trackTags, fixed = TRUE))
    }

    data
  })

  # Aggregate track statistics
  track_stats <- reactive({
    req(rv$tracks_data)
    req(rv$play_reports_data)

    # Start with all active tracks
    all_tracks <- rv$tracks_data %>%
      select(trackTitle = title, trackSlug = slug, trackTags = tags, trackId = id)

    # Apply tag filter to the base tracks list
    if (input$track_tag_filter != "all") {
      all_tracks <- all_tracks %>%
        filter(grepl(input$track_tag_filter, trackTags, fixed = TRUE))
    }

    # Get filtered play reports for the date range
    play_stats <- rv$play_reports_data %>%
      filter(trackId %in% rv$tracks_data$id) %>%
      mutate(date = as.Date(started)) %>%
      filter(date >= input$tracks_date_range[1] & date <= input$tracks_date_range[2]) %>%
      group_by(trackTitle, trackSlug, trackTags) %>%
      summarise(
        plays = n(),
        active_hours = sum(hoursConsumed, na.rm = TRUE),
        avg_hours_per_play = mean(hoursConsumed, na.rm = TRUE),
        avg_completion = mean(completionPercent, na.rm = TRUE),
        completed_plays = sum(completionPercent >= 100),
        completion_rate = (sum(completionPercent >= 100) / n()) * 100,
        unique_users = n_distinct(userEmail),
        .groups = "drop"
      )

    # Left join to include all tracks, even those with no plays
    result <- all_tracks %>%
      left_join(play_stats, by = c("trackTitle", "trackSlug", "trackTags")) %>%
      mutate(
        plays = ifelse(is.na(plays), 0, plays),
        active_hours = ifelse(is.na(active_hours), 0, active_hours),
        avg_hours_per_play = ifelse(is.na(avg_hours_per_play), 0, avg_hours_per_play),
        avg_completion = ifelse(is.na(avg_completion), 0, avg_completion),
        completed_plays = ifelse(is.na(completed_plays), 0, completed_plays),
        completion_rate = ifelse(is.na(completion_rate), 0, completion_rate),
        unique_users = ifelse(is.na(unique_users), 0, unique_users)
      ) %>%
      select(-trackId) %>%
      arrange(desc(active_hours))

    result
  })

  # Total tracks value
  output$total_tracks_value <- renderText({
    req(track_stats())
    nrow(track_stats())
  })

  # Total plays value
  output$total_plays_value <- renderText({
    req(track_stats())
    sum(track_stats()$plays)
  })

  # Tracks table
  output$tracks_table <- DT::renderDataTable({
    req(track_stats())

    track_stats() %>%
      select(
        Track = trackTitle,
        Tags = trackTags,
        `Active Hours` = active_hours,
        Plays = plays,
        `Avg Hours/Play` = avg_hours_per_play
      ) %>%
      DT::datatable(
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 25,
          order = list(list(2, 'desc')),  # Sort by Active Hours descending (0-indexed)
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = c(2, 3, 4), className = 'dt-right'),  # Right-align numeric columns
            list(width = '40%', targets = 0),  # Track name
            list(width = '25%', targets = 1),  # Tags
            list(width = '15%', targets = 2),  # Active Hours
            list(width = '10%', targets = 3),  # Plays
            list(width = '10%', targets = 4)   # Avg Hours/Play
          ),
          language = list(
            search = "Search tracks:",
            lengthMenu = "Show _MENU_ tracks per page"
          )
        ),
        class = 'cell-border stripe hover'
      ) %>%
      DT::formatRound(columns = c('Active Hours', 'Avg Hours/Play'), digits = 2) %>%
      DT::formatStyle(
        'Active Hours',
        background = styleColorBar(range(track_stats()$active_hours), '#3c8dbc'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Plays',
        fontWeight = 'bold',
        color = styleInterval(c(5, 10), c('#999', '#000', '#2c5f91'))
      )
  })

  # ===== INVITES PAGE LOGIC =====

  # Last updated text for invites page
  output$invites_last_updated <- renderText({
    if (!is.null(rv$last_update)) {
      paste("Updated:", format(rv$last_update, "%b %d, %Y %H:%M:%S"))
    } else {
      "Not loaded"
    }
  })

  # Filtered invites data
  filtered_invites <- reactive({
    req(rv$invites_data)

    data <- rv$invites_data

    # Apply track filter
    if (input$invite_track_filter != "all") {
      data <- data %>%
        filter(track_title == input$invite_track_filter)
    }

    # Apply status filter using API status
    if (input$invite_status_filter == "active") {
      data <- data %>%
        filter(tolower(status) == "active")
    } else if (input$invite_status_filter == "expired") {
      data <- data %>%
        filter(tolower(status) == "expired")
    }

    data
  })

  # Total invites value
  output$total_invites_value <- renderText({
    req(filtered_invites())
    nrow(filtered_invites())
  })

  # Total claims value
  output$total_claims_value <- renderText({
    req(filtered_invites())
    sum(filtered_invites()$num_claims)
  })

  # Average claims per invite value
  output$avg_claims_per_invite_value <- renderText({
    req(filtered_invites())
    total_invites <- nrow(filtered_invites())
    total_claims <- sum(filtered_invites()$num_claims)
    if (total_invites > 0) {
      avg <- total_claims / total_invites
      sprintf("%.1f", avg)
    } else {
      "0"
    }
  })

  # Invites table
  output$invites_table <- DT::renderDataTable({
    req(filtered_invites())

    filtered_invites() %>%
      mutate(
        # Use status from API (capitalize first letter for display)
        status_display = tools::toTitleCase(tolower(status)),
        # Safely parse created date with time (ISO 8601 format from API)
        created_datetime = sapply(created, function(created_val) {
          if (created_val == "" || created_val == "NA") {
            return("")
          }
          # Parse ISO 8601 format (e.g., "2025-10-31T17:38:53Z")
          parsed_dt <- tryCatch(
            as.POSIXct(created_val, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            error = function(e) NULL
          )
          if (is.null(parsed_dt) || is.na(parsed_dt)) {
            return(created_val)  # Return original if parsing fails
          }
          # Convert to Eastern Time
          et_dt <- format(parsed_dt, "%Y-%m-%d %H:%M", tz = "America/New_York")
          paste(et_dt, "ET")
        }),
        # Safely parse expires date with time (ISO 8601 format from API)
        expires_datetime = sapply(expires_at, function(exp_at) {
          if (exp_at == "" || exp_at == "NA") {
            return("Never")
          }
          # Parse ISO 8601 format (e.g., "2025-11-14T17:00:00Z")
          parsed_dt <- tryCatch(
            as.POSIXct(exp_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            error = function(e) NULL
          )
          if (is.null(parsed_dt) || is.na(parsed_dt)) {
            return(exp_at)  # Return original if parsing fails
          }
          # Convert to Eastern Time
          et_dt <- format(parsed_dt, "%Y-%m-%d %H:%M", tz = "America/New_York")
          paste(et_dt, "ET")
        }),
        # Format claim rate - convert to integer first
        claim_rate = ifelse(max_claims == Inf,
                           sprintf("%d claims", as.integer(num_claims)),
                           sprintf("%d / %d", as.integer(num_claims), as.integer(max_claims))),
        # Format track display
        tracks_display = ifelse(num_tracks == 1, track_title,
                               ifelse(num_tracks == 0, "No tracks", sprintf("%d tracks", num_tracks)))
      ) %>%
      select(
        Invite = invite_title,
        Track = tracks_display,
        Status = status_display,
        `Created By` = created_by,
        Created = created_datetime,
        Expires = expires_datetime,
        Claims = claim_rate
      ) %>%
      DT::datatable(
        rownames = FALSE,
        filter = 'top',
        options = list(
          pageLength = 25,
          order = list(list(4, 'desc')),  # Sort by Created date descending (0-indexed)
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = c(6), className = 'dt-right'),  # Right-align Claims
            list(width = '20%', targets = 0),  # Invite
            list(width = '15%', targets = 1),  # Track
            list(width = '10%', targets = 2),  # Status
            list(width = '15%', targets = 3),  # Created By
            list(width = '15%', targets = 4),  # Created
            list(width = '15%', targets = 5),  # Expires
            list(width = '10%', targets = 6)   # Claims
          ),
          language = list(
            search = "Search invites:",
            lengthMenu = "Show _MENU_ invites per page"
          )
        ),
        class = 'cell-border stripe hover'
      ) %>%
      DT::formatStyle(
        'Status',
        backgroundColor = styleEqual(c('Active', 'Expired'), c('#d4edda', '#f8d7da')),
        color = styleEqual(c('Active', 'Expired'), c('#155724', '#721c24')),
        fontWeight = 'bold',
        textAlign = 'center'
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
