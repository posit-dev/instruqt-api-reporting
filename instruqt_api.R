# Instruqt API Client
# Functions for fetching Instruqt consumption data

library(httr)
library(jsonlite)
library(dplyr)

# Global debug flag - set to TRUE to enable verbose logging
DEBUG_MODE <- Sys.getenv("INSTRUQT_DEBUG", "FALSE") == "TRUE"

#' Log debug message
#'
#' @param ... Messages to log
log_debug <- function(...) {
  if (DEBUG_MODE) {
    message("[DEBUG] ", ...)
  }
}

#' Log info message
#'
#' @param ... Messages to log
log_info <- function(...) {
  message("[INFO] ", ...)
}

#' Log error message
#'
#' @param ... Messages to log
log_error <- function(...) {
  message("[ERROR] ", ...)
}

#' Execute GraphQL Query
#'
#' @param query GraphQL query string
#' @param variables Named list of variables for the query
#' @param api_key Instruqt API key (defaults to INSTRUQT_API_KEY env var)
#' @return Parsed JSON response
execute_graphql_query <- function(query, variables = NULL, api_key = Sys.getenv("INSTRUQT_API_KEY")) {
  if (api_key == "") {
    log_error("API key is required. Set INSTRUQT_API_KEY environment variable or pass api_key parameter.")
    stop("API key is required. Set INSTRUQT_API_KEY environment variable or pass api_key parameter.")
  }

  endpoint <- "https://play.instruqt.com/graphql"

  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }

  log_debug("Executing GraphQL query...")
  log_debug("Variables: ", toJSON(variables, auto_unbox = TRUE))

  if (DEBUG_MODE) {
    log_debug("Query:\n", query)
  }

  response <- tryCatch({
    POST(
      endpoint,
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw"
    )
  }, error = function(e) {
    log_error("HTTP request failed: ", e$message)
    stop(e)
  })

  status <- status_code(response)
  log_debug("Response status: ", status)

  if (status != 200) {
    error_content <- content(response, "text")
    log_error("API request failed with status ", status)
    log_error("Response: ", error_content)
    stop(sprintf("API request failed with status %d: %s", status, error_content))
  }

  result <- tryCatch({
    fromJSON(content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    log_error("Failed to parse JSON response: ", e$message)
    log_error("Raw response: ", content(response, "text"))
    stop(e)
  })

  if (!is.null(result$errors)) {
    log_error("GraphQL errors received")
    log_error(toJSON(result$errors, auto_unbox = TRUE, pretty = TRUE))
    stop(sprintf("GraphQL errors: %s", toJSON(result$errors, auto_unbox = TRUE)))
  }

  log_debug("Query executed successfully")
  return(result$data)
}

#' Get Consumption Details (Daily Breakdown)
#'
#' @param start_date Start date (ISO 8601 format, e.g., "2025-05-11T00:00:00Z")
#' @param end_date End date (ISO 8601 format, e.g., "2025-11-07T23:59:59Z")
#' @param play_type Play type filter: "ALL", "NORMAL", or "DEVELOPER" (defaults to "ALL")
#' @param team_slug Team slug (defaults to INSTRUQT_TEAM_SLUG env var)
#' @return Data frame with daily consumption data
get_consumption_details <- function(start_date, end_date, play_type = "ALL", team_slug = Sys.getenv("INSTRUQT_TEAM_SLUG")) {
  log_info("Fetching consumption data from ", start_date, " to ", end_date)

  query <- '
    query GetConsumption($teamSlug: String!, $input: PlayInsightSeriesInput!) {
      team(teamSlug: $teamSlug) {
        insights {
          consumption {
            series(input: $input) {
              granularity
              totalSandboxTime {
                items {
                  bucketStartTime
                  value
                }
              }
              totalHotStartTime {
                items {
                  bucketStartTime
                  value
                }
              }
            }
          }
        }
      }
    }
  '

  variables <- list(
    teamSlug = team_slug,
    input = list(
      dateRangeFilter = list(
        from = start_date,
        to = end_date
      ),
      playType = play_type,
      granularity = "DAILY"
    )
  )

  result <- execute_graphql_query(query, variables)

  if (is.null(result$team$insights$consumption$series$totalSandboxTime$items)) {
    log_error("No consumption data returned")
    return(data.frame(date = character(), sandboxHours = numeric(), hotStartHours = numeric()))
  }

  sandbox_items <- result$team$insights$consumption$series$totalSandboxTime$items
  hotstart_items <- result$team$insights$consumption$series$totalHotStartTime$items

  # Convert to data frame with hours
  df <- data.frame(
    date = sandbox_items$bucketStartTime,
    sandboxHours = sapply(sandbox_items$value, function(x) ifelse(is.null(x), 0, x / 3600)),
    hotStartHours = sapply(hotstart_items$value, function(x) ifelse(is.null(x), 0, x / 3600)),
    stringsAsFactors = FALSE
  )

  log_info("Retrieved consumption data for ", nrow(df), " days")

  return(df)
}
