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

#' Get Play Reports
#'
#' @param limit Number of records to retrieve
#' @param offset Offset for pagination
#' @param team_slug Team slug (defaults to INSTRUQT_TEAM_SLUG env var)
#' @return List with play reports data and pagination info
get_play_reports <- function(limit = 50, offset = 0, team_slug = Sys.getenv("INSTRUQT_TEAM_SLUG")) {
  log_info("Fetching play reports (limit=", limit, ", offset=", offset, ")")

  query <- '
    query GetPlayReports($input: PlayReportInput!) {
      playReports(input: $input) {
        items {
          id
          track {
            id
            slug
            title
            tags
          }
          user {
            profile {
              email
            }
          }
          completionPercent
          timeSpent
          pauseDuration
          startedAt
          totalChallenges
          completedChallenges
        }
        totalItems
      }
    }
  '

  variables <- list(
    input = list(
      limit = limit,
      offset = offset,
      teamSlug = team_slug
    )
  )

  result <- execute_graphql_query(query, variables)

  if (!is.null(result$playReports$totalItems)) {
    log_info("Retrieved ", length(result$playReports$items), " of ", result$playReports$totalItems, " total play reports")
  }

  return(result$playReports)
}

#' Get All Play Reports (with pagination)
#'
#' @param max_records Maximum number of records to retrieve
#' @param page_size Number of records per page
#' @return Data frame with all play reports
get_all_play_reports <- function(max_records = 1000, page_size = 100) {
  log_info("Fetching all play reports (API returns complete dataset)")

  result <- get_play_reports(limit = max_records, offset = 0)

  if (is.null(result$items) || nrow(result$items) == 0) {
    return(data.frame())
  }

  items_df <- result$items

  # Extract nested data frames
  track_df <- items_df$track
  user_df <- items_df$user
  profile_df <- user_df$profile

  # Extract tags - handle both list and character vector formats
  tags_list <- track_df$tags
  if (is.list(tags_list)) {
    # Convert list of vectors to comma-separated strings
    tags_str <- sapply(tags_list, function(x) {
      if (is.null(x) || length(x) == 0) return("")
      paste(x, collapse = ", ")
    })
  } else {
    tags_str <- rep("", nrow(track_df))
  }

  # Build data frame
  df <- data.frame(
    id = items_df$id,
    trackId = track_df$id,
    trackSlug = track_df$slug,
    trackTitle = track_df$title,
    trackTags = tags_str,
    userEmail = profile_df$email,
    completionPercent = ifelse(is.na(items_df$completionPercent), 0, items_df$completionPercent),
    timeSpent = ifelse(is.na(items_df$timeSpent), 0, items_df$timeSpent),
    hoursConsumed = ifelse(is.na(items_df$timeSpent), 0, items_df$timeSpent / 3600),
    pauseDuration = ifelse(is.na(items_df$pauseDuration), 0, items_df$pauseDuration),
    started = items_df$startedAt,
    totalChallenges = ifelse(is.na(items_df$totalChallenges), 0, items_df$totalChallenges),
    completedChallenges = ifelse(is.na(items_df$completedChallenges), 0, items_df$completedChallenges),
    state = ifelse(items_df$completionPercent >= 100, "completed",
                  ifelse(items_df$completionPercent > 0, "started", "not_started")),
    stringsAsFactors = FALSE
  )

  # Remove any duplicate records by ID
  df <- dplyr::distinct(df, id, .keep_all = TRUE)

  log_info("Retrieved ", nrow(df), " unique play reports")

  return(df)
}

#' Get All Tracks with Metadata
#'
#' @param team_slug Team slug (defaults to INSTRUQT_TEAM_SLUG env var)
#' @return Data frame with track metadata including owner, created/modified dates, and description
get_all_tracks <- function(team_slug = Sys.getenv("INSTRUQT_TEAM_SLUG")) {
  log_info("Fetching all tracks for team: ", team_slug)

  query <- '
    query GetTracks($teamSlug: String!) {
      team(teamSlug: $teamSlug) {
        tracks {
          id
          slug
          title
          description
          status
          owner
          created
          createdAt
          last_update
          updated_by {
            profile {
              email
            }
          }
          developers {
            profile {
              email
            }
          }
          tags
        }
      }
    }
  '

  variables <- list(
    teamSlug = team_slug
  )

  result <- execute_graphql_query(query, variables)

  if (is.null(result$team$tracks) || length(result$team$tracks) == 0) {
    log_error("No tracks returned")
    return(data.frame())
  }

  tracks <- result$team$tracks

  # Extract updated_by information
  updated_by_emails <- sapply(tracks$updated_by, function(x) {
    if (is.null(x) || is.null(x$profile) || is.null(x$profile$email)) return("")
    x$profile$email
  })

  # Extract developers information (team access)
  developers_list <- sapply(tracks$developers, function(devs) {
    if (is.null(devs) || length(devs) == 0) return("")

    # Handle both data frame and list formats
    if (is.data.frame(devs)) {
      emails <- devs$profile$email
    } else if (is.list(devs)) {
      emails <- sapply(devs, function(dev) {
        if (is.null(dev$profile) || is.null(dev$profile$email)) return(NA)
        dev$profile$email
      })
    } else {
      return("")
    }

    # Remove NA values and paste together
    emails <- emails[!is.na(emails)]
    if (length(emails) == 0) return("")
    paste(emails, collapse = ", ")
  })

  # Handle tags - convert list to comma-separated strings
  tags_list <- tracks$tags
  if (is.list(tags_list)) {
    tags_str <- sapply(tags_list, function(x) {
      if (is.null(x) || length(x) == 0) return("")
      paste(x, collapse = ", ")
    })
  } else {
    tags_str <- rep("", length(tracks$id))
  }

  # Build data frame
  df <- data.frame(
    id = tracks$id,
    slug = tracks$slug,
    title = tracks$title,
    description = ifelse(is.na(tracks$description) | is.null(tracks$description), "", tracks$description),
    status = tracks$status,
    owner = ifelse(is.na(tracks$owner) | is.null(tracks$owner), "", tracks$owner),
    team_access = developers_list,
    created = ifelse(is.na(tracks$created) | is.null(tracks$created), "", tracks$created),
    created_at = ifelse(is.na(tracks$createdAt) | is.null(tracks$createdAt), "", tracks$createdAt),
    last_update = ifelse(is.na(tracks$last_update) | is.null(tracks$last_update), "", tracks$last_update),
    updated_by_email = updated_by_emails,
    tags = tags_str,
    stringsAsFactors = FALSE
  )

  log_info("Retrieved ", nrow(df), " tracks")

  return(df)
}

#' Get All Track Invites
#'
#' @param team_slug Team slug (defaults to INSTRUQT_TEAM_SLUG env var)
#' @param play_reports Optional play reports data to calculate usage stats
#' @return Data frame with invite information including claims and usage
get_all_invites <- function(team_slug = Sys.getenv("INSTRUQT_TEAM_SLUG"), play_reports = NULL) {
  log_info("Fetching all invites for team: ", team_slug)

  query <- '
    query GetInvites($teamSlug: String!) {
      trackInvites(teamSlug: $teamSlug) {
        id
        title
        publicTitle
        created
        authors {
          id
          user {
            profile {
              email
            }
          }
        }
        last_updated
        expiresAt
        allowAnonymous
        inviteLimit
        inviteCount
        status
        tracks {
          id
          slug
          title
          tags
        }
      }
    }
  '

  variables <- list(
    teamSlug = team_slug
  )

  result <- execute_graphql_query(query, variables)

  if (is.null(result$trackInvites) || length(result$trackInvites) == 0) {
    log_error("No invites returned")
    return(data.frame())
  }

  invites <- result$trackInvites

  # Process each invite
  invites_list <- lapply(seq_len(nrow(invites)), function(i) {
    invite <- invites[i, ]

    # Extract tracks info (invites can have multiple tracks)
    tracks_data <- invite$tracks

    # Handle the list structure - tracks_data is a list containing a data.frame
    if (!is.null(tracks_data) && is.list(tracks_data) && length(tracks_data) > 0) {
      # Extract the first element which should be the data.frame
      if (is.data.frame(tracks_data[[1]])) {
        tracks_df <- tracks_data[[1]]
      } else {
        tracks_df <- NULL
      }
    } else if (!is.null(tracks_data) && is.data.frame(tracks_data)) {
      # Direct data.frame (shouldn't happen based on API structure, but handle it)
      tracks_df <- tracks_data
    } else {
      tracks_df <- NULL
    }

    # Check if we have valid tracks
    has_tracks <- !is.null(tracks_df) && is.data.frame(tracks_df) && nrow(tracks_df) > 0

    if (has_tracks) {
      track_id <- tracks_df$id[1]
      track_slug <- tracks_df$slug[1]
      track_title <- tracks_df$title[1]
      # Handle tags which is a list column
      tags_data <- tracks_df$tags[[1]]
      if (!is.null(tags_data) && length(tags_data) > 0) {
        track_tags <- paste(tags_data, collapse = ", ")
      } else {
        track_tags <- ""
      }
      num_tracks <- nrow(tracks_df)
    } else {
      track_id <- ""
      track_slug <- ""
      track_title <- ""
      track_tags <- ""
      num_tracks <- 0
    }

    # Get claim count - handle NA values
    num_claims <- ifelse(is.null(invite$inviteCount) || is.na(invite$inviteCount), 0, invite$inviteCount)
    max_claims <- ifelse(is.null(invite$inviteLimit) || is.na(invite$inviteLimit), Inf, invite$inviteLimit)

    # Calculate usage stats from play reports if provided
    total_plays <- 0
    total_hours <- 0
    unique_users <- 0

    if (!is.null(play_reports) && nrow(play_reports) > 0) {
      # Filter play reports for this invite (need to match by invite_id if available)
      # For now, we'll estimate based on track if invite metadata not in play reports
      # This is a limitation - may need to enhance play reports query
    }

    # Safely extract scalar values with NA handling
    invite_id <- ifelse(is.null(invite$id) || is.na(invite$id), "", as.character(invite$id))
    invite_title_val <- ifelse(is.null(invite$title) || is.na(invite$title) || invite$title == "",
                                ifelse(is.null(invite$publicTitle) || is.na(invite$publicTitle), "", as.character(invite$publicTitle)),
                                as.character(invite$title))
    public_title_val <- ifelse(is.null(invite$publicTitle) || is.na(invite$publicTitle), "", as.character(invite$publicTitle))
    created_val <- ifelse(is.null(invite$created) || is.na(invite$created), "", as.character(invite$created))

    # Extract authors/creators emails (can be multiple)
    # authors is a list containing a data.frame (same structure as tracks)
    created_by_val <- ""
    if (!is.null(invite$authors) && is.list(invite$authors) && length(invite$authors) > 0) {
      # Extract the data.frame from the list
      if (is.data.frame(invite$authors[[1]])) {
        authors_df <- invite$authors[[1]]
        # Try to extract email from nested user profile
        author_emails <- sapply(seq_len(nrow(authors_df)), function(j) {
          author <- authors_df[j, ]
          # Try to get email from user.profile.email
          if (!is.null(author$user) && is.list(author$user) &&
              !is.null(author$user$profile) && !is.null(author$user$profile$email)) {
            return(as.character(author$user$profile$email))
          }
          # Fallback: return ID
          if (!is.null(author$id) && !is.na(author$id)) {
            return(as.character(author$id))
          }
          return(NA)
        })
        # Remove NAs and combine into comma-separated string
        author_emails <- author_emails[!is.na(author_emails) & author_emails != ""]
        if (length(author_emails) > 0) {
          created_by_val <- paste(author_emails, collapse = ", ")
        }
      }
    }

    last_updated_val <- ifelse(is.null(invite$last_updated) || is.na(invite$last_updated), "", as.character(invite$last_updated))
    expires_at_val <- ifelse(is.null(invite$expiresAt) || is.na(invite$expiresAt), "", as.character(invite$expiresAt))
    allow_anonymous_val <- ifelse(is.null(invite$allowAnonymous) || is.na(invite$allowAnonymous), FALSE, as.logical(invite$allowAnonymous))
    status_val <- ifelse(is.null(invite$status) || is.na(invite$status), "", as.character(invite$status))


    data.frame(
      invite_id = invite_id,
      invite_title = invite_title_val,
      public_title = public_title_val,
      track_id = track_id,
      track_slug = track_slug,
      track_title = track_title,
      track_tags = track_tags,
      num_tracks = num_tracks,
      created = created_val,
      created_by = created_by_val,
      last_updated = last_updated_val,
      expires_at = expires_at_val,
      status = status_val,
      max_claims = max_claims,
      allow_anonymous = allow_anonymous_val,
      num_claims = num_claims,
      unique_users = unique_users,
      total_plays = total_plays,
      hours_consumed = total_hours,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, invites_list)

  log_info("Retrieved ", nrow(df), " invites")

  return(df)
}
