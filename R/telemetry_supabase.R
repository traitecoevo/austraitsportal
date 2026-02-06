#' Supabase REST API Telemetry Functions
#' Uses HTTP instead of PostgreSQL driver

library(httr2)
library(jsonlite)

# Global connection settings
supabase_url <- NULL
supabase_key <- NULL

#' Initialize Supabase Telemetry
init_supabase_telemetry <- function(url, key) {
  supabase_url <<- url
  supabase_key <<- key
  cat("✓ Supabase telemetry initialized\n")
}

#' Start session (log login event)
start_telemetry_session <- function() {
  if (is.null(supabase_url)) return(NULL)
  
  tryCatch({
    req <- request(paste0(supabase_url, "/rest/v1/telemetry_events")) |>
      req_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", supabase_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        type = "login",
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        session_id = paste0("session_", as.numeric(Sys.time()))
      )) |>
      req_perform()
  }, error = function(e) {
    warning("Telemetry session start failed: ", e$message)
  })
}

#' Log custom event
log_telemetry_event <- function(event_type, details = list()) {
  if (is.null(supabase_url)) return(NULL)
  
  tryCatch({
    req <- request(paste0(supabase_url, "/rest/v1/telemetry_events")) |>
      req_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", supabase_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        type = event_type,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        details = details
      )) |>
      req_perform()
  }, error = function(e) {
    warning("Telemetry log failed: ", e$message)
  })
}

#' Read telemetry metrics
read_telemetry_metrics <- function(from_date = "2020-01-01", to_date = NULL) {
  if (is.null(supabase_url)) return(tibble::tibble())
  
  if (is.null(to_date)) to_date <- as.character(Sys.Date() + 1)
  
  tryCatch({
    # Query with date range
    url <- paste0(
      supabase_url, 
      "/rest/v1/telemetry_events?",
      "timestamp=gte.", from_date,
      "&timestamp=lte.", to_date,
      "&order=timestamp.desc"
    )
    
    resp <- request(url) |>
      req_headers(
        "apikey" = supabase_key,
        "Authorization" = paste("Bearer", supabase_key)
      ) |>
      req_perform() |>
      resp_body_json()
    
    # Convert to data frame
    if (length(resp) > 0) {
      tibble::tibble(
        type = sapply(resp, function(x) if(is.null(x$type)) NA else x$type),
        timestamp = sapply(resp, function(x) if(is.null(x$timestamp)) NA else x$timestamp),
        details = sapply(resp, function(x) {
          if (!is.null(x$details)) jsonlite::toJSON(x$details, auto_unbox = TRUE) else ""
        })
      )
    } else {
      tibble::tibble(type = character(), timestamp = character(), details = character())
    }
  }, error = function(e) {
    warning("Telemetry read failed: ", e$message)
    tibble::tibble(type = character(), timestamp = character(), details = character())
  })
}