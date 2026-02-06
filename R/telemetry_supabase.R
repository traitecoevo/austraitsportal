#' Supabase REST API Telemetry Functions
#' Uses HTTP instead of PostgreSQL driver

library(httr2)
library(jsonlite)

#' Initialize Supabase Telemetry
init_supabase_telemetry <- function(url, key) {
  options(supabase_telemetry_url = url)
  options(supabase_telemetry_key = key)
  cat("✓ Supabase telemetry initialized\n")
}

#' Start session (log login event)
start_telemetry_session <- function() {
  url <- getOption("supabase_telemetry_url")
  key <- getOption("supabase_telemetry_key")
  
  if (is.null(url) || is.null(key)) return(NULL)
  
  tryCatch({
    request(paste0(url, "/rest/v1/telemetry_events")) |>
      req_headers(
        "apikey" = key,
        "Authorization" = paste("Bearer", key),
        "Content-Type" = "application/json",
        "Prefer" = "return=minimal"
      ) |>
      req_body_json(list(
        type = "login",
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        session_id = paste0("session_", as.numeric(Sys.time()))
      )) |>
      req_perform()
  }, error = function(e) {
    warning("Telemetry session start failed: ", e$message)
  })
}

#' Log custom event
log_telemetry_event <- function(event_type, details = list()) {
  url <- getOption("supabase_telemetry_url")
  key <- getOption("supabase_telemetry_key")
  
  if (is.null(url) || is.null(key)) return(NULL)
  
  tryCatch({
    request(paste0(url, "/rest/v1/telemetry_events")) |>
      req_headers(
        "apikey" = key,
        "Authorization" = paste("Bearer", key),
        "Content-Type" = "application/json",
        "Prefer" = "return=minimal"
      ) |>
      req_body_json(list(
        type = event_type,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        details = if(length(details) > 0) details else NULL
      )) |>
      req_perform()
  }, error = function(e) {
    warning("Telemetry log failed (", event_type, "): ", e$message)
  })
}

#' Read telemetry metrics
read_telemetry_metrics <- function(from_date = "2020-01-01", to_date = NULL) {
  url <- getOption("supabase_telemetry_url")
  key <- getOption("supabase_telemetry_key")
  
  if (is.null(url) || is.null(key)) {
    return(tibble::tibble(type = character(), timestamp = character(), details = character()))
  }
  
  if (is.null(to_date)) to_date <- as.character(Sys.Date() + 1)
  
  tryCatch({
    query_url <- paste0(
      url, 
      "/rest/v1/telemetry_events?",
      "timestamp=gte.", from_date,
      "&timestamp=lte.", to_date,
      "&order=timestamp.desc"
    )
    
    resp <- request(query_url) |>
      req_headers(
        "apikey" = key,
        "Authorization" = paste("Bearer", key)
      ) |>
      req_perform() |>
      resp_body_json()
    
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