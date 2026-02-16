# Tests for telemetry functions

test_that("init_supabase_telemetry sets options", {
  init_supabase_telemetry("test_url", "test_key")
  
  expect_equal(getOption("supabase_telemetry_url"), "test_url")
  expect_equal(getOption("supabase_telemetry_key"), "test_key")
  
  # Clean up
  options(supabase_telemetry_url = NULL)
  options(supabase_telemetry_key = NULL)
})

test_that("start_telemetry_session works in local mode", {
  options(telemetry_mode = "local")
  
  # Mock telemetry object
  mock_telemetry <- list(
    start_session = function(track_inputs = FALSE) {
      return("session_started")
    }
  )
  options(telemetry_object = mock_telemetry)
  
  expect_silent(start_telemetry_session())
  
  # Clean up
  options(telemetry_mode = NULL)
  options(telemetry_object = NULL)
})

test_that("start_telemetry_session handles missing credentials in cloud mode", {
  options(telemetry_mode = "cloud")
  options(supabase_telemetry_url = NULL)
  options(supabase_telemetry_key = NULL)
  
  expect_silent(start_telemetry_session())
  
  # Clean up
  options(telemetry_mode = NULL)
})

test_that("log_telemetry_event works in local mode", {
  options(telemetry_mode = "local")
  
  # Mock telemetry object
  logged_events <- list()
  mock_telemetry <- list(
    log_custom_event = function(event_type, details = list()) {
      logged_events <<- c(logged_events, list(list(type = event_type, details = details)))
      return(TRUE)
    }
  )
  options(telemetry_object = mock_telemetry)
  
  log_telemetry_event("test_event", list(key = "value"))
  
  expect_length(logged_events, 1)
  expect_equal(logged_events[[1]]$type, "test_event")
  
  # Clean up
  options(telemetry_mode = NULL)
  options(telemetry_object = NULL)
})

test_that("log_telemetry_event handles missing credentials gracefully", {
  options(telemetry_mode = "cloud")
  options(supabase_telemetry_url = NULL)
  options(supabase_telemetry_key = NULL)
  
  # Should not error, just return silently
  expect_silent(log_telemetry_event("test_event"))
  
  # Clean up
  options(telemetry_mode = NULL)
})

test_that("read_telemetry_metrics returns empty data frame when no data", {
  options(telemetry_mode = "local")
  
  # Mock telemetry object that returns empty data
  mock_telemetry <- list(
    data_storage = list(
      read_event_data = function(from_date, to_date) {
        return(tibble::tibble(
          type = character(),
          timestamp = character(),
          details = character()
        ))
      }
    )
  )
  options(telemetry_object = mock_telemetry)
  
  result <- read_telemetry_metrics("2020-01-01", "2020-12-31")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  
  # Clean up
  options(telemetry_mode = NULL)
  options(telemetry_object = NULL)
})

test_that("read_telemetry_metrics handles cloud mode with missing credentials", {
  options(telemetry_mode = "cloud")
  options(supabase_telemetry_url = NULL)
  options(supabase_telemetry_key = NULL)
  
  result <- read_telemetry_metrics("2020-01-01", "2020-12-31")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  
  # Clean up
  options(telemetry_mode = NULL)
})

test_that("telemetry functions handle NULL date gracefully", {
  options(telemetry_mode = "local")
  
  mock_telemetry <- list(
    data_storage = list(
      read_event_data = function(from_date, to_date) {
        expect_false(is.null(to_date))  # Should be filled with current date + 1
        return(tibble::tibble(
          type = character(),
          timestamp = character(),
          details = character()
        ))
      }
    )
  )
  options(telemetry_object = mock_telemetry)
  
  result <- read_telemetry_metrics("2020-01-01", NULL)
  
  expect_s3_class(result, "data.frame")
  
  # Clean up
  options(telemetry_mode = NULL)
  options(telemetry_object = NULL)
})
