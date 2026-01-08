test_that("app ui works", {
  ui <- app_ui()
  expect_type(ui, "list")
})

test_that("app server works", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have correct parameters
  fmls <- formals(app_server)
  expect_true("input" %in% names(fmls))
  expect_true("output" %in% names(fmls))
  expect_true("session" %in% names(fmls))
})

test_that("app launches", {
  # Simple test that app doesn't crash on startup
  expect_error(app_ui(), NA)
})