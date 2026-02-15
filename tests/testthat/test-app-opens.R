test_that("Can the app run successfully?", {
  expect_visible(shiny::shinyApp(ui = app_ui, server = app_server))
})
