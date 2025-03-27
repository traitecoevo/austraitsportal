test_that("Context search by app", {
  
  # Create baseline data
  test_fire_contexts_baseline <- austraits |> 
    extract_data(table = "contexts",
                 col = "context_property", 
                 col_value = "fire") |> 
    format_database_for_download() |> # We have extra columns from global.R
    filter(dataset_id == "Nicholson_2017")
  
  # Read in app generated data 
  app_fire_contexts <- readr::read_csv("test_data/app_fire_contexts_Nicholson_2017.csv")

  expect_equal(ncol(test_fire_contexts_baseline), ncol(app_fire_contexts))
})
