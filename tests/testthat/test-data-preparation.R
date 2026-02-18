# Tests for data preparation functions

test_that("prepare_data_for_portal creates required files", {
  
  # This is an integration test that would require a full database
  # Typically run manually during data preparation
  temp_dir <- tempdir()
  
  # Mock austraits object would be needed here
  expect_no_failure({
    austraits:::austraits_5.0.0_lite |>
      prepare_data_for_portal(temp_dir, overwrite = TRUE)
  })
  
  # Expected output files
  files <- c(
    "austraits-data.parquet",
    "austraits-display.parquet",
    "austraits-species-averages-display.parquet",
    "austraits-species-averages.parquet",
    "definitions.rds",
    "dropdown_cache.rds",
    "flora_links.rds",
    "metadata.rds",
    "sources.bib",
    "sources.rds",
    "trait_groups.rds"
  )
  
  # Check that expected files are created
  for (file in files) {
    expect_true(file.exists(file.path(temp_dir, file)), info = paste("Missing expected file:", file))
  }
})
