#!/usr/bin/env Rscript

#' Benchmarking script for AusTraits Portal query performance
#' 
#' Run this script to test and benchmark the speed of various filtering
#' and querying operations outside the Shiny app context.
#' 
#' Usage: Rscript benchmark_queries.R

library(dplyr)
library(duckdb)
library(arrow)
library(microbenchmark)
library(ggplot2)

# ════════════════════════════════════════════════════════════════
# SETUP
# ════════════════════════════════════════════════════════════════

cat("\n📊 AusTraits Portal Query Benchmarking\n")
cat(paste0("=", strrep("=", 50), "\n\n"))

# Load the package to get access to data and functions
devtools::load_all()

# Check that data is loaded
if (!exists("austraits_display_duckdb")) {
  stop("Data not loaded. Please run devtools::load_all() first.")
}

cat("✓ Data loaded\n")
cat("✓ DuckDB connection active\n\n")

# ════════════════════════════════════════════════════════════════
# BENCHMARK 1: COUNT OPERATION
# ════════════════════════════════════════════════════════════════

cat("📈 BENCHMARK 1: Count operation (basic dplyr pipeline)\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_count <- microbenchmark(
  "count() + collect() + pull()" = {
    austraits_display_duckdb |> 
      dplyr::count() |> 
      dplyr::collect() |> 
      dplyr::pull(n)
  },
  "nrow(collect())" = {
    austraits_display_duckdb |> 
      dplyr::collect() |> 
      nrow()
  },
  "SQL COUNT query" = {
    DBI::dbGetQuery(duckdb_con, 
      "SELECT COUNT(*) as n FROM austraits_display") |>
      pull(n)
  },
  times = 10
)

print(benchmark_count)
cat("\n")

# ════════════════════════════════════════════════════════════════
# BENCHMARK 2: FILTERING OPERATIONS
# ════════════════════════════════════════════════════════════════

cat("📈 BENCHMARK 2: Single filter conditions\n")
cat(paste0("-", strrep("-", 48), "\n"))

# Create sample filter inputs
filter_input <- list(
  trait_name = c("wood_density", "leaf_area"),
  basis_of_record = "measurement",
  life_stage = "adult"
)

benchmark_filters <- microbenchmark(
  "Single trait filter" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Single BOR filter" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Combined filters" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  times = 10
)

print(benchmark_filters)
cat("\n")

# ════════════════════════════════════════════════════════════════
# BENCHMARK 3: FILTERING + DATA LOAD
# ════════════════════════════════════════════════════════════════

cat("📈 BENCHMARK 3: Filter + data loading strategies\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_load <- microbenchmark(
  "Load all rows" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::collect()
  },
  "Load first 100 rows with head()" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      head(100) |>
      dplyr::collect()
  },
  "Collect then filter rows" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::collect() |>
      head(100)
  },
  times = 10
)

print(benchmark_load)
cat("\n")

# ════════════════════════════════════════════════════════════════
# BENCHMARK 5: DISTINCT VALUE RETRIEVAL
# ════════════════════════════════════════════════════════════════

cat("📈 BENCHMARK 5: Distinct value retrieval (for dropdowns)\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_distinct <- microbenchmark(
  "Get distinct trait_names" = {
    austraits_display_duckdb |>
      dplyr::select(trait_name) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1) |>
      sort()
  },
  "Get distinct basis_of_record" = {
    austraits_display_duckdb |>
      dplyr::select(basis_of_record) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1) |>
      sort()
  },
  "Get distinct taxon_name (large)" = {
    austraits_display_duckdb |>
      dplyr::select(taxon_name) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1) |>
      sort()
  },
  times = 10
)

print(benchmark_distinct)
cat("\n")

# ════════════════════════════════════════════════════════════════
# BENCHMARK 6: APPLIED FILTERS PIPELINE (Real-world scenario)
# ════════════════════════════════════════════════════════════════

cat("📈 BENCHMARK 6: Full applied filter pipeline (real-world)\n")
cat(paste0("-", strrep("-", 48), "\n"))

# Simulate actual app filters
real_filters <- list(
  trait_name = c("wood_density", "leaf_area", "seed_mass"),
  basis_of_record = "measurement",
  life_stage = "adult",
  min_latitude = -30,
  max_latitude = -20,
  min_longitude = 145,
  max_longitude = 155
)

benchmark_full <- microbenchmark(
  "Apply all filters + count" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area|seed_mass)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::filter(.data$`latitude (deg)` != "NA") |>
      dplyr::filter(.data$`longitude (deg)` != "NA") |>
      dplyr::filter(as.numeric(.data$`latitude (deg)`) >= -30) |>
      dplyr::filter(as.numeric(.data$`latitude (deg)`) <= -20) |>
      dplyr::filter(as.numeric(.data$`longitude (deg)`) >= 145) |>
      dplyr::filter(as.numeric(.data$`longitude (deg)`) <= 155) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Apply filters + load 100 rows" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area|seed_mass)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::filter(.data$`latitude (deg)` != "NA") |>
      dplyr::filter(.data$`longitude (deg)` != "NA") |>
      dplyr::filter(as.numeric(.data$`latitude (deg)`) >= -30) |>
      dplyr::filter(as.numeric(.data$`latitude (deg)`) <= -20) |>
      dplyr::filter(as.numeric(.data$`longitude (deg)`) >= 145) |>
      dplyr::filter(as.numeric(.data$`longitude (deg)`) <= 155) |>
      head(100) |>
      dplyr::collect()
  },
  times = 5
)

print(benchmark_full)
cat("\n")

# ════════════════════════════════════════════════════════════════
# SUMMARY STATISTICS
# ════════════════════════════════════════════════════════════════

cat("📋 SUMMARY\n")
cat(paste0("=", strrep("=", 50), "\n"))

# Get total rows in dataset
total_rows <- austraits_display_duckdb |>
  dplyr::count() |>
  dplyr::collect() |>
  dplyr::pull(n)

cat(sprintf("Total rows in austraits_display: %s\n", 
            format(total_rows, big.mark = ",")))

# Count rows for species dataset
species_rows <- austraits_species_display_duckdb |>
  dplyr::count() |>
  dplyr::collect() |>
  dplyr::pull(n)

cat(sprintf("Total rows in austraits_species_display: %s\n", 
            format(species_rows, big.mark = ",")))

cat("\n✓ Benchmarking complete!\n\n")

# ════════════════════════════════════════════════════════════════
# EXPORT RESULTS (Optional)
# ════════════════════════════════════════════════════════════════

cat("💾 To save these results, run:\n")
cat("  saveRDS(benchmark_count, 'benchmark_count.rds')\n")
cat("  saveRDS(benchmark_filters, 'benchmark_filters.rds')\n")
cat("  # etc.\n\n")
