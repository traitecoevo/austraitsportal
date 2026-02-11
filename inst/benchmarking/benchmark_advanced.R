#!/usr/bin/env Rscript

#' Advanced benchmarking script for AusTraits Portal
#' 
#' This script provides deeper performance analysis including:
#' - Memory usage tracking
#' - Query plan analysis
#' - Comparison with alternative approaches
#' - Profiling of helper functions

library(dplyr)
library(duckdb)
library(arrow)
library(microbenchmark)
library(profvis)

# ════════════════════════════════════════════════════════════════
# SETUP
# ════════════════════════════════════════════════════════════════

cat("\n🔬 AusTraits Portal - Advanced Benchmarking\n")
cat(paste0("=", strrep("=", 50), "\n\n"))

# Load package and data
devtools::load_all()

if (!exists("austraits_display_duckdb")) {
  stop("Data not loaded.")
}

# ════════════════════════════════════════════════════════════════
# TEST 1: STRING MATCHING PERFORMANCE
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 1: String matching patterns for filters\n")
cat(paste0("-", strrep("-", 48), "\n"))

traits <- c("wood_density", "leaf_area", "seed_mass", "plant_height")
pattern_1 <- paste(traits, collapse = "|")
pattern_2 <- paste0("^(", pattern_1, ")$")

benchmark_patterns <- microbenchmark(
  "Basic OR pattern (unanchored)" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, pattern_1)) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Anchored pattern (^...$)" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, pattern_2)) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Fixed string matching" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "wood_density")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  times = 15
)

print(benchmark_patterns)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 2: LAZY EVALUATION BENEFITS
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 2: Lazy evaluation - count before collect\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_lazy <- microbenchmark(
  "Count BEFORE collect (lazy)" = {
    filtered_query <- austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$"))
    
    total <- filtered_query |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
    
    if (total > 100) {
      data <- filtered_query |> head(100) |> dplyr::collect()
    } else {
      data <- filtered_query |> dplyr::collect()
    }
    
    list(total = total, rows = nrow(data))
  },
  "Collect THEN count (eager)" = {
    data <- austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::collect()
    
    list(total = nrow(data), rows = min(100, nrow(data)))
  },
  times = 10
)

print(benchmark_lazy)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 3: FILTER ORDER IMPACT
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 3: Impact of filter order on performance\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_order <- microbenchmark(
  "Trait filter FIRST (selective)" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "BOR filter FIRST (less selective)" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  times = 15
)

print(benchmark_order)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 4: HEAD() VS LIMIT() FOR ROW LIMITING
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 4: head() vs collect-then-limit for limiting rows\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_head <- microbenchmark(
  "head(100) before collect" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      head(100) |>
      dplyr::collect()
  },
  "collect all then head(100)" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::collect() |>
      head(100)
  },
  times = 15
)

print(benchmark_head)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 5: DISTINCT VALUE RETRIEVAL OPTIMIZATION
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 5: Distinct value retrieval optimization\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_distinct_opt <- microbenchmark(
  "Standard distinct + sort" = {
    austraits_display_duckdb |>
      dplyr::select(basis_of_record) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1) |>
      sort()
  },
  "dplyr distinct + arrange" = {
    austraits_display_duckdb |>
      dplyr::select(basis_of_record) |>
      dplyr::distinct() |>
      dplyr::arrange(basis_of_record) |>
      dplyr::collect() |>
      dplyr::pull(basis_of_record)
  },
  times = 15
)

print(benchmark_distinct_opt)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 6: SPECIES vs OBSERVATION DATASET PERFORMANCE
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 6: Performance comparison - Species vs Observations\n")
cat(paste0("-", strrep("-", 48), "\n"))

benchmark_datasets <- microbenchmark(
  "Observations - all rows count" = {
    austraits_display_duckdb |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Species - all rows count" = {
    austraits_species_display_duckdb |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Observations - filtered count" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "Species - filtered count" = {
    austraits_species_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  times = 10
)

print(benchmark_datasets)
cat("\n")

# ════════════════════════════════════════════════════════════════
# TEST 6B: LAZY vs IN-MEMORY (THE BIG QUESTION!)
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 6B: Lazy DuckDB vs In-Memory Data Frame\n")
cat(paste0("-", strrep("-", 48), "\n"))
cat("Loading full dataset into memory for comparison...\n")

# Load entire dataset into memory
load_start <- Sys.time()
austraits_in_memory <- austraits_display_duckdb |> dplyr::collect()
load_time <- as.numeric(Sys.time() - load_start, units = "secs")
mem_size <- format(object.size(austraits_in_memory), units = "MB")

cat(sprintf("  ✓ Loaded: %s in %.1f seconds\n", mem_size, load_time))
cat(sprintf("  ✓ Rows: %s\n", format(nrow(austraits_in_memory), big.mark = ",")))
cat(sprintf("  ✓ Cols: %d\n\n", ncol(austraits_in_memory)))

# Test 1: Simple filtering + count
cat("Scenario 1: Filter + Count (common operation)\n")
benchmark_lazy_vs_memory_count <- microbenchmark(
  "Lazy: filter + count" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "In-memory: filter + count" = {
    austraits_in_memory |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      nrow()
  },
  times = 20
)
print(benchmark_lazy_vs_memory_count)
cat("\n")

# Test 2: Filtering + collecting data
cat("Scenario 2: Filter + Collect 1000 rows\n")
benchmark_lazy_vs_memory_collect <- microbenchmark(
  "Lazy: filter + collect 1000" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      head(1000) |>
      dplyr::collect()
  },
  "In-memory: filter + head 1000" = {
    austraits_in_memory |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
      head(1000)
  },
  times = 20
)
print(benchmark_lazy_vs_memory_collect)
cat("\n")

# Test 3: Complex multi-filter query
cat("Scenario 3: Complex multi-filter query + count\n")
benchmark_lazy_vs_memory_complex <- microbenchmark(
  "Lazy: multi-filter + count" = {
    austraits_display_duckdb |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      dplyr::count() |>
      dplyr::collect() |>
      dplyr::pull(n)
  },
  "In-memory: multi-filter + count" = {
    austraits_in_memory |>
      dplyr::filter(stringr::str_detect(trait_name, "^(wood_density|leaf_area)$")) |>
      dplyr::filter(stringr::str_detect(basis_of_record, "^(measurement)$")) |>
      dplyr::filter(stringr::str_detect(life_stage, "^(adult)$")) |>
      nrow()
  },
  times = 20
)
print(benchmark_lazy_vs_memory_complex)
cat("\n")

# Test 4: Distinct values (for dropdowns)
cat("Scenario 4: Get distinct values (dropdown population)\n")
benchmark_lazy_vs_memory_distinct <- microbenchmark(
  "Lazy: distinct + collect" = {
    austraits_display_duckdb |>
      dplyr::select(basis_of_record) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1)
  },
  "In-memory: distinct" = {
    austraits_in_memory |>
      dplyr::select(basis_of_record) |>
      dplyr::distinct() |>
      dplyr::pull(1)
  },
  times = 20
)
print(benchmark_lazy_vs_memory_distinct)
cat("\n")

cat("💡 ANALYSIS:\n")
cat(paste0("-", strrep("-", 48), "\n"))
cat(sprintf("Initial load cost: %.1f seconds, %s memory per session\n", load_time, mem_size))
cat("\nFor Shiny app with multiple concurrent users:\n")
cat("  • Lazy approach: ~50MB × N users = manageable\n")
cat(sprintf("  • In-memory approach: %s × N users = potential issue\n\n", mem_size))

# ════════════════════════════════════════════════════════════════
# PROFILE: Helper function performance
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 7: Profiling helper functions\n")
cat(paste0("-", strrep("-", 48), "\n"))

# Create a sample filter input
sample_filter <- list(
  trait_name = c("wood_density", "leaf_area"),
  basis_of_record = "measurement",
  life_stage = "adult",
  location = NULL,
  min_latitude = NULL,
  max_latitude = NULL,
  min_longitude = NULL,
  max_longitude = NULL,
  custom_col_1 = NULL,
  custom_val_1 = NULL
)

cat("\n⏱️  Profiling apply_filters_categorical function...\n")
cat("(This may take a few seconds)\n\n")

prof_result <- tryCatch({
  profvis::profvis({
    for (i in 1:20) {
      austraits_display_duckdb |>
        apply_filters_categorical(sample_filter) |>
        dplyr::count() |>
        dplyr::collect() |>
        dplyr::pull(n)
    }
  }, interval = 0.005)
}, error = function(e) {
  cat("Note: Profiling encountered an error (may be due to query optimization)\n")
  cat("Error:", e$message, "\n")
  NULL
})

# Print summary (full profile viewer opens in RStudio)
cat("✓ Profile complete\n\n")

# ════════════════════════════════════════════════════════════════
# MEMORY USAGE ANALYSIS
# ════════════════════════════════════════════════════════════════

cat("📊 TEST 8: Memory usage analysis\n")
cat(paste0("-", strrep("-", 48), "\n"))

# Get memory size before operations
gc()
mem_before <- as.numeric(object.size(ls()))

# Load different amounts of data
mem_results <- list()

for (n_rows in c(100, 1000, 10000)) {
  gc()
  
  data <- austraits_display_duckdb |>
    dplyr::filter(stringr::str_detect(trait_name, "^(wood_density)$")) |>
    head(n_rows) |>
    dplyr::collect()
  
  mem_used <- as.numeric(object.size(data)) / 1024^2  # Convert to MB
  mem_results[[as.character(n_rows)]] <- mem_used
}

cat("Memory usage for collected data:\n")
for (n_rows in c(100, 1000, 10000)) {
  cat(sprintf("  %6d rows: %6.2f MB\n", n_rows, mem_results[[as.character(n_rows)]]))
}

cat("\n")

# ════════════════════════════════════════════════════════════════
# EXPORT RECOMMENDATIONS
# ════════════════════════════════════════════════════════════════

cat("💡 RECOMMENDATIONS\n")
cat(paste0("=", strrep("=", 50), "\n\n"))

cat("✓ Use lazy evaluation: Count before collect to avoid loading unnecessary data\n")
cat("✓ Apply selective filters first: More specific filters early in pipeline\n")
cat("✓ Use head() for row limiting: Equivalent to SQL LIMIT\n")
cat("✓ Cache distinct values: Pre-compute for dropdown menus\n")
cat("✓ Consider dataset size: Use species dataset for faster queries when possible\n\n")

cat("✓ Benchmarking complete!\n\n")
