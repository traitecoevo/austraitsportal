#!/usr/bin/env Rscript

#' Benchmarking script for AusTraits Portal - use of duckDB
#'
#' Run this script to test and benchmark the speed of various filtering   
#' and querying operations outside the Shiny app context.


library(arrow)
library(duckdb)
library(dplyr)

source("R/global.R")

# Setup DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")
duckdb::duckdb_register_arrow(con, "austraits_display", austraits_display)
duckdb_tbl <- tbl(con, "austraits_display")

cat("\n════════════════════════════════════════\n")
cat("ARROW vs DUCKDB BENCHMARK\n")
cat("════════════════════════════════════════\n")

# TEST 1: COUNT
cat("\nTEST 1: COUNT\n")
cat("Arrow: ")
t1_arrow <- system.time({austraits_display |> filter(family == "Fabaceae") |> count() |> collect()})[3]
cat(sprintf("%.2f sec\n", t1_arrow))

cat("DuckDB: ")
t1_duckdb <- system.time({duckdb_tbl |> filter(family == "Fabaceae") |> count() |> collect()})[3]
cat(sprintf("%.2f sec\n", t1_duckdb))
cat(sprintf("Speedup: %.1fx\n", t1_arrow / t1_duckdb))

# TEST 2: COLLECT 100 (use head() for DuckDB)
cat("\nTEST 2: COLLECT 100\n")
cat("Arrow: ")
t2_arrow <- system.time({austraits_display |> filter(family == "Fabaceae") |> slice_head(n = 100) |> collect()})[3]
cat(sprintf("%.2f sec\n", t2_arrow))

cat("DuckDB: ")
t2_duckdb <- system.time({duckdb_tbl |> filter(family == "Fabaceae") |> head(100) |> collect()})[3]
cat(sprintf("%.2f sec\n", t2_duckdb))
cat(sprintf("Speedup: %.1fx\n", t2_arrow / t2_duckdb))

# TEST 3: COMPLEX FILTER
cat("\nTEST 3: COMPLEX FILTER\n")
cat("Arrow: ")
t3_arrow <- system.time({austraits_display |> filter(family == "Fabaceae", trait_name == "wood_density") |> collect()})[3]
cat(sprintf("%.2f sec\n", t3_arrow))

cat("DuckDB: ")
t3_duckdb <- system.time({duckdb_tbl |> filter(family == "Fabaceae", trait_name == "wood_density") |> collect()})[3]
cat(sprintf("%.2f sec\n", t3_duckdb))
cat(sprintf("Speedup: %.1fx\n", t3_arrow / t3_duckdb))

# TEST 4: FULL WORKFLOW
cat("\nTEST 4: FULL WORKFLOW (count + collect)\n")
cat("Arrow: ")
t4_arrow <- system.time({
q <- austraits_display |> filter(family == "Fabaceae")
total <- q |> count() |> collect()
data <- q |> slice_head(n = 100) |> collect()})[3]
cat(sprintf("%.2f sec\n", t4_arrow))

cat("DuckDB: ")
t4_duckdb <- system.time({
q <- duckdb_tbl |> filter(family == "Fabaceae")
total <- q |> count() |> collect()
data <- q |> head(100) |> collect()})[3]
cat(sprintf("%.2f sec\n", t4_duckdb))
cat(sprintf("Speedup: %.1fx\n", t4_arrow / t4_duckdb))

# SUMMARY
cat("\n════════════════════════════════════════\n")
cat("SUMMARY\n")
cat("════════════════════════════════════════\n")
cat(sprintf("%-20s %10s %10s %10s\n", "Test", "Arrow", "DuckDB", "Speedup"))
cat("────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s %9.2fs %9.2fs %9.1fx\n", "Count", t1_arrow, t1_duckdb, t1_arrow/t1_duckdb))
cat(sprintf("%-20s %9.2fs %9.2fs %9.1fx\n", "Collect 100", t2_arrow, t2_duckdb, t2_arrow/t2_duckdb))
cat(sprintf("%-20s %9.2fs %9.2fs %9.1fx\n", "Complex Filter", t3_arrow, t3_duckdb, t3_arrow/t3_duckdb))
cat(sprintf("%-20s %9.2fs %9.2fs %9.1fx\n", "Full Workflow", t4_arrow, t4_duckdb, t4_arrow/t4_duckdb))
cat("════════════════════════════════════════════════════════\n")

avg_speedup <- mean(c(t1_arrow/t1_duckdb, t2_arrow/t2_duckdb, t3_arrow/t3_duckdb, t4_arrow/t4_duckdb))
cat(sprintf("\n AVERAGE SPEEDUP: %.1fx FASTER with DuckDB!\n", avg_speedup))

if (avg_speedup > 5) {
  cat("\n more than 5x \n")
} else if (avg_speedup > 2) {
  cat("\n STRONGLY RECOMMEND: Switch to DuckDB\n")
} else {
  cat("\n DuckDB is faster but not dramatic\n")
}

dbDisconnect(con, shutdown = TRUE)
