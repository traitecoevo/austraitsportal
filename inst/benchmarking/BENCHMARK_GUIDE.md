# Query Performance Benchmarking Guide

This directory contains benchmarking scripts to test and optimize query performance in the AusTraits Portal, without running the full Shiny app.

## Scripts

### 1. `benchmark_queries.R` - Main benchmarking suite

Comprehensive benchmarks for the most common query patterns:

- **COUNT operation**: Different ways to count rows
- **Single filters**: Individual filter conditions (trait, basis_of_record, life_stage)
- **Combined filters**: Multiple filter conditions together
- **Filter + data load**: Compare strategies for loading data after filtering
- **Location filtering**: Geographic filtering performance
- **Distinct value retrieval**: Dropdown/selectize performance
- **Full pipeline**: Real-world combined filters

**Run it:**
```r
source("benchmark_queries.R")
```

**Expected output:**
- Timing comparisons in milliseconds
- Summary statistics about dataset sizes
- Recommendations for fastest approaches

### 2. `benchmark_advanced.R` - Deep performance analysis

Advanced profiling and optimization analysis:

- **String matching patterns**: Different regex approaches
- **Lazy evaluation benefits**: count() before collect() vs after
- **Filter order impact**: Does filter order affect speed?
- **head() vs slice_head()**: Different row-limiting approaches
- **Distinct value optimization**: SQL vs dplyr approaches
- **Species vs Observations**: Dataset-specific performance
- **Memory usage**: How much RAM for different data sizes
- **Function profiling**: Where does time go in helper functions?

**Run it:**
```r
source("benchmark_advanced.R")
```

**Expected output:**
- Detailed timing for each test
- Memory usage statistics in MB
- Profile visualization in RStudio Viewer
- Recommendations for optimization

## Quick Start

### Prerequisites
```r
# Make sure these packages are installed
install.packages(c("microbenchmark", "profvis", "ggplot2"))
```

### Basic workflow

1. **Load the package and data** (one-time setup):
```r
setwd("~/GitHub/packages/austraits/austraits.portal")
devtools::load_all()  # Loads data automatically
```

2. **Run main benchmarks**:
```r
source("benchmark_queries.R")
```

3. **Get detailed analysis** (optional):
```r
source("benchmark_advanced.R")
```

## Interpreting Results

### Microbenchmark output

```
Unit: milliseconds
                   expr   min    lq  mean median    uq   max
 count() + pull(n)      34.2  35.1  36.8   36.2  37.5  42.1
 nrow(collect())        45.3  46.2  48.1   48.5  49.2  51.3
```

- `min`: Fastest run
- `lq/uq`: 25th/75th percentiles (interquartile range)
- `mean`: Average time
- `median`: Middle value (less affected by outliers)

**Faster is better** - lower values = better performance

## Common Performance Patterns

### Pattern 1: Counting Results
```r
# FAST - Uses lazy evaluation ✓
filtered_query |> 
  dplyr::count() |> 
  dplyr::collect() |> 
  dplyr::pull(n)

# SLOW - Loads all data first ✗
filtered_query |>
  dplyr::collect() |>
  nrow()
```

### Pattern 2: Limiting Rows
```r
# FAST - Stops at DuckDB level ✓
filtered_query |> 
  head(100) |> 
  dplyr::collect()

# SLOWER - Collects all then limits ✗
filtered_query |>
  dplyr::collect() |>
  head(100)
```

### Pattern 3: Filter Order
```r
# BEST - Most selective filter first ✓
data |>
  dplyr::filter(trait_name == "wood_density") |>    # Reduces to ~1000 rows
  dplyr::filter(basis_of_record == "measurement")    # Reduces further

# SLOWER - Less selective filter first ✗
data |>
  dplyr::filter(basis_of_record == "measurement") |  # Still many rows
  dplyr::filter(trait_name == "wood_density")
```

## Performance Tips

✓ **DO:**
- Use lazy evaluation (count before collect)
- Apply selective filters early
- Use `head()` to limit rows before collecting
- Cache distinct values for dropdowns
- Pre-compute frequently accessed queries

✗ **AVOID:**
- Collecting entire dataset without limits
- Using complex operations on collected data when possible
- Filtering after collect()
- Repeated distinct() queries on same column

## Tracking Improvements

Run benchmarks before and after optimizations:

```r
# Before
source("benchmark_queries.R")
# Note the median times

# Make optimization changes...

# After
source("benchmark_queries.R")
# Compare with previous results
```

## Customizing Benchmarks

Edit the scripts to test specific scenarios:

```r
# In benchmark_queries.R, modify filter inputs:
filter_input <- list(
  trait_name = c("YOUR_TRAIT_HERE"),
  basis_of_record = "YOUR_VALUE",
  # ... etc
)
```

## Saving Results

Save benchmark results for comparison:

```r
# In R console after running benchmark_queries.R
saveRDS(benchmark_count, "benchmark_results_2024-02-11.rds")
saveRDS(benchmark_filters, "benchmark_filters_2024-02-11.rds")

# Load and compare later
old_results <- readRDS("benchmark_results_2024-02-11.rds")
print(old_results)
```

## Getting Help

- For DuckDB query optimization: https://duckdb.org/docs/guides/performance/
- For dplyr performance: https://dplyr.tidyverse.org/
- For R profiling: `help(profvis::profvis)`

---

Last updated: February 2024
