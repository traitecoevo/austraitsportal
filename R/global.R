options(shiny.launch.browser = TRUE)

# Performance optimizations
#' Configure Shiny Application Options
#'
#' Sets global Shiny options to optimize application performance and resource handling:
#' - `shiny.autoreload = FALSE`: Disables automatic reloading to prevent interruptions during development
#' - `shiny.reactlog = FALSE`: Disables reactivity logging to reduce memory overhead and improve performance
#' - `shiny.maxRequestSize = 30 * 1024 ^ 2`: Sets maximum upload file size to 30 MB, preventing excessive memory consumption from large file uploads
options(
  shiny.autoreload = FALSE,
  shiny.reactlog = FALSE,
  shiny.maxRequestSize = 30 * 1024 ^ 2
)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see branch data-load
## Use austraits R package load_austraits() function to download data to the file path below
## Then create this parquet following code in data-raw/create-flat-austraits.R

# create place for cache
dir.create(".cache/taxon_text", showWarnings = FALSE, recursive = TRUE)

# Custom logic
`%not_in%` <- Negate(`%in%`)

# set the path to the data
data_path <- "inst/extdata/austraits/austraits-5.0.0-lite"
# data_path <- "inst/extdata/austraits/austraits-7.0.0-full"

# Load the datasets
austraits <- arrow::open_dataset(file.path(data_path, "austraits-data.parquet"))
austraits_display <- arrow::open_dataset(file.path(data_path, "austraits-display.parquet"))
austraits_species_averages <- arrow::open_dataset(file.path(data_path, "austraits-species-averages.parquet"))
austraits_species <- arrow::open_dataset(file.path(data_path, "austraits-species-averages.parquet"))
austraits_species_display <- arrow::open_dataset(file.path(data_path, "austraits-species-averages-display.parquet"))

# After loading Arrow datasets, ADD:

# ════════════════════════════════════════
# DUCKDB SETUP FOR PERFORMANCE
# ════════════════════════════════════════
cat("[STARTUP] Setting up DuckDB...\n")
duckdb_setup_start <- Sys.time()

library(duckdb)

# Create DuckDB connection
duckdb_con <- dbConnect(duckdb::duckdb(), ":memory:")

# Register Arrow datasets with DuckDB
duckdb::duckdb_register_arrow(duckdb_con, "austraits_display", austraits_display)
duckdb::duckdb_register_arrow(duckdb_con, "austraits_species_display", austraits_species_display)
duckdb::duckdb_register_arrow(duckdb_con, "austraits_data", austraits)
duckdb::duckdb_register_arrow(duckdb_con, "austraits_species_data", austraits_species)

# Create DuckDB table references (no library needed)
austraits_display_duckdb <- dplyr::tbl(duckdb_con, "austraits_display")
austraits_species_display_duckdb <- dplyr::tbl(duckdb_con, "austraits_species_display")
austraits_duckdb <- dplyr::tbl(duckdb_con, "austraits_data")
austraits_species_duckdb <- dplyr::tbl(duckdb_con, "austraits_species_data")

cat(sprintf("[STARTUP] ✅ DuckDB setup: %.2f sec\n\n", 
    as.numeric(Sys.time() - duckdb_setup_start, units = "secs")))

# Load sources (RDS faster than CSV)
sources <- readRDS(file.path(data_path, "sources.rds"))

# Load trait definitions (RDS faster than YAML)
trait_definitions <- readRDS(file.path(data_path, "definitions.rds"))

# Load trait groups (RDS faster than CSV)
trait_groups <- readRDS(file.path(data_path, "trait_groups.rds"))

# Load metadata (RDS faster than JSON)
metatdata <- readRDS(file.path(data_path, "metadata.rds"))

columns_display <- c(
  "dataset_id", "taxon_name", "genus", "family", "trait_name", "value", "unit",
  "entity_type", "value_type", "basis_of_value", "replicates", "basis_of_record",
  "life_stage", "collection_date", "measurement_remarks", "original_name", 
  "location_name", "latitude (deg)", "longitude (deg)", "location_properties",
  "treatment_context_properties", "plot_context_properties", 
  "entity_context_properties", "temporal_context_properties", 
  "method_context_properties", "source_primary_citation", "data_contributors",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

# Set up possible values for selectize menus
# Load precomputed dropdown values for faster startup
dropdown_cache_path <- file.path(data_path, "dropdown_cache.rds")

# Load from cache (much faster)
dropdowns <- readRDS(dropdown_cache_path)

## Location
# TODO: Not yet implemented.
### Coordinates - circle/bbox around coordinates?

### States by location properties

# Load state flora link mappings (precomputed in RDS)
flora_links <- readRDS(file.path(data_path, "flora_links.rds"))

# Define controlled vocabulary columns (dropdown)
controlled_vocab_columns <- c(
  "dataset_id", "entity_type", "value_type", "basis_of_value",
  "basis_of_record", "life_stage", "original_name", "taxon_rank",
  "establishment_means"
)

# Define free-text columns (text input)
free_text_columns <- c(
  "collection_date", "measurement_remarks", "location_name",
  "location_properties", "treatment_context_properties",
  "plot_context_properties", "entity_context_properties",
  "temporal_context_properties", "method_context_properties",
  "source_primary_citation", "data_contributors"
)

# Define columns to omit from custom filter
omit_from_custom_filter <- c(
  "taxon_name", "genus", "family", "trait_name",
  "unit", "value", "replicates", "taxon_distribution",
  "latitude (deg)", "longitude (deg)"
)

# Available columns for custom filter
custom_filter_columns <- c(controlled_vocab_columns, free_text_columns)

# Custom filter columns for SPECIES AVERAGES (only columns that exist)
custom_filter_columns_species <- c(
  "dataset_id", "value_type", 
  "taxon_rank", "establishment_means"
  # Only controlled vocab columns that exist in species avg
)

# Custom Github hyperlink icon
target <- bsplus::shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/traitecoevo/austraits.portal"

# Columns to display for species averages (different from raw data)
columns_display_species <- c(
  "dataset_id", "taxon_name", "genus", "family", "trait_name", 
  "value_mean", "value_min", "value_max", "value_median", "unit",
  "value_count", "all_replicates",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

# TELEMETRY - Supabase REST API with SQLite fallback
library(httr2)
source("R/telemetry_supabase.R")

# Read credentials from config.yml (production)
config_file <- "config.yml"

if (file.exists(config_file)) {
  # Read from config.yml
  cfg <- config::get(config = "production", file = config_file)
  supabase_url <- cfg$supabase_url
  supabase_key <- cfg$supabase_key
} else {
  # Fallback to environment variables
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_key <- Sys.getenv("SUPABASE_KEY")
}

if (nchar(supabase_url) > 0 && nchar(supabase_key) > 0) {
  message("✓ Using Supabase telemetry (cloud)")
  init_supabase_telemetry(supabase_url, supabase_key)
  options(telemetry_mode = "cloud")
} else {
  message("✓ Using local SQLite telemetry")
  library(shiny.telemetry)
  dir.create("inst/telemetry", showWarnings = FALSE, recursive = TRUE)
  telemetry <- shiny.telemetry::Telemetry$new(
    app_name = "austraits_portal",
    data_storage = shiny.telemetry::DataStorageSQLite$new(
      db_path = "inst/telemetry/telemetry.db"
    )
  )
  options(
    telemetry_mode = "local",
    telemetry_object = telemetry
  )
}
