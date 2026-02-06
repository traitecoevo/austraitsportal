options(shiny.launch.browser = TRUE)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see branch data-load
## Use austraits R package load_austraits() function to download data to the file path below
## Then create this parquet following code in data-raw/create-flat-austraits.R

# Custom logic
`%not_in%` <- Negate(`%in%`)

# set the path to the data
data_path <- "inst/extdata/austraits/austraits-5.0.0-lite"
data_path <- "inst/extdata/austraits/austraits-7.0.0-full"

# Load the datasets
austraits <- arrow::open_dataset(file.path(data_path, "austraits-data.parquet"))
austraits_display <- arrow::open_dataset(file.path(data_path, "austraits-display.parquet"))
austraits_species_averages <- arrow::open_dataset(file.path(data_path, "austraits-species-averages.parquet"))
austraits_species <- arrow::open_dataset(file.path(data_path, "austraits-species-averages.parquet"))
austraits_species_display <- arrow::open_dataset(file.path(data_path, "austraits-species-averages-display.parquet"))
sources <- readr::read_csv(file.path(data_path, "sources.csv"), show_col_types = FALSE)

trait_definitions <- yaml::read_yaml(file.path(data_path, "definitions.yml"))

trait_groups <- readr::read_csv(
  "inst/extdata/austraits/trait_groups_for_portal.csv",
  col_types = readr::cols(.default = readr::col_character())
  )
metatdata <- jsonlite::read_json("inst/extdata/austraits/austraits.json")

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
## Taxonomy
### Unique values of family
all_family <- austraits |>
  extract_distinct_values(family)

### Unique values of genus
all_genus <- austraits |>
  extract_distinct_values(genus)

## Unique values of taxon_name
all_taxon_names <- austraits |>
  extract_distinct_values(taxon_name)

## Location
# TODO: Not yet implemented.
### Coordinates - circle/bbox around coordinates?

### States by location properties


### APC distribution - May need APCalign::create_species_state_origin_matrix()
all_states_territories <- austraits  |> 
  extract_distinct_values(taxon_distribution) |> 
  paste(collapse = ", ")  |> 
  stringr::str_split(",")  |> 
  purrr::map(~trimws(.x))  |> 
  purrr::list_c() |>
  unique()  |> 
  stringr::word(1)  |> 
  unique()  |> 
  sort()

## Traits
### Unique values of taxon_name
all_traits <- austraits |>
  extract_distinct_values(trait_name)

## Other sidebar values
### Unique values of BoR
all_bor <- austraits |>
  extract_distinct_values(basis_of_record)

## Unique values of age/lifestage
all_age <- austraits |>
  extract_distinct_values(life_stage)

# Load state flora link mappings
atrp_links <- readr::read_csv(
  "inst/extdata/ATRP_links.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(url = formatted) |> 
  dplyr::select(taxon_name, url) |>
  dplyr::filter(!is.na(url), url != "")

nt_links <- readr::read_csv(
  "inst/extdata/NT_links.csv",
  show_col_types = FALSE
) |>
  dplyr::select(taxon_name, url) |> 
  dplyr::filter(!is.na(url), url != "")

vic_links <- readr::read_csv(
  "inst/extdata/Vic_links.csv",
  show_col_types = FALSE
) |>
  dplyr::select(taxon_name, url) |> 
  dplyr::filter(!is.na(url), url != "")
## Trait groupings and keywords
all_trait_groupings <- trait_groups |>
  dplyr::pull(trait_group_for_portal) |>
  unique() |>
  sort()

all_structure_measured <- trait_groups |>
  dplyr::pull(structure_measured) |>
  stringr::str_remove_all("\\[.*?\\]") |> 
  stringr::str_split("; |,") |>              
  unlist() |>                              
  stringr::str_trim() |>
  unique() |>
  sort()

all_keywords <- trait_groups |>
    dplyr::pull(keywords) |>
    stringr::str_split("; |,") |>
    unlist() |>
    stringr::str_trim() |>
    unique() |>
    sort()

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


# For species averages - split semicolon-separated dataset_id (Get done cause IDs get clubbed, should be understandable)
temp_species_ids <- austraits_species_display |> 
  dplyr::select(dataset_id) |> 
  dplyr::distinct() |> 
  dplyr::collect() |> 
  dplyr::pull(dataset_id)

all_dataset_ids_species <- unique(sort(unlist(strsplit(temp_species_ids, "; "))))
rm(temp_species_ids)

# Columns to display for species averages (different from raw data)
columns_display_species <- c(
  "dataset_id", "taxon_name", "genus", "family", "trait_name", 
  "value_mean", "value_min", "value_max", "value_median", "unit",
  "value_count", "all_replicates",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

# TELEMETRY - Usage tracking via shiny.telemetry
library(shiny.telemetry)

# Global telemetry object — PostgreSQL for persistent storage
if(FALSE) {

library(dplyr)
library(tidyr)
library(stringr)
library(shiny.telemetry)
library(RPostgreSQL)

Sys.setenv(POSTGRES_HOST = "aws-1-ap-southeast-2.pooler.supabase.com")
Sys.setenv(POSTGRES_DB = "postgres")
Sys.setenv(POSTGRES_PORT = "6543")
# Sys.setenv(POSTGRES_USER = "YOUR_USERNAME_HERE")
# Sys.setenv(POSTGRES_PASSWORD = "YOUR_PASSWORD_HERE")

telemetry <- shiny.telemetry::Telemetry$new(
  app_name = "austraits_portal",
  data_storage = shiny.telemetry::DataStoragePostgreSQL$new(
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD"),
    host = Sys.getenv("POSTGRES_HOST"),
    dbname = Sys.getenv("POSTGRES_DB"),
    port = as.integer(Sys.getenv("POSTGRES_PORT", "5432"))
  )
)
}

dir.create("inst/telemetry", showWarnings = FALSE, recursive = TRUE)
telemetry <- shiny.telemetry::Telemetry$new(
  app_name = "austraits_portal",
  data_storage = shiny.telemetry::DataStorageSQLite$new(
    db_path = "inst/telemetry/telemetry.db"
  )
)
