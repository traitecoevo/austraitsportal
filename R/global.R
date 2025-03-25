# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <-
  # arrow::open_dataset("inst/extdata/austraits/austraits-lite.parquet") |>
  # dplyr::collect()
  load_austraits(version = "6.0.0", path = "inst/extdata/austraits/") |>
  join_taxa()

## Set up possible values for selectize
# Unique values of taxon_name
all_taxon_names <- austraits$traits$taxon_name |> unique() |> sort()

# Unique values of genus
all_genus <- austraits$traits$genus |> unique() |> sort()

## Set up possible family
# Unique values of genus
all_family <- austraits$traits$family |> unique() |> sort()
