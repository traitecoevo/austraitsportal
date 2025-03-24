# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <- 
  open_dataset("data/austraits/austraits-lite.parquet") |> 
  collect()

## Set up possible genus
# Unique values of genus
all_genus <- austraits$genus |> unique() |> sort()

## Set up possible family
# Unique values of genus
all_family <- austraits$family |> unique() |> sort()