library(austraits)
library(arrow)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases
austraits <- load_austraits(version = "6.0.0")

# Wrangle data
## Flatten the database
flatten_austraits <- austraits |> flatten_database()

## Save this as a parquet
write_parquet(flatten_austraits, "data/austraits/austraits-6.0.0-flatten.parquet")

## Save lite version as a parquet
write_parquet(austraits:::austraits_5.0.0_lite |> flatten_database(), "data/austraits/austraits-lite.parquet")