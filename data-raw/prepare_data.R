library(austraits)
library(arrow)
library(dplyr)
source("R/a-helper.R")
source("R/species_means.R")


# Prepare the data for the portal

# Lite version
austraits:::austraits_5.0.0_lite |> 
  prepare_data_for_portal("inst/extdata/austraits/austraits-5.0.0-lite", overwrite = TRUE)

# Austraits v7.0.0 full version
austraits_7.0.0 <- 
  austraits::load_austraits(version = "7.0.0", path = "inst/extdata/austraits", update = FALSE)
# A small fix for Austraits v7.0.0. The source_primary_key for Bryant_2021_2 Bryant_2021_3  dataset is incorrectly set
austraits_7.0.0$methods <- austraits_7.0.0$methods |>
  mutate(
    source_primary_key = ifelse(grepl("Bryant_2021", dataset_id), dataset_id, source_primary_key)
)

austraits_7.0.0 |> 
 prepare_data_for_portal("inst/extdata/austraits/austraits-7.0.0-full", overwrite = TRUE)
