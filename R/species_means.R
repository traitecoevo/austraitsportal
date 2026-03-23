# Numeric traits


#' Estimate Species Trait Means
#'
#' Calculates mean trait values for each species in the provided Austraits dataset.
#'
#' @param austraits A data frame or list containing trait data, typically in the Austraits format.
#'
#' @return A data frame with estimated mean trait values for each species.
#'
#' @details
#' This function processes the input Austraits dataset to compute mean values of traits for each species.
#'
#' @examples
#' \dontrun{
#' means <- estimate_species_trait_means(austraits)
#' }
#'
#' @export
estimate_species_trait_means <- function(austraits) {

  traits <-
    austraits |>
    select(trait_name, unit) |>
    dplyr::distinct() |>
    dplyr::mutate(
      type = ifelse(is.na(unit), "categorical", "numerical")
    ) |>
    split(~type)
  
  data_means_numerical <- 
    austraits |>
    estimate_species_trait_means_numerical(
      traits = traits$numerical$trait_name)
  
  data_means_categorical <-
    austraits |> estimate_species_trait_summary_categorical(
      traits = traits$categorical$trait_name)
  
  # Build species means for database
  austraits_species_averages <- 
    data_means_numerical |>
      dplyr::mutate(value_mean = as.character(value_mean)) |>
    # combine the two datasets
    dplyr::bind_rows(data_means_categorical) |>
    # add in the trait type
    dplyr::left_join(by = c("trait_name"),
      traits |> dplyr::bind_rows() |> dplyr::select(trait_name, type),
    ) |>
    # add in taxon info
    dplyr::left_join(by = "taxon_name",
      austraits |>
        dplyr::select(taxon_name, taxon_rank:scientific_name_id, -taxon_name_alternatives, -value_type) |>
        dplyr::distinct()
    ) |>
    dplyr::relocate("value_range", .after = "value_mean") |>
    # Add row_id after aggregation
    dplyr::mutate(row_id = dplyr::row_number())

  # Reorder citations in source columns
  sources <- austraits |>
    select(key = source_primary_key, source_primary_citation) |>
    distinct()

  ids <- austraits_species_averages |> 
    dplyr::select(source_primary_key) |> 
    dplyr::distinct() |>
    dplyr::mutate(key = source_primary_key) |> 
    tidyr::separate_rows(key, sep = "; ") |>
    dplyr::left_join(sources, by = c("key")) |>
    dplyr::group_by(source_primary_key) |>
    dplyr::summarise(.groups = "drop",
      source_primary_citation = paste(source_primary_citation, collapse = "; ")
    )

  austraits_species_averages |>
    dplyr::left_join(ids, by = "source_primary_key")  
}

#' @keywords internal
#' @noRd
estimate_species_trait_means_numerical <- function(austraits, traits) {

  # any data that is a mean, median or raw, create a site mean
  location_means <- austraits |> estimate_species_trait_means_locations(traits)

  # any data that is a max or a min (range) and basically from a flora, create a mean value
  flora_means <- austraits |> estimate_species_trait_means_floras(traits)
  
  # combine the two, then take means across site and flora replicates
  means <- location_means |>
    dplyr::bind_rows(flora_means) |>
    dplyr::group_by(taxon_name, trait_name, unit) |>
    dplyr::summarise(.groups = "drop",
      value_mean = mean(value_mean),
      value_min = min(value_min),
      value_max = max(value_max),
      value_median = median(value_median),
      value_geom_mean = 10^mean(suppressWarnings(log10(value_mean)), na.rm= TRUE),
      all_replicates = sum(all_replicates),
      location_replicates = sum(location_replicates),
      flora_replicates = sum(flora_replicates),
      # record sources
      dataset_id = paste(unique(dataset_id), collapse = "; "),
      source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
    ) |>
    dplyr::distinct()
  
  means
}

#' @keywords internal
#' @noRd
estimate_species_trait_means_locations <- function(austraits, traits) {
  # any data that is a mean, median or raw, create a location mean
  x <- austraits |>
  dplyr::filter(
    trait_name %in% traits,
    value_type %in% c("mean", "raw", "median")
  ) |>
  dplyr::mutate(
    value = as.numeric(value),
    replicates = 1,
    log10_value = suppressWarnings(log10(value))
  ) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(taxon_name, trait_name, dataset_id, location_id, unit) |>
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(value, list(mean = mean, min = min, max = max, median = median)),
    dplyr::across(c("latitude (deg)", "longitude (deg)", "location_name"), dplyr::first),
    all_replicates = sum(replicates),
    value_geom_mean = 10^mean(log10_value, na.rm= TRUE),
    observation_id = paste(unique(observation_id), collapse = "; "),
    source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
  ) |>
  dplyr::mutate(
    value_type = "location_mean",
    location_replicates = 1,
    flora_replicates = 0
  )
}

#' @keywords internal
#' @noRd
estimate_species_trait_means_floras <- function(austraits, traits) {
  flora_data <- 
    austraits |>
    dplyr::filter(
      trait_name %in% traits,
      value_type %in% c("minimum", "maximum"), 
      basis_of_record %in% c("preserved_specimen", "literature")
    )
  
  # early exit if no data available
  if(nrow(flora_data) == 0) {return(flora_data)}
  
  flora_data |>
  dplyr::mutate(
    value = as.numeric(value),
  )|>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(taxon_name, trait_name, unit, dataset_id, observation_id, original_name) |> 
  dplyr::summarise(.groups = "keep",
    dplyr::across(value, list(mean = mean, min = min, max = max)), 
    source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
  ) |>
  dplyr::mutate(
    location_replicates = 0,
    flora_replicates = 1,
    all_replicates = 1,
    value_median = value_mean,
    value_type = "flora_mean"
  ) |> 
  dplyr::ungroup()

}

#' @keywords internal
#' @noRd
estimate_species_trait_summary_categorical <- function(austraits,
traits) {

  austraits |>
  estimate_species_trait_value_summary_categorical(traits) |>
  dplyr::group_by(taxon_name, trait_name) |>
  dplyr:: mutate(
      tmp_summary = paste0(value, " (", replicates, ")"),
      value_range = paste0(tmp_summary, collapse = "; "),
      all_replicates = sum(replicates),
      dataset_id = paste(unique(dataset_id), collapse = "; "),
      source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
    ) |>
    # for overall value mean, retain the trait value(s) with the maximum number of replicates
    dplyr::filter(replicates == max(replicates)) |>
    # for instances with multiple equally reported trait values, merge those into a single string
    dplyr::mutate(
      value_mean = paste0(value, collapse = " ")
    ) |>
  dplyr::ungroup() |>
  # sometimes there are equally common trait values and they have each had the same dataset_id,
  # need to retain only one of these, and want them reordered across various trait values
  tidyr::separate_longer_delim(c(dataset_id, source_primary_key), delim = "; ") |>
  dplyr::arrange(taxon_name, trait_name, dataset_id) |>
  dplyr::group_by(taxon_name, trait_name) |>
  dplyr:: mutate(
      dataset_id = paste(unique(dataset_id), collapse = "; "),
      source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
    ) |>
  dplyr::ungroup() |>
  dplyr::select(-dplyr::all_of(c("tmp_summary", "value", "observation_id", "replicates"))) |>
  dplyr::select(taxon_name, dataset_id, trait_name, value_mean, value_range, all_replicates, value_type, source_primary_key) |>
  dplyr::distinct()
}

#' @keywords internal
#' @noRd
estimate_species_trait_value_summary_categorical <- function(austraits, traits) {

  austraits |>
  dplyr::filter(trait_name %in% traits) |>
  dplyr::select(dplyr::all_of(c("dataset_id", "taxon_name", "trait_name", "location_id", "observation_id", "value", "source_primary_key")))|>
  dplyr::mutate(value = stringr::str_split(value, " ")) |>
  tidyr::unnest_longer(value) |>
  dplyr::mutate(
    replicates = 1
  ) |>
  dplyr::group_by(taxon_name, trait_name, value) |>
  summarise(.groups = "drop",
    value = first(value),
    replicates = sum(replicates),
    dataset_id = paste(unique(dataset_id), collapse = "; "),
    observation_id = paste(unique(observation_id), collapse = "; "),
    source_primary_key = paste(unique(na.omit(source_primary_key)), collapse = "; ")
  ) |>
  mutate(
    value_type = "value_range"
  )
}
