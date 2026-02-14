#' Prepare AusTraits data for the portal
#'
#' @description Flattens the AusTraits database, saves definitions and sources, 
#'   and writes parquet files for data and display
#' @param austraits The AusTraits database object
#' @param output_dir Directory to write output files
#' @param overwrite Logical; overwrite existing files if TRUE
#' @keywords internal
#' @noRd
prepare_data_for_portal <- function(austraits, output_dir, overwrite = FALSE) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ## Prepare the database and export (if doesn't already exist)
  filename_data <- file.path(output_dir, "austraits-data.parquet")
  filename_display <- file.path(output_dir, "austraits-display.parquet")
  
  ## Early exit if file already exists
  if (file.exists(filename_data) & !overwrite) { 
    message("✓ Data files already exist (use overwrite=TRUE to regenerate)")
    return()
  }

  # Flatten the database
  message("Flattening AusTraits database...")
  austraits_full_flatten <-
    austraits |>
    austraits::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::mutate(
      measurement_remarks = iconv(measurement_remarks,
        from = "", # Let's R guess the current encoding
        to = "UTF-8", # Convert to UTF-8
        sub = "" # Remove any bytes that can't be converted
      )
    )
  message("✓ Database flattened")

  # create species means dataset
  # list of traits to take means for - core traits only
  message("Computing species averages for core traits...")
  trait_groups <- readr::read_csv(
    "inst/extdata/austraits/trait_groups_for_portal.csv", 
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  core_traits <- 
    trait_groups |>
    dplyr::filter(!is.na(core_trait)) |>
    dplyr::pull(trait)

  austraits_species_averages <-
    austraits_full_flatten |>
    dplyr::filter(trait_name %in% core_traits) |>    
    estimate_species_trait_means()
  message("✓ Species averages computed")

  # Save the flattened database
  message("Writing parquet files...")
  austraits_full_flatten |>
    arrow::write_parquet(filename_data)
  
  # Save the display version of the flattened database
  austraits_full_flatten |>
    format_database_for_display() |>
    arrow::write_parquet(filename_display)
  
  # Save the species averages dataset
  austraits_species_averages |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages.parquet"))
  
  # Save the display version of the species averages dataset
  austraits_species_averages_display <- austraits_species_averages
    
  austraits_species_averages_display |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages-display.parquet")) 
  message("✓ Parquet files saved") 
  
  # Saving definitions
  message("Saving definitions...")
  austraits$definitions |> yaml::write_yaml(file.path(output_dir, "definitions.yml"))
  saveRDS(austraits$definitions, file.path(output_dir, "definitions.rds"))
  message("✓ Definitions saved")

  # Sources
  message("Saving sources...")
  austraits$sources |> RefManageR::WriteBib(file.path(output_dir, "sources.bib"))
  sources_df <- austraits_full_flatten |> 
    dplyr::select(source_primary_key, source_primary_citation) |>
    dplyr::distinct()
  saveRDS(sources_df, file.path(output_dir, "sources.rds"))
  message("✓ Sources saved")
  
  # Save trait_groups as RDS for faster loading
  message("Saving trait groups...")
  saveRDS(trait_groups, file.path(output_dir, "trait_groups.rds"))
  message("✓ Trait groups saved")
  
  # Save metadata as RDS (faster than JSON)
  message("Saving metadata...")
  metatdata <- jsonlite::read_json("inst/extdata/austraits/austraits.json")
  saveRDS(metatdata, file.path(output_dir, "metadata.rds"))
  message("✓ Metadata saved")
  
  # Combine and save state flora links as single RDS
  message("Consolidating flora links...")
  flora_links <- list(
    atrp = readr::read_csv("inst/extdata/ATRP_links.csv", show_col_types = FALSE) |>
      dplyr::rename(url = formatted) |> 
      dplyr::select(taxon_name, url) |>
      dplyr::filter(!is.na(url), url != ""),
    nt = readr::read_csv("inst/extdata/NT_links.csv", show_col_types = FALSE) |>
      dplyr::select(taxon_name, url) |> 
      dplyr::filter(!is.na(url), url != ""),
    vic = readr::read_csv("inst/extdata/Vic_links.csv", show_col_types = FALSE) |>
      dplyr::select(taxon_name, url) |> 
      dplyr::filter(!is.na(url), url != "")
  )
  saveRDS(flora_links, file.path(output_dir, "flora_links.rds"))
  message("✓ Flora links saved")
  
  # Cache dropdown values for faster app startup
  message("Caching dropdown values...")
  
  dropdown_cache <- list(
    all_family = austraits_full_flatten |> dplyr::distinct(family) |> dplyr::pull() |> sort(),
    all_genus = austraits_full_flatten |> dplyr::distinct(genus) |> dplyr::pull() |> sort(),
    all_taxon_names = austraits_full_flatten |> dplyr::distinct(taxon_name) |> dplyr::pull() |> sort(),
    all_traits = austraits_full_flatten |> dplyr::distinct(trait_name) |> dplyr::pull() |> sort(),
    all_bor = austraits_full_flatten |> dplyr::distinct(basis_of_record) |> dplyr::pull() |> sort(),
    all_age = austraits_full_flatten |> dplyr::distinct(life_stage) |> dplyr::pull() |> sort()
  )
  
  # Process states/territories
  dropdown_cache$all_states_territories <- austraits_full_flatten |> 
    dplyr::distinct(taxon_distribution) |> 
    dplyr::pull() |> 
    paste(collapse = ", ") |> 
    stringr::str_split(",") |> 
    purrr::map(~trimws(.x)) |> 
    purrr::list_c() |>
    unique() |> 
    stringr::word(1) |> 
    unique() |> 
    sort()
  
  # Add trait groupings and keywords
  dropdown_cache$all_trait_groupings <- trait_groups |>
    dplyr::pull(trait_group_for_portal) |>
    unique() |>
    sort()
  
  dropdown_cache$all_structure_measured <- trait_groups |>
    dplyr::pull(structure_measured) |>
    stringr::str_remove_all("\\[.*?\\]") |> 
    stringr::str_split("; |,") |>              
    unlist() |>                              
    stringr::str_trim() |>
    unique() |>
    sort()
  
  dropdown_cache$all_keywords <- trait_groups |>
    dplyr::pull(keywords) |>
    stringr::str_split("; |,") |>
    unlist() |>
    stringr::str_trim() |>
    unique() |>
    sort()
  
  # Precompute species dataset IDs (split semicolon-separated values)
  message("Computing species dataset IDs...")
  temp_species_ids <- austraits_species_averages_display |> 
    dplyr::select(dataset_id) |> 
    dplyr::distinct() |> 
    dplyr::collect() |> 
    dplyr::pull(dataset_id)
  
  dropdown_cache$all_dataset_ids_species <- unique(sort(unlist(strsplit(temp_species_ids, "; "))))
  
  saveRDS(dropdown_cache, file.path(output_dir, "dropdown_cache.rds"))
  message("✓ Dropdown cache saved")
}

#' Format flattened database for display
#' 
#' @param database Flattened traits.build object
#' @return Formatted database
#' @keywords internal 
#' @noRd 
#' @importFrom tidyselect ends_with starts_with
format_database_for_display <- function(database) {
  database |> 
    dplyr::select(
      -c(ends_with("_id")),
      -starts_with("source"),
      -c("methods",
         "description", 
         "assistants", 
         "dataset_curators", 
         "sampling_strategy"),
      "dataset_id", 
      "source_primary_citation", 
      "source_primary_key", # For usage text
      "location_id", # For trait profile
      "observation_id", # For trait profile,
      "taxon_name", "taxon_distribution", "taxon_rank":"aligned_name_taxonomic_status", # For trait profile
      "row_id"
    ) |> 
    dplyr::relocate("dataset_id", .before = "taxon_name") |> 
    dplyr::relocate("source_primary_citation", .after = "method_context_properties") |> 
    dplyr::relocate(c("genus", "family"), .after = "taxon_name") |>
    dplyr::arrange(family, taxon_name, trait_name)
}

#' Format hyperlinks in flattened database for display
#' 
#' @param database Flattened traits.build object
#' @return Database with formatted hyperlinks
#' @keywords internal 
#' @noRd
format_hyperlinks_for_display <- function(database) {
  database |> 
    dplyr::mutate(
      source_primary_citation_URL = stringr::str_match(.data$source_primary_citation, "\\((https?://[^\\s)]+)\\)")[,2], # Extract URL
      source_primary_citation = gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", .data$source_primary_citation), # Remove DOI MD link structure
      source_primary_citation = gsub("_([^_]+)_", "<i>\\1</i>", .data$source_primary_citation), # Replace MD italics with HTML italics
      source_primary_citation = dplyr::if_else(
        !is.na(.data$source_primary_citation_URL),
        paste0('<a href="', .data$source_primary_citation_URL, '" target="_blank">', .data$source_primary_citation, '</a>'),
        .data$source_primary_citation
      )
    ) |>
    dplyr::select(-"source_primary_citation_URL") 
}

#' Retrieve all assets from a GitHub Release
#'
#' @param version_tag The version tag of the GitHub release (e.g., "6.0.0")
#' @param output_dir The local directory to save the downloaded files
#' @return A vector of paths to the downloaded files
#' @export
retrieve_github_release_parquet <- function(version_tag = "6.0.0", 
                                              output_dir = system.file("extdata/austraits", package = "austraits.portal")) {
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Construct the URL for the GitHub release API
  api_url <- paste0("https://api.github.com/repos/traitecoevo/austraits.portal/releases/tags/", version_tag)
  
  # Make a GET request to fetch release information
  response <- tryCatch(
    httr2::request(api_url) |> 
      httr2::req_perform(),
    error = function(e) {
      warning("Failed to fetch release information: ", e$message)
      return(NULL)
    }
  )
  
  # If the response is NULL, return early
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check if the request was successful
  if (httr2::resp_status(response) != 200) {
    warning("Failed to fetch release information. HTTP status: ", httr2::resp_status(response))
    return(NULL)
  }
  
  # Parse the response body as JSON
  release_info <- httr2::resp_body_json(response)
  
  # Extract asset information
  assets <- release_info$assets
  if (length(assets) == 0) {
    stop("No assets found in the release.")
  }
  
  # Download parquet only 
  asset_url <- assets[[1]]$browser_download_url
  file_name <- assets[[1]]$name
  output_path <- file.path(output_dir, file_name)
  
  # Check if the file already exists
  if (file.exists(output_path)) {
    message("Asset already exists: ", output_path)
    return(output_path)
  }

  # Download the asset
  asset_response <- httr2::request(asset_url) |>
    httr2::req_perform()
  
  # Check if the request was successful
  if (httr2::resp_status(asset_response) != 200) {
    stop("Failed to download asset: ", file_name, ". HTTP status: ", httr2::resp_status(asset_response))
  }
  
  # Write the content to the specified output path
  writeBin(httr2::resp_body_raw(asset_response), output_path)
  message("Asset downloaded successfully: ", output_path)
  return(output_path)
}