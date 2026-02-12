#' Check if input has filters
#' @keywords internal

has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Memoised helper to get distinct values from a column (cached for performance)
#' This version takes a string column name for better memoisation
#' @keywords internal
get_distinct_values_cached <- memoise::memoise(function(data, column_name, limit = NULL) {
  query <- data |>
    dplyr::select(dplyr::all_of(column_name)) |>
    dplyr::distinct()
  
  if (!is.null(limit)) {
    query <- query |> head(limit)
  }
  
  query |>
    dplyr::collect() |>
    dplyr::pull(1) |>
    na.omit() |>
    sort()
})

#' Memoised helper to filter trait groups (cached for performance)
#' @keywords internal
get_matching_traits_cached <- memoise::memoise(function(trait_groups, selected_grouping = NULL, 
                                                         selected_structure = NULL, 
                                                         selected_keywords = NULL) {
  matching_traits <- trait_groups$trait
  
  if (!is.null(selected_grouping) && length(selected_grouping) > 0) {
    matching_traits <- trait_groups |>
      dplyr::filter(trait_group_for_portal %in% selected_grouping) |>
      dplyr::pull(trait) |>
      unique()
  }
  
  if (!is.null(selected_structure) && length(selected_structure) > 0) {
    structure_pattern <- paste(selected_structure, collapse = "|")
    structure_traits <- trait_groups |>
      dplyr::filter(stringr::str_detect(structure_measured, structure_pattern)) |>
      dplyr::pull(trait)
    matching_traits <- intersect(matching_traits, structure_traits)
  }
  
  if (!is.null(selected_keywords) && length(selected_keywords) > 0) {
    keyword_pattern <- paste(selected_keywords, collapse = "|")
    keyword_traits <- trait_groups |>
      dplyr::filter(stringr::str_detect(keywords, keyword_pattern)) |>
      dplyr::pull(trait)
    matching_traits <- intersect(matching_traits, keyword_traits)
  }
  
  sort(matching_traits)
})

#' Determine valid filters in the input list
#' @keywords internal


valid_filters <- function(input, exclude_taxon_rank = TRUE){
  
  # Get the names of the input variables
  v <- names(input)
  # Limit to values in the database
  v <- v[v %in% names(austraits)]
  # remove null values
  v <- v[purrr::map_lgl(v, ~ !is.null(input[[.x]]))]
  # remove taxon rank if requested
  if (exclude_taxon_rank) {
    v <- v[v != "taxon_rank"]
  }

  v
}

#' Apply filters
#' @keywords internal
#' Apply all filters (categorical + location + custom)
#' @keywords internal
#' Apply all filters in ONE pass (categorical + location + custom)
#' @keywords internal
apply_filters_categorical <- function(data = austraits, parsed_filters){
  
  filter_expressions <- list()
  
  # 1. TRAIT METADATA FILTERS
  if (!is.null(parsed_filters$trait$trait_name) || 
      !is.null(parsed_filters$trait$trait_grouping) ||
      !is.null(parsed_filters$trait$structure_measured) ||
      !is.null(parsed_filters$trait$keywords)) {
    
    # Get matching traits (compute ONCE, in R memory, not on DuckDB)
    trait_groups <- get("trait_groups", envir = .GlobalEnv)
    matching_traits <- trait_groups$trait
    
    # Filter in R (fast, small dataframe)
    if (!is.null(parsed_filters$trait$trait_grouping)) {
      matching_traits <- trait_groups[trait_groups$trait_group_for_portal %in% parsed_filters$trait$trait_grouping, ]$trait
    }
    
    if (!is.null(parsed_filters$trait$structure_measured)) {
      structure_pattern <- paste(parsed_filters$trait$structure_measured, collapse = "|")
      matching_structure <- trait_groups[grepl(structure_pattern, trait_groups$structure_measured), ]$trait
      matching_traits <- intersect(matching_traits, matching_structure)
    }
    
    if (!is.null(parsed_filters$trait$keywords)) {
      keyword_pattern <- paste(parsed_filters$trait$keywords, collapse = "|")
      matching_keywords <- trait_groups[grepl(keyword_pattern, trait_groups$keywords), ]$trait
      matching_traits <- intersect(matching_traits, matching_keywords)
    }
    
    if (!is.null(parsed_filters$trait$trait_name)) {
      matching_traits <- intersect(matching_traits, parsed_filters$trait$trait_name)
    }
    
    # Build expression (will be applied with all others in ONE filter call)
    if (length(matching_traits) > 0) {
      filter_expressions[[length(filter_expressions) + 1]] <- expr(trait_name %in% !!matching_traits)
    }
  }
  
  # 2. TAXON FILTERS
  if (!is.null(parsed_filters$taxon$family)) {
    pattern <- paste0("^(", paste(parsed_filters$taxon$family, collapse = "|"), ")$")
    filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(family, !!pattern))
  }
  
  if (!is.null(parsed_filters$taxon$genus)) {
    pattern <- paste0("^(", paste(parsed_filters$taxon$genus, collapse = "|"), ")$")
    filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(genus, !!pattern))
  }
  
  if (!is.null(parsed_filters$taxon$taxon_name)) {
    pattern <- paste0("^(", paste(parsed_filters$taxon$taxon_name, collapse = "|"), ")$")
    filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(taxon_name, !!pattern))
  }
  
  # 3. OTHER CATEGORICAL FILTERS
  if (!is.null(parsed_filters$other$basis_of_record)) {
    pattern <- paste0("^(", paste(parsed_filters$other$basis_of_record, collapse = "|"), ")$")
    filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(basis_of_record, !!pattern))
  }
  
  if (!is.null(parsed_filters$other$life_stage)) {
    pattern <- paste0("^(", paste(parsed_filters$other$life_stage, collapse = "|"), ")$")
    filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(life_stage, !!pattern))
  }
  
  # 4. LOCATION FILTERS
  if (!is.null(parsed_filters$location$location) && parsed_filters$location$location != "") {
    
    # Check if geo columns exist (species dataset doesn't have them!)
    has_geo_cols <- all(c("latitude (deg)", "longitude (deg)") %in% names(data))
    
    if (parsed_filters$location$location == "georeferenced" && has_geo_cols) {
      filter_expressions[[length(filter_expressions) + 1]] <- expr(!is.na(`latitude (deg)`) & !is.na(`longitude (deg)`))
      
      # Bounding box
      if (!is.null(parsed_filters$location$min_latitude)) {
        filter_expressions[[length(filter_expressions) + 1]] <- expr(`latitude (deg)` >= !!parsed_filters$location$min_latitude)
      }
      if (!is.null(parsed_filters$location$max_latitude)) {
        filter_expressions[[length(filter_expressions) + 1]] <- expr(`latitude (deg)` <= !!parsed_filters$location$max_latitude)
      }
      if (!is.null(parsed_filters$location$min_longitude)) {
        filter_expressions[[length(filter_expressions) + 1]] <- expr(`longitude (deg)` >= !!parsed_filters$location$min_longitude)
      }
      if (!is.null(parsed_filters$location$max_longitude)) {
        filter_expressions[[length(filter_expressions) + 1]] <- expr(`longitude (deg)` <= !!parsed_filters$location$max_longitude)
      }
    }
    
    # APC taxon distribution
    if (parsed_filters$location$location == "apc" && !is.null(parsed_filters$location$apc_taxon_distribution)) {
      pattern <- paste(parsed_filters$location$apc_taxon_distribution, collapse = "|")
      filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(taxon_distribution, !!pattern))
    }
  }
  
  # 5. CUSTOM FILTERS
  controlled_vocab <- get("controlled_vocab_columns", envir = .GlobalEnv)
  
  for (custom_filter in parsed_filters$custom) {
    column <- custom_filter$column
    values <- custom_filter$value
    
    if (column %in% controlled_vocab) {
      # Special case: dataset_id with semicolon-separated values
      if (column == "dataset_id") {
        pattern <- paste(values, collapse = "|")
        filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(!!rlang::sym(column), !!pattern))
      } else {
        filter_expressions[[length(filter_expressions) + 1]] <- expr(!!rlang::sym(column) %in% !!values)
      }
    } else {
      # Free text - pattern matching
      filter_expressions[[length(filter_expressions) + 1]] <- expr(stringr::str_detect(!!rlang::sym(column), regex(!!values, ignore_case = TRUE)))
    }
  }
  
  if (length(filter_expressions) > 0) {
    cat("[FILTER] Applying combined filter expression\n")

    txt <- 
      purrr::map_chr(filter_expressions, rlang::expr_text) |> paste(collapse = " &\n\t ")
    cat("\t", txt, "\n")

    data <- data |> dplyr::filter(!!!filter_expressions)
  } else {
    cat("[FILTER] No filters to apply\n")
  }


  
  return(data)
}

#' Prepare Austraits data for the portal
#'
#' Flattens the Austraits database, saves definitions and sources, and writes parquet files for data and display.
#'
#' @param austraits The Austraits database object.
#' @param output_dir Directory to write output files.
#' @param overwrite Logical; overwrite existing files if TRUE.
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
    #format_hyperlinks_for_display() |>
    arrow::write_parquet(filename_display)
  
  # Save the species averages dataset
  austraits_species_averages |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages.parquet"))
  
  # Save the display version of the species averages dataset
  austraits_species_averages_display <-     
    austraits_species_averages
    #format_hyperlinks_for_display()
    
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
    select(source_primary_key, source_primary_citation) |>
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
  
  # Load trait groups for processing
  trait_groups <- readr::read_csv(
    "inst/extdata/austraits/trait_groups_for_portal.csv", 
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  
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
#' @keywords internal 
#' @noRd 
#' @param database flattened traits.build object
#' @importFrom tidyselect ends_with starts_with

format_database_for_display <- function(database){
  
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
#' @keywords internal 
#' @noRd
#' @param database flattened traits.build object

format_hyperlinks_for_display <- function(database){
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
  dplyr::select(
    -"source_primary_citation_URL"
  ) 
}

#' Retrieve all assets from a GitHub Release
#'
#' @param version_tag The version tag of the GitHub release (e.g., "6.0.0")
#' @param output_dir The local directory to save the downloaded files
#' @return A vector of paths to the downloaded files
#' @export

retrieve_github_release_parquet <- function(version_tag = "6.0.0", output_dir = system.file("extdata/austraits", package = "austraits.portal")) {
  
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
  downloaded_files <- 
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

#' Parse and validate filter inputs
#' @param input Raw filter inputs from mod_filters
#' @return Parsed and validated filter object
#' @keywords internal
parse_filters <- function(input) {
  
  # Extract taxon filters
  taxon_filters <- list(
    taxon_rank = input$taxon_rank,
    family = if (input$taxon_rank == "family" && !is.null(input$family) && length(input$family) > 0) {
      input$family
    } else {
      NULL
    },
    genus = if (input$taxon_rank == "genus" && !is.null(input$genus) && length(input$genus) > 0) {
      input$genus
    } else {
      NULL
    },
    taxon_name = if (input$taxon_rank == "taxon_name" && !is.null(input$taxon_name) && length(input$taxon_name) > 0) {
      input$taxon_name
    } else {
      NULL
    }
  )
  
  # Extract trait filters
  trait_filters <- list(
    trait_name = if (!is.null(input$trait_name) && length(input$trait_name) > 0) input$trait_name else NULL,
    trait_grouping = if (!is.null(input$trait_grouping) && length(input$trait_grouping) > 0) input$trait_grouping else NULL,
    structure_measured = if (!is.null(input$structure_measured) && length(input$structure_measured) > 0) input$structure_measured else NULL,
    keywords = if (!is.null(input$keywords) && length(input$keywords) > 0) input$keywords else NULL
  )
  
# Extract location filters
  location_filters <- list(
    location = if (!is.null(input$location) && input$location != "") input$location else NULL,
    apc_taxon_distribution = if (!is.null(input$apc_taxon_distribution) && length(input$apc_taxon_distribution) > 0) input$apc_taxon_distribution else NULL,
    min_latitude = NULL,  # Only set if location == "georeferenced"
    max_latitude = NULL,
    min_longitude = NULL,
    max_longitude = NULL
  )
  
  # Only parse bounding box if location is actually "georeferenced"
  if (!is.null(location_filters$location) && location_filters$location == "georeferenced") {
    location_filters$min_latitude <- if (!is.null(input$min_latitude) && !is.na(as.numeric(input$min_latitude))) as.numeric(input$min_latitude) else NULL
    location_filters$max_latitude <- if (!is.null(input$max_latitude) && !is.na(as.numeric(input$max_latitude))) as.numeric(input$max_latitude) else NULL
    location_filters$min_longitude <- if (!is.null(input$min_longitude) && !is.na(as.numeric(input$min_longitude))) as.numeric(input$min_longitude) else NULL
    location_filters$max_longitude <- if (!is.null(input$max_longitude) && !is.na(as.numeric(input$max_longitude))) as.numeric(input$max_longitude) else NULL
  }
  
  # Extract other filters
  other_filters <- list(
    basis_of_record = if (!is.null(input$basis_of_record) && length(input$basis_of_record) > 0) input$basis_of_record else NULL,
    life_stage = if (!is.null(input$life_stage) && length(input$life_stage) > 0) input$life_stage else NULL
  )
  
  # Extract custom filters
  custom_filters <- list()
  for (i in 1:3) {
    col <- input[[paste0("custom_col_", i)]]
    val <- input[[paste0("custom_val_", i)]]
    if (!is.null(col) && !is.null(val) && length(val) > 0 && nchar(paste(val, collapse = "")) > 0) {
      custom_filters[[i]] <- list(column = col, value = val)
    }
  }
  
# Check if there are ANY filters applied
  has_filters <- any(
    !is.null(taxon_filters$family),
    !is.null(taxon_filters$genus),
    !is.null(taxon_filters$taxon_name),
    !is.null(trait_filters$trait_name),
    !is.null(trait_filters$trait_grouping),
    !is.null(trait_filters$structure_measured),
    !is.null(trait_filters$keywords),
    !is.null(other_filters$basis_of_record),
    !is.null(other_filters$life_stage),
    !is.null(location_filters$location),
    length(custom_filters) > 0
  )
  
  return(list(
    taxon = taxon_filters,
    trait = trait_filters,
    location = location_filters,
    other = other_filters,
    custom = custom_filters,
    has_filters = has_filters,
    dataset_type = input$dataset_type
  ))
}
