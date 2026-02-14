#' Apply all filters to dataset
#' 
#' @description Applies categorical, location, and custom filters in one pass
#' @param data Base dataset (DuckDB or Arrow table)
#' @param parsed_filters Parsed filter object from parse_filters()
#' @return Filtered dataset (lazy query)
#' @keywords internal
#' @noRd
apply_filters <- function(data = austraits, parsed_filters) {
  
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
    has_geo_cols <- all(c("latitude (deg)", "longitude (deg)") %in% colnames(data))
    
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
  
  # APPLY ALL FILTERS
  
  if (length(filter_expressions) > 0) {
    cat("[FILTER] Applying combined filter expression\n")
    
    # Log expressions for debugging
    txt <- purrr::map_chr(filter_expressions, rlang::expr_text) |> 
      paste(collapse = " &\n\t ")
    cat("\t", txt, "\n")
    
    # Apply all filters at once using splice operator
    data <- data |> dplyr::filter(!!!filter_expressions)
  } else {
    cat("[FILTER] No filters to apply\n")
  }
  
  return(data)
}