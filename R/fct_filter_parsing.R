#' Parse and validate filter inputs
#' 
#' @description Converts raw filter inputs into structured, validated filter object
#' @param input Raw filter inputs from mod_filters
#' @return Parsed and validated filter object with structure:
#'   - taxon: list(taxon_rank, family, genus, taxon_name)
#'   - trait: list(trait_name, trait_grouping, structure_measured, keywords)
#'   - location: list(location, apc_taxon_distribution, min/max lat/long)
#'   - other: list(basis_of_record, life_stage)
#'   - custom: list of custom filter objects
#'   - has_filters: boolean indicating if any filters applied
#'   - dataset_type: "raw" or "species"
#' @keywords internal
#' @noRd
parse_filters <- function(input) {
  
  # TAXON FILTERS
  
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
  
  # TRAIT FILTERS
  
  trait_filters <- list(
    trait_filter_type = input$trait_filter_type,
    trait_name = if (!is.null(input$trait_name) && length(input$trait_name) > 0) input$trait_name else NULL,
    trait_grouping = if (input$trait_filter_type == "features" && !is.null(input$trait_grouping) && length(input$trait_grouping) > 0) input$trait_grouping else NULL,
    structure_measured = if (input$trait_filter_type == "features" && !is.null(input$structure_measured) && length(input$structure_measured) > 0) input$structure_measured else NULL,
    keywords = if (input$trait_filter_type == "features" && !is.null(input$keywords) && length(input$keywords) > 0) input$keywords else NULL
  )
  
  # LOCATION FILTERS
  
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
  
  # OTHER FILTERS
  
  other_filters <- list(
    basis_of_record = if (!is.null(input$basis_of_record) && length(input$basis_of_record) > 0) input$basis_of_record else NULL,
    life_stage = if (!is.null(input$life_stage) && length(input$life_stage) > 0) input$life_stage else NULL
  )
  
  # CUSTOM FILTERS
  
  custom_filters <- list()
  for (i in 1:3) {
    col <- input[[paste0("custom_col_", i)]]
    val <- input[[paste0("custom_val_", i)]]
    if (!is.null(col) && !is.null(val) && length(val) > 0 && nchar(paste(val, collapse = "")) > 0) {
      custom_filters[[i]] <- list(column = col, value = val)
    }
  }
  
  # VALIDATION
  
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