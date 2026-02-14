#' Data utility functions for AusTraits portal
#' 
#' @description Small helper functions for data manipulation
#' @noRd

#' Check if input has filters
#' @keywords internal
has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Memoised helper to get distinct values from a column (cached for performance)
#' 
#' This version takes a string column name for better memoisation
#' @param data Dataset (DuckDB or Arrow table)
#' @param column_name String name of column
#' @param limit Optional limit on number of values
#' @return Sorted vector of unique values
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
#' 
#' @param trait_groups Trait groups dataframe
#' @param selected_grouping Selected trait groupings
#' @param selected_structure Selected structure measured
#' @param selected_keywords Selected keywords
#' @return Vector of matching trait names
#' @keywords internal
get_matching_traits_cached <- memoise::memoise(function(trait_groups, 
                                                         selected_grouping = NULL, 
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
#' 
#' @param input Input list
#' @param exclude_taxon_rank Whether to exclude taxon_rank
#' @return Vector of valid filter names
#' @keywords internal
valid_filters <- function(input, exclude_taxon_rank = TRUE) {
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