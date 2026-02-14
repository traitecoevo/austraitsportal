#' Server logic for filter dropdown updates
#' 
#' @description Handles all updateSelectizeInput logic for filters
#' @param input,output,session Shiny session objects
#' @param filters Reactive filters from mod_filters_server
#' @param filtered_query_cache Reactive value for cached query
#' @param current_austraits_display Reactive for current display dataset
#' 
#' @noRd
srv_filter_updates <- function(input, output, session, filters, filtered_query_cache, current_austraits_display) {
  
  # STATIC DROPDOWN INITIALIZATION
  
  # Server-side selectizeInput update for other options that are not conditional
  updateSelectizeInput(session, "filters-trait_name", choices = dropdowns$all_traits, server = TRUE)
  updateSelectizeInput(session, "filters-trait_grouping", choices = dropdowns$all_trait_groupings, server = TRUE)
  updateSelectizeInput(session, "filters-structure_measured", choices = dropdowns$all_structure_measured, server = TRUE)
  updateSelectizeInput(session, "filters-keywords", choices = dropdowns$all_keywords, server = TRUE)
  updateSelectizeInput(session, "filters-basis_of_record", choices = dropdowns$all_bor, server = TRUE)
  updateSelectizeInput(session, "filters-life_stage", choices = dropdowns$all_age, server = TRUE)
  
  # Populate custom column filters
  updateSelectizeInput(session, "filters-custom_col_1", 
                      choices = custom_filter_columns, 
                      selected = character(0),
                      server = TRUE)
  updateSelectizeInput(session, "filters-custom_col_2", 
                      choices = custom_filter_columns,
                      selected = character(0),
                      server = TRUE)
  updateSelectizeInput(session, "filters-custom_col_3", 
                      choices = custom_filter_columns,
                      selected = character(0),
                      server = TRUE)
  
  # DYNAMIC DROPDOWN UPDATES
  
  # Update trait names when trait features are selected
  observeEvent(
    list(
      input[["filters-trait_grouping"]],
      input[["filters-structure_measured"]],
      input[["filters-keywords"]]
    ),
    {
      # Get current selections
      selected_grouping <- input[["filters-trait_grouping"]]
      selected_structure <- input[["filters-structure_measured"]]
      selected_keywords <- input[["filters-keywords"]]
      
      # If any trait feature is selected, filter traits
      if (!is.null(selected_grouping) && length(selected_grouping) > 0 ||
          !is.null(selected_structure) && length(selected_structure) > 0 ||
          !is.null(selected_keywords) && length(selected_keywords) > 0) {
        
        # Use cached helper function
        matching_traits <- get_matching_traits_cached(
          trait_groups,
          selected_grouping,
          selected_structure,
          selected_keywords
        )
        
        # Update trait_name dropdown
        current_trait_selection <- input[["filters-trait_name"]]
        updateSelectizeInput(session, "filters-trait_name",
                            choices = matching_traits,
                            selected = current_trait_selection,
                            server = TRUE)
      } else {
        # No trait features selected - show all traits
        current_trait_selection <- input[["filters-trait_name"]]
        updateSelectizeInput(session, "filters-trait_name",
                            choices = dropdowns$all_traits,
                            selected = current_trait_selection,
                            server = TRUE)
      }
    },
    ignoreInit = TRUE
  )
  
  # Update custom filter columns based on dataset type
  observeEvent(input[["filters-dataset_type"]], {
    cols <- if (input[["filters-dataset_type"]] == "species") {
      custom_filter_columns_species
    } else {
      custom_filter_columns
    }
    
    updateSelectizeInput(session, "filters-custom_col_1", 
                        choices = cols, 
                        selected = character(0), 
                        server = TRUE)
    updateSelectizeInput(session, "filters-custom_col_2", 
                        choices = cols, 
                        selected = character(0), 
                        server = TRUE)
    updateSelectizeInput(session, "filters-custom_col_3", 
                        choices = cols, 
                        selected = character(0), 
                        server = TRUE)
  })
  
  # When column selected, populate values (only for controlled vocab)
  for (i in 1:3) {
    local({
      num <- i
      observeEvent(input[[paste0("filters-custom_col_", num)]], {
        req(input[[paste0("filters-custom_col_", num)]])
        
        column_name <- input[[paste0("filters-custom_col_", num)]]
        
        # Only populate dropdown for controlled vocabulary columns
        if (column_name %in% controlled_vocab_columns) {
          # ALWAYS use full dataset query, not just loaded 100 rows
          query <- filtered_query_cache()

          if (!is.null(query) && column_name %in% names(query)) {
            # Special handling for dataset_id in species averages
            if (column_name == "dataset_id" && filters()$dataset_type == "species") {
              unique_values <- dropdowns$all_dataset_ids_species
            } else {
              # Use cached helper with limit to avoid large collect operations
              unique_values <- get_distinct_values_cached(query, column_name, limit = 1000)
            }
          } else {
            # Special handling for dataset_id in species averages
            if (column_name == "dataset_id" && filters()$dataset_type == "species") {
              unique_values <- dropdowns$all_dataset_ids_species
            } else {
              # Use cached helper with limit
              unique_values <- get_distinct_values_cached(current_austraits_display(), column_name, limit = 1000)
            }
          }
          
          updateSelectizeInput(session, paste0("filters-custom_val_", num), 
                              choices = unique_values, 
                              server = TRUE,
                              selected = NULL)
        }
      })
    })
  }
}

## To be copied in the UI
# svr_filter_updates_ui("filter_updates_1")
    
## To be copied in the server
# svr_filter_updates_server("filter_updates_1")