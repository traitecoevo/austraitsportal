#' Server logic for URL query parameter handling
#' 
#' @description Loads filters from URL on app startup
#' @param input,output,session Shiny session objects
#' @param family_choices Reactive for family choices
#' @param genus_choices Reactive for genus choices
#' @param taxon_name_choices Reactive for taxon name choices
#' 
#' @noRd
srv_url_params <- function(input, output, session, 
                           family_choices, 
                           genus_choices, 
                           taxon_name_choices,
                           loading_from_url) {
  
  # URL Query Parameter Handler - Load filters from URL on app startup
  url_processed <- reactiveVal(FALSE)

observe({
    # Only run once
    if (url_processed()) return()
    
    query <- parseQueryString(session$clientData$url_search)
    
    # Only process if query parameters exist
    if (length(query) > 0) {

        loading_from_url(TRUE)
      
      # FREEZE reactivity while updating all inputs
      isolate({
        
        # Handle taxon rank first
        if (!is.null(query$taxon_rank) && query$taxon_rank %in% c("all", "family", "genus", "taxon_name")) {
          updateRadioButtons(session, "filters-taxon_rank", selected = query$taxon_rank)
        }
        
        # Handle taxon selections WITH CHOICES
        if (!is.null(query$family)) {
          updateSelectizeInput(session, "filters-family", 
                              choices = family_choices(), 
                              selected = query$family, 
                              server = TRUE)
        }
        
        if (!is.null(query$genus)) {
          updateSelectizeInput(session, "filters-genus", 
                              choices = genus_choices(), 
                              selected = query$genus, 
                              server = TRUE)
        }
        
        if (!is.null(query$taxon_name)) {
          updateSelectizeInput(session, "filters-taxon_name", 
                              choices = taxon_name_choices(), 
                              selected = query$taxon_name, 
                              server = TRUE)
        }
        
        # Handle trait name WITH CHOICES
        if (!is.null(query$trait_name)) {
          updateSelectizeInput(session, "filters-trait_name", 
                              choices = dropdowns$all_traits, 
                              selected = query$trait_name, 
                              server = TRUE)
        }
        
        # Handle location filters
        if (!is.null(query$location) && query$location %in% c("georeferenced", "apc")) {
          updateRadioButtons(session, "filters-location", selected = query$location)
        }
        
        if (!is.null(query$apc_taxon_distribution)) {
          states <- strsplit(query$apc_taxon_distribution, ",")[[1]]
          updateSelectizeInput(session, "filters-apc_taxon_distribution", 
                              choices = dropdowns$all_states_territories, 
                              selected = states, 
                              server = TRUE)
        }
        
        # Handle basis of record WITH CHOICES
        if (!is.null(query$basis_of_record)) {
          updateSelectizeInput(session, "filters-basis_of_record", 
                              choices = dropdowns$all_bor, 
                              selected = query$basis_of_record, 
                              server = TRUE)
        }
        
        # Handle life stage WITH CHOICES
        if (!is.null(query$life_stage)) {
          updateSelectizeInput(session, "filters-life_stage", 
                              choices = dropdowns$all_age, 
                              selected = query$life_stage, 
                              server = TRUE)
        }
        
        # Handle tab switching
        if (!is.null(query$tab)) {
          tab_name <- utils::URLdecode(query$tab)
          updateNavbarPage(session, "main_tabs", selected = tab_name)
        }
        
      })
      Sys.sleep(1)
        loading_from_url(FALSE)
      # Mark as processed
      url_processed(TRUE)
    }
  })
}
