#' taxon_view UI Function
#'
#' @description A shiny Module for displaying taxon profiles
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_taxon_view_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_header("AusTraits taxon profile"),
    card_body(
      htmlOutput(ns("taxon_text"))
    )
  )
}
    
#' taxon_view Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filters Reactive containing filter values
#' @param filtered_database Reactive containing filtered data
#' @param current_tab Reactive containing current tab name
#'
#' @noRd

mod_taxon_view_server <- function(id, filters, filtered_database, current_tab, apply_filters_trigger){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    taxon_text <- reactiveVal(NULL)
    
    # Cached reactive for generating taxon text
taxon_text_generated <- eventReactive(apply_filters_trigger(), {
      req(current_tab() == "Taxon View")
      
      start_time <- Sys.time()
      
      filter_vals <- filters()
      cache_key_rank <- filter_vals$taxon_type
      cache_key_name <- filter_vals$taxon_name
      
      # Must be in taxon_name mode with exactly 1 selected taxon
      if (cache_key_rank != "taxon_name" ||
          is.null(cache_key_name) ||
          length(cache_key_name) != 1) {
        return(NULL)
      }
      
      selected_taxon <- cache_key_name[[1]]
      txt <- generate_taxon_text_cached(selected_taxon)

      elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
      cat("[TAXON VIEW] Completed in", round(elapsed, 3), "seconds\n")
      
      txt
    })

    output$taxon_text <- renderUI({
      filter_vals <- filters()

      # Helper function to show empty state message
      show_empty_state <- function() {
        tags$div(
          style = "background: #e3f2fd; border-left: 4px solid #2196f3; padding: 20px; margin: 20px 0; border-radius: 4px;",
          tags$div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 12px;",
            icon("info-circle", style = "color: #1976d2; font-size: 24px;"),
            tags$h4(
              style = "color: #1565c0; margin: 0;",
              "Select a Taxon to View Profile"
            )
          ),
          tags$p(
            style = "margin: 0 0 10px 0; font-size: 15px; color: #424242;",
            "To view a taxon profile, please:"
          ),
          tags$ol(
            style = "margin: 0; padding-left: 20px; color: #424242;",
            tags$li("Set the taxonomy filter to 'Taxon name'"),
            tags$li("Select exactly one taxon from the dropdown"),
            tags$li("Click 'Apply Filters'")
          )
        )
      }

      # Check if conditions are met for taxon view
      if (filter_vals$taxon_type != "taxon_name" ||
          is.null(filter_vals$taxon_name) ||
          length(filter_vals$taxon_name) != 1) {
        return(show_empty_state())
      }

      # Show the taxon text if available, otherwise show empty state
      txt <- taxon_text()
      if (is.null(txt)) {
        return(show_empty_state())
      }

      txt |>
        commonmark::markdown_html() |>
        add_target_blank() |>
        HTML()
    })
    
    observeEvent(
      list(current_tab(), apply_filters_trigger()),
      {
        # Only act when Taxon View tab is active
        req(current_tab() == "Taxon View")
        
        filter_vals <- filters()
        
        # Must be in taxon_name mode with exactly 1 selected taxon
        if (filter_vals$taxon_type != "taxon_name" ||
            is.null(filter_vals$taxon_name) ||
            length(filter_vals$taxon_name) != 1) {
          
          taxon_text(NULL)
          return()
        }
        
        selected_taxon <- filter_vals$taxon_name[[1]]
        
        # Prefer current filtered DB, otherwise fetch
        data <- filtered_database()
        if (is.null(data)) {
          data <- austraits_display |>
            apply_filters(filter_vals) |>
            dplyr::collect()
          filtered_database(data)
        }
        
        if (nrow(data) == 0) {
          taxon_text(NULL)
          return()
        }
        
        # Get cached taxon text
        txt <- taxon_text_generated()
        
        # Collapse if it's a vector, check if empty
        if (is.null(txt) || length(txt) == 0 || all(nchar(txt) == 0)) {
          taxon_text(NULL)
          return()
        }
        
        # Collapse the vector into a single string if needed
        if (length(txt) > 1) {
          txt <- paste(txt, collapse = "\n\n")
        }
        
        taxon_text(txt)
      },
      ignoreInit = TRUE
    )
  })
}
    
## To be copied in the UI
# mod_taxon_view_ui("taxon_view_1")
    
## To be copied in the server
# mod_taxon_view_server("taxon_view_1")
