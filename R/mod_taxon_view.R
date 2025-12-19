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
mod_taxon_view_server <- function(id, filters, filtered_database, current_tab){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    taxon_text <- reactiveVal(NULL)
    
    output$taxon_text <- renderUI({
      req(taxon_text())
      HTML(commonmark::markdown_html(taxon_text()))
    })
    
    observeEvent(
      list(current_tab(), filters()),
      {
        # Only act when Taxon View tab is active
        req(current_tab() == "Taxon View")
        
        filter_vals <- filters()
        
        # Must be in taxon_name mode with exactly 1 selected taxon
        if (filter_vals$taxon_rank != "taxon_name" ||
            is.null(filter_vals$taxon_name) ||
            length(filter_vals$taxon_name) != 1) {
          
          taxon_text(NULL)
          showNotification(
            "Taxon View needs Taxon rank = Taxon name and exactly 1 taxon selected.",
            type = "warning",
            duration = 3
          )
          return()
        }
        
        selected_taxon <- filter_vals$taxon_name[[1]]
        
        # Prefer current filtered DB, otherwise fetch
        data <- filtered_database()
        if (is.null(data)) {
          data <- austraits_display |>
            apply_filters_categorical(filter_vals) |>
            dplyr::collect()
          filtered_database(data)
        }
        
        if (nrow(data) == 0) {
          taxon_text(NULL)
          showNotification(
            "No data available for the selected taxon name",
            type = "warning",
            duration = 5
          )
          return()
        }
        
        # Generate taxon text
        txt <- generate_taxon_text(data, selected_taxon)
        
        # Collapse if it's a vector, check if empty
        if (is.null(txt) || length(txt) == 0 || all(nchar(txt) == 0)) {
          taxon_text(NULL)
          showNotification(
            "Taxon profile text came back empty (generate_taxon_text returned nothing).",
            type = "error",
            duration = 6
          )
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