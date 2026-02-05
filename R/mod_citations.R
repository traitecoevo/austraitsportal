#' citations UI Function
#'
#' @description A shiny Module for displaying citations
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_citations_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_header("Referencing your filtered data"),
    card_body(
      htmlOutput(ns("usage_text"))
    )
  )
}
    
#' citations Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_query_cache Reactive containing full arrow query (not paginated display data)
#'
#' @noRd 
mod_citations_server <- function(id, filtered_query_cache){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    usage_text <- reactiveVal(NULL)
    
    # Update usage text when filtered data changes
    observe({
      query_data <- filtered_query_cache()
      
      if (!is.null(query_data)) {
        # Collect ALL rows for citations (not just display limit)
        data_collected <- query_data |> 
          dplyr::select(source_primary_citation, source_primary_key) |>
          dplyr::distinct() |>
          dplyr::collect()
        
        if (nrow(data_collected) > 0) {
          usage_text(generate_usage_and_citations_text(data_collected))
        } else {
          usage_text(NULL)
        }
      } else {
        usage_text(NULL)
      }
    })
    
    output$usage_text <- renderUI({
      usage_text()
    })
    
    # Return usage_text for download handler
    return(reactive({ usage_text() }))
  })
}
    
## To be copied in the UI
# mod_citations_ui("citations_1")
    
## To be copied in the server
# mod_citations_server("citations_1")