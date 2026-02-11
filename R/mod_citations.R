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
    
    # Cached reactive for generating citations text
    citations_data <- reactive({
      start_time <- Sys.time()
      cat("\n[CITATIONS] Starting citations generation\n")
      
      query_data <- filtered_query_cache()
      
      if (!is.null(query_data)) {
        # Collect ALL rows for citations (not just display limit)
        data_collected <- query_data |> 
          dplyr::select(source_primary_citation, source_primary_key) |>
          dplyr::distinct() |>
          dplyr::collect()
        
        if (nrow(data_collected) > 0) {
          result <- generate_usage_and_citations_text(data_collected)
          elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
          cat("[CITATIONS] Completed in", round(elapsed, 3), "seconds\n")
          return(result)
        }
      }
      return(NULL)
    })
    
    usage_text <- reactiveVal(NULL)
    
    # Update usage_text from cached reactive
    observe({
      usage_text(citations_data())
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
