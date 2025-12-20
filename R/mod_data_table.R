#' data_table UI Function
#'
#' @description A shiny Module for displaying the data table
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_table_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_body(
      fillable = TRUE,
      DT::DTOutput(ns("data_table"))
    )
  )
}
    
#' data_table Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_database Reactive containing filtered data
#' @param columns_display Vector of column names to display
#'
#' @noRd 
mod_data_table_server <- function(id, filtered_database, columns_display){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dt_proxy <- reactiveVal(NULL)
    
    output$data_table <- DT::renderDT({
      display_data <- filtered_database()
      
      if (is.null(display_data)) {
        return(DT::datatable(data.frame(), options = list(pageLength = 10)))
      }
      
      # Column configurations
      no_filter_cols <- which(names(display_data) %in% c("value", "unit", "entity_type", "value_type", "replicates"))
      hide_cols <- which(names(display_data) %not_in% columns_display)
      thin_cols <- which(names(display_data) %not_in% c("taxon_name", "trait_name", "genus", "family"))
      
      dt <- DT::datatable(
        data = display_data,
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        class = "cell-border stripe nowrap",
        options = list(
          pageLength = 10,
          searching = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(targets = no_filter_cols - 1, searchable = FALSE),
            list(targets = hide_cols - 1, visible = FALSE),
            list(targets = thin_cols - 1, className = "truncated")
          ),
          serverSide = FALSE
        )
      )
      
      dt_proxy(DT::dataTableProxy(ns("data_table")))
      return(dt)
    })
    
    # Return proxy and visible rows for download functionality
    return(list(
      dt_proxy = dt_proxy,
      visible_rows = reactive(input$data_table_rows_all)
    ))
  })
}
    
## To be copied in the UI
# mod_data_table_ui("data_table_1")
    
## To be copied in the server
# mod_data_table_server("data_table_1")