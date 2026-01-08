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
      div(
        DT::DTOutput(ns("data_table"))
      ),
      div(
        uiOutput(ns("load_more_button"))
      )
    )
  )
}
    
#' data_table Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_database Reactive containing filtered data
#' @param filtered_query_cache Reactive containing the unfiltered query for loading more data
#' @param columns_display Vector of column names to display
#'
#' @noRd 
mod_data_table_server <- function(id, filtered_database, filtered_query_cache, columns_display){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dt_proxy <- reactiveVal(NULL)
    desired_start <- reactiveVal(0) 
    
    output$data_table <- DT::renderDT({
      display_data <- filtered_database()
      
      if (is.null(display_data)) {
        return(DT::datatable(data.frame(), options = list(pageLength = 10)))
      }
            
      total_rows <- attr(display_data, "total_rows")
      if (is.null(total_rows)) total_rows <- nrow(display_data)
      
      safe_columns_display <- columns_display[columns_display %in% names(display_data)]

      no_filter_cols <- which(names(display_data) %in% c("value", "unit", "entity_type", "value_type", "replicates"))
      hide_cols <- which(names(display_data) %not_in% safe_columns_display)
      thin_cols <- which(names(display_data) %not_in% c("taxon_name", "trait_name", "genus", "family"))
      
      info_text <- paste0("Showing _START_ to _END_ of ", total_rows, " entries (loaded ", nrow(display_data), " rows)")
      
      dt <- DT::datatable(
        data = display_data,
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        class = "cell-border stripe nowrap",
        options = list(
          pageLength = 10,
          displayStart = desired_start(),
          searching = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          info = TRUE,
          language = list(info = info_text),
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

    # Show "Load More" button if there's more data
    output$load_more_button <- renderUI({
      data <- filtered_database()
      if (is.null(data)) return(NULL)
      
      total_rows <- attr(data, "total_rows")
      if (is.null(total_rows)) total_rows <- nrow(data)
      
      if (nrow(data) < total_rows) {
        tags$div(
          style = "bottom: 20px; right: 20px; text-align: right; padding: 15px; margin-top: 10px; background: #f8f9fa; border-top: 1px solid #dee2e6;",
          actionButton(
            ns("load_more"), 
            sprintf("Load next 100 rows (%d of %d total)", nrow(data), total_rows),
            class = "btn btn-primary",
            icon = icon("download")
          )
        )
      }
    })

    return(list(
      dt_proxy = dt_proxy,
      visible_rows = reactive(input$data_table_rows_all),
      load_more_click = reactive(input$load_more),  # ADD THIS
      set_start = function(x) desired_start(x)
    ))
  })
}
    
## To be copied in the UI
# mod_data_table_ui("data_table_1")
    
## To be copied in the server
# mod_data_table_server("data_table_1")