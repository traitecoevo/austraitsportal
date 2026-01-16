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
    current_order <- reactiveVal(NULL)
    
    output$data_table <- DT::renderDT({
      display_data <- filtered_database()
      
      if (is.null(display_data)) {
        return(DT::datatable(data.frame(), options = list(pageLength = 10)))
      }
            
      total_rows <- attr(display_data, "total_rows")
      if (is.null(total_rows)) total_rows <- nrow(display_data)
      
      safe_columns_display <- columns_display[columns_display %in% names(display_data)]

      no_filter_cols <- which(names(display_data) %in% c("replicates"))
      hide_cols <- which(names(display_data) %not_in% safe_columns_display)
      thin_cols <- which(names(display_data) %not_in% c("taxon_name", "trait_name", "genus", "family"))
      
      info_text <- paste0("Showing _START_ to _END_ of ", total_rows, " entries (loaded ", nrow(display_data), " rows)")
      
      dt <- DT::datatable(
        data = display_data,
        escape = FALSE,
        rownames = FALSE,
        filter = "none",
        class = "cell-border stripe nowrap",
        options = list(
          pageLength = 10,
          displayStart = desired_start(),
          searching = FALSE,  # Keep FALSE - we'll handle filtering manually
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
    
    # Track last filter state to avoid unnecessary reloads
    last_column_filters <- reactiveVal("")

    # Debounce column filter changes
    column_filters_debounced <- debounce(reactive({
      paste(input$data_table_search_columns, collapse = "|")
    }), 2000)

    observeEvent(column_filters_debounced(), {
      req(filtered_query_cache())
      
      # Check if filters actually changed
      current_filters <- paste(input$data_table_search_columns, collapse = "|")
      if (identical(current_filters, last_column_filters())) {
        return()
      }
      last_column_filters(current_filters)
      
      column_filters <- input$data_table_search_columns
      
      # Check if any column has a filter value
      has_column_filters <- any(!is.na(column_filters) & column_filters != "")
      
      if (has_column_filters) {
        cat("Column filters applied, querying server\n")
        
        query <- filtered_query_cache()
        display_data <- filtered_database()
        
        # Apply column filters to query
        for (i in seq_along(column_filters)) {
          filter_value <- column_filters[i]
          
          if (!is.na(filter_value) && filter_value != "") {
            col_name <- names(display_data)[i]
            
            # Use partial matching - type "leaf|seed" for multi-select
            query <- query |>
              dplyr::filter(stringr::str_detect(!!rlang::sym(col_name), !!filter_value))
            
            cat("Applied filter on", col_name, ":", filter_value, "\n")
          }
        }
        
        # Get total count of filtered results
        filtered_count <- query |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n)

        # Only use lazy loading for large datasets (> 10,000 rows)
        if (filtered_count > 10000) {
          # Load first 100 rows only
          filtered_data <- query |>
            dplyr::slice_head(n = 100) |>
            dplyr::collect()
          cat("Loaded 100 of", filtered_count, "filtered rows\n")
        } else {
          # Load all data for small datasets
          filtered_data <- query |>
            dplyr::collect()
          cat("Loaded all", filtered_count, "filtered rows\n")
        }

        attr(filtered_data, "total_rows") <- filtered_count

        isolate({
          filtered_database(filtered_data)
        })
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # Observe sorting changes
    observeEvent(input$data_table_order, {
      req(filtered_query_cache())
      
      order_info <- input$data_table_order
      
      if (!identical(order_info, current_order())) {
        current_order(order_info)
        
        cat("Sorting changed, reloading top 100 sorted rows\n")
        
        query <- filtered_query_cache()
        display_data <- filtered_database()
        total_rows <- attr(display_data, "total_rows")
        
        if (is.null(total_rows)) return()
        
        # Apply any existing column filters first
        column_filters <- input$data_table_search_columns
        if (!is.null(column_filters)) {
          for (i in seq_along(column_filters)) {
            filter_value <- column_filters[i]
            if (!is.na(filter_value) && filter_value != "") {
              col_name <- names(display_data)[i]
              filter_pattern <- paste0("^(", filter_value, ")$")
              query <- query |>
                dplyr::filter(stringr::str_detect(!!rlang::sym(col_name), filter_pattern))
            }
          }
        }
        
        # Apply sorting
        if (!is.null(order_info) && length(order_info) > 0) {
          col_idx <- order_info[[1]][[1]] + 1
          direction <- order_info[[1]][[2]]
          col_name <- names(display_data)[col_idx]
          
          if (direction == "asc") {
            sorted_data <- query |>
              dplyr::arrange(!!rlang::sym(col_name)) |>
              dplyr::slice_head(n = 100) |>
              dplyr::collect()
          } else {
            sorted_data <- query |>
              dplyr::arrange(desc(!!rlang::sym(col_name))) |>
              dplyr::slice_head(n = 100) |>
              dplyr::collect()
          }
          
          attr(sorted_data, "total_rows") <- total_rows
          filtered_database(sorted_data)
          
          cat("Loaded top 100 rows sorted by", col_name, direction, "\n")
        }
      }
    }, ignoreInit = TRUE)

    output$load_more_button <- renderUI({
      data <- filtered_database()
      if (is.null(data)) return(NULL)
      
      total_rows <- attr(data, "total_rows")
      if (is.null(total_rows)) total_rows <- nrow(data)
      
      if (nrow(data) < total_rows) {
        tags$div(
          style = "text-align: right; padding: 15px; margin-top: 10px; background: #f8f9fa; border-top: 1px solid #dee2e6;",
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
      load_more_click = reactive(input$load_more),
      set_start = function(x) desired_start(x),
      current_order = current_order
    ))
  })
}
    
## To be copied in the UI
# mod_data_table_ui("data_table_1")
    
## To be copied in the server
# mod_data_table_server("data_table_1")