#' Server logic for data download
#' 
#' @description Handles download UI and download handler
#' @param input,output,session Shiny session objects
#' @param filtered_database Reactive value for filtered data
#' @param filtered_query_cache Reactive value for cached query
#' @param current_austraits Reactive for current full dataset
#' @param filters Reactive filters from mod_filters_server
#' @param data_table_outputs Output from data table module
#' @param full_filtered_cache Reactive value for full filtered cache
#' 
#' @noRd
srv_download <- function(input, output, session, 
                         filtered_database, 
                         filtered_query_cache, 
                         current_austraits,
                         filters,
                         data_table_outputs,
                         full_filtered_cache) {
  
  # DOWNLOAD DATA PREPARATION
  
  # Set up download data as reactive expression
  download_data_table <- reactive({
    # Use the query cache to get ALL filtered data
    query <- filtered_query_cache()
    
    if (is.null(query)) {
      return(NULL)
    }
    
    # Check if user applied DataTable column filters
    if (!is.null(data_table_outputs$visible_rows())) {
      visible_rows <- data_table_outputs$visible_rows()
      display_db <- filtered_database()
      
      if (!is.null(display_db) && length(visible_rows) > 0 && length(visible_rows) < nrow(display_db)) {
        # User filtered within DataTable - only download visible rows
        # Collect filtered query first, then filter in R
        full_data <- query |> dplyr::collect()
        display_db_filtered <- display_db[visible_rows, , drop = FALSE]
        
        # Filter in R (not DuckDB)
        result <- full_data |>
          dplyr::filter(row_id %in% display_db_filtered$row_id)
        
        # Join with full dataset to get all columns
        row_ids <- result$row_id
        return(current_austraits() |> 
                 dplyr::collect() |>  # Collect EVERYTHING first
                 dplyr::filter(row_id %in% row_ids))
      }
    }
    
    # Default: download ALL filtered data
    # Just collect the filtered query and join with full dataset
    filtered_ids <- query |> 
      dplyr::select(row_id) |> 
      dplyr::collect() |>
      dplyr::pull(row_id)
    
    # Collect full dataset and filter in R
    current_austraits() |> 
      dplyr::collect() |>
      dplyr::filter(row_id %in% filtered_ids)
  })
  
  # DYNAMIC DOWNLOAD UI WITH WARNING
  
  output[["filters-download_ui"]] <- renderUI({
    display_db <- filtered_database()
    
    if (is.null(display_db)) {
      return(downloadButton("filters-download_data", "Download Data", class = "btn-primary w-100", icon = icon("download")))
    }
    
    total_rows <- attr(display_db, "total_rows")
    if (is.null(total_rows)) total_rows <- nrow(display_db)
    
    if (total_rows >= 100000) {
      # Large dataset - show warning
      tagList(
        div(
          style = "background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404;"),
          span(
            style = "color: #856404; margin-left: 5px; font-weight: bold;",
            sprintf("⚠️ Large download: %s rows", format(total_rows, big.mark = ","))
          )
        ),
        downloadButton(
          "filters-download_data",
          sprintf("Confirm & Download (%s rows)", format(total_rows, big.mark = ",")),
          class = "btn-warning w-100",
          icon = icon("download")
        )
      )
    } else {
      # Small dataset - direct download
      downloadButton("filters-download_data", "Download Data", class = "btn-primary w-100", icon = icon("download"))
    }
  })
  
  # DOWNLOAD HANDLER
  
  output[["filters-download_data"]] <- downloadHandler(
    filename = function() {
      paste("austraits-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Clear memory caches before download
      full_filtered_cache(NULL)
      gc()
      
      tmpdir = tempfile(pattern="tempdir", fileext = ".dir")
      dir.create(tmpdir)
      csv_file <- file.path(tmpdir, "austraits-data.csv")
      bib_file <- file.path(tmpdir, "sources.bib")
      sources_file <- file.path(tmpdir, "sources.csv")
      html_file <- file.path(tmpdir, "usage.html")

      data_query <- download_data_table()

      if (is.null(data_query)) {
        data.frame(message = "No data selected") |> 
          utils::write.csv(csv_file, row.names = FALSE)
        writeLines("", bib_file)
        writeLines("<p>No data selected</p>", html_file)
      } else {
        arrow::write_csv_arrow(data_query, csv_file)
        
        # Only collect distinct keys, not full dataset
        keys <- data_query |> 
          dplyr::select(source_primary_key) |> 
          dplyr::distinct() |> 
          dplyr::pull(source_primary_key) |>
          # This handles multiple keys pasted together with "; ", an issue for species averages
          stringr::str_split(pattern = "; ") |> unlist() |> sort() |> unique()

        sources |>
          dplyr::filter(source_primary_key %in% keys) |>
          arrow::write_csv_arrow(sources_file)

        export_bibtex_for_data(keys, bib_file)
        
        # Only collect a small sample for usage text to avoid memory crash
        total_rows <- attr(filtered_database(), "total_rows")
        if (is.null(total_rows)) total_rows <- nrow(filtered_database())

        if (!is.null(total_rows) && total_rows < 50000) {
          # Small dataset - generate full usage text
          sample_data <- data_query |> 
            dplyr::slice_head(n = 1000)
          usage_text_content <- generate_usage_and_citations_text(sample_data)
          htmltools::save_html(usage_text_content, html_file)
          rm(sample_data)
          gc()
        } else {
          # Large dataset - skip usage text to save memory
          writeLines("<h3>Usage Information</h3><p>For large datasets, please refer to the AusTraits documentation at <a href='https://traitecoevo.github.io/austraits/'>https://traitecoevo.github.io/austraits/</a></p>", html_file)
        }
      }
      
      # Log download event
      log_telemetry_event("download", details = list(
        dataset_type = filters()$dataset_type,
        trait = filters()$trait_name
      ))
      
      showNotification("Downloading filtered data...",
                      type = "message",
                      duration = 3)

      zip::zip(
        zipfile = file, 
        files = list.files(tmpdir, full.names = TRUE), 
        mode = "cherry-pick"
      )

      unlink(tmpdir, recursive = TRUE)

      # Force garbage collection to free memory
      gc()
    },
    contentType = "application/zip"
  )
}