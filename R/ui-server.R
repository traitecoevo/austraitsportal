#' User interface (UI) for AusTraits Data Portal

austraits_ui <- function(){
  
  ui <- page_sidebar(
    
    # Set the overall theme of the app
    theme = bs_theme(preset = "flatly"),
    
    # Title of the portal
    title = "AusTraits Data Portal",
    
    # Create a sidebar for the app
    sidebar = sidebar(
      # Filter by taxonomic information
      h5("Taxonomic information"),
      
      radioButtons("user_taxon_rank", 
                  label = "Filter by which taxon name:",
                  choices = c('Taxon name' = "taxon_name",
                              'Genus' = "genus", 
                              'Family' = "family"
                              )
                  ),
      
      # Only show this panel if Taxon name is selected
      conditionalPanel(
        condition = 'input.user_taxon_rank == "taxon_name"',
        ## By taxon_name
        selectizeInput("user_taxon_name",
                       label = "Taxon name:",
                       choices = NULL,
                       multiple = TRUE)
        ),
      
    # Only show this panel if Genus is selected
    conditionalPanel(
      condition = 'input.user_taxon_rank == "genus"',
      ## By genus
      selectizeInput("user_genus",
                     label = "Genus:",
                     choices = NULL,
                     multiple = TRUE
    )
  ),
  # Only show this panel if family is selected  
  conditionalPanel(
    condition = 'input.user_taxon_rank == "family"',
  ## By family
      selectizeInput("user_family",
                     label = "Family:",
                     choices = NULL,
                     multiple = TRUE
      )
  ),
      
      br(),
      actionButton("clear_filters", "Clear Filters", 
                   class = "btn-warning w-100"),
      
      # Download button
      downloadButton("download_data", "Download displayed data")
    ),
    
    # Data display
    card(
      card_header("Data Preview"),
      card_body(
        fillable = TRUE,
        DT::DTOutput("data_table")
      )
    )
  )
}


#' AusTraits Data Portal Server Logic
#'
#' @param input 
#' @param output 
#' @param session 

austraits_server <- function(input, output, session) {
  # Server-side selectize for taxon_name
  updateSelectizeInput(
    session,
    "user_taxon_name",
    choices = all_taxon_names,
    selected = NULL,
    server = TRUE
  )
  
    # Server-side selectize for genus
  updateSelectizeInput(
    session,
    "user_genus",
    choices = all_genus,
    selected = NULL,
    server = TRUE
  )
  
  # Server-side selectize for family
  updateSelectizeInput(
    session,
    "user_family",
    choices = all_family,
    selected = NULL,
    server = TRUE
  )
  
  # Reactive value to store the filtered data later
  filtered_database <- reactiveVal(NULL)

  # Filter data by taxonomic information
  # Watch for changes in user_taxon_name
  observeEvent(input$user_taxon_name,
               {
                 # Requirements for this modules to work
                 # Input returns as a character vector of genus
                 req(input$user_taxon_name)
                 
                 # Filter by taxonomic info
                 filtered_by_taxonomy <- austraits |> 
                   extract_taxa(taxon_name = input$user_taxon_name)
                 
                 # Store in reactive
                 filtered_database(filtered_by_taxonomy)
               }
  )
  
  # Watch for changes in user-genus
  observeEvent(input$user_genus,
               {
                 # Requirements for this modules to work
                 # Input returns as a character vector of genus
                 req(input$user_genus)
                 
                 # Filter by taxonomic info
                 filtered_by_taxonomy <- austraits |> 
                   extract_taxa(genus = input$user_genus)
                 
                 # Store in reactive
                 filtered_database(filtered_by_taxonomy)
               }
  )
  
  # Watch for changes in user-family
  observeEvent(input$user_family,
               {
                 # Requirements for this modules to work
                 # Input returns as a character vector of genus
                 req(input$user_family)
                 
                 # Filter by taxonomic info
                 filtered_by_taxonomy <- austraits |> 
                   extract_taxa(family = input$user_family)
                 
                 # Store in reactive
                 filtered_database(filtered_by_taxonomy)
               }
  )

  # Clear filters button action
  observeEvent(input$clear_filters, {
    req(filtered_database())
    
    # Clear the filter values
    # Server-side selectize for taxon_name
    updateSelectizeInput(
      session,
      "user_taxon_name",
      choices = all_taxon_names,
      selected = NULL,
      server = TRUE
    )
  
    
    # Server-side selectize for genus
    updateSelectizeInput(
      session,
      "user_genus",
      choices = all_genus,
      selected = NULL,
      server = TRUE
    )
    
    # Server-side selectize for family
    updateSelectizeInput(
      session,
      "user_family",
      choices = all_family,
      selected = NULL,
      server = TRUE
    )
    
    # Store nothing in filtered_data()
    filtered_database(NULL)
    
    # Show notification
    showNotification("Filters have been cleared", 
                     type = "message", 
                     duration = 3)
  })
  
  # Set up display data
  display_data_table <- reactive({
    # Get the current filtered database
    filtered_db <- filtered_database()
    
    # Check if it's NULL and return appropriate value
    if (is.null(filtered_db)) {
      return(NULL)
    }
    
    # Format the database for display
    format_database_for_display(filtered_db)
  })
  
  # Set up download data as reactive expression  
  download_data_table <- reactive({
    # Get the current filtered database
    filtered_db <- filtered_database()
    
    # Check if it's NULL and return appropriate value
    if (is.null(filtered_db)) {
      return(NULL)
    }
    
    # Format the database for download
    format_database_for_download(filtered_db)
  })
  
  ### Render user selected data table output
  output$data_table <- DT::renderDT({
    datatable(
      data = display_data_table(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = 'none',
      class = 'cell-border stripe'
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("austraits-6.0.0-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(download_data_table(), file, row.names = FALSE)
    }
  )
}


