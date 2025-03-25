#' User interface (UI) for AusTraits Data Portal

austraits_ui <- function(){
  
  ui <- page_sidebar(
    
    # Set the overall theme of the app
    theme = bs_theme(preset = "flatly"),
    
    # Title of the portal
    title = "AusTraits Data Portal",
    
    # Create a sidebar for the app
    sidebar = sidebar(
      title = "Controls",
      
      # Filter by taxonomic information
      ## By genus
      selectizeInput("user_genus",
                     label = "Filter by genus:",
                     choices = NULL,
                     multiple = TRUE
      ),
      ## By family
      selectizeInput("user_family",
                     label = "Filter by family:",
                     choices = NULL,
                     multiple = TRUE
      ),
      
      # Download button
      downloadButton("download_data", "Download displayed data")
    ),
    
    # Data display
    card(
      card_header("Data Preview"),
      DT::DTOutput("data_table")
    )
    
  )
}


#' AusTraits Data Portal Server Logic
#'
#' @param input 
#' @param output 
#' @param session 

austraits_server <- function(input, output, session) {
  # Server-side selectize for genus
  updateSelectizeInput(
    session,
    "user_genus",
    choices = all_genus,
    selected = NULL,
    server = TRUE
  )
  
  # Reactive value to store the filtered data later
  filtered_data <- reactiveVal(NULL)

  # Filter data by taxonomic information
  # Watch for changes in user-genus
  observeEvent(
    list(input$user_genus
         ),
               {
                 # Requirements for this modules to work
                 # Input returns as a character vector of genus
                 req(input$user_genus)
                 
                 # Filter by taxonomic info
                 filtered_by_taxonomy <- austraits |> 
                   semi_join(tibble(genus = input$user_genus))
                 
                 # Store in reactive
                 filtered_data(filtered_by_taxonomy)
                 
                 # Server-side selectize for family
                 updateSelectizeInput(
                   session,
                   "user_family",
                   choices = sort(unique(filtered_data()$family)),
                   server = TRUE
                 )
               }
  )

  # Render user selected data table output
  output$data_table <- DT::renderDT({
    datatable(
      data = filtered_data(),
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
      utils::write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}


