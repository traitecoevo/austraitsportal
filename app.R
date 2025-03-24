# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <- 
  open_dataset("data/austraits/austraits-lite.parquet") |> 
  collect()

## Set up possible genus
# Unique values of genus
all_genus <- austraits$genus |> unique() |> sort()

# User interface (UI)
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
    )
  ),
  
  # Data display
  card(
    card_header("Data Preview"),
    DTOutput("data_table")
  )
  
)


# Define server logic required to filter and display data
server <- function(input, output, session) {
  # Server-side selectize
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
  observeEvent(input$user_genus,
               {
                 # Requirements for this modules to work
                 # Input returns as a chacter vector of genus
                 req(input$user_genus)
                 
                 # Filter by genus
                 filtered_by_genus <- austraits |> 
                   semi_join(tibble(genus = input$user_genus))
                 
                 # Store in reactive
                 filtered_data(filtered_by_genus)
               }
  )

  # Render user selected data table output
  output$data_table <- renderDT({
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
}

# Run the application 
shinyApp(ui = ui, server = server)
