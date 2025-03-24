# Dependencies we need
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(austraits)
library(arrow)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <- 
  open_dataset("data/austraits/austraits-lite.parquet") |> 
  collect()

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
    selectizeInput("user-genus",
                   label = "Search for a genus",
                   choices = "All")
  ),
  
  # Data display
  card(
    card_header("Data Preview"),
    DTOutput("data_table")
  )
  
)


# Define server logic required to filter and display data
server <- function(input, output, session) {
  
  # # Reactive value to store the data
  # data_reactive <- reactiveVal(NULL)
  # 
  # # Put austraits in reactive
  # data_reactive(austraits)
  # 
  # # Update selection
  # observeEvent(input$user-genus,
  #   {
  #     req(data_reactive(), input$user-genus) # Requirements for this to work
  #     
  #     data <- data_reactive()
  #     all_genus <- data["genus"] |> unique() |> sort()
  #     
  #     print(data)
      # updateSelectizeInput(
      #   session,
      #   "user-genus",
      #   choices = c("All", all_genus),
      #   selected = NULL,
      #   server = TRUE
      # )
  #   }
  # )

  # # Filter by taxonomic information
  # ## Store filtered data into a reactive object
  # filtered_data <- reactive({
  #   
  #   if(input$user-genus == "All"){
  #     data <- austraits
  #   } else {
  #   data   <- austraits |> 
  #       filter(genus == input$user-genus) 
  #   }
  #   
  #   # Collect the data
  #   output <- data |> 
  #     collect()
  #   
  #   # Return output
  #   return(output)
  # })
  # 
  # 
  # Render user selected data table output
  output$data_table <- renderDT({
    datatable(
      data = austraits,
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
