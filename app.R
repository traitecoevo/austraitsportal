# Dependencies we need
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(austraits)
library(arrow)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <- 
  open_dataset("data/austraits/austraits-6.0.0-flatten.parquet") |> 
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
    )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(
    list(
      input$user-genus
    ),
    {
      updateSelectizeInput(
        session,
        "user-genus",
        selected = "All",
        choices = c("All", sort(unique(
          austraits$genus
        ))),
        server = TRUE
      )
    }
  )

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
  # # Render user selected data table output
  # output$data_table <- renderDT({
  #   datatable(
  #     filtered_data(),
  #     options = list(
  #       pageLength = 10,
  #       scrollX = TRUE,
  #       dom = 'Bfrtip',
  #       buttons = c('copy', 'csv', 'excel')
  #     ),
  #     rownames = FALSE,
  #     filter = 'top',
  #     class = 'cell-border stripe'
  #   )
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
