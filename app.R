library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(palmerpenguins)

# Load penguins dataset
data(penguins)
# Remove rows with NA values for simplicity
penguins_clean <- na.omit(penguins)

ui <- page_sidebar(
  title = "Palmer Penguins Data Portal",
  sidebar = sidebar(
    title = "Filters",
    
    # Species filter
    checkboxGroupInput(
      "species", 
      "Species:",
      choices = unique(penguins_clean$species),
      selected = unique(penguins_clean$species)
    ),
    
    # Island filter
    checkboxGroupInput(
      "island", 
      "Island:",
      choices = unique(penguins_clean$island),
      selected = unique(penguins_clean$island)
    ),
    
    # Sex filter
    checkboxGroupInput(
      "sex", 
      "Sex:",
      choices = unique(penguins_clean$sex),
      selected = unique(penguins_clean$sex)
    ),
    
    # Year filter
    selectInput(
      "year", 
      "Year:",
      choices = c("All", unique(penguins_clean$year)),
      selected = "All"
    ),
    
    # Numeric range filters
    h4("Bill Length (mm)"),
    sliderInput(
      "bill_length", 
      NULL,
      min = min(penguins_clean$bill_length_mm),
      max = max(penguins_clean$bill_length_mm),
      value = c(min(penguins_clean$bill_length_mm), max(penguins_clean$bill_length_mm))
    ),
    
    h4("Bill Depth (mm)"),
    sliderInput(
      "bill_depth", 
      NULL,
      min = min(penguins_clean$bill_depth_mm),
      max = max(penguins_clean$bill_depth_mm),
      value = c(min(penguins_clean$bill_depth_mm), max(penguins_clean$bill_depth_mm))
    ),
    
    h4("Flipper Length (mm)"),
    sliderInput(
      "flipper_length", 
      NULL,
      min = min(penguins_clean$flipper_length_mm),
      max = max(penguins_clean$flipper_length_mm),
      value = c(min(penguins_clean$flipper_length_mm), max(penguins_clean$flipper_length_mm))
    ),
    
    h4("Body Mass (g)"),
    sliderInput(
      "body_mass", 
      NULL,
      min = min(penguins_clean$body_mass_g),
      max = max(penguins_clean$body_mass_g),
      value = c(min(penguins_clean$body_mass_g), max(penguins_clean$body_mass_g))
    ),
    
    # Download button
    downloadButton("downloadData", "Download Filtered Data")
  ),
  
  navset_card_tab(
    title = "Penguins Data Explorer",
    nav_panel(
      title = "Data Table",
      DTOutput("data_table")
    ),
    nav_panel(
      title = "Size Measurements",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("x_var", "X-axis:", 
                     choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                     selected = "bill_length_mm"),
          selectInput("y_var", "Y-axis:", 
                     choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                     selected = "flipper_length_mm"),
          checkboxInput("add_smooth", "Add smoothing line", value = TRUE)
        ),
        card(
          card_header("Penguin Size Measurements"),
          plotOutput("scatter_plot", height = "600px")
        )
      )
    ),
    nav_panel(
      title = "Species Distribution",
      layout_sidebar(
        sidebar = sidebar(
          radioButtons("dist_var", "Distribution of:", 
                      choices = c("Species by Island" = "island", 
                                  "Sex by Species" = "sex"),
                      selected = "island")
        ),
        card(
          card_header("Distribution Analysis"),
          plotOutput("dist_plot", height = "600px")
        )
      )
    ),
    nav_panel(
      title = "Summary Statistics",
      card(
        card_header("Summary Statistics by Species"),
        tableOutput("summary_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtered data reactive
  filtered_data <- reactive({
    data <- penguins_clean
    
    # Apply filters
    if (!is.null(input$species)) {
      data <- data %>% filter(species %in% input$species)
    }
    
    if (!is.null(input$island)) {
      data <- data %>% filter(island %in% input$island)
    }
    
    if (!is.null(input$sex)) {
      data <- data %>% filter(sex %in% input$sex)
    }
    
    if (input$year != "All") {
      data <- data %>% filter(year == as.numeric(input$year))
    }
    
    data <- data %>% 
      filter(
        bill_length_mm >= input$bill_length[1] & bill_length_mm <= input$bill_length[2],
        bill_depth_mm >= input$bill_depth[1] & bill_depth_mm <= input$bill_depth[2],
        flipper_length_mm >= input$flipper_length[1] & flipper_length_mm <= input$flipper_length[2],
        body_mass_g >= input$body_mass[1] & body_mass_g <= input$body_mass[2]
      )
    
    return(data)
  })
  
  # Data table output
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    req(input$x_var, input$y_var)
    
    p <- ggplot(filtered_data(), aes_string(x = input$x_var, y = input$y_var, color = "species", shape = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$y_var, "vs", input$x_var),
           x = gsub("_", " ", input$x_var),
           y = gsub("_", " ", input$y_var),
           color = "Species",
           shape = "Species") +
      scale_color_brewer(palette = "Set1")
    
    if (input$add_smooth) {
      p <- p + geom_smooth(method = "lm", se = TRUE, alpha = 0.3)
    }
    
    return(p)
  })
  
  # Distribution plot
  output$dist_plot <- renderPlot({
    req(input$dist_var)
    
    if (input$dist_var == "island") {
      ggplot(filtered_data(), aes(x = island, fill = species)) +
        geom_bar(position = "dodge") +
        theme_minimal(base_size = 14) +
        labs(title = "Species Distribution by Island",
             x = "Island",
             y = "Count",
             fill = "Species") +
        scale_fill_brewer(palette = "Set1")
    } else {
      ggplot(filtered_data(), aes(x = species, fill = sex)) +
        geom_bar(position = "dodge") +
        theme_minimal(base_size = 14) +
        labs(title = "Sex Distribution by Species",
             x = "Species",
             y = "Count",
             fill = "Sex") +
        scale_fill_brewer(palette = "Set2")
    }
  })
  
  # Summary statistics
  output$summary_table <- renderTable({
    filtered_data() %>%
      group_by(species) %>%
      summarize(
        `Count` = n(),
        `Avg Bill Length (mm)` = round(mean(bill_length_mm), 2),
        `Avg Bill Depth (mm)` = round(mean(bill_depth_mm), 2),
        `Avg Flipper Length (mm)` = round(mean(flipper_length_mm), 2),
        `Avg Body Mass (g)` = round(mean(body_mass_g), 2),
        `Min Body Mass (g)` = min(body_mass_g),
        `Max Body Mass (g)` = max(body_mass_g)
      )
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("penguins-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
