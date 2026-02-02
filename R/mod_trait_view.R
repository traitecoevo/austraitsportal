#' trait_view UI Function
#'
#' @description A shiny Module for displaying trait profiles
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_trait_view_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_header("AusTraits trait profile"),
    card_body(
      card(
        htmlOutput(ns("trait_profile")),
        min_height = 600
      ),
      card(
        card_header("Observed values"),
        card_body(
          uiOutput(ns("trait_histogram_text")),
          plotly::plotlyOutput(ns("trait_beeswarm_plot"))
        ),
        min_height = 650,
        full_screen = TRUE,
        fillable = FALSE
      ),
      # Conditionally show map only for raw data
      uiOutput(ns("trait_geo_card"))
    )
  )
}
    
#' trait_view Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_data Reactive containing filtered data
#' @param filters Reactive containing filter values
#'
#' @noRd 
mod_trait_view_server <- function(id, filtered_data, filters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Check if current dataset is species averages
    is_species_avg <- reactive({
      req(filtered_data())
      data <- filtered_data()
      # Species avg has value_mean column, raw data has value column
      "value_mean" %in% names(data)
    })
    
trait_profile <- reactive({
      req(filtered_data())
      
      if (is_species_avg()) {
        # Add dummy location columns to prevent crash
        data_with_location <- filtered_data() |>
          dplyr::mutate(
            `latitude (deg)` = NA_real_,
            `longitude (deg)` = NA_real_
          )
        
        # Generate full profile (works now with dummy location)
        full_profile <- tryCatch({
          generate_trait_profile(data_with_location)
        }, error = function(e) {
          # Fallback if still fails
          return(list(tags$p("Trait profile unavailable"), NULL, NULL, NULL))
        })
        
        # Add species averages banner at top
        banner <- tags$div(
          style = "background: #e3f2fd; border-left: 4px solid #2196f3; padding: 12px 16px; margin-bottom: 20px; border-radius: 4px;",
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon("info-circle", style = "color: #1976d2;"),
            tags$span(
              style = "color: #1565c0; font-weight: 500;",
              "Species Averages Dataset"
            )
          ),
          tags$p(
            style = "margin: 8px 0 0 0; font-size: 0.9em; color: #424242;",
            "Showing aggregated species-level means. Location data not available."
          )
        )
        
        # Combine banner with trait info
        trait_info_with_banner <- tagList(banner, full_profile[[1]])
        
        # Return with banner, skip location/map
        return(list(trait_info_with_banner, NULL, NULL, NULL))
      }
      
      # For raw data, use full profile
      generate_trait_profile(filtered_data())
    })
    
    output$trait_profile <- renderUI({
      tagList(trait_profile()[[1]])
    })
    
    output$trait_histogram_text <- renderUI({
      if (is_species_avg()) {
        tagList(
          p("The plot below shows the distribution of species-level mean values for this trait."),
          p("Each point represents the mean trait value for a species, calculated from all available observations for that species.")
        )
      } else {
        tagList(
          p("The plot below shows the distribution of selected data for this trait, including data collected on individuals of all age classes (seedling, sapling, adult), both field-collected and experimental data, and data representing individuals and population means."),
          p("Visualising data records across the families with the most data for the trait indicates the taxonomic breadth of information for this trait")
        )
      }
    })
    
output$trait_beeswarm_plot <- plotly::renderPlotly({
      req(filtered_data(), filters()$trait_name)
      
      data <- filtered_data()
      
      # For species averages, use value_mean instead of value
      if (is_species_avg()) {
        data <- data |>
          dplyr::mutate(value = value_mean)
      }
      
      # Calculate dynamic height based on number of families
      num_families <- length(unique(data$family))
      # Min 400px, add 15px per family after first 20
      plot_height <- max(400, 300 + (num_families * 15))
      
      plot_trait_distribution(data, filters()$trait_name) |>
        plotly::ggplotly(tooltip = c("x", "y", "text"), height = plot_height)
    })
    
    # Conditionally render geography card
    output$trait_geo_card <- renderUI({
      if (is_species_avg()) {
        # Hiding map for species averages
        return(NULL)
      } else {
        # Show map for raw data
        card(
          card_header("Geographical distribution of trait data"),
          min_height = 600,
          card_body(
            uiOutput(ns("trait_geo_text")),
            leaflet::leafletOutput(ns("trait_geo_map"), height = "600px")
          )
        )
      }
    })
    
    output$trait_geo_text <- renderUI({
      if (!is_species_avg()) {
        trait_profile()[[3]]
      }
    })
    
    output$trait_geo_map <- leaflet::renderLeaflet({
      if (!is_species_avg()) {
        trait_profile()[[4]]
      }
    })
  })
}
    
## To be copied in the UI
# mod_trait_view_ui("trait_view_1")
    
## To be copied in the server
# mod_trait_view_server("trait_view_1")