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
      card(
        card_header("Geographical distribution of trait data"),
        min_height = 600,
        card_body(
          uiOutput(ns("trait_geo_text")),
          leaflet::leafletOutput(ns("trait_geo_map"), height = "600px")
        )
      )
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
    
    trait_profile <- reactive({
      req(filtered_data())
      generate_trait_profile(filtered_data())
    })
    
    output$trait_profile <- renderUI({
      tagList(trait_profile()[[1]])
    })
    
    output$trait_histogram_text <- renderUI({
      tagList(
        p("The plot below shows the distribution of selected data for this trait, including data collected on individuals of all age classes (seedling, sapling, adult), both field-collected and experimental data, and data representing individuals and population means."),
        p("Visualising data records across the families with the most data for the trait indicates the taxonomic breadth of information for this trait")
      )
    })
    
    output$trait_beeswarm_plot <- plotly::renderPlotly({
      req(filtered_data(), filters()$trait_name)
      plot_trait_distribution(filtered_data(), filters()$trait_name) |>
        plotly::ggplotly(tooltip = c("x", "y", "text"), height = 400)
    })
    
    output$trait_geo_text <- renderUI({
      trait_profile()[[3]]
    })
    
    output$trait_geo_map <- leaflet::renderLeaflet({
      trait_profile()[[4]]
    })
  })
}
    
## To be copied in the UI
# mod_trait_view_ui("trait_view_1")
    
## To be copied in the server
# mod_trait_view_server("trait_view_1")