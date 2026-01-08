#' app_info UI Function
#'
#' @description A shiny Module for displaying app information
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_app_info_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_header("How to Use the App"),
    card_body(
      p("This application allows users to filter and explore the AusTraits dataset."),
      p("Use the sidebar to apply filters based on taxonomy, traits, location, and additional criteria."),
      p("Filtered data will be displayed in the 'Data Preview' tab."),
      p("You can download the filtered data using the 'Download displayed data' button."),
      tags$a(href = "https://www.austraits.org", target = "_blank", "AusTraits Website")
    )
  )
}
    
#' app_info Server Functions
#'
#' @noRd 
mod_app_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # No server logic needed - just static content
  })
}
    
## To be copied in the UI
# mod_app_info_ui("app_info_1")
    
## To be copied in the server
# mod_app_info_server("app_info_1")