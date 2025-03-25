#' Open AusTraits Data Portal Locally
#'
#' @param ... 

open_data_portal <- function(...){
  # Run the application 
  shiny::shinyApp(ui = austraits_ui, server = austraits_server, ...)
}