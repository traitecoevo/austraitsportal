open_data_portal <- function(...){
  # Run the application 
  shinyApp(ui = ui, server = server)
}