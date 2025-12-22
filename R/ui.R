#' User interface (UI) for AusTraits Data Portal

austraits_ui <- function() {
  ui <- page_sidebar(
    # Custom CSS for DataTable
      tags$head(
        tags$style(HTML("
          /* Set max widths for truncated columns */
          .truncated {
            max-width: 20rem !important;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
          }
        ")
      )),
    # Set the overall theme of the app
    theme = bs_theme(preset = "flatly"),

    # Title of the portal
    title = "AusTraits Data Portal",

    footer =  tags$footer(
      "Powered by ",
      tags$a(href = "https://www.unsw.edu.au/science", target = "_blank", "UNSW Faculty of Science"), 
      align = "right", style = "padding: 30px",
      
      div("Created by AusTraits Team",  
          target)
      ),
    # Create a sidebar for the app
    sidebar = mod_filters_ui("filters"),

    # Data display
    navset_bar(
      id = "main_tabs",

      nav_panel(
        title = "Data Preview",
        mod_data_table_ui("data_table")
      ),
      nav_panel(
        title = "App Information",
        mod_app_info_ui("app_info")
      ),
      nav_panel(
        title = "Taxon View",
        mod_taxon_view_ui("taxon_view")
      ),
      nav_panel(
        title = "Trait View",
        mod_trait_view_ui("trait_view")
      ),
      nav_panel(
        title = "Citations",
        mod_citations_ui("citations")
      )
    )
  )
}