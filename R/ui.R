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
      tags$a(href = "https://www.unsw.edu.au/science", "UNSW Faculty of Science"), 
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
        card(
          card_body(
            fillable = TRUE,
            DT::DTOutput("data_table")
          )
        )
      ),
      nav_panel(
        title = "App Information",
        card(
          card_header("How to Use the App"),
            card_body(
            p("This application allows users to filter and explore the AusTraits dataset."),
            p("Use the sidebar to apply filters based on taxonomy, traits, location, and additional criteria."),
            p("Filtered data will be displayed in the 'Data Preview' tab."),
            p("You can download the filtered data using the 'Download displayed data' button."),
            tags$a(href = "https://www.austraits.org", "AusTraits Website")
          )
        )
      ),
      nav_panel(
        title = "Taxon View",
        card(
          card_header("AusTraits taxon profile"),
          card_body(
            htmlOutput("taxon_text"),
          )
        )
      ),
      nav_panel(
        title = "Trait View",
        card(
          card_header("AusTraits trait profile"),
          card_body(
            card(
              htmlOutput("trait_profile"),
              min_height = 600
            ),
            card(
              card_header("Observed values"),
              card_body(
                        uiOutput("trait_histogram_text"),
                        plotly::plotlyOutput("trait_beeswarm_plot")
              ),
              min_height = 650,
              full_screen = TRUE,
              fillable = FALSE
            ),
            card(
              card_header("Geographical distribution of trait data"),
              min_height = 600,
              card_body(
                uiOutput("trait_geo_text"),
                leaflet::leafletOutput("trait_geo_map", height = "600px")
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Citations",
        card(
          card_header("Referencing your filtered data"),
          card_body(
            htmlOutput("usage_text")
          )
        )
      )
    )
  )
}
