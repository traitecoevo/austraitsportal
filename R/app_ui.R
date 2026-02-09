#' User interface (UI) for AusTraits Data Portal

app_ui <- function() {
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
    
    /* Tooltip styling */
    .cell-tooltip {
      position: absolute;
      background-color: #333;
      color: white;
      padding: 8px 12px;
      border-radius: 4px;
      font-size: 13px;
      z-index: 9999;
      max-width: 400px;
      word-wrap: break-word;
      box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      pointer-events: none;
    }
  ")),
  
  # JavaScript for tooltip on truncated cells
  tags$script(HTML("
    $(document).ready(function() {
      // Create tooltip element
      if ($('.cell-tooltip').length === 0) {
        $('body').append('<div class=\"cell-tooltip\" style=\"display: none;\"></div>');
      }
      
    // Add help cursor and show tooltip ONLY for actually truncated cells
    $(document).on('mouseenter', '.truncated', function(e) {
      var cell = $(this);
      var text = cell.text().trim();
      
      // Check if text is actually truncated (content wider than container)
      if (this.offsetWidth < this.scrollWidth) {
        // Add help cursor
        cell.css('cursor', 'help');
        
        // Show tooltip
        $('.cell-tooltip')
          .text(text)
          .css({
            top: e.pageY + 15 + 'px',
            left: e.pageX + 15 + 'px',
            display: 'block'
          });
      } else {
        // Remove help cursor if not truncated
        cell.css('cursor', 'default');
      }
    });
      
      // Move tooltip with mouse
      $(document).on('mousemove', '.truncated', function(e) {
        if ($('.cell-tooltip').is(':visible')) {
          $('.cell-tooltip').css({
            top: e.pageY + 15 + 'px',
            left: e.pageX + 15 + 'px'
          });
        }
      });
      
      // Hide tooltip on mouse leave
      $(document).on('mouseleave', '.truncated', function() {
        $('.cell-tooltip').hide();
      });
    });
  "))
),

      # Loading spinner
      shinybusy::add_busy_spinner(
        spin = "fading-circle",
        position = "top-right",
        color = "#00FFC6",
        height = "60px",
        width = "60px"
      ),

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