#' Filters UI Module
#'
#' @description Sidebar filters for the AusTraits portal
#'
#' @param id Shiny module id
#'
#' @noRd
#'
#' @importFrom shiny NS
mod_filters_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    h5("Taxonomy"),

    radioButtons(
      ns("taxon_rank"),
      label = "Filter by which taxon rank:",
      choices = c(
        "All taxa"   = "all",
        "Family"     = "family",
        "Genus"      = "genus",
        "Taxon name" = "taxon_name"
      ),
      selected = "family"
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "taxon_name"', ns("taxon_rank")),
      selectizeInput(
        ns("taxon_name"),
        label = "Taxon name:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "genus"', ns("taxon_rank")),
      selectizeInput(
        ns("genus"),
        label = "Genus:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "family"', ns("taxon_rank")),
      selectizeInput(
        ns("family"),
        label = "Family:",
        choices = NULL,
        multiple = TRUE,
        selected = "Fabaceae"
      )
    ),

    h5("Trait"),
    selectizeInput(
      ns("trait_name"),
      label = "Trait name(s):",
      choices = NULL,
      multiple = TRUE,
      options = list(create = TRUE)
    ),

    radioButtons(
      ns("trait_filter_type"),
      label = "Additional trait filters:",
      choices = c(
        "None" = "name",
        "Trait features" = "features"
      ),
      selected = "name"
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "features"', ns("trait_filter_type")),
      selectizeInput(
        ns("trait_grouping"),
        label = "Trait grouping:",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        ns("structure_measured"),
        label = "Structure measured:",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        ns("keywords"),
        label = "Keywords:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    h5("Location"),
    radioButtons(
      ns("location"),
      label = "Filter by which location filter:",
      choices = c(
        "Georeferenced records" = "georeferenced",
        "APC taxon distribution" = "apc"
      ),
      selected = character(0)
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "apc"', ns("location")),
      selectizeInput(
        ns("apc_taxon_distribution"),
        label = "State/territory:",
        choices = all_states_territories,
        multiple = TRUE
      )
    ),

    h5("Custom Filters"),

    selectizeInput(
      ns("custom_col_1"),
      label = "Filter 1 - Column:",
      choices = NULL,
      multiple = FALSE
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] != "" && input["%s"] != null', ns("custom_col_1"), ns("custom_col_1")),
      selectizeInput(
        ns("custom_val_1"),
        label = "Filter 1 - Values:",
        choices = NULL,
        multiple = TRUE
      ),
      
      # Filter 2 appears only when Filter 1 has values
      selectizeInput(
        ns("custom_col_2"),
        label = "Filter 2 - Column:",
        choices = NULL,
        multiple = FALSE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] != "" && input["%s"] != null && input["%s"] != null && input["%s"].length > 0', 
                        ns("custom_col_1"), ns("custom_col_1"), ns("custom_val_1"), ns("custom_val_1")),
      selectizeInput(
        ns("custom_val_2"),
        label = "Filter 2 - Values:",
        choices = NULL,
        multiple = TRUE
      ),
      
      # Filter 3 appears only when Filter 2 has values
      selectizeInput(
        ns("custom_col_3"),
        label = "Filter 3 - Column:",
        choices = NULL,
        multiple = FALSE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] != "" && input["%s"] != null && input["%s"] != null && input["%s"].length > 0 && input["%s"] != null && input["%s"].length > 0', 
                        ns("custom_col_2"), ns("custom_col_2"), ns("custom_val_1"), ns("custom_val_1"), ns("custom_val_2"), ns("custom_val_2")),
      selectizeInput(
        ns("custom_val_3"),
        label = "Filter 3 - Values:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    br(),

    actionButton(
      ns("clear_filters"),
      "Clear Filters",
      class = "btn-warning w-100"
    ),

    uiOutput(ns("rows_info")),
    uiOutput(ns("download_ui"))
  )
}

#' Filters Server Module
#'
#' @param id Shiny module id
#' @param filtered_database reactiveVal used to store filtered data
#' @noRd
mod_filters_server <- function(
  id,
  filtered_database,
  family_choices,
  genus_choices,
  taxon_name_choices
) {
  moduleServer(
    id,
    function(input, output, session) {
      # React when taxon rank changes
      observeEvent(input$taxon_rank, {

        # Reset filtered data when rank changes
        filtered_database(NULL)

        # Clear family selection if switching away from family
        if (input$taxon_rank != "family") {
          updateSelectizeInput(
            session,
            "family",
            choices = family_choices(),
            selected = NULL,
            server = TRUE
          )
        }

        if (input$taxon_rank == "taxon_name") {
          updateSelectizeInput(
            session,
            "taxon_name",
            choices = taxon_name_choices(),
            selected = "Abutilon oxycarpum var. oxycarpum",
            server = TRUE
          )

        } else if (input$taxon_rank == "genus") {
          updateSelectizeInput(
            session,
            "genus",
            choices = genus_choices(),
            selected = "Abutilon",
            server = TRUE
          )

        } else if (input$taxon_rank == "family") {
          updateSelectizeInput(
            session,
            "family",
            choices = family_choices(),
            selected = "Fabaceae",
            server = TRUE
          )
        }
      })
      observeEvent(input$clear_filters, {

        updateRadioButtons(
          session,
          "taxon_rank",
          selected = "all"
        )

        updateRadioButtons(
          session,
          "location",
          selected = character(0)
        )

        updateSelectizeInput(session, "family", selected = NULL)
        updateSelectizeInput(session, "genus", selected = NULL)
        updateSelectizeInput(session, "taxon_name", selected = NULL)
        updateSelectizeInput(session, "trait_name", selected = NULL)
        updateSelectizeInput(session, "trait_grouping", selected = NULL) 
        updateSelectizeInput(session, "structure_measured", selected = NULL) 
        updateSelectizeInput(session, "keywords", selected = NULL)
        updateSelectizeInput(session, "basis_of_record", selected = NULL)
        updateSelectizeInput(session, "life_stage", selected = NULL)
        updateSelectizeInput(session, "apc_taxon_distribution", selected = NULL)
        updateSelectizeInput(session, "custom_column", selected = NULL)
        updateSelectizeInput(session, "custom_values", selected = NULL)
        filtered_database(NULL)
      })

      # ---- RETURN FILTER STATE TO MAIN SERVER ----
      return(
        reactive({
          list(
            taxon_rank = input$taxon_rank,
            family = input$family,
            genus = input$genus,
            taxon_name = input$taxon_name,
            trait_filter_type = input$trait_filter_type,
            trait_name = input$trait_name,
            trait_grouping = input$trait_grouping,
            structure_measured = input$structure_measured,
            keywords = input$keywords,
            basis_of_record = input$basis_of_record,
            life_stage = input$life_stage,
            location = input$location,
            apc_taxon_distribution = input$apc_taxon_distribution,
            custom_col_1 = input$custom_col_1,
            custom_val_1 = input$custom_val_1,
            custom_col_2 = input$custom_col_2,
            custom_val_2 = input$custom_val_2,
            custom_col_3 = input$custom_col_3,
            custom_val_3 = input$custom_val_3
          )
        })
      )
    }
  )
}
