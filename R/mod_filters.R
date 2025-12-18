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

    h5("Additional"),
    selectizeInput(
      ns("basis_of_record"),
      label = "Basis of Record:",
      choices = NULL,
      multiple = TRUE
    ),

    selectizeInput(
      ns("life_stage"),
      label = "Life stage:",
      choices = NULL,
      multiple = TRUE
    ),

    br(),

    actionButton(
      ns("clear_filters"),
      "Clear Filters",
      class = "btn-warning w-100"
    ),

    uiOutput(ns("rows_info")),
    downloadButton(ns("download_data"), "Download displayed data")
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
          selected = character(0)
        )

        updateRadioButtons(
          session,
          "location",
          selected = character(0)
        )

        updateSelectizeInput(session, "family", selected = NULL)
        updateSelectizeInput(session, "genus", selected = NULL)
        updateSelectizeInput(session, "taxon_name", selected = NULL)
      })

      # ---- RETURN FILTER STATE TO MAIN SERVER ----
      return(
        reactive({
          list(
            taxon_rank = input$taxon_rank,
            family = input$family,
            genus = input$genus,
            taxon_name = input$taxon_name,
            trait_name = input$trait_name,
            basis_of_record = input$basis_of_record,
            life_stage = input$life_stage,
            location = input$location,
            apc_taxon_distribution = input$apc_taxon_distribution
          )
        })
      )
    }
  )
}
