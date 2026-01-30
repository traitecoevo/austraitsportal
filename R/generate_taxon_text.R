
#' Generate Text Summary for a Taxon
#'
#' Creates a textual summary for a specified taxon based on the provided data.
#'
#' @param data A data frame or tibble containing taxonomic and trait information.
#' @param taxon A character string specifying the name of the taxon to summarize.
#'
#' @return A character string containing the generated summary for the specified taxon.
#'
#' @examples
#' \dontrun{
#' generate_taxon_text(austraits_data, "Eucalyptus globulus")
#' }

generate_taxon_text <- function(data, taxon) {

  data_taxon <- data |>
    dplyr::filter(taxon_name == taxon)

  data_taxon_trait_means <- austraits_species_averages |>
    dplyr::filter(taxon_name == taxon) |>
    dplyr::collect()

  taxon_info <- data_taxon |>
    dplyr::select(
      taxon_name, 
      taxon_distribution, 
      taxon_rank, 
      taxonomic_status, 
      taxonomic_dataset,
      genus, 
      family, 
      binomial,
      trinomial, 
      establishment_means,
      scientific_name,
      taxon_id,
      taxon_id_genus, 
      taxon_id_family,
      scientific_name_id
    ) |>
    dplyr::slice(1) |>
    as.list()

  # Generate links to other portals
  portal_links <- generate_taxon_portal_links(taxon_info)

  # Load trait links and merge with trait means
  trait_info_all <- 
    trait_groups |>
    dplyr::rename(trait_name = trait) |>
    dplyr::mutate(core_trait = ifelse(!is.na(core_trait) & core_trait == "core_trait", "core", "other")) |>
    dplyr::left_join(data_taxon_trait_means, by = "trait_name") |>
    dplyr::mutate(
      data_available = !is.na(type),
      # Add units from trait_definitions
      units = sapply(trait_name, function(t) {
        def <- trait_definitions[[t]]
        if (!is.null(def) && !is.null(def$units)) def$units else ""
      })
    )

  trait_info_na <- trait_info_all |> dplyr::filter(!data_available)
  
  # Generate the trait information for display, for those with data
  trait_info_have_data <- 
    trait_info_all |> 
    dplyr::filter(data_available) |> 
    dplyr::mutate(text = 
      ifelse(type == "categorical", 
        sprintf("- [%s](%s) (categorical): %s  [sources: %s]", trait_name, Entity, value_count, dataset_id),
        sprintf("- [%s](%s) (numerical): %s %s (%s-%s %s)  [sources: %s]", 
          trait_name, Entity, 
          round(as.numeric(value_mean), 2), units,
          round(as.numeric(value_min), 2), round(as.numeric(value_max), 2), units,
          dataset_id)
      )) |>
    dplyr::group_by(trait_group_for_portal, core_trait) |>
    dplyr::summarise(
      text = paste(text, collapse = "\n"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      text2 = sprintf(
        "<details> <summary> %s </summary>\n\n %s </details>", trait_group_for_portal, text)
    ) |>
    split(~core_trait)

  data_taxon_summary_dataset <-
    data_taxon |>
    dplyr::group_by(dataset_id) |>
    dplyr::summarise(
      n_traits = dplyr::n_distinct(trait_name),
      n_records = n(),
      reference = source_primary_citation[1],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      text = sprintf("%s (%s)", dataset_id, n_records) |> paste(collapse = ", ")
    ) |>
    dplyr::relocate(reference, .after = text) |>
    dplyr::arrange(desc(n_records))

  # Generate the taxon description for display
  taxon_description <- 
    sprintf(
"## %s

**FAMILY** %s

**Taxonomy**: %s

**Distribution (APC)**: %s

**Profiles**: %s

## Traits 

**Download**: (not active) [species summary](%s), [full data](%s)

**Top source datasets**: %s.     See below for a full list of sources.<br>
**Summary**: AusTraits contains %s traits from %s records in %s datasets.<br>

**Traits by category** (click to expand):<br>

- For categorical traits, the number of records for each value is shown. For numerical traits, the mean and range are shown.<br>

%s

## Sources 


", 
    taxon,
    taxon_info$family, 
    taxon_info$scientific_name, 
    taxon_info$taxon_distribution,
    sprintf("[%s](%s)", portal_links$source, portal_links$url) |> paste(collapse = ", "),
    "link", "link", 
    data_taxon_summary_dataset |> dplyr::slice_head(n=5) |> dplyr::pull(text) |> paste(collapse = ", "), 
    dplyr::n_distinct(data_taxon$trait_name),
    nrow(data_taxon),
    dplyr::n_distinct(data_taxon$dataset_id), 
    trait_info_have_data$core$text2 |> paste(collapse = "\n\n")
    )

  sources <- 
    data_taxon_summary_dataset |>
    dplyr::mutate(
      # convert reference column to html
      reference = purrr::map_chr(reference,     
          ~commonmark::markdown_html(.x) )
    ) |>
    dplyr::select(-text) |>
    # convert to html
    kableExtra::kable(format = "html") |>
    kableExtra::kable_styling("striped", full_width = F) |>
    kableExtra::column_spec(1, width = "10em") |>
    kableExtra::column_spec(2, width = "5em") |>
    # Clean up html
    stringr::str_remove_all("<p>") |> 
    stringr::str_remove_all("</p>") |>
    stringr::str_replace_all("&lt;", "<") |>
    stringr::str_replace_all("&gt;", ">")

  c(add_target_blank(taxon_description), add_target_blank(sources))
}

#' Generate Portal Links for a Taxon
#' @keywords internal
#' @noRd 
generate_taxon_portal_links <- function(taxon_info) {
  # Get distribution and taxon name
  distribution <- if (is.null(taxon_info$taxon_distribution)) "" else taxon_info$taxon_distribution
  taxon <- taxon_info$taxon_name
  
  # PERMANENT LINKS (always show, in specified order)
  links <- dplyr::tribble(
    ~source, ~url,
    "APC", taxon_info$taxon_id,
    "Flora of Australia", sprintf("https://profiles.ala.org.au/opus/foa/profile/%s", gsub(" ", "%20", taxon)),
    "ALA", sprintf("https://bie.ala.org.au/species/%s", taxon_info$taxon_id),
    "iNaturalist", sprintf("https://www.inaturalist.org/taxa/search?q=%s", 
      gsub(" ", "-", gsub(" var\\. | subsp\\. ", " ", taxon)))
  )
  
  # STATE/REGIONAL LINKS (conditional based on distribution)
  
  # NSW Flora only if distributed in NSW
  if (grepl("NSW", distribution, ignore.case = TRUE)) {
    links <- links |> 
      dplyr::add_row(
        source = "NSW Flora",
        url = sprintf("https://plantnet.rbgsyd.nsw.gov.au/cgi-bin/NSWfl.pl?page=nswfl&lvl=sp&name=%s", 
                     gsub(" ", "~", taxon))
      )
  }
  
  # NT eFlora only if distributed in NT
  if (grepl("NT", distribution, ignore.case = TRUE)) {
    # Try to find in CSV
    nt_match <- nt_links |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(nt_match) > 0 && !is.na(nt_match$url[1])) {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "NT eFlora",
          url = nt_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "NT eFlora",
          url = "https://eflora.nt.gov.au/"
        )
    }
  }
  
  # SA eFlora only if distributed in SA (homepage until we get numeric IDs)
  if (grepl("SA", distribution, ignore.case = TRUE)) {
    links <- links |>
      dplyr::add_row(
        source = "SA eFlora",
        url = "https://flora.sa.gov.au/"
      )
  }
  
  # Vic Flora only if distributed in Vic/Victoria
  if (grepl("Vic", distribution, ignore.case = TRUE)) {
    # Try to find in CSV
    vic_match <- vic_links |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(vic_match) > 0 && !is.na(vic_match$url[1])) {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "Vic Flora",
          url = vic_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "Vic Flora",
          url = "https://vicflora.rbg.vic.gov.au/flora/taxon/"
        )
    }
  }
  
  # Florabase only if distributed in WA (homepage until we get numeric IDs)
  if (grepl("WA", distribution, ignore.case = TRUE)) {
    links <- links |>
      dplyr::add_row(
        source = "Florabase",
        url = "https://florabase.dbca.wa.gov.au/"
      )
  }
  
  # ATRP only if distributed in Qld (Australian Tropical Rainforest Plants)
    atrp_match <- atrp_links |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(atrp_match) > 0)  {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "ATRP",
          url = atrp_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "ATRP",
          url = "https://apps.lucidcentral.org/rainforest/"
        )
    }
  
  return(links)
}

#' Export BibTeX Entries for Data
#'
#' Exports BibTeX entries corresponding to the provided keys to a specified file.
#'
#' @param keys A character vector of BibTeX entry keys to export.
#' @param filename A string specifying the path to the output file where the BibTeX entries will be saved.
#' @param refs A BibTeX object containing the references. This is typically read from a file.
#' @return Invisibly returns \code{NULL}. The function is called for its side effect of writing to a file.
#'
#' @examples
#' \dontrun{
#' export_bibtex_for_data(c("key1", "key2"), "output.bib")
#' }
#'
#' @export
export_bibtex_for_data <- function(keys, filename, 
    refs = 
        RefManageR::ReadBib(
        file = paste(data_path, "/sources.bib", sep = ""),
        check = FALSE, .Encoding = "UTF-8")
) {
  # Get the bibtex for the keys
  refs <- refs[keys]

  # Write the bibtex to a file
  RefManageR::WriteBib(
    refs,
    file = filename,
    .Encoding = "UTF-8",
    check = FALSE
  )
}
