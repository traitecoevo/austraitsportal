#' Format relational database for display
#'
#' @param database traits.build object

format_database_for_display <- function(database){
  flatten_database(database, include_description = FALSE) |> 
    select(-ends_with(".x"),
           -ends_with("_id"),
           -starts_with("source"),
           -c(population_id:original_name),
           -c(methods:data_contributors),
           -c(binomial:trinomial),
           -c(taxon_id_genus:aligned_name_taxonomic_status),
           -unit,
           -taxon_name_alternatives,
           -scientific_name,
           "dataset_id", 
           "source_primary_citation") |> 
    rename(genus = "genus.y",
           family = "family.y",
           taxon_rank = "taxon_rank.y",
           establishment_means = "establishment_means.y") |> 
    relocate("dataset_id", .before = "taxon_name") |> 
    relocate("source_primary_citation", .after = "method_context_properties")
}


#' Format database for download handler
#'
#' @param database traits.build object

format_database_for_download <- function(database){
  flatten_database(database, include_description = FALSE) |>
    select(-ends_with(".x")) |> 
    rename(genus = "genus.y",
           family = "family.y",
           taxon_rank = "taxon_rank.y",
           establishment_means = "establishment_means.y") 
}