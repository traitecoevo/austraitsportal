#' Format relational database for display
#'
#' @param database 

format_database_for_display <- function(database){
  flatten_database(database) |> 
    select(-ends_with(".x"),
           -ends_with("_id"),
           -starts_with("source"),
           -c(methods,
              description, 
              assistants, 
              dataset_curators, 
              aligned_name,
              binomial, 
              trinomial, 
              taxon_name_alternatives, 
              sampling_strategy),
           dataset_id, 
           source_primary_citation) |> 
    rename(genus = genus.y,
           family = family.y,
           taxon_rank = taxon_rank.y,
           establishment_means = establishment_means.y) |> 
    relocate(dataset_id, .before = "taxon_name") |> 
    relocate(source_primary_citation, .after = "method_context_properties")
}


#' Format database for download handler
#'
#' @param database 

format_database_for_download <- function(database){
  flatten_database(database) |>
    select(-ends_with(".x")) |> 
    rename(genus = genus.y,
           family = family.y,
           taxon_rank = taxon_rank.y,
           establishment_means = establishment_means.y) 
}