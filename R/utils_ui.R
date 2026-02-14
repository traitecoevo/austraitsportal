#' Add target blank to all links in HTML
#' @param html_text Character string containing HTML
#' @return HTML string with target="_blank" added to all links
#' @keywords internal
#' @noRd
add_target_blank <- function(html_text) {
  gsub('<a href=', '<a target="_blank" href=', html_text, fixed = TRUE)
}