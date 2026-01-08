#' Build a Zenodo query string
#'
#' @param query_text User-provided query.
#' @return Query string or NULL.
build_query <- function(query_text) {
  query_text <- trimws(query_text)
  if (query_text == "") NULL else query_text
}
