zenodo_base <- "https://zenodo.org/api/records"

#' Build a Zenodo records API URL
#'
#' @importFrom utils URLencode
#'
#' @param query Optional search query string.
#' @param community Optional community id.
#' @param size Page size for the request.
#' @param page Page number for the request.
#' @return A fully-qualified URL string.
build_api_url <- function(query, community, size, page) {
  params <- list(q = query, communities = community, size = size, page = page)
  params <- params[
    !vapply(params, function(x) is.null(x) || x == "", logical(1))
  ]
  encoded <- vapply(
    names(params),
    function(name) {
      paste0(
        name,
        "=",
        utils::URLencode(as.character(params[[name]]), reserved = TRUE)
      )
    },
    character(1)
  )
  paste0(zenodo_base, "?", paste(encoded, collapse = "&"))
}

#' Search Zenodo records (unauthenticated)
#'
#' @importFrom httr2 request req_url_query req_user_agent req_headers req_perform resp_body_json
#'
#' @param query Optional search query string.
#' @param community Optional community id.
#' @param size Page size for the request.
#' @param page Page number for the request.
#' @return A list parsed from the Zenodo JSON response.
zenodo_search <- function(query, community = NULL, size = 200, page = 1) {
  params <- list(communities = community, size = size, page = page)
  if (!is.null(query) && query != "") {
    params$q <- query
  }
  params <- params[!vapply(params, function(x) is.null(x) || x == "", logical(1))]

  req <- httr2::request(zenodo_base)
  req <- do.call(httr2::req_url_query, c(list(req), params))
  req <- httr2::req_user_agent(req, "shiny-zenodmap/0.1")
  req <- httr2::req_headers(req, Accept = "application/json")
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Search Zenodo records (authenticated)
#'
#' @importFrom httr2 request req_url_query req_user_agent req_headers req_perform resp_body_json
#'
#' @param query Optional search query string.
#' @param community Optional community id.
#' @param size Page size for the request.
#' @param page Page number for the request.
#' @param token Zenodo API token.
#' @return A list parsed from the Zenodo JSON response.
zenodo_search_auth <- function(query, community = NULL, size = 200, page = 1, token = NULL) {
  params <- list(communities = community, size = size, page = page)
  if (!is.null(query) && query != "") {
    params$q <- query
  }
  params <- params[!vapply(params, function(x) is.null(x) || x == "", logical(1))]

  req <- httr2::request(zenodo_base)
  req <- do.call(httr2::req_url_query, c(list(req), params))
  req <- httr2::req_user_agent(req, "shiny-zenodmap/0.1")
  req <- httr2::req_headers(req, Accept = "application/json")
  if (!is.null(token) && token != "") {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", token))
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Fetch a single Zenodo record by id
#'
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @param record_id Zenodo record id.
#' @return A list parsed from the Zenodo JSON response.
zenodo_fetch_record <- function(record_id) {
  req <- httr2::request(paste0(zenodo_base, "/", record_id))
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
