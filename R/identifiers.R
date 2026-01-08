#' Check if an identifier is a Zenodo DOI
#'
#' @param identifier Identifier string.
#' @return Logical scalar.
is_zenodo_doi <- function(identifier) {
  grepl("^10\\.5281/zenodo\\.\\d+$", tolower(identifier))
}

#' Extract a Zenodo record id from DOI or record URL
#'
#' @param identifier Identifier string.
#' @return Record id as character or NA if not found.
zenodo_id_from_identifier <- function(identifier) {
  identifier <- tolower(trimws(identifier))
  doi_match <- regexpr("10\\.5281/zenodo\\.(\\d+)", identifier, perl = TRUE)
  if (doi_match[1] != -1) {
    return(sub(".*10\\.5281/zenodo\\.(\\d+).*", "\\1", identifier))
  }
  url_match <- regexpr("zenodo\\.org/records?/(\\d+)", identifier, perl = TRUE)
  if (url_match[1] != -1) {
    return(sub(".*zenodo\\.org/records?/(\\d+).*", "\\1", identifier))
  }
  NA_character_
}

#' Extract a DOI from identifier text
#'
#' @param identifier Identifier string.
#' @return DOI string or empty string.
extract_doi <- function(identifier) {
  identifier <- tolower(trimws(identifier))
  if (identifier == "") {
    return("")
  }
  doi_url <- regexpr("doi\\.org/(10\\.[^\\s]+)", identifier, perl = TRUE)
  if (doi_url[1] != -1) {
    return(sub(".*doi\\.org/(10\\.[^\\s]+).*", "\\1", identifier))
  }
  doi_match <- regexpr("(10\\.[0-9]{4,9}/[^\\s]+)", identifier, perl = TRUE)
  if (doi_match[1] != -1) {
    return(sub(".*(10\\.[0-9]{4,9}/[^\\s]+).*", "\\1", identifier))
  }
  ""
}
