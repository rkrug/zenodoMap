#' Extract relation types present between community records
#'
#' @param records List of Zenodo records.
#' @param community_ids Character vector of community record ids.
#' @param concept_map Optional map of concept id to record id.
#' @return Character vector of relation labels.
extract_relations <- function(records, community_ids = NULL, concept_map = NULL) {
  relations <- unique(unlist(lapply(records, function(rec) {
    related <- rec$metadata$related_identifiers
    if (is.null(related) || length(related) == 0) {
      return(character(0))
    }
    rels <- vapply(related, function(ri) {
      rel <- ri$relation %||% ""
      ident <- ri$identifier %||% ""
      zenodo_id <- zenodo_id_from_identifier(ident)
      if (!is.null(concept_map) && !is.na(zenodo_id) && zenodo_id %in% names(concept_map)) {
        zenodo_id <- concept_map[[zenodo_id]]
      }
      if (!is.null(community_ids) && (is.na(zenodo_id) || !(zenodo_id %in% community_ids))) {
        return("")
      }
      rel
    }, character(1))
    rels[rels != ""]
  })))
  sort(relations)
}
