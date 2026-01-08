#' Normalize keywords for matching
#'
#' @param x Character vector.
#' @return Normalized character vector.
normalize_keywords <- function(x) {
  tolower(trimws(x))
}

#' Filter records by keyword tokens
#'
#' @param records List of Zenodo records.
#' @param keywords_text Vector or string of keywords.
#' @return Filtered list of records.
filter_by_keywords <- function(records, keywords_text) {
  if (is.null(keywords_text) || length(keywords_text) == 0) {
    return(records)
  }
  if (length(keywords_text) > 1) {
    tokens <- normalize_keywords(keywords_text)
  } else {
    tokens <- normalize_keywords(unlist(strsplit(keywords_text, "[,;]")))
  }
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) {
    return(records)
  }

  keep <- vapply(
    records,
    function(rec) {
      kws <- rec$metadata$keywords
      if (is.null(kws)) {
        return(FALSE)
      }
      kws <- normalize_keywords(kws)
      any(tokens %in% kws)
    },
    logical(1)
  )

  records[keep]
}

#' Validate a record object
#'
#' @param rec Record candidate.
#' @return Logical scalar.
is_valid_record <- function(rec) {
  is.list(rec) && !is.null(rec$id) && !is.null(rec$metadata)
}

#' Normalize records to a list of record objects
#'
#' @param records List or data.frame.
#' @return List of records.
normalize_records <- function(records) {
  if (is.data.frame(records)) {
    return(split(records, seq_len(nrow(records))))
  }
  if (is.list(records)) {
    return(records)
  }
  list()
}

#' Sanitize records by normalizing and filtering invalid entries
#'
#' @param records List or data.frame.
#' @return List of valid records.
sanitize_records <- function(records) {
  records <- normalize_records(records)
  Filter(is_valid_record, records)
}

#' Build a metadata table for display
#'
#' @param records List of Zenodo records.
#' @return Data frame with display columns.
records_to_table <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(data.frame(
      Id = character(0),
      Title = character(0),
      DOI = character(0),
      Related = character(0)
    ))
  }
  rows <- lapply(records, function(rec) {
    rid <- as.character(rec$id)
    title <- rec$metadata$title %||% ""
    doi <- rec$metadata$doi %||% rec$metadata$prereserve_doi$doi %||% ""
    related <- rec$metadata$related_identifiers
    related_items <- character(0)
    if (!is.null(related) && length(related) > 0) {
      related_items <- vapply(related, function(ri) {
        rel <- ri$relation %||% ""
        ident <- ri$identifier %||% ""
        doi_val <- extract_doi(ident)
        if (doi_val == "") {
          return("")
        }
        paste0(rel, " <a href=\"https://doi.org/", doi_val, "\" target=\"_blank\">", doi_val, "</a>")
      }, character(1))
      related_items <- unique(related_items[related_items != ""])
    }
    doi_link <- if (doi != "") {
      paste0("<a href=\"https://doi.org/", doi, "\" target=\"_blank\">", doi, "</a>")
    } else {
      ""
    }
    data.frame(
      Id = rid,
      Title = title,
      DOI = doi_link,
      Related = if (length(related_items) > 0) paste(related_items, collapse = "<br>") else "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Map concept ids to the latest version record id
#'
#' @param records List of Zenodo records.
#' @return Named list mapping concept id to record id.
build_concept_map <- function(records) {
  map <- list()
  times <- list()
  for (rec in records) {
    cid <- rec$conceptrecid %||% zenodo_id_from_identifier(rec$metadata$conceptdoi %||% "")
    if (is.null(cid) || cid == "") {
      next
    }
    cid <- as.character(cid)
    stamp <- rec$updated %||% rec$created %||% rec$metadata$publication_date %||% ""
    ts <- suppressWarnings(as.numeric(as.POSIXct(stamp, tz = "UTC")))
    if (is.na(ts)) {
      ts <- 0
    }
    if (is.null(times[[cid]]) || ts >= times[[cid]]) {
      map[[cid]] <- as.character(rec$id)
      times[[cid]] <- ts
    }
  }
  map
}
