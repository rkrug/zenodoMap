#' Build nodes and edges for the network graph
#'
#' @param records List of Zenodo records.
#' @param depth Expansion depth for related Zenodo records.
#' @param max_expand Maximum records to expand into.
#' @param allowed_relations Relation filter vector.
#' @param community_ids Community record ids.
#' @param community_only Logical; keep only community-to-community links.
#' @param title_map Optional id to title map.
#' @param concept_map Optional concept id to record id map.
#' @return List with `nodes` and `edges` data frames.
build_graph <- function(
  records,
  depth = 0,
  max_expand = 300,
  allowed_relations = NULL,
  community_ids = NULL,
  community_only = FALSE,
  title_map = NULL,
  concept_map = NULL
) {
  nodes <- list()
  edges <- list()
  seen_records <- character(0)
  seen_nodes <- character(0)
  relation_filter <- tolower(allowed_relations %||% character(0))
  allow_all_relations <- length(relation_filter) == 0 ||
    "all" %in% relation_filter
  node_group <- function(id) {
    if (!is.null(community_ids) && id %in% community_ids) {
      "community"
    } else {
      "external"
    }
  }

  add_node <- function(id, label, group, title = NULL) {
    if (id %in% seen_nodes) {
      return()
    }
    seen_nodes <<- c(seen_nodes, id)
    nodes[[length(nodes) + 1]] <<- data.frame(
      id = id,
      label = label,
      group = group,
      title = title %||% label,
      stringsAsFactors = FALSE
    )
  }

  add_edge <- function(from, to, label = NULL) {
    edges[[length(edges) + 1]] <<- data.frame(
      from = from,
      to = to,
      label = label %||% "",
      arrows = "to",
      stringsAsFactors = FALSE
    )
  }

  process_record <- function(rec) {
    rid <- as.character(rec$id)
    is_community <- !is.null(community_ids) && rid %in% community_ids
    if (community_only && !is_community) {
      return(character(0))
    }
    title <- if (!is.null(title_map) && rid %in% names(title_map)) {
      title_map[[rid]]
    } else {
      rec$metadata$title %||% paste("Record", rid)
    }
    add_node(rid, title, node_group(rid), title)

    related <- rec$metadata$related_identifiers
    if (is.null(related) || length(related) == 0) {
      return(character(0))
    }

    zenodo_ids <- character(0)
    for (ri in related) {
      ident <- ri$identifier %||% ""
      rel <- ri$relation %||% ""
      if (!allow_all_relations && !(tolower(rel) %in% relation_filter)) {
        next
      }
      zenodo_id <- zenodo_id_from_identifier(ident)
      if (!is.null(concept_map) && !is.na(zenodo_id) && zenodo_id %in% names(concept_map)) {
        zenodo_id <- concept_map[[zenodo_id]]
      }
      if (!is.na(zenodo_id)) {
        if (!is.null(community_ids) && community_only && !(zenodo_id %in% community_ids)) {
          next
        }
        target <- as.character(zenodo_id)
        target_title <- if (!is.null(title_map) && target %in% names(title_map)) {
          title_map[[target]]
        } else {
          paste("Zenodo", target)
        }
        add_node(target, target_title, node_group(target), target_title)
        zenodo_ids <- unique(c(zenodo_ids, target))
      } else {
        next
      }
      add_edge(rid, target, rel)
    }
    zenodo_ids
  }

  queue <- character(0)
  for (rec in records) {
    seen_records <- unique(c(seen_records, as.character(rec$id)))
    queue <- unique(c(queue, process_record(rec)))
  }

  if (depth > 0 && length(queue) > 0) {
    for (d in seq_len(depth)) {
      if (length(seen_records) >= max_expand) {
        break
      }
      next_queue <- character(0)
      for (rid in queue) {
        if (rid %in% seen_records) {
          next
        }
        if (length(seen_records) >= max_expand) {
          break
        }
        rec <- tryCatch(zenodo_fetch_record(rid), error = function(e) NULL)
        if (is.null(rec$id)) {
          next
        }
        seen_records <- unique(c(seen_records, as.character(rec$id)))
        new_ids <- process_record(rec)
        next_queue <- unique(c(next_queue, new_ids))
      }
      queue <- setdiff(next_queue, seen_records)
      if (length(queue) == 0) break
    }
  }

  list(
    nodes = if (length(nodes) > 0) do.call(rbind, nodes) else data.frame(),
    edges = if (length(edges) > 0) do.call(rbind, edges) else data.frame()
  )
}
