library(shiny)
library(httr2)
library(jsonlite)
library(visNetwork)
library(shinyFiles)
library(DT)

zenodo_base <- "https://zenodo.org/api/records"

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

zenodo_search <- function(query, community = NULL, size = 200, page = 1) {
  params <- list(communities = community, size = size, page = page)
  if (!is.null(query) && query != "") {
    params$q <- query
  }
  params <- params[!vapply(params, function(x) is.null(x) || x == "", logical(1))]

  req <- request(zenodo_base)
  req <- do.call(req_url_query, c(list(req), params))
  req <- req_user_agent(req, "shiny-zenodmap/0.1")
  req <- req_headers(req, Accept = "application/json")
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}

zenodo_search_auth <- function(query, community = NULL, size = 200, page = 1, token = NULL) {
  params <- list(communities = community, size = size, page = page)
  if (!is.null(query) && query != "") {
    params$q <- query
  }
  params <- params[!vapply(params, function(x) is.null(x) || x == "", logical(1))]

  req <- request(zenodo_base)
  req <- do.call(req_url_query, c(list(req), params))
  req <- req_user_agent(req, "shiny-zenodmap/0.1")
  req <- req_headers(req, Accept = "application/json")
  if (!is.null(token) && token != "") {
    req <- req_headers(req, Authorization = paste("Bearer", token))
  }
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}

zenodo_fetch_record <- function(record_id) {
  req <- request(paste0(zenodo_base, "/", record_id))
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}

is_zenodo_doi <- function(identifier) {
  grepl("^10\\.5281/zenodo\\.\\d+$", tolower(identifier))
}

zenodo_id_from_identifier <- function(identifier) {
  if (!is_zenodo_doi(identifier)) {
    return(NA_character_)
  }
  sub("^10\\.5281/zenodo\\.", "", tolower(identifier))
}

normalize_keywords <- function(x) {
  tolower(trimws(x))
}

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

is_valid_record <- function(rec) {
  is.list(rec) && !is.null(rec$id) && !is.null(rec$metadata)
}

normalize_records <- function(records) {
  if (is.data.frame(records)) {
    return(split(records, seq_len(nrow(records))))
  }
  if (is.list(records)) {
    return(records)
  }
  list()
}

sanitize_records <- function(records) {
  records <- normalize_records(records)
  Filter(is_valid_record, records)
}

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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

build_query <- function(query_text) {
  query_text <- trimws(query_text)
  if (query_text == "") NULL else query_text
}

ui <- fluidPage(
  titlePanel("Zenodo Network Explorer"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Data"),
      textInput("community", "Community id", "ipbes"),
      textInput("query", "Additional query (optional)", ""),
      numericInput(
        "max_records",
        "Max records (initial search)",
        100,
        min = 5,
        max = 1000,
        step = 5
      ),
      passwordInput("token", "Zenodo API token (optional, allows size up to 100)", ""),
      actionButton("fetch", "Fetch metadata"),
      fileInput("upload_rds", "Upload data (RDS)", accept = ".rds"),
      shinySaveButton(
        "save_rds",
        "Save data (RDS)",
        "Save",
        filename = paste0("zenodo_records_", Sys.Date(), ".rds")
      ),
      tags$hr(),
      tags$h4("Graph"),
      selectInput(
        "depth",
        "Expansion depth",
        choices = c("0" = 0, "1" = 1, "2" = 2),
        selected = 1
      ),
      checkboxInput(
        "community_only",
        "Only community-to-community links",
        FALSE
      ),
      selectizeInput(
        "relations",
        "Relation types",
        choices = c(
          "All",
          "IsCitedBy",
          "Cites",
          "IsSupplementTo",
          "IsSupplementedBy",
          "IsContinuedBy",
          "Continues",
          "IsNewVersionOf",
          "IsPreviousVersionOf",
          "IsPartOf",
          "HasPart",
          "IsReferencedBy",
          "References",
          "IsDocumentedBy",
          "Documents",
          "IsCompiledBy",
          "Compiles",
          "IsVariantFormOf",
          "IsOriginalFormOf",
          "IsIdenticalTo",
          "IsReviewedBy",
          "Reviews",
          "IsDerivedFrom",
          "IsSourceOf",
          "IsRequiredBy",
          "Requires",
          "IsObsoletedBy",
          "Obsoletes",
          "IsDescribedBy",
          "Describes",
          "HasMetadata",
          "IsMetadataFor"
        ),
        selected = "All",
        multiple = TRUE,
        options = list(placeholder = "All")
      ),
      selectizeInput(
        "keywords",
        "Keyword filter",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Choose keywords")
      ),
      actionButton("refresh_graph", "Refresh graph"),
      helpText("Expansion uses Zenodo DOIs only and is capped for performance.")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tabsetPanel(
        tabPanel("Metadata", dataTableOutput("metadata_table")),
        tabPanel("Graph", visNetworkOutput("graph", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {
  records_val <- reactiveVal(NULL)
  graph_trigger <- reactiveVal(0)

  observeEvent(input$fetch, {
    query <- build_query(input$query)
    req(input$community)
    per_page <- if (is.null(input$token) || input$token == "") 25 else 100
    total_limit <- max(1, input$max_records)
    api_url <- build_api_url(query, input$community, per_page, 1)
    withProgress(message = "Fetching Zenodo records...", value = 0, {
      err <- NULL
      err_detail <- NULL
      page <- 1
      pages_fetched <- 0
      total_hits <- "unknown"
      records <- list()
      raw <- tryCatch(
        zenodo_search_auth(
          query,
          community = input$community,
          size = per_page,
          page = page,
          token = input$token
        ),
        error = function(e) {
          err <<- conditionMessage(e)
          resp <- tryCatch(e$resp, error = function(...) NULL)
          if (!is.null(resp)) {
            err_detail <<- tryCatch(resp_body_string(resp), error = function(...) NULL)
          }
          NULL
        }
      )
      if (!is.null(raw) && !is.null(raw$hits$total)) {
        total_hits <- raw$hits$total
      }
      if (!is.null(raw) && !is.null(raw$hits$hits)) {
        records <- c(records, raw$hits$hits)
        pages_fetched <- pages_fetched + 1
      }

      target_total <- if (is.numeric(total_hits)) {
        min(total_limit, total_hits)
      } else {
        total_limit
      }
      max_pages <- ceiling(target_total / per_page)
      while (length(records) < target_total && page < max_pages) {
        page <- page + 1
        raw <- tryCatch(
          zenodo_search_auth(
            query,
            community = input$community,
            size = per_page,
            page = page,
            token = input$token
          ),
          error = function(e) {
            err <<- conditionMessage(e)
            resp <- tryCatch(e$resp, error = function(...) NULL)
            if (!is.null(resp)) {
              err_detail <<- tryCatch(resp_body_string(resp), error = function(...) NULL)
            }
            NULL
          }
        )
        if (is.null(raw) || is.null(raw$hits$hits) || length(raw$hits$hits) == 0) {
          break
        }
        records <- c(records, raw$hits$hits)
        pages_fetched <- pages_fetched + 1
      }

      if (length(records) == 0) {
        msg <- if (!is.null(err)) {
          detail <- if (!is.null(err_detail) && err_detail != "") {
            paste0(" | Response: ", err_detail)
          } else {
            ""
          }
          paste0("Zenodo API error: ", err, detail)
        } else {
          "No response from Zenodo API."
        }
        records_val(list(
          records = NULL,
          status = paste(msg, api_url),
          api_url = api_url,
          query = query,
          per_page = per_page,
          total_limit = total_limit,
          pages_fetched = pages_fetched,
          total_hits = total_hits
        ))
        return(NULL)
      }

      if (length(records) > total_limit) {
        records <- records[seq_len(total_limit)]
      }
      all_keywords <- sort(unique(unlist(lapply(records, function(rec) {
        kws <- rec$metadata$keywords
        if (is.null(kws)) {
          character(0)
        } else {
          trimws(kws)
        }
      }))))
      updateSelectizeInput(
        session,
        "keywords",
        choices = all_keywords,
        selected = input$keywords,
        server = TRUE
      )
      records_val(list(
        records = records,
        status = NULL,
        api_url = api_url,
        query = query,
        per_page = per_page,
        total_limit = total_limit,
        pages_fetched = pages_fetched,
        total_hits = total_hits
      ))
      graph_trigger(graph_trigger() + 1)
      NULL
    })
  })

  observeEvent(input$upload_rds, {
    req(input$upload_rds$datapath)
    loaded <- tryCatch(readRDS(input$upload_rds$datapath), error = function(e) NULL)
    loaded <- sanitize_records(loaded)
    if (length(loaded) == 0) {
      showNotification("Uploaded file is not a valid records list.", type = "error")
      return(NULL)
    }
    records_val(list(
      records = loaded,
      status = paste("Loaded records from", input$upload_rds$name),
      api_url = "local:rds",
      query = NULL,
      per_page = NA,
      total_limit = length(loaded),
      pages_fetched = NA,
      total_hits = length(loaded)
    ))
    graph_trigger(graph_trigger() + 1)
  })

  roots <- c(Home = normalizePath("~"), shinyFiles::getVolumes()())
  shinyFileSave(input, "save_rds", roots = roots, session = session)

  observeEvent(input$save_rds, {
    fileinfo <- parseSavePath(roots, input$save_rds)
    if (nrow(fileinfo) == 0) {
      return(NULL)
    }
    payload <- records_val()
    if (is.null(payload) || is.null(payload$records)) {
      showNotification("No records loaded. Click 'Fetch metadata' first.", type = "error")
      return(NULL)
    }
    path <- fileinfo$datapath
    if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".rds")
    }
    saveRDS(payload$records, file = path)
    showNotification(paste("Saved to", path), type = "message")
  })

  observeEvent(input$refresh_graph, {
    graph_trigger(graph_trigger() + 1)
  })

  filtered_records <- reactive({
    payload <- records_val()
    if (is.null(payload) || is.null(payload$records)) {
      return(list())
    }
    records <- sanitize_records(payload$records)
    filter_by_keywords(records, input$keywords)
  })

  table_data <- reactive({
    records_to_table(filtered_records())
  })

  graph_data <- eventReactive(graph_trigger(), {
    payload <- records_val()
    if (is.null(payload)) {
      return(list(nodes = data.frame(), edges = data.frame(), status = "Fetch metadata first."))
    }
    records <- filtered_records()
    if (length(records) == 0) {
      return(list(nodes = data.frame(), edges = data.frame(), status = payload$status %||% "No records loaded."))
    }
    withProgress(message = "Building graph...", value = 0, {
      base_records <- sanitize_records(payload$records)
      community_ids <- vapply(base_records, function(rec) as.character(rec$id), character(1))
      concept_map <- build_concept_map(base_records)
      rels <- extract_relations(records, community_ids, concept_map)
      rel_choices <- c("All", rels)
      current <- isolate(input$relations)
      if (is.null(current) || length(current) == 0) {
        current <- "All"
      }
      current <- current[current %in% rel_choices]
      if (length(current) == 0) {
        current <- "All"
      }
      updateSelectizeInput(
        session,
        "relations",
        choices = rel_choices,
        selected = current,
        server = TRUE
      )
      incProgress(0.6, detail = "Building graph")
      title_map <- vapply(records, function(rec) {
        rec$metadata$title %||% paste("Record", rec$id)
      }, character(1))
      names(title_map) <- vapply(records, function(rec) as.character(rec$id), character(1))
      graph <- build_graph(
        records,
        depth = as.integer(input$depth),
        max_expand = length(records),
        allowed_relations = input$relations,
        community_ids = community_ids,
        community_only = isTRUE(input$community_only),
        title_map = title_map,
        concept_map = concept_map
      )
      total <- payload$total_hits %||% length(records)
      graph$status <- paste(
        "Query:",
        paste0("communities=", input$community),
        if (!is.null(payload$query) && payload$query != "") paste0(" q=", payload$query) else "",
        "| Total hits:",
        total,
        "| Records used:",
        length(records),
        "| Page size:",
        payload$per_page %||% "n/a",
        "| Pages:",
        payload$pages_fetched %||% "n/a",
        "\nAPI:",
        payload$api_url
      )
      graph
    })
  })

  output$status <- renderText({
    data <- graph_data()
    data$status %||% "Ready."
  })

  output$metadata_table <- renderDataTable({
    datatable(
      table_data(),
      selection = "single",
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ),
      callback = JS(
        "Shiny.addCustomMessageHandler('scrollToId', function(id) {",
        "  if (!id) { return; }",
        "  var data = table.column(0).data().toArray();",
        "  var idx = data.indexOf(id.toString());",
        "  if (idx === -1) { return; }",
        "  table.row(idx).select();",
        "  var node = table.row(idx).node();",
        "  if (node && node.scrollIntoView) { node.scrollIntoView({block: 'center'}); }",
        "});"
      )
    )
  })

  observeEvent(input$selected_node_id, {
    proxy <- dataTableProxy("metadata_table")
    id <- input$selected_node_id
    if (is.null(id)) {
      selectRows(proxy, NULL)
      return(NULL)
    }
    rows <- table_data()
    idx <- which(rows$Id == as.character(id))
    if (length(idx) == 0) {
      selectRows(proxy, NULL)
    } else {
      selectRows(proxy, idx[1])
    }
    session$sendCustomMessage("scrollToId", as.character(id))
    NULL
  })

  output$graph <- renderVisNetwork({
    data <- graph_data()
    if (nrow(data$nodes) == 0) {
      return(visNetwork(data.frame(id = 1, label = "No results"), data.frame()))
    }
    visNetwork(data$nodes, data$edges) |>
      visGroups(groupname = "community", color = list(background = "#2B6CB0", border = "#1A4F7A")) |>
      visGroups(groupname = "external", color = list(background = "#DD6B20", border = "#B44C11")) |>
      visLegend(useGroups = TRUE, position = "right") |>
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) |>
      visEvents(
        doubleClick = "function(params) {
          if (params.nodes.length > 0) {
            var id = params.nodes[0];
            window.open('https://zenodo.org/record/' + id, '_blank');
          }
        }",
        selectNode = "function(params) {
          if (params.nodes.length > 0) {
            Shiny.setInputValue('selected_node_id', params.nodes[0], {priority: 'event'});
          }
        }",
        deselectNode = "function(params) {
          Shiny.setInputValue('selected_node_id', null, {priority: 'event'});
        }"
      ) |>
      visPhysics(stabilization = TRUE)
  })
}

shinyApp(ui, server)
