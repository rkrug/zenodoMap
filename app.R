library(shiny)
library(httr2)
library(jsonlite)
library(visNetwork)

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

build_graph <- function(
  records,
  depth = 0,
  max_expand = 300,
  allowed_relations = NULL
) {
  nodes <- list()
  edges <- list()
  seen_records <- character(0)
  seen_nodes <- character(0)
  relation_filter <- tolower(allowed_relations %||% character(0))
  allow_all_relations <- length(relation_filter) == 0 ||
    "all" %in% relation_filter

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
    title <- rec$metadata$title %||% paste("Record", rid)
    add_node(rid, title, "record", title)

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
      if (!is.na(zenodo_id)) {
        target <- as.character(zenodo_id)
        add_node(target, paste("Zenodo", target), "record")
        zenodo_ids <- unique(c(zenodo_ids, target))
      } else {
        target <- paste0("ext:", ident)
        add_node(target, ident, "related")
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
        25,
        min = 5,
        max = 25,
        step = 5
      ),
      passwordInput("token", "Zenodo API token (optional, allows size up to 100)", ""),
      actionButton("fetch", "Fetch metadata"),
      tags$hr(),
      tags$h4("Graph"),
      selectInput(
        "depth",
        "Expansion depth",
        choices = c("0" = 0, "1" = 1, "2" = 2),
        selected = 1
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
      visNetworkOutput("graph", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  records_val <- reactiveVal(NULL)
  graph_trigger <- reactiveVal(0)

  observeEvent(input$fetch, {
    query <- build_query(input$query)
    req(input$community)
    size_limit <- if (is.null(input$token) || input$token == "") 25 else 100
    size <- min(input$max_records, size_limit)
    api_url <- build_api_url(query, input$community, size, 1)
    withProgress(message = "Fetching Zenodo records...", value = 0, {
      err <- NULL
      err_detail <- NULL
      raw <- tryCatch(
        zenodo_search_auth(
          query,
          community = input$community,
          size = size,
          page = 1,
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
      if (is.null(raw) || is.null(raw$hits$hits)) {
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
          size = size
        ))
        return(NULL)
      }

      records <- raw$hits$hits
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
        size = size
      ))
      graph_trigger(graph_trigger() + 1)
      NULL
    })
  })

  observeEvent(input$refresh_graph, {
    graph_trigger(graph_trigger() + 1)
  })

  graph_data <- eventReactive(graph_trigger(), {
    payload <- records_val()
    if (is.null(payload)) {
      return(list(nodes = data.frame(), edges = data.frame(), status = "Fetch metadata first."))
    }
    records <- payload$records
    if (is.null(records) || length(records) == 0) {
      return(list(nodes = data.frame(), edges = data.frame(), status = payload$status %||% "No records loaded."))
    }
    withProgress(message = "Building graph...", value = 0, {
      records <- filter_by_keywords(records, input$keywords)
      incProgress(0.6, detail = "Building graph")
      graph <- build_graph(
        records,
        depth = as.integer(input$depth),
        max_expand = payload$size,
        allowed_relations = input$relations
      )
      total <- length(records)
      graph$status <- paste(
        "Query:",
        paste0("communities=", input$community),
        if (!is.null(payload$query) && payload$query != "") paste0(" q=", payload$query) else "",
        "| Total hits:",
        total,
        "| Records used:",
        length(records),
        "| Page size:",
        payload$size,
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

  output$graph <- renderVisNetwork({
    data <- graph_data()
    if (nrow(data$nodes) == 0) {
      return(visNetwork(data.frame(id = 1, label = "No results"), data.frame()))
    }
    visNetwork(data$nodes, data$edges) |>
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) |>
      visPhysics(stabilization = TRUE)
  })
}

shinyApp(ui, server)
