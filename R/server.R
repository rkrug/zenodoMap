#' Shiny server logic for the Zenodo network explorer
#'
#' @importFrom shiny reactiveVal observeEvent req withProgress updateSelectizeInput
#' @importFrom shiny showNotification reactive eventReactive renderText isolate incProgress
#' @importFrom DT renderDataTable datatable dataTableProxy selectRows JS
#' @importFrom visNetwork renderVisNetwork visNetwork visGroups visLegend
#' @importFrom visNetwork visOptions visEvents visPhysics
#' @importFrom shinyFiles getVolumes shinyFileSave parseSavePath
#' @importFrom httr2 resp_body_string
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
server <- function(input, output, session) {
  records_val <- shiny::reactiveVal(NULL)
  graph_trigger <- shiny::reactiveVal(0)

  shiny::observeEvent(input$fetch, {
    query <- build_query(input$query)
    shiny::req(input$community)
    per_page <- if (is.null(input$token) || input$token == "") 25 else 100
    total_limit <- max(1, input$max_records)
    api_url <- build_api_url(query, input$community, per_page, 1)
    shiny::withProgress(message = "Fetching Zenodo records...", value = 0, {
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
            err_detail <<- tryCatch(httr2::resp_body_string(resp), error = function(...) NULL)
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
              err_detail <<- tryCatch(httr2::resp_body_string(resp), error = function(...) NULL)
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
      shiny::updateSelectizeInput(
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

  shiny::observeEvent(input$upload_rds, {
    shiny::req(input$upload_rds$datapath)
    loaded <- tryCatch(readRDS(input$upload_rds$datapath), error = function(e) NULL)
    loaded <- sanitize_records(loaded)
    if (length(loaded) == 0) {
      shiny::showNotification("Uploaded file is not a valid records list.", type = "error")
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
  shinyFiles::shinyFileSave(input, "save_rds", roots = roots, session = session)

  shiny::observeEvent(input$save_rds, {
    fileinfo <- shinyFiles::parseSavePath(roots, input$save_rds)
    if (nrow(fileinfo) == 0) {
      return(NULL)
    }
    payload <- records_val()
    if (is.null(payload) || is.null(payload$records)) {
      shiny::showNotification("No records loaded. Click 'Fetch metadata' first.", type = "error")
      return(NULL)
    }
    path <- fileinfo$datapath
    if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".rds")
    }
    saveRDS(payload$records, file = path)
    shiny::showNotification(paste("Saved to", path), type = "message")
  })

  shiny::observeEvent(input$refresh_graph, {
    graph_trigger(graph_trigger() + 1)
  })

  filtered_records <- shiny::reactive({
    payload <- records_val()
    if (is.null(payload) || is.null(payload$records)) {
      return(list())
    }
    records <- sanitize_records(payload$records)
    filter_by_keywords(records, input$keywords)
  })

  table_data <- shiny::reactive({
    records_to_table(filtered_records())
  })

  graph_data <- shiny::eventReactive(graph_trigger(), {
    payload <- records_val()
    if (is.null(payload)) {
      return(list(nodes = data.frame(), edges = data.frame(), status = "Fetch metadata first."))
    }
    records <- filtered_records()
    if (length(records) == 0) {
      return(list(nodes = data.frame(), edges = data.frame(), status = payload$status %||% "No records loaded."))
    }
    shiny::withProgress(message = "Building graph...", value = 0, {
      base_records <- sanitize_records(payload$records)
      community_ids <- vapply(base_records, function(rec) as.character(rec$id), character(1))
      concept_map <- build_concept_map(base_records)
      rels <- extract_relations(records, community_ids, concept_map)
      rel_choices <- c("All", rels)
      current <- shiny::isolate(input$relations)
      if (is.null(current) || length(current) == 0) {
        current <- "All"
      }
      current <- current[current %in% rel_choices]
      if (length(current) == 0) {
        current <- "All"
      }
      shiny::updateSelectizeInput(
        session,
        "relations",
        choices = rel_choices,
        selected = current,
        server = TRUE
      )
      shiny::incProgress(0.6, detail = "Building graph")
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

  output$status <- shiny::renderText({
    data <- graph_data()
    data$status %||% "Ready."
  })

  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      table_data(),
      selection = "single",
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ),
      callback = DT::JS(
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

  shiny::observeEvent(input$selected_node_id, {
    proxy <- DT::dataTableProxy("metadata_table")
    id <- input$selected_node_id
    if (is.null(id)) {
      DT::selectRows(proxy, NULL)
      return(NULL)
    }
    rows <- table_data()
    idx <- which(rows$Id == as.character(id))
    if (length(idx) == 0) {
      DT::selectRows(proxy, NULL)
    } else {
      DT::selectRows(proxy, idx[1])
    }
    session$sendCustomMessage("scrollToId", as.character(id))
    NULL
  })

  output$graph <- visNetwork::renderVisNetwork({
    data <- graph_data()
    if (nrow(data$nodes) == 0) {
      return(visNetwork::visNetwork(data.frame(id = 1, label = "No results"), data.frame()))
    }
    visNetwork::visNetwork(data$nodes, data$edges) |>
      visNetwork::visGroups(groupname = "community", color = list(background = "#2B6CB0", border = "#1A4F7A")) |>
      visNetwork::visGroups(groupname = "external", color = list(background = "#DD6B20", border = "#B44C11")) |>
      visNetwork::visLegend(useGroups = TRUE, position = "right") |>
      visNetwork::visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) |>
      visNetwork::visEvents(
        doubleClick = "function(params) {",
        "  if (params.nodes.length > 0) {",
        "    var id = params.nodes[0];",
        "    window.open('https://zenodo.org/record/' + id, '_blank');",
        "  }",
        "}",
        selectNode = "function(params) {",
        "  if (params.nodes.length > 0) {",
        "    Shiny.setInputValue('selected_node_id', params.nodes[0], {priority: 'event'});",
        "  }",
        "}",
        deselectNode = "function(params) {",
        "  Shiny.setInputValue('selected_node_id', null, {priority: 'event'});",
        "}"
      ) |>
      visNetwork::visPhysics(stabilization = TRUE)
  })
}
