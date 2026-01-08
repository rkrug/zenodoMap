test_that("graph builds from fixture records", {
  path <- testthat::test_path("fixtures", "zenodo_records_2026-01-07.rds")
  records <- sanitize_records(readRDS(path))
  community_ids <- vapply(records, function(rec) as.character(rec$id), character(1))
  concept_map <- build_concept_map(records)

  graph <- build_graph(
    records = records,
    depth = 0,
    max_expand = length(records),
    allowed_relations = "All",
    community_ids = community_ids,
    community_only = FALSE,
    title_map = vapply(records, function(rec) rec$metadata$title %||% "", character(1)),
    concept_map = concept_map
  )

  expect_true(all(c("nodes", "edges") %in% names(graph)))
  expect_s3_class(graph$nodes, "data.frame")
  expect_s3_class(graph$edges, "data.frame")
})

test_that("build_graph handles depth > 0 without expansion", {
  records <- list(list(
    id = 1,
    metadata = list(
      title = "Test record",
      related_identifiers = list(
        list(identifier = "https://example.com/other", relation = "References")
      )
    )
  ))
  graph <- build_graph(
    records = records,
    depth = 1,
    max_expand = 10,
    allowed_relations = "All",
    community_ids = "1",
    community_only = FALSE,
    title_map = c("1" = "Test record"),
    concept_map = list()
  )

  expect_s3_class(graph$nodes, "data.frame")
  expect_s3_class(graph$edges, "data.frame")
  expect_true(nrow(graph$nodes) >= 1)
})
test_that("build_graph expands with depth > 0 using mocked fetch", {
  records <- list(list(
    id = 1,
    metadata = list(
      title = "Root",
      related_identifiers = list(
        list(identifier = "10.5281/zenodo.2", relation = "References")
      )
    )
  ))

  testthat::local_mocked_bindings(
    zenodo_fetch_record = function(record_id) {
      list(
        id = as.integer(record_id),
        metadata = list(
          title = paste("Record", record_id),
          related_identifiers = list()
        )
      )
    }
  )

  graph <- build_graph(
    records = records,
    depth = 1,
    max_expand = 10,
    allowed_relations = "All",
    community_ids = c("1", "2"),
    community_only = FALSE,
    title_map = c("1" = "Root"),
    concept_map = list()
  )

  expect_true(any(graph$nodes$id == "2"))
  expect_true(nrow(graph$edges) >= 1)
})
