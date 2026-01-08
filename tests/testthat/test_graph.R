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

