test_that("fixture records load and sanitize", {
  path <- testthat::test_path("fixtures", "zenodo_records_2026-01-07.rds")
  records <- readRDS(path)
  expect_type(records, "list")

  cleaned <- sanitize_records(records)
  expect_true(length(cleaned) > 0)
  expect_true(all(vapply(cleaned, is_valid_record, logical(1))))
})

test_that("keyword filter reduces records", {
  path <- testthat::test_path("fixtures", "zenodo_records_2026-01-07.rds")
  records <- sanitize_records(readRDS(path))
  keywords <- unique(unlist(lapply(records, function(rec) rec$metadata$keywords %||% character(0))))
  if (length(keywords) == 0) {
    testthat::skip("No keywords in fixture")
  }
  filtered <- filter_by_keywords(records, keywords[1])
  expect_true(length(filtered) <= length(records))
})

test_that("records_to_table returns expected columns", {
  path <- testthat::test_path("fixtures", "zenodo_records_2026-01-07.rds")
  records <- sanitize_records(readRDS(path))
  tbl <- records_to_table(records)

  expect_true(all(c("Id", "Title", "DOI", "Related") %in% names(tbl)))
  expect_true(nrow(tbl) == length(records))
  expect_type(tbl$Id, "character")
  expect_type(tbl$Title, "character")
})

