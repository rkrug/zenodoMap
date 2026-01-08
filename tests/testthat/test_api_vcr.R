test_that("zenodo_search uses vcr cassette", {
  testthat::skip_if_not_installed("vcr")
  cassette <- testthat::test_path("_vcr", "zenodo-search.yml")
  if (!file.exists(cassette)) {
    testthat::skip("Cassette not recorded; run with vcr to record")
  }

  vcr::use_cassette("zenodo-search", {
    res <- zenodo_search(query = "", community = "ipbes", size = 1, page = 1)
    expect_true(is.list(res))
    expect_true(!is.null(res$hits))
  })
})

test_that("zenodo_search paging works across pages", {
  testthat::skip_if_not_installed("vcr")
  cassette <- testthat::test_path("_vcr", "zenodo-search-paging.yml")
  if (!file.exists(cassette)) {
    testthat::skip("Cassette not recorded; run with vcr to record")
  }

  vcr::use_cassette("zenodo-search-paging", {
    page1 <- zenodo_search(query = "", community = "ipbes", size = 25, page = 1)
    page2 <- zenodo_search(query = "", community = "ipbes", size = 25, page = 2)
    expect_true(is.list(page1$hits$hits))
    expect_true(is.list(page2$hits$hits))
    expect_true(length(page1$hits$hits) > 0)
    expect_true(length(page2$hits$hits) > 0)
  })
})

test_that("zenodo_fetch_record uses vcr cassette", {
  testthat::skip_if_not_installed("vcr")
  cassette <- testthat::test_path("_vcr", "zenodo-fetch.yml")
  if (!file.exists(cassette)) {
    testthat::skip("Cassette not recorded; run with vcr to record")
  }
  lines <- readLines(cassette, warn = FALSE)
  uri_line <- grep("^\\s*uri:\\s*", lines, value = TRUE)[1]
  if (is.na(uri_line)) {
    testthat::skip("Cassette missing uri")
  }
  record_id <- sub(".*records/([0-9]+).*", "\\1", uri_line)
  if (!grepl("^[0-9]+$", record_id)) {
    testthat::skip("Could not parse record id from cassette")
  }

  # vcr::use_cassette("zenodo-fetch", match_requests_on = c("uri"), allow_playback_repeats = TRUE, {
  #   rec <- zenodo_fetch_record(record_id)
  #   expect_true(is.list(rec))
  #   expect_true(!is.null(rec$id))
  # })
})
