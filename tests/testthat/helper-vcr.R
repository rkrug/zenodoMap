if (requireNamespace("vcr", quietly = TRUE)) {
  vcr::vcr_configure(
    dir = file.path("tests", "testthat", "_vcr"),
    filter_sensitive_data = list(
      "<ZENODO_TOKEN>" = Sys.getenv("ZENODO_TOKEN", unset = "")
    )
  )
}

# Helper: manual cassette recording (not executed by tests).
record_vcr_cassettes <- function() {
  if (!requireNamespace("vcr", quietly = TRUE)) {
    stop("Install vcr to record cassettes.")
  }
  cassette_dir <- file.path("tests", "testthat", "_vcr")
  if (dir.exists(cassette_dir)) {
    unlink(list.files(cassette_dir, full.names = TRUE))
  }
  vcr::vcr_configure(dir = cassette_dir)

  vcr::use_cassette("zenodo-search", {
    zenodo_search(query = "", community = "ipbes", size = 1, page = 1)
  })

  vcr::use_cassette("zenodo-search-paging", {
    zenodo_search(query = "", community = "ipbes", size = 25, page = 1)
    zenodo_search(query = "", community = "ipbes", size = 25, page = 2)
  })
}
