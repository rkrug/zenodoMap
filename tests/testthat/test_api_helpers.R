test_that("build_api_url assembles query parameters", {
  url <- build_api_url("climate", "ipbes", size = 5, page = 2)
  expect_true(grepl("communities=ipbes", url))
  expect_true(grepl("size=5", url))
  expect_true(grepl("page=2", url))
  expect_true(grepl("q=climate", url))
})
