test_that("extract_doi handles DOI strings and doi.org URLs", {
  expect_equal(extract_doi("10.5281/zenodo.12345"), "10.5281/zenodo.12345")
  expect_equal(extract_doi("https://doi.org/10.5281/zenodo.67890"), "10.5281/zenodo.67890")
  expect_equal(extract_doi(""), "")
})

test_that("zenodo_id_from_identifier handles DOI and record URLs", {
  expect_equal(zenodo_id_from_identifier("10.5281/zenodo.12345"), "12345")
  expect_equal(zenodo_id_from_identifier("https://zenodo.org/records/98765"), "98765")
  expect_true(is.na(zenodo_id_from_identifier("not-a-doi")))
})
