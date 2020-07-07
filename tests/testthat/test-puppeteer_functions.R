test_that("pup_get_source is able to get page source and scrape `'years'", {
  page_source <- pup_get_source(link = "https://pigeon-ndb.com/races/")
  years <- extract_years(parsed_html = page_source)
  expect_true(stringr::str_detect(years$year[nrow(years)], "2010"))
})
