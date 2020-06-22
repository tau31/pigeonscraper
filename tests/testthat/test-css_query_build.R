skip_on_cran()

context("Testing css queries generating functions")

# Testing extract_years function ---------------
start_chrome_remDr(kill = TRUE)
remDr <- connect_remDr()
Sys.sleep(1)
remDr$open(silent = TRUE)
Sys.sleep(1)
remDr_go_to_link(remDr = remDr, "https://pigeon-ndb.com/races/")

page_source <- get_page_source(
  remDr = remDr,
  link = "https://pigeon-ndb.com/races/")

test_that("output is a data frame", {
  out <- extract_years(page_source)
  expect_s3_class(out, "data.frame")
})

test_that("tibble has more then one entry", {
  out <- extract_years(page_source)
  expect_gt(nrow(out), 1)
})

test_that("number of list positions in queries is bigger then one", {
  out <- extract_years(page_source)
  expect_match(out$year_query, regexp = "[[:digit:]]")
})

# Testing extract_orgs function ---------------

years <- extract_years(page_source)
css_queries <- extract_orgs(years[2,], remDr = remDr)

test_that("output extract orgs is a data frame", {
  expect_s3_class(css_queries, "data.frame")
})

test_that("tibble css_queries has more then one entry", {
  expect_gt(nrow(css_queries), 1)
})

test_that("output extract orgs must contain css queries with list positions", {
  expect_match(css_queries$css_query_year, regexp = "[[:digit:]]")
  expect_match(css_queries$css_query_org, regexp = "[[:digit:]]")
})


