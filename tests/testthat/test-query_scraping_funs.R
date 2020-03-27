context("Test querying builder and craping functions")

# Query builder function ------
css_query_tbl <- readRDS("css_query_tbl.rds")


test_that("test that query builder oytputs a data frame", {
  expect_s3_class(css_query_tbl, "data.frame")
})


test_that("test that query builder oytputs a data frame", {
  expect_s3_class(css_query_tbl, "data.frame")
})

test_that("query contains numeric on the years column", {
  query_years <- css_query_tbl$year %>%
    readr::parse_number() %>%
    unique()
  expect_true(is.numeric(query_years))
})

test_that("query contains years on the years column", {
  query_years <- css_query_tbl %>%
    dplyr::select(year) %>%
    .$year
  query_years <- readr::parse_number(query_years) %>%
    unique()
  expect_true(query_years %in% c(2010:2020) %>% unique)
})

# Scraping function -----
#
# start_chrome_remDr(kill = TRUE)
#
# remDr <- connect_remDr()
# remDr_go_to_link()
# page_source <- get_page_source(
#   remDr = remDr,
#   link = "https://pigeon-ndb.com/races/")
#
# years <- extract_years(parsed_html = page_source)
#
# css_query_tbl <- extract_orgs(years, remDr = remDr)
#
# pigeon_scrapper(css_query_tbl[1,], remDr)
