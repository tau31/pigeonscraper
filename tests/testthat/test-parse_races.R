context("test race tables parsing")

start_chrome_remDr(kill = TRUE)
remDr <- connect_remDr()
page_source <- get_page_source(
  remDr = remDr,
  link = "https://pigeon-ndb.com/races/")

years <- extract_years(parsed_html = page_source)
css_query_tbl <- extract_orgs(years[1,], remDr = remDr)[1,]

# Extract race options ------------------
race_html <- extract_race_table_html(css_query_tbl = css_query_tbl,
                                     remDr = remDr)

test_that("parsed races html have text on option fields", {
  expect_match(race_html %>% rvest::html_text(), regexp = "[[:alnum:]]")
})
