context("test race tables parsing")

start_chrome_remDr(kill = TRUE)
remDr <- connect_remDr()
page_source <- get_page_source(
  remDr = remDr,
  link = "https://pigeon-ndb.com/races/")

years <- extract_years(parsed_html = page_source)
css_query_tbl <- extract_orgs(years[1,], remDr = remDr)[1,]

# Extract race options ------------------
race_html <- extract_race_html_options(css_query_tbl = css_query_tbl,
                                       remDr = remDr)

test_that("parsed races html have text on option fields", {
  expect_match(race_html %>% rvest::html_text(), regexp = "[[:alnum:]]")
})

# Generate xml documents with tables to be parsed ---------------
xml_doc <-race_table_parse(race_xml_nodeset = race_html[[1]], remDr= remDr)

test_that("race_table_parse outputs xml document with table", {
  table_test <-
    xml_doc[[1]] %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)  %>%
    .[[1]]   %>%
    tibble::as_tibble()
  expect_match(names(table_test)[1], regexp = "Pos")
  expect_match(names(table_test)[2], regexp = "Name")
  expect_match(names(table_test)[3], regexp = "Section")
})

