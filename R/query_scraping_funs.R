# wrapper functions for scrapping ----

# Build css query table for years and organizations

#' Build css query table for years and organizations
#'
#' Takes in a remote driver selenium driver and scrapes the APRU website to
#' build a query table used to scrape individual race information.
#'
#' @param remDr Object class remote driver previosly connected using
#' \code{\link{connect_remDr}}.
#'
#' @return tibble with year and organization info and corresponding css queries.
#'
#' @import RSelenium

pigeon_query_builder <-
  function(remDr) {
    page_source <- get_page_source(
      remDr = remDr,
      link = "https://pigeon-ndb.com/races/")
    years <- extract_years(parsed_html = page_source)
    css_query_tbl <- extract_orgs(years, remDr = remDr)
    return(css_query_tbl)
  }
