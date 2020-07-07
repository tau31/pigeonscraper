#' Get htlm page source using puppeteer API
#'
#' Gets html source of a website.
#'
#' @param link link to website.
#'
#' @return parsed html source file.

pup_get_source <- function(link) {
  temp_file <- tempfile(fileext = ".html")
  processx::run(
    command = "node",
    args = c(
      "pup_get_source.js",
      temp_file,
      link
    ),
    wd = system.file("node", package = "pigeonscraper")
  )
  page_source <- xml2::read_html(temp_file)
  unlink(temp_file)
  return(page_source)
}
