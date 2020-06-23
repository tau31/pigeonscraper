#' Download ARPU race results
#'
#' @return race_results tibble.
download_race_data <- function() {
  temp_file_path <- paste0(tempdir(), "/race_results.rds")
  cat("Downloading data \n")
  curl::curl_download(
    url = "https://raw.githubusercontent.com/tiagocabaco/pigeonscraper/master/data-raw/race_results.rds",
    destfile = temp_file_path
  )
  cat("Saving data as R object *race_results*")
  race_results <- readr::read_rds(temp_file_path)
}
