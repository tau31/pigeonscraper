
# Post-process functions --------------------------------------------------

#' Check if saved tables contain errors
#'
#' This function takes a series of rds files where we stored the scraped data
#' and checks wether any errors where captured by the the \code{purrr::safely}
#' function.
#'
#' @return data frame withe the query index where the error occured. Data frame
#' with 0 rows if no errors where captured.
#'
#' @importFrom rlang .data

error_capture <- function() {
  raw_data <- list.files(path = here::here("inst", "raw_data")) %>%
    sort

raw_data <- tibble::tibble(
    rds_file = list.files(path = here::here("inst", "raw_data"))
  ) %>%
    dplyr::mutate(query_index = readr::parse_number(.data$rds_file)) %>%
    dplyr::arrange(.data$query_index)

  future::plan(multiprocess)

  df_errors <- furrr::future_map_dfr(
    1:nrow(raw_data),
    function(t) {
      error <- readRDS(here::here("inst", "raw_data", raw_data$rds_file[t]))
      df <- tibble::tibble(
        index = raw_data$query_index[t],
        error_is_null = ifelse(is.null(error[[1]]$error), TRUE, FALSE)
      )
    }
  ) %>%
    dplyr::filter(.data$error_is_null == FALSE)
  return(df_errors)
}
