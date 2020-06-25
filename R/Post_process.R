
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

  future::plan(future::multiprocess)

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

#' Transform state names into state abbreviations in a string.
#'
#' This function takes a string or vector of strings, captures if a name of a
#' State is presente and changes it to its corresponding abbreviation.
#'
#' @param raw_string string to be parsed by the function.
#'
#' @return String with state names replaced by their abbreviatures.
state_abb_trans <-
  function(raw_string) {
    regex_states <- glue::glue('(?<!\\d\\W{{1,10}})(?<=[A-z]\\W{{1,10}}){state.name}|
             (?<=[A-z]\\W{{1,10}}){state.name}')
    string <- stringr::str_to_title(raw_string)
    state <- stringr::str_match_all(string, regex_states) %>% unlist
    if(length(state) > 0) {
      state_abb <- state.abb[which(state.name == state)]
      string <- stringr::str_replace(
        string = string,
        pattern = glue::glue('(?<!\\d\\W{{1,10}})(?<=\\w\\W{{1,10}}){state}'),
        replacement = state_abb)
    } else {
      string <- raw_string
    }
    return(stringr::str_to_upper(string))
  }

#' Transform raw pigeon arrival characther data into prediods.
#'
#' Taks raw characther arrival data, processes it and outputs a lubridate
#' peridod.
#'
#' @param raw_arrival scrapped arrival variable
arrival_tf <- function(raw_arrival) {
  tf_arrival <- stringr::str_extract(raw_arrival, "\\d.+\\d")
  tf_arrival <- dplyr::case_when(
    stringr::str_detect(tf_arrival, "(\\d{1,2}:){3}") ~
      stringr::str_replace(tf_arrival, "(?<=(\\d{1,2}:){2}\\d{1,2})\\W","."),
    TRUE ~ tf_arrival) %>%
    lubridate::hms()
  return(tf_arrival)
}

#' Compute the diference between departure and arrival time.
#'
#' Function that computes the difference between departure time and arrival time.
#' It also accounts for the the fact that at times the arrival time is smaller
#' then the departure time, by assuming that the race time is reported.
#'
#' @param arrival Arrival time in period.
#' @param departure Departure time in period.
#'
#' @return bird race time in hours.
#'
diff_time <- function(arrival, departure) {
  diff <- lubridate::period_to_seconds(arrival) -
    lubridate::period_to_seconds(departure)
  diff <- dplyr::case_when(
    diff < 0 ~ lubridate::period_to_seconds(arrival)/60,
    TRUE ~ diff / 60
  )
  return(diff)
}

#' Replace real names by cryptographically generated random identifiers.
#'
#' Takes in a vector of real names and replaces it with a randomly generated
#' identifier.
#'
#' @param competitor_names
#' @return tibble with column with old names and column with of competitor
#' cryptographically generated random identifiers.
names_to_ids <- function(competitor_names) {
  # get vector of unique names
  unique_competitors <- unique(competitor_names)
  # length of unique names
  n_ids <- length(unique_competitors)
  # generate unique ids
  bytes <- 4
  competitor_id <- ids::random_id(n = n_ids, bytes = bytes)

  # check if number of unique ids is equal the number of unique names
  all_unique <- length(unique(competitor_id)) == length(unique_competitors)
  while (all_unique == FALSE) {
    competitor_id <- ids::random_id(n_ids, bytes = bytes)
    all_unique <- length(unique(competitor_id)) == length(unique_competitors)
    # if length of names and ids is not unique, add one byte to the id generator
    bytes <- bytes + 1
  }
  cat("ids generated with", bytes, "bytes")

  return(tibble::tibble(
    competitor = unique_competitors,
    competitor_id = competitor_id
  ))
}
