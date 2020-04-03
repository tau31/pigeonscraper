library(tidyverse)



raw_data <- tibble::tibble(
  rds_file = list.files(path = here::here("inst", "raw_data"))
) %>%
  dplyr::mutate(query_index = readr::parse_number(.data$rds_file)) %>%
  dplyr::arrange(.data$query_index)

future::plan(multiprocess)

race_info <-
  furrr::future_map_dfr(
    1:nrow(raw_data),
    function(i) {
      race_info_temp <- readRDS(here::here("inst", "raw_data", raw_data$rds_file[i]))
      race_info_temp[[1]]$result$race_info
    }
  )

race_results <-
  furrr::future_map_dfr(
    1:nrow(raw_data),
    function(i) {
      race_info_temp <- readRDS(here::here("inst", "raw_data", raw_data$rds_file[i]))
      race_info_temp[[1]]$result$race_results
    }
  )

# I messed up race_id, therefore I will only work for
# now with the unique identifiers

unique_id <- race_info %>%
  select(race_id) %>%
  group_by(race_id) %>%
  count %>%
  filter(n == 1) %>%
  pull(race_id)

race_complete <- race_info %>%
  filter(race_id %in% unique_id) %>%
  left_join(race_results)

usethis::use_data(race_info, race_results, race_complete, overwrite = TRUE)
