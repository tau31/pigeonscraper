library(lubridate)
data(race_info)

race_results %>%
  mutate(count_i = if_else(is.na(arrival_time),
                           0,
                           1),
  ) %>%
  group_by(race_id) %>%
  summarize(n = sum(count_i)) %>%
  filter(n < 1)

race_id_check <- race_info %>%
  select(race_id, departure_time) %>%
  right_join(
    race_results %>% select(race_id, arrival_time, miles),
    by = "race_id"
  ) %>%
  mutate(
    race_time =
      (period_to_seconds(
        x = arrival_time
      ) - period_to_seconds(
        departure_time
      ) ) / 60
  ) %>%
  filter(race_time <= 0) %>%
  select(race_id, departure_time, arrival_time, miles) %>%
  distinct()


raw_data <- tibble::tibble(
  rds_file = list.files(path = here::here("inst", "raw_data"))
) %>%
  dplyr::mutate(query_index = readr::parse_number(.data$rds_file)) %>%
  dplyr::arrange(.data$query_index)


# code to prepare race_info -----------------------------------------------
race_info_check <-
  parallel::mclapply(
    1:nrow(raw_data),
    function(i) {
      race_info_temp <- readRDS(
        here::here("inst", "raw_data", raw_data$rds_file[i]))
      race_info_temp[[1]]$result$race_info
    },
    mc.cores = 4
  ) %>%
  do.call("rbind", .)

race_results_check <-
  parallel::mclapply(
    1:nrow(raw_data),
    function(i) {
      race_results_temp <- readRDS(
        here::here("inst", "raw_data", raw_data$rds_file[i]))
      race_results_temp[[1]]$result$race_results
    },
    mc.cores = 4
  ) %>%
  do.call("rbind", .)

race_info_check %>%
  right_join(race_id_check, by = "race_id") %>%
  select(race_id, departure_time, raw_arrival_weather) %>%
  left_join(race_results, by = "race_id") %>%
  select(race_id, departure_time, arrival_time, arrival_time, raw_arrival_weather) %>%
  View
