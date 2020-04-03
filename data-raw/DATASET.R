# Extracting raw_data list of rds objects ---------------------------------

raw_data <- tibble::tibble(
  rds_file = list.files(path = here::here("inst", "raw_data"))
) %>%
  dplyr::mutate(query_index = readr::parse_number(.data$rds_file)) %>%
  dplyr::arrange(.data$query_index)


# code to prepare race_info -----------------------------------------------

race_info <-
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

# First version of the data, I made a mistake with the race_id unique key, by
# making it not unique... will fix soon.

unique_id <- race_info %>%
  select(race_id) %>%
  group_by(race_id) %>%
  count %>%
  filter(n == 1) %>%
  pull(race_id)

race_info <-
  race_info %>%
  filter(race_id %in% unique_id)


# readr::write_csv(race_info, "data-raw/race_info.csv")
# usethis::use_data(race_info, overwrite = TRUE)

# code to prepare race_results --------------------------------------------

race_results <-
  parallel::mclapply(
    1:nrow(raw_data),
    function(i) {
      race_info_temp <- readRDS(
        here::here("inst", "raw_data", raw_data$rds_file[i]))
      race_info_temp[[1]]$result$race_results
    },
    mc.cores = 4
  ) %>%
  do.call("rbind", .)

race_results <-
  race_info %>%
  left_join(race_results)



# readr::write_csv(race_results, "data-raw/race_results.csv")
# usethis::use_data(race_results, internal = TRUE)

