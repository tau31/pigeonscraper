devtools::load_all(".")
library(tidyverse)
library(stringr)

# Save state.abb and state.name -------------------------------------------

usethis::use_data(state.abb, state.name, internal = TRUE, overwrite = TRUE)

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

# regex patterns for processing location, city, state and roman numerals

pattern_1 <- regex("((?!\\d|\\d\\WMile\\W)(\\w+\\W{1,}){1,}[A-z]{2}\\b)")
regex_city <- regex("((\\w+\\W{1,}){1,})(?!\\W{1,}[A-z]{2}\\b)")
regex_state <- regex("(?!(\\w+\\W{1,}){1,})([A-z]{2}\\b)")
roman_numerals <- regex("\\W[MDCLXVI]+\\W")


# Process year, date and organization name --------------------------------------
race_info <-
  race_info %>%
  mutate(
    raw_organization = organization,
    obyb = stringr::str_split(
      year,
      # pattern seperate by white space
      patter = regex("\\s"),
      simplify = TRUE) %>% .[,2],
    year = readr::parse_number(year),
    date = str_extract(raw_info, "\\d{1,}/\\d{1,}/\\d{1,}") %>%
      lubridate::mdy(),
    # pattern extract names up until " -"
    organization = str_extract(organization, pattern = regex(".+?(?= -)|.+")) %>%
      str_squish() %>%
      str_to_title(locale = "en")
  ) %>%
  mutate(obyb = if_else(obyb == "OB", "old bird", "young bird"))

# Process location --------------------------------------------------------
race_info <-
  race_info %>%
  mutate(
    raw_info = pmap_chr(list(raw_info), state_abb_trans),
    location = str_match(
      raw_info,
      regex("(?<=--  ).+(?= --)"))[,1]
  ) %>%
  mutate(location =
           case_when(
             str_detect(
               location,
               pattern_1
             ) ~ str_extract(
               location,
               pattern_1
             ),
             TRUE ~ NA_character_
           )) %>%
  mutate(
    city = str_extract(location, pattern = regex_city) %>%
      str_replace_all( ., "[^[A-z]]", " ") %>%
      str_squish() %>%
      str_to_title(),
    state = str_extract(location, pattern = regex_state) %>%
      str_to_upper()
  ) %>%
  mutate(
    city = if_else(is.na(location),
                   str_extract(raw_info,
                               regex("(([A-Z]{1,}\\s{1,}){1,}|([A-z]){1,})")),
                   city) %>%
      str_squish() %>%
      str_to_title(),
    state = if_else(state %in% state.abb == FALSE, NA_character_, state)
  ) %>%
  select(-raw_location, -location)


# inpute state data from cities -------------------------------------------

complete_race_info <-
  race_info %>%
  select(organization, city, state) %>%
  filter(!is.na(state) & !is.na(city))

race_info <-
  race_info %>%
  mutate(state_inputed =
           case_when(
             is.na(state) & !is.na(city) ~
               pmap_chr(
                 list(organization, city),
                 function(org, cty) {
                   unique_state <- complete_race_info %>%
                     filter(organization == org & city == cty) %>%
                     pull(state) %>%
                     unique()

                   replacement <- ifelse(length(unique_state) == 1,
                                         unique_state,
                                         NA_character_)
                   return(replacement)
                 }
               ),
             TRUE ~ state),
         is_inputed = ifelse(is.na(state) & !is.na(state_inputed),
                             "yes",
                             "no")
  )


# Process release weather -------------------------------------------------
race_info <-
  race_info %>%
  mutate(
    raw_release_weather = str_extract(
      raw_release_weather, regex("[^(Release Wx:)].+")),
    release_sky = if_else(
      !is.na(raw_release_weather),
      str_split(raw_release_weather,
                pattern = ", ",
                simplify = TRUE)[,1],
      NA_character_),
    release_wind = if_else(
      !is.na(raw_release_weather),
      str_split(raw_release_weather,
                pattern = ", ",
                simplify = TRUE)[,2],
      NA_character_),
    release_temperature = if_else(
      !is.na(raw_release_weather),
      str_split(raw_release_weather,
                pattern = ", ",
                simplify = TRUE)[,3],
      NA_character_) %>% as.numeric()
  )


# Process arrival weather -------------------------------------------------

# Arrival weather is scraped from a faulty html node. The way this information
# could be extracted was by scrapping the whole text from the table header and
# then parse the specific information we need. This turned out to be useful to
# extract more information that was contained on the header.
race_info <-
  race_info %>%
  mutate(
    arrival_info = str_extract(
      raw_arrival_weather,
      regex("(?<=val Wx:).*$")),
    arrival_sky = str_split(
      arrival_info,
      pattern = ", ",
      simplify = TRUE)[,1] %>%
      str_extract(regex("(\\w{1,}\\W{1,}|\\w{1,}){1,}")) %>%
      str_to_sentence(locale = "en"),
    arrival_sky = if_else(arrival_sky == "None listed",
                          NA_character_,
                          arrival_sky),
    arrival_wind = str_split(
      arrival_info,
      pattern = ", ",
      simplify = TRUE)[,2] %>%
      str_extract(regex("(\\w{1,}\\W{1,}|\\w{1,}){1,}")),
    arrival_temperature = str_split(
      arrival_info,
      pattern = ", ",
      simplify = TRUE)[,3] %>%
      str_extract(regex("(\\w{1,}\\W{1,}|\\w{1,}){1,}")) %>%
      as.numeric()
  ) %>%
  select(-arrival_info)


# Process race details ----------------------------------------------------

# add a few details, processed from raw text on table header
race_info <-
  race_info %>%
  mutate(
    departure_time = str_extract(
      raw_info,
      regex("(\\d{1,2}:\\d{1,2})|(\\d\\s:\\d\\d)")
      ) %>%
      lubridate::hm(quiet = TRUE),
    n_birds = str_extract(
      raw_arrival_weather,
      regex("(?<=Birds: )\\d{1,}")) %>%
      as.numeric(),
    n_lofts = str_extract(
      raw_arrival_weather,
      regex("(?<=Lofts: )\\d{1,}")) %>%
      as.numeric()
  ) %>%
  select(
    race_id, year, date, obyb, organization, org_number, city, state, state_inputed, is_inputed, departure_time,
    release_sky, release_wind, release_temperature, everything()
    ) %>%
  # remove entries without option information
  filter(str_detect(raw_info, "None Found", negate = TRUE) &
         str_detect(raw_info, "First Select", negate = TRUE)) %>%
  select(-starts_with("raw"))


# code to prepare race_results --------------------------------------------

race_results <-
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

# clean names -------------------------------------------------------------
race_results <-
  race_results %>%
  janitor::clean_names()

# Process names -----------------------------------------------------------

race_results <-
  race_results %>%
  mutate(
    loft = str_extract(
      name,
      regex("(?<=\\()(\\w{1,}\\W{1,}|\\w{1,}){1,}(?=\\))")),
    competitor = str_extract(
      name,
      regex("([A-z]{1,}\\s{1,}|[A-z]{1,}){1,}(?=\\b|[^A-z])")) %>%
      str_to_title(locale =  "en")
  ) %>%
  select(race_id, competitor, loft, everything()) %>%
  select(-name)

# Process Band ------------------------------------------------------------
race_results <-
  race_results %>%
  mutate(
    bird_serial_number = str_extract(band, regex("^\\d{1,10}")),
    bird_association = str_split(band, regex("\\s{1,}"), simplify = TRUE)[,2],
    bird_club = str_extract(band, regex("\\w{1,}$")),
    bird_birthyear = str_extract(band, regex("(?<=\\w\\s{1,5})\\d{2,4}")) %>%
      as.numeric()
  ) %>%
  mutate(
    bird_association = case_when(
      bird_association == "" ~ NA_character_,
      str_detect(bird_association, "^AU") ~ "AU",
      str_detect(bird_association, "ARPU") ~ "AU",
      TRUE ~ bird_association)
  )

# process color and sex ---------------------------------------------------
race_results <-
  race_results %>%
  mutate(
    bird_color = str_match(color, "[A-z]{1,}")[,1] %>% str_to_upper,
    bird_sex = str_match(sex, "[A-z]{1,}")[,1] %>% str_to_upper()
  ) %>%
  mutate(
    bird_color = case_when(
      str_detect(bird_color, "UNK") ~ NA_character_,
      TRUE ~ bird_color)
  ) %>%
  select(-color, -sex)


# Process arrival time ----------------------------------------------------
race_results <-
  race_results %>%
  mutate(arrival_time = arrival_tf(arrival)) %>%
  select(-arrival) %>%
  select(race_id, competitor, loft, pos, section, band, arrival_time, miles,
         to_win, ndb_points, everything())

# process race_time -------------------------------------------------------
race_results <-
  race_results %>%
  left_join(race_info %>%
  select(race_id, departure_time),
  by = "race_id") %>%
  mutate(race_time = diff_time(arrival_time, departure_time),
         race_time_adjusted = case_when(
           (lubridate::period_to_seconds(arrival_time) -
             lubridate::period_to_seconds(departure_time) < 0 ~ "yes"),
           TRUE ~ "no"
         ))

# Process ndb points ------------------------------------------------------
race_results <-
race_results %>%
  mutate(ndb_points = as.numeric(ndb_points))

# compose race_results table ----------------------------------------------
race_results <-
race_results %>%
  select(race_id, competitor, loft, section, pos, band, departure_time,
         arrival_time, race_time, race_time_adjusted, miles, ypm, ndb_points,
         starts_with("bird"))

# add summary variables to race_info --------------------------------------

# number of race records that exist per race.
race_info <-
  race_info %>%
  left_join(
    race_results %>%
      mutate(count_i = if_else(is.na(arrival_time),
                               0, 1),
      ) %>%
      group_by(race_id) %>%
      summarize(n_records = sum(count_i)),
    by = "race_id"
  ) %>%
  mutate(n_records = ifelse(n_records == 0, NA, n_records))


# Save data sets locally --------------------------------------------------

usethis::use_data(race_info, overwrite = TRUE, compress = "xz")
readr::write_rds(race_results, "data-raw/race_results.rds", compress = "xz")

