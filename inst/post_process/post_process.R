library(tidyverse)
library(stringr)

# load data
data(race_info)

# regex patterns for processing location, city, state and roman numerals

pattern_1 <- regex("((?!\\d|\\d\\WMile\\W)(\\w+\\W{1,}){1,}[A-z]{2}\\b)")
regex_city <- regex("((\\w+\\W{1,}){1,})(?!\\W{1,}[A-z]{2}\\b)")
regex_state <- regex("(?!(\\w+\\W{1,}){1,})([A-z]{2}\\b)")
roman_numerals <- regex("\\W[MDCLXVI]+\\W")


# Process year and organization name --------------------------------------
race_info <-
  race_info %>%
  mutate(
    year_ob_yb = stringr::str_split(
      year,
      # pattern seperate by white space
      patter = regex("\\s"),
      simplify = TRUE) %>% .[,2],
    year = readr::parse_number(year),
    # pattern extract names up until " -"
    organization = str_extract(organization, pattern = regex(".+?(?= -)")) %>%
      str_squish() %>%
      str_to_title(locale = "en")
  )

# Process ocation --------------------------------------------------------
race_info <-
  race_info %>%
  mutate(location =
           case_when(
             str_detect(raw_location, pattern_1) == TRUE ~
               str_extract(raw_location, pattern_1),
             TRUE ~ NA_character_
           )) %>%
  mutate(
    location = case_when(
      # set locations with roman numerals to NA
      str_detect(location,  roman_numerals) == TRUE ~ NA_character_,
      # set locations with numbers on the name as NA
      # this is imperfect, but a lot of the locations scrapped, had information,
      # about the week of the race and no so much about where the race occured
      str_detect(location, regex("[0-9]+")) == TRUE ~ NA_character_,
      # Remove particular locations with MID WK pattern
      str_detect(location, regex("MID WK")) == TRUE ~ NA_character_,
      TRUE ~ location),
    # process city
    city = str_extract(location, pattern = regex_city) %>%
      str_replace_all( ., "[^[:alnum:]]", " ") %>%
      str_squish() %>%
      str_to_title(),
    # process state
    state = str_extract(location, pattern = regex_state) %>%
      str_to_upper()
  ) %>%
  mutate(
    # if a processed state is not a real state two-letter abreviation,
    # set city and state to NA
    # this is conservative, but it is a starting point.
    state = if_else(state %in% state.abb == FALSE, NA_character_, state),
    city = if_else(state %in% state.abb == FALSE, NA_character_, city)) %>%
  select(-raw_location, -location)


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
      raw_arrival_weather,
      regex("(?<=\\(A\\): )\\d{2}:\\d{2}")) %>%
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
    race_id, year, year_ob_yb, organization, city, state, date,
    release_sky, release_wind, release_temperature, everything()) %>%
  select(-starts_with("raw"))

