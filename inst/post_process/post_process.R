library(tidyverse)
library(stringr)

data(race_info)


race_info_pr <-
  race_info %>%
  mutate(
    year_ob_yb = stringr::str_split(
      year,
      # pattern seperate by white space
      patter = regex("\\s"),
      simplify = TRUE) %>% .[,2],
    year = readr::parse_number(year),
    # pattern extract names up until " -"
    organization = str_extract(organization, pattern = regex(".+?(?= -)"))) %>%
  select(race_id, year, year_ob_yb, everything())

# City,St
pattern_1 <- regex("[A-z]+,+[A-z][A-z]")
# City, St
pattern_2 <- regex("[A-z]+,+\\s+[A-z][A-z]")
# City , St
pattern_3 <- regex("[A-z]+\\s+,+\\s+[A-z][A-z]")
# City St
pattern_4 <- regex("[A-z]+\\s+[A-z]{2}")
# CUMMING<GA
CUMMING<GA


race_info_pr %>%
  select(raw_location) %>%
  mutate(location =
  case_when(
    str_detect(raw_location, pattern_1) == TRUE ~ str_extract(raw_location, pattern_1),
    str_detect(raw_location, pattern_2) == TRUE ~ str_extract(raw_location, pattern_2),
    str_detect(raw_location, pattern_3) == TRUE ~ str_extract(raw_location, pattern_3),
    str_detect(raw_location, pattern_4) == TRUE ~ str_extract(raw_location, pattern_4),
    TRUE ~ NA_character_
  )) %>%
  View


race_info_pr$raw_release_weather
glimpse(race_info_pr)
