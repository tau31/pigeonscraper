library(tidyverse)
library(stringr)
data(race_info)


# clean names -------------------------------------------------------------
race_results <-
  race_results %>%
  janitor::clean_names() %>%
  select(-starts_with("bird"))


# Process names -----------------------------------------------------------

race_results_temp <-
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

race_results_temp <-
  race_results_temp %>%
  mutate(
    serial_number = str_extract(band, regex("^\\d{1,10}")),
    association = str_split(band, regex("\\s{1,}"), simplify = TRUE)[,2],
    club = str_extract(band, regex("\\w{1,}$")),
    birthyear = str_extract(band, regex("(?<=\\w\\s{1,5})\\d{2,4}")) %>%
      as.numeric()
  ) %>%
  mutate(association = if_else(
    association == "",
    NA_character_,
    association)
    )

# process color and sex ---------------------------------------------------

# race_results_temp %>%
#   mutate(color = str_match(color, "[A-z]{1,}")) %>%
#   filter(is.na(color)) %>%
#   group_by(color) %>%
#   count
