library(tidyverse)
library(stringr)
data(race_info)


# clean names -------------------------------------------------------------
race_results_temp <-
  race_results %>%
  janitor::clean_names() %>%
  select(-starts_with("bird"))


# Process names -----------------------------------------------------------

race_results_temp <-
  race_results_temp %>%
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

glimpse(race_results_temp)

# process color and sex ---------------------------------------------------
race_results_temp <-
  race_results_temp %>%
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

race_results_temp <-
  race_results_temp %>%
  mutate(arrival_time = str_extract(arrival, "\\d.+\\d")) %>%
  mutate(
    arrival_time =
      case_when(
        str_detect(arrival_time, "(\\d{1,2}:){3}") ~
          str_replace(arrival_time, "(?<=(\\d{1,2}:){2}\\d{1,2})\\W","."),
        TRUE ~ arrival_time) %>%
      lubridate::hms()
    ) %>%
  select(-arrival)

