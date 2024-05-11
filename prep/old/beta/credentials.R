
library(googlesheets4)
library(dplyr)

credentials <- data.frame(
  user = c("taylor", "kaue"), # mandatory
  password = c("taylor_atlas", "kaue_atlas"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2024-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# open new set o credentials
credentials1 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rtjWgxjqr8Xaw_0H679bqhWVkJglUoBZSlSrF0wOIvQ/edit?usp=sharing",
                           range = "A2:N114") %>%
  janitor::clean_names() %>%
  select(user = atlas_username, password = atlas_password ) %>%
  filter(!is.na(user)) %>%
  distinct(user, .keep_all = TRUE)

# bindd
credentials_new <- bind_rows(credentials, credentials1) %>%
  mutate(start = "2019-04-15", expire = "2024-12-31", admin = TRUE)

# export
readr::write_rds(credentials_new, "data/credentials.rds")
