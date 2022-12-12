library(sf)
library(dplyr)
library(data.table)
library(readr)

# countries rank and countries shape
atlas_country <- read_rds("data/sample5/atlas_country_polygons.rds")

# calculate size of each group
country_ranks <- atlas_country %>%
  st_set_geometry(NULL) %>%
  mutate(across(city_poptotal_1975:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
  # create totals - NEED FIX
  mutate(n = n()) %>%
  mutate(rank_type = "country_world")

dir.create("data/sample5/ranks")

readr::write_rds(country_ranks, sprintf("data/sample5/ranks/rank_country.rds"))


world <- dir("data/sample5", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{5}.rds")
data_world <- lapply(world, function(x) st_set_geometry(read_rds(x), NULL)) %>% rbindlist(fill = TRUE)


prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0621"
  # ghsl <- "1445" # recife
  # ghsl <- "1445" # recife
  # ghsl <- "0634"
  
  # calculate ranks for admin level 8 (cities for fortaleza - test)
  # compare to: other cities in the world, in the country, in the metro
  
  data <- data_world %>% filter(hdc == ghsl)
  
  # in the world
  rank_world <- data_world %>%
    # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
    filter(admin_level != 10) %>%
    # delete indicators that are NA
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(across(city_poptotal_1975:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
    # create totals - NEED FIX
    mutate(n = n()) %>%
    mutate(rank_type = "world") %>%
    ungroup()
  
  # in the country
  
  # metro
  rank_city_metro <- data %>%
    filter(admin_level != 0) %>%
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(across(city_poptotal_1975:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
    # create totals - NEED FIX
    mutate(n = n()) %>%
    # create type of rank
    mutate(rank_type = "metro")
  
  # bind the comparions
  rank_complete <- rbind(rank_world, rank_city_metro)
  
  
  
  
  # export
  readr::write_rds(rank_complete, sprintf("data/sample5/ranks/rank_%s.rds", ghsl))
  
}

cities_available <- unique(data_world$hdc)
purrr::walk(cities_available, 
            prep_data)
