library(sf)
library(dplyr)
library(data.table)
library(readr)

# countries rank and countries shape
atlas_country <- read_rds("data/sample3/atlas_country_polygons.rds")

# calculate size of each group
country_ranks <- atlas_country %>%
  st_set_geometry(NULL) %>%
  mutate(across(walk_pnh_2019:last_col(), rank, ties = "first")) %>%
  mutate(n = n()) %>%
  mutate(across(walk_pnh_2019:last_col(1), ~n - .x + 1)) %>%
  # select(-n) %>%
  mutate(rank_type = "country_world")

dir.create("data/sample3/ranks")

readr::write_rds(country_ranks, sprintf("data/sample3/ranks/rank_country.rds"))



prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0621"
  # ghsl <- "1445" # recife
  
  world <- dir("data/sample3", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{4}.rds")
  data_world <- lapply(world, read_rds) %>% rbindlist(fill = TRUE)  %>% st_sf() %>% st_set_geometry(NULL)
  data <- read_rds(sprintf("data/sample3/ghsl_%s/indicators_%s.rds", ghsl, ghsl)) %>% st_set_geometry(NULL)
  
  # calculate ranks for admin level 8 (cities for fortaleza - test)
  # compare to: other cities in the world, in the country, in the metro
  
  # in the world
  rank_world <- data_world %>%
    # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
    filter(admin_level != 10) %>%
    # delete indicators that are NA
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(across(walk_pnh_2019:last_col(), rank, ties = "first")) %>%
    mutate(n = n()) %>%
    mutate(across(walk_pnh_2019:last_col(1), ~n - .x + 1)) %>%
    # select(-n) %>%
    mutate(rank_type = "world")
  
  # in the country
  
  # metro
  rank_city_metro <- data %>%
    filter(admin_level != 0) %>%
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(across(walk_pnh_2019:last_col(), rank, ties = "first")) %>%
    mutate(n = n()) %>%
    mutate(across(walk_pnh_2019:last_col(1), ~n - .x + 1)) %>%
    # select(-n) %>%
    # create type of rank
    mutate(rank_type = "metro")
  
  # bind the comparions
  rank_complete <- rbind(rank_world, rank_city_metro)
  
  
  
  
  # export
  readr::write_rds(rank_complete, sprintf("data/sample3/ranks/rank_%s.rds", ghsl))
  
}


purrr::walk(c("0088", "0200", "0561", "0621", "1406", "1445"), prep_data)
