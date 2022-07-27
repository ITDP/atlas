library(sf)
library(dplyr)
library(data.table)
library(readr)

# countries rank
atlas_country <- read_rds("data/sample3_prep/atlas_country_polygons.rds")

# calculate size of each group
country_ranks <- atlas_country %>%
  mutate(n = n()) %>%
  mutate(across(walk_healthcare_2019:performance_bike60_2019, rank, ties = "first")) %>%
  mutate(across(walk_healthcare_2019:performance_bike60_2019, ~n - .x + 1)) %>%
  mutate(rank_type = "country_world") %>% 
  setDT()

dir.create("data/sample3_prep/ranks")

readr::write_rds(country_ranks, sprintf("data/sample3_prep/ranks/rank_country.rds"))



prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0621"
  # ghsl <- "1445" # recife
  
  world <- dir("data/sample3_prep", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{4}.rds")
  data_world <- lapply(world, read_rds) %>% rbindlist(fill = TRUE) %>% st_sf()
  data <- read_rds(sprintf("data/sample3_prep/ghsl_%s/indicators_%s.rds", ghsl, ghsl)) %>% st_set_geometry(NULL)
  
  # calculate ranks for admin level 8 (cities for fortaleza - test)
  # compare to: other cities in the world, in the country, in the metro
  
  # in the world
  rank_world <- data_world %>%
    # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
    filter(admin_level != 10) %>%
    # delete indicators that are NA
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(n = n()) %>%
    mutate(across(walk_healthcare_2019:performance_bike60_2019, rank, ties = "first")) %>%
    mutate(across(walk_healthcare_2019:performance_bike60_2019, ~n - .x + 1)) %>%
    mutate(rank_type = "world") %>% setDT()
  
  # in the country
  
  # metro
  rank_city_metro <- data %>%
    filter(admin_level != 0) %>%
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(n = n()) %>%
    mutate(across(walk_healthcare_2019:performance_bike60_2019, rank, ties = "first")) %>%
    mutate(across(walk_healthcare_2019:performance_bike60_2019, ~n - .x + 1)) %>%
    # create type of rank
    mutate(rank_type = "metro") %>%
    setDT()
  
  # bind the comparions
  rank_complete <- rbind(rank_world, rank_city_metro, fill = TRUE)
  
  
  
  
  # export
  readr::write_rds(rank_complete, sprintf("data/sample3_prep/ranks/rank_%s.rds", ghsl))
  
}


purrr::walk(c("0088", "0200", "0561", "0621", "1406", "1445"), prep_data)
