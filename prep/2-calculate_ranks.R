library(sf)
library(dplyr)
library(data.table)
library(readr)

# countries rank and countries shape
atlas_country <- read_rds("data/sample3/atlas_country_polygons.rds")

# calculate size of each group
country_ranks <- atlas_country %>%
  st_set_geometry(NULL) %>%
  mutate(across(city_poptotal_1975:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
  # create totals - NEED FIX
  mutate(n = n()) %>%
  mutate(rank_type = "country_world")

dir.create("data/sample3/ranks")

readr::write_rds(country_ranks, sprintf("data/sample3/ranks/rank_country.rds"))


world <- dir("data/sample3", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{4}.rds")
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
  readr::write_rds(rank_complete, sprintf("data/sample3/ranks/rank_%s.rds", ghsl))
  
}


purrr::walk(c("0088", "0200", "0561", "0621", "1406", "1445",
              "0014", "0154", "0634"), 
            prep_data)
