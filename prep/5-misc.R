#get list of indicators

library(googlesheets4)
library(readr)
library(data.table)
library(dplyr)
library(sf)


list_indicators <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
  sheet = "Indicators"
) 


# salvar
write_rds(list_indicators, "data/data_final/list_indicators.rds")


overlay_table <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/194T-zZZRhwAAvon7lOeyjT49jzij3SKhDM-2HyBi3Hc/edit?usp=sharing") %>%
  mutate(overlay = basename(overlay_dir)) %>%
  mutate(overlay = sub(pattern = ".geojson", replacement = "", x = overlay)) %>%
  mutate(overlay = sub(pattern = ".tif", replacement = "", x = overlay))

# salvar
readr::write_rds(overlay_table, "data/data_final/overlay_table.rds")

# # block density list
# list_block <- read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/1c3KL909dthuTFMq_snm55fURK4v5yCWJ-30kRqtnaqw/edit?usp=sharing",
#   sheet = "Sheet1"
# ) %>% as.data.frame()
# 
# 
# # salvar
# write_rds(list_block, "data/sample5/list_block_density.rds")




# identify and save brazilian cities - those would have a popup for the indicator peop --------


indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

brazil_cities <- indicators_all %>%
  st_set_geometry(NULL) %>%
  filter(country == "Brazil") %>%
  distinct(country, hdc)

# save
readr::write_rds(brazil_cities, "data/data_final/brazil_cities.rds")




# create a file with all the data for each agglomeration ------------------


