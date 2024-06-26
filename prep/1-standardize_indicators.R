library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
sf::sf_use_s2(FALSE)


# source folder -------------------------------------------------------------------------------

folder <- "data-raw/data_final/"


# # duplicate the pop data for 2022 as well -----------------------------------------------------
# list_pop_2020 <- dir(sprintf("%s/cities", folder), recursive = TRUE, pattern = "pop_2020.tif$", full.names = TRUE)
# # reanem to 2022
# list_pop_2022 <- stringr::str_replace(list_pop_2020, pattern = "2020(?=.tif$)", replacement = "2022")
# # copy
# purrr::walk2(list_pop_2020, list_pop_2022, file.copy)



# rename folder to 5 chars ------------------------------------------------
list_folders <- dir(sprintf("%s/cities", folder), full.names = TRUE)
# identify the code
codes <- stringr::str_extract(list_folders, "\\d{1,}$")
# make them 5 chars
codes_full <- stringr::str_pad(codes, width = 5, side = "left", pad = 0)
# substitute in the original string
list_folders_updated <- stringr::str_replace(list_folders, pattern = "\\d{1,}$", replacement = codes_full)
# rename
purrr::map2(list_folders, list_folders_updated, file.rename)


# go
# base_dir <- sprintf("data-raw/data_sample_2022_08_19/city_results/ghsl_region_%s/", ghsl)
# data <- st_read(paste0(base_dir, "indicator_values.gpkg"))
world <- dir(sprintf("%s/cities", folder), full.names = TRUE, recursive = TRUE)
# world <- c(world, dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE))
world <- world[grepl("ghsl_region_\\d{5}/indicator_values.gpkg$", world)]
# fun to open all data
open_data <- function(file) {
  
  
  # file <- world[50]
  
  a <- st_read(file)
  a <- st_cast(a, "MULTIPOLYGON")
  a <- tidyr::fill(a, hdc)
  # a <- tidyr::fill(a, hdc)
  a <- a %>% mutate(osmid = as.character(osmid),
                    # level_name_eng = as.character(level_name_eng),
                    # level_name_local = as.character(level_name_local),
                    across(starts_with("block_density"), as.numeric),
                    across(starts_with("km"), as.numeric),
                    across(starts_with("rtr"), as.numeric)
  )
  
  # # identfy origin
  # origin1 <- stringr::str_extract(file, "sample_3")
  # a <- a %>% mutate(origin = origin1)
  # 
  # if (isTRUE(origin1 == "sample_3")) {
  #   
  #   a <- select(a, name, hdc, osmid, admin_level, performance_bike_lts2_45, performance_walk_45)
  #   
  # }
  
  
  
}
data_all <- lapply(world, open_data) %>% rbindlist(fill = TRUE)
# ghsl to 5 characters
data_all <- data_all %>% mutate(hdc = stringr::str_pad(hdc, width = 5, side = "left", pad = 0))


# remove indicators that we wont use
data_all <- data_all %>%
  # dplyr::select(-starts_with("rtr_")) %>%
  dplyr::select(-starts_with("stns_")) %>%
  # dplyr::select(-starts_with("km_")) %>%
  dplyr::select(-starts_with("n_points_special")) %>%
  dplyr::select(-starts_with("performance")) %>%
  # select(-performance_walk_30, -performance_walk_60, -performance_bike_lts1_30, -performance_bike_lts1_45,
  # -performance_bike_lts1_60, -performance_bike_lts2_30, -performance_bike_lts2_60) %>%
  # select(-density) %>%
  # select(-blockmean_density) %>%
  dplyr::select(-geospatial_calctime, -summary_calctime)





prep_data <- function(ghsl) {
  # ghsl <- "01406"
  # ghsl <- "02051" # barcelona
  # ghsl <- "01445" # rec
  # ghsl <- "00021"
  # ghsl <- "12080"
  # ghsl <- "00154"
  # ghsl <- "00010"
  # ghsl <- "01397"
  # ghsl <- "01575"
  # ghsl <- "00017" #pheonix
  
  # base_dir <- sprintf("data-raw/sample_3/ghsl_region_%s/", ghsl)
  
  # filter only city
  data <- data_all %>% filter(hdc == ghsl) %>% st_sf(crs = 4326)
  
  attr(data, "agr") <- NULL
  
  # extract country
  country1 <- unique(data$country)[!is.na(unique(data$country))][1]
  
  # levl name
  data <- data %>% mutate(level_name = ifelse(is.na(level_name), level_name_eng, level_name))
  # identify the ghsl level as 0
  data <- data %>% mutate(admin_level = ifelse(level_name == "Agglomeration", 0, 
                                               ifelse(level_name =="Brazilian Metro Areas", 1, admin_level)))
  data <- data %>% filter(level_name != "Brazilian Metro Areas")
  
  
  # identify the ghsl number to the osmid
  data <- data %>% mutate(osmid = ifelse(is.na(osmid), hdc, osmid))
  # fill the name of the country
  data <- data %>% mutate(country = country1)
  
  # por enquanto, apagar o que esta com o nome NA
  data <- data %>% filter(!is.na(name))
  
  # it may be necessary to scale the admin_level - from 0 to max
  ordered_admin <- sort(as.numeric(unique(data$admin_level)))
  admin_level_oder <- data.frame(admin_level = as.character(ordered_admin), admin_level_ordered = seq(from = 1, to = length(ordered_admin)))
  data <- data %>% mutate(admin_level = as.character(admin_level))
  data <- left_join(data, admin_level_oder, by = "admin_level")
  
  # data <- data %>% mutate(admin_level = level_name)
  
  # organize columns
  data <- data %>% dplyr::select(hdc, a3 = country, osmid, name, admin_level, admin_level_ordered, admin_level_name = level_name, everything())
  
  
  # get available indicators for this city
  ind_columns <- colnames(data)[colnames(data) %nin% c("hdc", "a3", "osmid", "name", "admin_level", "admin_level_ordered", "admin_level_name",  "geom")]
  
  
  # first, rename indicators that are divided by year
  ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                      replacement = "city_popdensitytotal_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "^(density)_(\\d{4})",
                      replacement = "city_popdensity_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(block_density)_(\\d{4})",
                      replacement = "city_blockdensity_\\2",
                      x = ind_columns)
  # ind_columns <- gsub(pattern = "(journey_gap)_(\\d{4})",
  #                     replacement = "city_journeygap_\\2",
  #                     x = ind_columns,
  #                     perl = TRUE)
  
  
  ind_columns <- gsub(pattern = "(pnab)_(\\d{4})",
                      replacement = "bike_pnpbpnab_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(pnpb)_(\\d{4})",
                      replacement = "bike_pnpb_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(all_bikeways_km)_(\\d{4})",
                      replacement = "bike_pnpbabikewayskm_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(protected_bikeways_km)_(\\d{4})",
                      replacement = "bike_pnpbpbikewayskm_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(bikeshare)_(\\d{4})",
                      replacement = "bike_bikeshare_\\2",
                      x = ind_columns)
  
  
  ind_columns <- gsub(pattern = "(^healthcare)_(\\d{4})",
                      replacement = "walk_pnspnh_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(^n_points_healthcare)_(\\d{4})",
                      replacement = "walk_pnshealthpoints_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(^schools)_(\\d{4})",
                      replacement = "walk_pnspne_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(^n_points_schools)_(\\d{4})",
                      replacement = "walk_pnsschoolspoints_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(^h.s)_(\\d{4})",
                      replacement = "walk_pns_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(^carfree)_(\\d{4})",
                      replacement = "walk_pncf_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(people_not_near_highways)_(\\d{4})",
                      replacement = "walk_pnnhighways_\\2",
                      x = ind_columns,
                      perl = TRUE)
  ind_columns <- gsub(pattern = "(highway_km)_(\\d{4})",
                      replacement = "walk_pnnhighwayskm_\\2",
                      x = ind_columns,
                      perl = TRUE)
  
  
  
  ind_columns <- gsub(pattern = "(pnft)_(\\d{4})",
                      replacement = "transit_pnft_\\2",
                      x = ind_columns)
  
  ind_columns <- gsub(pattern = "(n_points_transit_pnft)_(\\d{4})",
                      replacement = "transit_pnftpoints_\\2",
                      x = ind_columns)
  
  
  ind_columns <- gsub(pattern = "(PNrT)_(all)_(\\d{4})",
                      replacement = "transit_\\L\\1_\\3",
                      x = ind_columns,
                      perl = TRUE)
  ind_columns <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
                      replacement = "transit_\\L\\1\\E\\2_\\3",
                      x = ind_columns,
                      perl = TRUE)
  ind_columns <- gsub(pattern = "(km)_([[:lower:]]{3})_(\\d{4})",
                      replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
                      x = ind_columns,
                      perl = TRUE)
  ind_columns <- gsub(pattern = "(rtr)_([[:lower:]]{3})_(\\d{4})",
                      replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
                      x = ind_columns,
                      perl = TRUE)
  
  
  ind_columns <- gsub(pattern = "(pnst)_(\\d{4})",
                      replacement = "transit_pnst_\\2",
                      x = ind_columns)
  
  
  
  # create new column names with new standardized id
  ind_columns_new <- fcase(
    # startsWith(ind_columns, "city_poptotal"), ind_columns,
    startsWith(ind_columns, "city_popdensity"), ind_columns,
    startsWith(ind_columns, "city_blockdensity"), ind_columns,
    startsWith(ind_columns, "city_journeygap"), ind_columns,
    
    startsWith(ind_columns, "bike_pnab"),                  ind_columns,
    startsWith(ind_columns, "bike_pnpb"),                  ind_columns,
    startsWith(ind_columns, "bike_abikewayskm"),       ind_columns,
    startsWith(ind_columns, "bike_pbikewayskm"), ind_columns,
    startsWith(ind_columns, "bike_bikeshare"),             ind_columns,
    
    startsWith(ind_columns, "walk_pns"),                   ind_columns,
    startsWith(ind_columns, "walk_pncf"),               ind_columns,
    startsWith(ind_columns, "walk_pnnhighways"),                  ind_columns,
    
    startsWith(ind_columns, "transit_pnft"),                  ind_columns,
    
    startsWith(ind_columns, "transit_pnrt"), ind_columns,
    
    startsWith(ind_columns, "transit_pnst"), ind_columns
    
    
  )
  
  # # bring country name
  data <- data %>%
    left_join(dplyr::select(maps::iso3166, a3, country = ISOname) %>% distinct(a3, .keep_all = TRUE), by = c("a3")) %>%
    tidyr::fill(a3, country) %>%
    ungroup()
  
  # select columns
  data <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name, everything())
  
  
  # rename indicators
  colnames(data) <- c("hdc", "country", "a3", "osmid", "name", "admin_level", "admin_level_ordered", "admin_level_name", ind_columns_new,"geom")
  
  # arrange data correctly
  data <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name,
                  # starts_with("city_poptotal"),
                  starts_with("city_popdensity"),
                  starts_with("city_blockdensity"),
                  starts_with("city_journeygap"),
                  starts_with("bike_pnab"),
                  starts_with("bike_pnpb"),
                  starts_with("walk_pns"),
                  starts_with("walk_pncf"),
                  starts_with("walk_pnnhighways"),
                  starts_with("transit_pnft"),
                  starts_with("transit_pnrt"),
                  starts_with("transit_pnst")
                  ) %>%
    mutate(across(starts_with("transit"), as.numeric))
  
  data$admin_level <- as.character(data$admin_level)
  data$admin_level_ordered <- as.character(data$admin_level_ordered)
  
  data <- data %>%
    mutate(across(starts_with("transit_pnft"), as.numeric)) %>%
    mutate(across(starts_with("city_journeygap"), as.numeric)) %>%
    arrange(as.numeric(admin_level))
  
  # exception for recife
  if (ghsl == "01445") {
    
    data$country <- "Brazil"
    data$a3 <- "BRA"
    
  }
  
  
  dir.create(sprintf("data/data_final/ghsl_%s", ghsl), recursive = TRUE)
  
  # save
  readr::write_rds(data, sprintf("data/data_final/ghsl_%s/indicators_%s.rds", ghsl, ghsl))
  
}


# apply to all cities
cities_available <- unique(data_all$hdc)
library(purrr)
library(furrr)
plan(multisession)
furrr::future_walk(cities_available, prep_data)










# calculate boundaries for the world view ---------------------
indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# # remove polygon and save the data
# indicators_all %>% st_set_geometry(NULL) %>% 
#   mutate(across(8:last_col(), round, 3)) %>%
#   # mutate(across(city_poptotal_1975:performance_bikep45_2019, round, 3)) %>%
#   readr::write_rds("data/sample5/all_indicators.rds")


# select the admin level NA - the ghsl level
indicators_ghsl <- indicators_all %>% filter(admin_level == 0) %>% st_sf(crs = 4326)
# centroids
indicators_ghsl_centroids <- st_centroid(indicators_ghsl)
# save
readr::write_rds(indicators_ghsl_centroids, "data/data_final/atlas_city_markers.rds")





# calculate mean for each country -----------------


atlas_country <- st_read(sprintf("%s/countries/country_results.geojson", folder))
# atlas_country <- rmapshaper::ms_simplify(atlas_country, keep = 0.1)
# atlas_country1 <- fread("data-raw/atlas_data_july_31/country_results/country_results.csv")

# bring the names
atlas_country <- atlas_country %>%
  filter(index != "-99") %>%
  left_join(
    countrycode::codelist %>% dplyr::select(country.name.en, iso3c),
    
    by = c("index" = "iso3c")) %>%
  # select(-name) %>%
  # rename(name = country.name.en) %>%
  select(a3 = index, name, everything())

# rename indicators
# get available indicators for this city
ind_columns <- colnames(atlas_country)[colnames(atlas_country) %nin% c("a3", "name", "geometry")]


# first, rename indicators that are divided by year
ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                    replacement = "city_popdensitytotal_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "^(density)_(\\d{4})",
                    replacement = "city_popdensity_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(block_density)_(\\d{4})",
                    replacement = "city_blockdensity_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(journey_gap)_(\\d{4})",
                    replacement = "city_journeygap_\\2",
                    x = ind_columns,
                    perl = TRUE)


ind_columns <- gsub(pattern = "(pnab)_(\\d{4})",
                    replacement = "bike_pnpbpnab_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(pnpb)_(\\d{4})",
                    replacement = "bike_pnpb_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(all_bikeways_km)_(\\d{4})",
                    replacement = "bike_pnpbabikewayskm_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(protected_bikeways_km)_(\\d{4})",
                    replacement = "bike_pnpbpbikewayskm_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(bikeshare)_(\\d{4})",
                    replacement = "bike_bikeshare_\\2",
                    x = ind_columns)


ind_columns <- gsub(pattern = "(^healthcare)_(\\d{4})",
                    replacement = "walk_pnspnh_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(^n_points_healthcare)_(\\d{4})",
                    replacement = "walk_pnshealthpoints_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(^schools)_(\\d{4})",
                    replacement = "walk_pnspne_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(^n_points_schools)_(\\d{4})",
                    replacement = "walk_pnsschoolspoints_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(^h.s)_(\\d{4})",
                    replacement = "walk_pns_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(^carfree)_(\\d{4})",
                    replacement = "walk_pncf_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(people_not_near_highways)_(\\d{4})",
                    replacement = "walk_pnnhighways_\\2",
                    x = ind_columns,
                    perl = TRUE)
ind_columns <- gsub(pattern = "(highway_km)_(\\d{4})",
                    replacement = "walk_pnnhighwayskm_\\2",
                    x = ind_columns,
                    perl = TRUE)



ind_columns <- gsub(pattern = "(pnft)_(\\d{4})",
                    replacement = "transit_pnft_\\2",
                    x = ind_columns)

ind_columns <- gsub(pattern = "(n_points_transit_pnft)_(\\d{4})",
                    replacement = "transit_pnftpoints_\\2",
                    x = ind_columns)


ind_columns <- gsub(pattern = "(PNrT)_(all)_(\\d{4})",
                    replacement = "transit_\\L\\1_\\3",
                    x = ind_columns,
                    perl = TRUE)
ind_columns <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
                    replacement = "transit_\\L\\1\\E\\2_\\3",
                    x = ind_columns,
                    perl = TRUE)
ind_columns <- gsub(pattern = "(km)_([[:lower:]]{3})_(\\d{4})",
                    replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
                    x = ind_columns,
                    perl = TRUE)
ind_columns <- gsub(pattern = "(rtr)_([[:lower:]]{3})_(\\d{4})",
                    replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
                    x = ind_columns,
                    perl = TRUE)


ind_columns <- gsub(pattern = "(pnst)_(\\d{4})",
                    replacement = "transit_pnst_\\2",
                    x = ind_columns)



# create new column names with new standardized id
ind_columns_new <- fcase(
  startsWith(ind_columns, "city_poptotal"), ind_columns,
  startsWith(ind_columns, "city_popdensity"), ind_columns,
  startsWith(ind_columns, "city_blockdensity"), ind_columns,
  startsWith(ind_columns, "city_journeygap"), ind_columns,
  
  startsWith(ind_columns, "bike_pnab"),                  ind_columns,
  startsWith(ind_columns, "bike_pnpb"),                  ind_columns,
  startsWith(ind_columns, "bike_abikewayskm"),       ind_columns,
  startsWith(ind_columns, "bike_pbikewayskm"), ind_columns,
  startsWith(ind_columns, "bike_bikeshare"),             ind_columns,
  
  startsWith(ind_columns, "walk_pns"),                   ind_columns,
  startsWith(ind_columns, "walk_pncf"),               ind_columns,
  startsWith(ind_columns, "walk_pnnhighways"),                  ind_columns,
  
  startsWith(ind_columns, "transit_pnft"),                  ind_columns,
  
  startsWith(ind_columns, "transit_pnrt"), ind_columns,
  startsWith(ind_columns, "transit_pnst"), ind_columns
  
  
  
)


colnames(atlas_country) <- c("a3", "name", ind_columns_new, "geometry")

# arrange data correctly
atlas_country <- atlas_country %>%
  dplyr::select(a3, name,
                starts_with("city_popdensity"),
                starts_with("city_blockdensity"),
                starts_with("city_journeygap"),
                starts_with("bike_pnpb"),
                starts_with("walk_pns"),
                starts_with("walk_pncf"),
                starts_with("walk_pnnhighways"),
                starts_with("transit_pnft"),
                starts_with("transit_pnrt"),
                starts_with("transit_pnst")
                )

atlas_country <- atlas_country %>% mutate(across(city_popdensity_1975:transit_pnst_2024, as.numeric))

# select only the indicators that we have in the indicators
atlas_country <- atlas_country %>%
  dplyr::select(a3, name,
                any_of(colnames(indicators_all))   
  ) %>%
  # round indicators
  mutate(across(c(bike_pnpbabikewayskm_2024, bike_pnpbpbikewayskm_2024, walk_pnshealthpoints_2024, walk_pnsschoolspoints_2024,
                  transit_pnftpoints_2024), round))

# save by indicator
save_countries <- function(ind) {
  
  
  atlas_country_ind <- atlas_country %>% 
    dplyr::select(a3, name, 
                  starts_with(ind)) 
  
  # # filter for no data
  # if (ind == "city_poptotal") {
  #   
  #   atlas_country_ind <- atlas_country_ind %>% mutate(across(starts_with("city_poptotal"), ~ifelse(.x == 0, NA, .x)))
  #   
  # }
  
  # drop all NA
  atlas_country_ind <- tidyr::drop_na(atlas_country_ind)
  
  # save
  if (nrow(atlas_country_ind) > 0) {
    
    readr::write_rds(atlas_country_ind, sprintf("data/data_final/countries/atlas_country_%s.rds",
                                                ind))
    
  }
}


# to long format
colnames_compare <- colnames(atlas_country)[3:ncol(atlas_country)]
# extract year
years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                      replacement = "\\1",
                      x = colnames_compare)
years_compare <- years_compare[-length(years_compare)]
ind_list <- unique(years_compare)
# apply
purrr::walk(ind_list, save_countries)







# list all osmid and names availables -------------------------------------

indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL)




# create list with osmid --------------------------------------------------

count(indicators_all_df, hdc, country, osmid, name, admin_level, admin_level_ordered, admin_level_name) %>%
  dplyr::select(-n) %>% View()
  readr::write_rds("data/data_final/list_osmid_name.rds")



# create indicator availability for each city -----------------------------

# to long format
colnames_compare <- colnames(indicators_all_df)[9:ncol(indicators_all_df)]
# extract year
# years_compare <- gsub(pattern = "(.*)_(.*)_(\\d{4}$)",
#                       replacement = "\\2_\\3",
#                       x = colnames_compare)

# colnames(indicators_all_df) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", 
#                                  years_compare)

indicators_all_df <- indicators_all_df %>%
  mutate(across(city_popdensity_1975:transit_pnst_2024, as.numeric))

indicators_all_df_long <- tidyr::pivot_longer(indicators_all_df,
                                              cols = 9:last_col(),
                                              names_sep = "_",
                                              names_to = c("ind_type", "ind", "year"),
                                              values_to = "value")
  # remove year 2025 for now
  # filter(year != 2025)

a1 <- distinct(indicators_all_df_long, hdc, ind, year, .keep_all = TRUE) %>%
  filter(!is.na(value)) %>%
  group_by(country, name, hdc, ind_type, ind) %>%
  summarise(availability = paste0(year, collapse = "|")) %>%
  ungroup() %>%
  arrange(country, name)
  # create id
# ungroup() %>%
# group_by(hdc) %>%
# summarise(ind = first(ind))
# select(hdc, ind, year)

# add recife manually

readr::write_rds(a1, "data/data_final/list_availability.rds")

# not available
indicators_all_df_long1 <- indicators_all_df_long %>%
  filter(admin_level == 0) %>%
  filter(year == 2024) %>%
  # filter(ind  %in% c("journeygap", "pnft", "pnrtall")) %>%
  mutate(available = ifelse(is.na(value), FALSE, TRUE)) %>%
  select(country, name, hdc, ind, available) %>%
  arrange(country, name) %>%
  mutate(id = rleid(hdc)) %>%
  select(hdc, ind, available) %>%
  filter(ind %in% c("popdensity", "blockdensity", "journeygap", "pnpb", "pns",
                    "pncf", "pnnhighways", "pnft", "pnrt", "pnst"))


  


readr::write_rds(indicators_all_df_long1, "data/data_final/list_availability_cities.rds")
