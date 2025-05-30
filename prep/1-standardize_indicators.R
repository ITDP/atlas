library(sf)
library(dplyr)
# library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
sf::sf_use_s2(FALSE)
options(scipen = 999999)


# source folder -------------------------------------------------------------------------------

# folder <- "data-raw/data_final/"
# folder <- "data-raw/data_test_202412"
folder <- "/media/kauebraga/data/pedestriansfirst"


# # duplicate the pop data for 2022 as well -----------------------------------------------------
# list_pop_2020 <- dir(sprintf("%s/cities", folder), recursive = TRUE, pattern = "pop_2020.tif$", full.names = TRUE)
# # reanem to 2022
# list_pop_2022 <- stringr::str_replace(list_pop_2020, pattern = "2020(?=.tif$)", replacement = "2022")
# # copy
# purrr::walk2(list_pop_2020, list_pop_2022, file.copy)


# go
# base_dir <- sprintf("data-raw/data_sample_2022_08_19/city_results/ghsl_region_%s/", ghsl)
# data <- st_read(paste0(base_dir, "indicator_values.gpkg"))
world <- dir(sprintf("%s/cities_out", folder), full.names = TRUE, recursive = TRUE)
# world <- c(world, dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE))
world <- world[grepl("ghsl_region_\\d{5}/indicator_values_\\d{4}", world)]


ghsl_all <- fread('../pedestriansfirst/input_data/pbf/pbf_hdc_country.csv', colClasses = list(character = "hdc"))


# 1) function to rename the columns to an standard --------------------------------------------

rename_columns <- function(data) {
  
  data <- gsub(pattern = "(total_pop)_(\\d{4})",
               replacement = "city_popdensitytotal_\\2",
               x = data)
  data <- gsub(pattern = "^(density)_(\\d{4})",
               replacement = "city_popdensity_\\2",
               x = data)
  data <- gsub(pattern = "(block_density)_(\\d{4})",
               replacement = "city_blockdensity_\\2",
               x = data)
  data <- gsub(pattern = "(journey_gap)_(\\d{4})",
               replacement = "city_journeygap_\\2",
               x = data,
               perl = TRUE)
  
  
  data <- gsub(pattern = "(pnab)_(\\d{4})",
               replacement = "bike_pnpbpnab_\\2",
               x = data)
  data <- gsub(pattern = "(pnpb)_(\\d{4})",
               replacement = "bike_pnpb_\\2",
               x = data)
  data <- gsub(pattern = "(all_bikeways_km)_(\\d{4})",
               replacement = "bike_pnpbabikewayskm_\\2",
               x = data)
  data <- gsub(pattern = "(protected_bikeways_km)_(\\d{4})",
               replacement = "bike_pnpbpbikewayskm_\\2",
               x = data)
  data <- gsub(pattern = "(bikeshare)_(\\d{4})",
               replacement = "bike_bikeshare_\\2",
               x = data)
  
  
  data <- gsub(pattern = "(^healthcare)_(\\d{4})",
               replacement = "walk_pnspnh_\\2",
               x = data)
  data <- gsub(pattern = "(^n_points_healthcare)_(\\d{4})",
               replacement = "walk_pnshealthpoints_\\2",
               x = data)
  data <- gsub(pattern = "(^schools)_(\\d{4})",
               replacement = "walk_pnspne_\\2",
               x = data)
  data <- gsub(pattern = "(^n_points_schools)_(\\d{4})",
               replacement = "walk_pnsschoolspoints_\\2",
               x = data)
  data <- gsub(pattern = "(^hs)_(\\d{4})",
               replacement = "walk_pns_\\2",
               x = data)
  data <- gsub(pattern = "(^carfree)_(\\d{4})",
               replacement = "walk_pncf_\\2",
               x = data)
  data <- gsub(pattern = "(people_not_near_highways)_(\\d{4})",
               replacement = "walk_pnnhighways_\\2",
               x = data,
               perl = TRUE)
  data <- gsub(pattern = "(highway_km)_(\\d{4})",
               replacement = "walk_pnnhighwayskm_\\2",
               x = data,
               perl = TRUE)
  
  
  
  data <- gsub(pattern = "(pnft)_(\\d{4})",
               replacement = "transit_pnft_\\2",
               x = data)
  
  data <- gsub(pattern = "(n_points_transit_pnft)_(\\d{4})",
               replacement = "transit_pnftpoints_\\2",
               x = data)
  
  
  data <- gsub(pattern = "(PNrT)_(all)_(\\d{4})",
               replacement = "transit_\\L\\1_\\3",
               x = data,
               perl = TRUE)
  data <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
               replacement = "transit_\\L\\1\\E\\2_\\3",
               x = data,
               perl = TRUE)
  data <- gsub(pattern = "(km)_([[:lower:]]{3})_(\\d{4})",
               replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
               x = data,
               perl = TRUE)
  data <- gsub(pattern = "(rtr)_([[:lower:]]{3})_(\\d{4})",
               replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
               x = data,
               perl = TRUE)
  data <- gsub(pattern = "(stns)_([[:lower:]]{3})_(\\d{4})",
               replacement = "transit_pnrt\\L\\1\\E\\2_\\3",
               x = data,
               perl = TRUE)
  
  
  data <- gsub(pattern = "(pnst)_(\\d{4})",
               replacement = "transit_pnst_\\2",
               x = data)
  
}







# ghsl <- "08154"
prep_data <- function(ghsl) {
  
  files <- c(sprintf("%s/cities_out/ghsl_region_%s/indicator_values_2023.csv", folder, ghsl),
             sprintf("%s/cities_out/ghsl_region_%s/indicator_values_2024.gpkg", folder, ghsl))
  # file <- files[1]
  
  # fun to open all data
  open_data <- function(file) {
    
    
    year <- stringr::str_extract(file, "\\d{4}(?=(.csv|.gpkg))")
    
    a <- if(year == "2023") {
      
      a <- fread(file) 
      
    } else {
      a <- st_read(file)
      
    }
    
    
    # create admin level
    if (!("admin_level" %in% colnames(a))) {
      
      a$admin_level <- NA
    }
    if (!("level_name_eng" %in% colnames(a))) {
      
      a$level_name_eng <- NA
    }
    if (!("level_name_local" %in% colnames(a))) {
      
      a$level_name_local <- NA
    }
    if (!("level_name_full" %in% colnames(a))) {
      
      a$level_name_full <- NA
    }
    
    a <- a %>%
      dplyr::select(-starts_with("n_points_special")) %>%
      dplyr::select(-starts_with("performance")) %>%
      dplyr::select(-geospatial_calctime, -summary_calctime, -matches("V1"),
                    -matches("name_long"))
    
    a <- a %>% 
      mutate(across(c("name", "name_short", "level_name", "agglomeration_country_name", "country", 
                      "admin_level", "level_name_eng", "level_name_local", "level_name_full"),
                    ~ ifelse(.x == "", NA, .x)))
    
    
    a <- tidyr::fill(a, hdc)
    # a <- tidyr::fill(a, hdc)
    
    # extract country
    country1 <- unique(a$country)[!is.na(unique(a$country))][1]
    
    # fill the name of the country
    a <- a %>% mutate(country = country1)
    
    
    
    a <- a %>% mutate(osmid = as.character(osmid),
                      # level_name_eng = as.character(level_name_eng),
                      # level_name_local = as.character(level_name_local),
                      across(starts_with("block_density"), as.numeric),
                      across(starts_with("km"), as.numeric),
                      across(starts_with("rtr"), as.numeric)
    )
    
    a <- a %>% mutate(hdc = stringr::str_pad(hdc, width = 5, side = "left", pad = 0))
    
    
    
    # identify the ghsl number to the osmid
    a <- a %>% mutate(osmid = ifelse(is.na(osmid), hdc, osmid)) %>%
      mutate(admin_level = as.character(admin_level))
    # fill the name of the country
    a <- a %>% mutate(country = country1)
    
    # por enquanto, apagar o que esta com o nome NA
    a <- a %>% filter(!is.na(name))
    a <- a %>% filter(name != "")
    
    
    # levl name
    a <- a %>% mutate(level_name = ifelse(is.na(level_name), level_name_eng, level_name))
    # identify the ghsl level as 0
    a <- a %>% mutate(admin_level = ifelse(level_name == "Agglomeration", 0, 
                                           ifelse(level_name =="Brazilian Metro Areas", 1, admin_level)))
    a <- a %>% filter(level_name != "Brazilian Metro Areas")
    
    
    
    # # we had a problem to create the osmid, so we will create a fake one
    # a <- a %>%
    #   mutate(osmid = case_when(level_name == "Agglomeration" ~ NA, 
    #                            .default = 1:n()))
    
    # extract only the neccesary columns from the first years, since we will join them
    if (year == 2023) {
      
      a <- a %>%
        select(hdc, osmid, name, admin_level, ends_with(year))
    } else if (year == 2024) {
      
      a <- a %>%
        select(-ends_with("2023"))
      
    }
    
    a <- a %>% mutate(admin_level = as.character(admin_level))
    
    return(a)
    
  }
  
  data <- lapply(files, open_data)
  
  
  # join the datasets
  data <- reduce(data, left_join, 
                 by = c("hdc", "osmid", "name", "admin_level")) %>% st_sf(crs = 4326)
  # %>% st_sf(crs = 4326)
  
  attr(data, "agr") <- NULL
  
  # scale the admin_level - from 0 to max
  ordered_admin <- sort(as.numeric(unique(data$admin_level)))
  admin_level_oder <- data.frame(admin_level = as.character(ordered_admin), admin_level_ordered = seq(from = 1, to = length(ordered_admin)))
  data <- data %>% mutate(admin_level = as.character(admin_level))
  data <- left_join(data, admin_level_oder, by = "admin_level")
  
  # data <- data %>% mutate(admin_level = level_name)
  
  # organize columns
  data <- data %>% dplyr::select(hdc, a3 = country, osmid, name, admin_level, admin_level_ordered, admin_level_name = level_name, everything())
  
  
  # get available indicators for this city
  ind_columns <- colnames(data)[colnames(data) %nin% c("hdc", "a3", "osmid", "name", "admin_level", "admin_level_ordered", "admin_level_name", "geom")]
  
  
  # rename indicators that are divided by year
  ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                      replacement = "city_popdensitytotal_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "^(density)_(\\d{4})",
                      replacement = "city_popdensity_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(block_density)_(\\d{4})",
                      replacement = "city_blockdensity_\\2",
                      x = ind_columns)
  
  
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
  ind_columns <- gsub(pattern = "(^hs)_(\\d{4})",
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
  ind_columns <- gsub(pattern = "(stns)_([[:lower:]]{3})_(\\d{4})",
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
  
  # adjustments
  data <- data %>% 
    mutate(name = ifelse(name == "The Jerusalem area", "The West Jerusalem area", name)) %>%
    mutate(name = ifelse(name == "The Maale Adumim / Ramallah / Bethlehem area", "The East Jerusalem / Ramallah / Bethlehem area", name)) %>%
    mutate(a3 = ifelse(a3 == "PAL", "PSE", a3))
  
  
  # # bring country name
  data <- data %>%
    left_join(dplyr::select(maps::iso3166, a3, country = ISOname) %>% distinct(a3, .keep_all = TRUE), by = c("a3")) %>%
    tidyr::fill(a3, country) %>%
    ungroup()
  
  # select columns
  data <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name, everything())
  
  
  # rename indicators
  colnames(data) <- c("hdc", "country", "a3", "osmid", "name", "admin_level", "admin_level_ordered", "admin_level_name", ind_columns_new, "geom")
  
  # arrange data correctly
  data1 <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name,
                  # starts_with("city_poptotal"),
                  matches("city_popdensity(total)?_(1975|1980|1985|1990|1995|2000|2005|2010|2015|2020)"),
                  matches("city_popdensity(total)?_2023"),
                  matches("city_popdensity(total)?_(2024)"),
                  matches("city_popdensity(total)?_(2025)"),
                  starts_with("city_blockdensity"),
                  starts_with("city_journeygap"),
                  starts_with("bike_pnab"),
                  starts_with("bike_pnpb"),
                  starts_with("walk_pns"),
                  starts_with("walk_pncf"),
                  starts_with("walk_pnnhighways"),
                  starts_with("transit_pnft"),
                  matches("transit_pnrt(.*)?_(1975|1980|1985|1990|1995|2000|2005|2010|2015|2020)"),
                  matches("transit_pnrt(.*)?_2023"),
                  matches("transit_pnrt(.*)?_(2024)"),
                  matches("transit_pnrt(.*)?_(2025)"),
                  starts_with("transit_pnst")
    ) %>%
    mutate(across(starts_with("transit"), as.numeric))
  
  # transform some columns to char
  data$admin_level <- as.character(data$admin_level)
  data$admin_level_ordered <- as.character(data$admin_level_ordered)
  
  # transform some columns to numeric
  data <- data %>%
    mutate(across(starts_with("transit_pnft"), as.numeric)) %>%
    mutate(across(starts_with("city_journeygap"), as.numeric)) %>%
    arrange(as.numeric(admin_level))
  
  # # exception for recife - not sure if this is necessary anymore
  # if (ghsl == "01445") {
  #   
  #   data$country <- "Brazil"
  #   data$a3 <- "BRA"
  #   
  # }
  
  
  # create directory to store the file
  dir.create(sprintf("data/data_final/ghsl_%s", ghsl), recursive = TRUE)
  
  # save the file
  readr::write_rds(data, sprintf("data/data_final/ghsl_%s/indicators_%s.rds", ghsl, ghsl))
  
  return("ok")
  
}


# apply to all cities
cities_available <- unique(ghsl_all$hdc)
library(purrr)
library(furrr)
plan(multisession)
results <- furrr::future_map(cities_available, possibly(prep_data, otherwise = "erro"))

# prep_data("05472") # jakarta








# calculate the boundaries for the world view ---------------------
indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# # remove polygon and save the data
# indicators_all %>% st_set_geometry(NULL) %>% 
#   mutate(across(8:last_col(), round, 3)) %>%
#   # mutate(across(city_poptotal_1975:performance_bikep45_2019, round, 3)) %>%
#   readr::write_rds("data/sample5/all_indicators.rds")


# select the admin level NA - the ghsl level
indicators_ghsl <- indicators_all %>% filter(admin_level == 0) %>% st_sf(crs = 4326) %>% st_make_valid()
# centroids
indicators_ghsl_centroids <- st_centroid(indicators_ghsl)
# order by indicator and year

# save
readr::write_rds(indicators_ghsl_centroids, "data/data_final/atlas_city_markers.rds")





# country and regional values -----------------


atlas_country_2023 <- read.csv(sprintf("%s/countries_out/country_results_2023.csv", folder)) %>%
  rename(index = X) %>%
  mutate(region_type = "country") %>%
  select(index, name, region_type, ends_with("2023"))
regions_2023 <- read.csv(sprintf("%s/countries_out/region_results_2023.csv", folder)) %>%
  # rename(index = X) %>%
  tidyr::separate(X, into = c("region_type", "name"), sep = " / ") %>%
  mutate(region_type = trimws(region_type, "both"),
         name = trimws(name, "both")) %>%
  mutate(name = ifelse(region_type == "world", "world", name)) %>%
  # create index
  mutate(index = janitor::make_clean_names(name, allow_dupes = TRUE)) %>%
  select(index, name, region_type, ends_with("2023"))
# put them together
regions_all_2023 <- rbind(atlas_country_2023, regions_2023)


atlas_country_2024 <- read.csv(sprintf("%s/countries_out/country_results_2024.csv", folder)) %>%
  rename(index = X) %>%
  select(-ends_with("2023")) %>%
  mutate(region_type = "country") %>%
  select(index, name, region_type, everything())
regions_2024 <- read.csv(sprintf("%s/countries_out/region_results_2024.csv", folder)) %>%
  # rename(index = X) %>%
  tidyr::separate(X, into = c("region_type", "name"), sep = " / ") %>%
  mutate(region_type = trimws(region_type, "both"),
         name = trimws(name, "both")) %>%
  mutate(name = ifelse(region_type == "world", "world", name)) %>%
  # create index
  mutate(index = janitor::make_clean_names(name, allow_dupes = TRUE)) %>%
  select( -ends_with("2023")) %>%
  select(index, name, region_type, everything())
# put them together
regions_all_2024 <- rbind(atlas_country_2024, regions_2024)


# atlas_country_2024 <- read.csv(sprintf("%s/countries_out/country_results_2024.csv", folder)) %>%
#   rename(index = X) %>%
#   select(-ends_with("2023"))
# atlas_country <- left_join(atlas_country_2023, atlas_country_2024, by  = "index")
# bring everything to the same dataset
regions_all <- left_join(regions_all_2023, regions_all_2024 %>% select(-name), by  = c("index", "region_type"))

# order by year?


# bring the geometries
regions_shapes <- st_read("../pedestriansfirst/input_data/regions/regions_all.gpkg") %>%
  select(index = region_code, region_type) %>%
  # simplify
  rmapshaper::ms_simplify(., keep = 0.05) %>%
  st_cast("MULTIPOLYGON")

regions_all <- left_join(regions_all, regions_shapes, by = c("index", "region_type")) %>% st_sf(crs = 4326)

# # bring the names
regions_all <- regions_all %>%
  #   filter(index != "-99") %>%
  #   left_join(
  #     countrycode::codelist %>% dplyr::select(country.name.en, iso3c),
  #     
  #     by = c("index" = "iso3c")) %>%
  #   # select(-name) %>%
  #   # rename(name = country.name.en) %>%
  select(a3 = index, name, region_type, everything())

# rename indicators
# get available indicators for this city
ind_columns <- colnames(regions_all)[colnames(regions_all) %nin% c("a3", "name", "region_type",  "geom")]


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
ind_columns <- gsub(pattern = "(^hs)_(\\d{4})",
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
ind_columns <- gsub(pattern = "(stns)_([[:lower:]]{3})_(\\d{4})",
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


colnames(regions_all) <- c("a3", "name", "region_type",  ind_columns_new, "geom")

# arrange data correctly
regions_all <- regions_all %>%
  dplyr::select(a3, name, region_type,
                matches("city_popdensity(total)?_(1975|1980|1985|1990|1995|2000|2005|2010|2015|2020)"),
                matches("city_popdensity(total)?_2023"),
                matches("city_popdensity(total)?_(2024)"),
                matches("city_popdensity(total)?_(2025)"),
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

regions_all <- regions_all %>% mutate(across(city_popdensity_1975:transit_pnst_2024, as.numeric))

# select only the indicators that we have in the indicators
regions_all <- regions_all %>%
  dplyr::select(a3, name, region_type,
                any_of(colnames(indicators_all))   
  ) %>%
  # round indicators
  mutate(across(c(bike_pnpbabikewayskm_2023, bike_pnpbpbikewayskm_2023, bike_pnpbabikewayskm_2024, bike_pnpbpbikewayskm_2024,
                  walk_pnshealthpoints_2023, walk_pnsschoolspoints_2023, walk_pnshealthpoints_2024, walk_pnsschoolspoints_2024,
                  transit_pnftpoints_2023, transit_pnftpoints_2024), round))

# save by indicator
# ind <- "transit_pnrt"
save_countries <- function(ind) {
  
  
  message("saving ", ind)
  
  # add a global avaregae for each indicator
  world_average <- regions_all %>%
    st_set_geometry(NULL) %>%
    select(city_popdensitytotal_2023,starts_with(ind)) %>%
    mutate(a3 = "WRD", name = "The World", region_type = "world") %>%
    group_by(a3, name, region_type) %>%
    summarise(
      across(matches("city_popdensity_|bike_pnpb_|bike_pnpbpnab_|city_blockdensity_|transit_pnft_|transit_pnrt_|transit_pnrtbrt_|transit_pnrtbrt_|transit_pnrtmrt_|transit_pnst_|transit_pnrtlrt_|walk_pncf_|walk_pnnhighways_|walk_pns_|walk_pnspne_|walk_pnspnh_"),
             ~weighted.mean(.x, w = city_popdensitytotal_2023, na.rm = TRUE)),
      across(matches("city_popdensitytotal|bike_pnpbabikewayskm|bike_pnpbpbikewayskm|walk_pnshealthpoints|walk_pnsschoolspoints|walk_pnnhighwayskm|transit_pnftpoints|transit_pnrtkmbrt|transit_pnrtkmlrt|transit_pnrtkmmrt|transit_pnrtkmall|transit_pnrtrtr|transit_pnrtstnsall|transit_pnrtstnslrt|transit_pnrtstnsbrt|transit_pnrtstnsmrt"), 
             ~sum(.x, na.rm = TRUE))) %>%
    ungroup()
  
  
  if (!(ind %in% c("city_popdensity", "city_popdensitytotal"))) {
    
    world_average <- world_average %>% select(-city_popdensitytotal_2023)
    
  }
  
  
  world_average <- st_sf(world_average, geom = lapply(1:nrow(world_average), function(x) st_multipolygon()), crs = 4326)
  
  regions_all_ind <- regions_all %>% 
    dplyr::select(a3, name, region_type,
                  starts_with(ind)) %>%
    rbind(world_average)
  
  # # filter for no data
  # if (ind == "city_poptotal") {
  #   
  #   atlas_country_ind <- atlas_country_ind %>% mutate(across(starts_with("city_poptotal"), ~ifelse(.x == 0, NA, .x)))
  #   
  # }
  
  # drop all NA
  regions_all_ind <- tidyr::drop_na(regions_all_ind)
  
  # save
  if (nrow(regions_all_ind) > 0) {
    
    
    readr::write_rds(regions_all_ind, sprintf("data/data_final/countries/atlas_regions_%s.rds",
                                              ind))
    
  }
}


# to long format
colnames_compare <- colnames(regions_all)[4:ncol(regions_all)]
# extract year
years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                      replacement = "\\1",
                      x = colnames_compare)
# remove geom string
years_compare <- years_compare[-length(years_compare)]
ind_list <- unique(years_compare)
# apply
dir.create("data/data_final/countries")
purrr::walk(ind_list, save_countries)







# list all osmid and names availables -------------------------------------

indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL)




# create list with osmid --------------------------------------------------

count(indicators_all_df, hdc, country, osmid, name, admin_level, admin_level_ordered, admin_level_name) %>%
  dplyr::select(-n) %>% 
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
  mutate(across(city_popdensitytotal_1975:transit_pnst_2023, as.numeric))

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
  filter(year == 2023) %>%
  # filter(ind  %in% c("journeygap", "pnft", "pnrtall")) %>%
  mutate(available = ifelse(is.na(value), FALSE, TRUE)) %>%
  select(country, name, hdc, ind, available) %>%
  arrange(country, name) %>%
  mutate(id = rleid(hdc)) %>%
  select(hdc, ind, available) %>%
  filter(ind %in% c("popdensity", "blockdensity", "journeygap", "pnpb", "pns",
                    "pncf", "pnnhighways", "pnft", "pnrt", "pnst"))





readr::write_rds(indicators_all_df_long1, "data/data_final/list_availability_cities.rds")
