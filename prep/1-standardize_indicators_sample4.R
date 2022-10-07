library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
sf::sf_use_s2(FALSE)




# go
data <- st_read(paste0(base_dir, "indicator_values.gpkg"))
world <- dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE)
world <- world[grepl("ghsl_region_\\d{4}/indicator_values.gpkg", world)]
# fun to open all data
open_data <- function(file) {
  
  a <- st_read(file)
  a <- st_cast(a, "MULTIPOLYGON")
  a <- tidyr::fill(a, hdc)
  
}
data_all <- purrr::map_dfr(world, open_data)
# ghsl to 4 characters
data_all <- data_all %>% mutate(hdc = stringr::str_pad(hdc, width = 4, side = "left", pad = 0))


# remove indicators that we wont use
data_all <- data_all %>%
  select(-starts_with("rtr_")) %>%
  select(-starts_with("stns_")) %>%
  select(-starts_with("km_")) %>%
  select(-performance_walk_30, -performance_walk_60, -performance_bike_lts1_30, -performance_bike_lts1_45,
         -performance_bike_lts1_60, -performance_bike_lts2_30, -performance_bike_lts2_60) %>%
  select(-density) %>%
  select(-blockmean_density) %>%
  select(-recording_time)





prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0561"
  # ghsl <- "0088"
  # ghsl <- "0634"
  # ghsl <- 0014
  
  base_dir <- sprintf("data-raw/sample_3/ghsl_region_%s/", ghsl)
  
  # filter only city
  data <- data_all %>% filter(hdc == ghsl) %>% st_sf(crs = 4326)
  
  
  
  # identify the ghsl level as 0
  data <- data %>% mutate(admin_level = ifelse(is.na(osmid), 0, admin_level))
  # identify the ghsl number to the osmid
  data <- data %>% mutate(osmid = ifelse(is.na(osmid), hdc, osmid))
  # it may be necessary to scale the admin_level - from 0 to max
  ordered_admin <- sort(as.numeric(unique(data$admin_level)))
  admin_level_oder <- data.frame(admin_level = as.character(ordered_admin), admin_level_ordered = seq(from = 1, to = length(ordered_admin)))
  data <- left_join(data, admin_level_oder, by = "admin_level")
  
  # organize columns
  data <- data %>% select(hdc, osmid, name, admin_level, admin_level_ordered, everything())
  
  
  # table(data$admin_level)
  # 
  # data %>% filter(is.na(admin_level)) %>% mapview() + filter(data, admin_level == 6) + filter(data, admin_level == 8)
  # data %>% filter(admin_level == 6) %>% mapview()
  # data %>% filter(admin_level == 8) %>% mapview()
  
  # get available indicators for this city
  ind_columns <- colnames(data)[colnames(data) %nin% c("hdc", "osmid", "name", "admin_level", "admin_level_ordered", "geom")]
  
  
  # first, rename indicators that are divided by year
  ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                      replacement = "city_poptotal_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(density)_(\\d{4})",
                      replacement = "city_density_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
                      replacement = "transit_\\L\\1\\E\\2_\\3",
                      x = ind_columns,
                      perl = TRUE)
  
  
  # create new column names with new standardized id
  ind_columns_new <- fcase(
    startsWith(ind_columns, "city_poptotal"), ind_columns,
    startsWith(ind_columns, "city_density"), ind_columns,
    startsWith(ind_columns, "transit_pnrt"), ind_columns,
    ind_columns == "pnab",                  "bike_pnab_2019",
    ind_columns == "pnpb",                  "bike_pnpb_2019",
    ind_columns == "all_bikeways_km",       "bike_abikeways_2019",
    ind_columns == "protected_bikeways_km", "bike_pbikeways_2019",
    ind_columns == "healthcare",            "walk_pnh_2019",
    ind_columns == "schools",               "walk_pne_2019",
    ind_columns == "h.s",                   "walk_pns_2019",
    ind_columns == "carfree",               "walk_pncf_2019",
    
    ind_columns == "performance_bike_lts2_30",               "performance_bikep30_2019",
    ind_columns == "performance_bike_lts2_45",               "performance_bikep45_2019",
    ind_columns == "performance_bike_lts2_60",               "performance_bikep60_2019",
    ind_columns == "performance_walk_30",               "performance_walkp30_2019",
    ind_columns == "performance_walk_45",               "performance_walkp45_2019",
    ind_columns == "performance_walk_60",               "performance_walkp60_2019"
    
  )
  
  # bring country name
  data <- data %>%
    # select(-geom) %>%
    # filter(admin_level == 0) %>%
    mutate(a3 = stringr::str_extract(name, "\\[[:upper:]{3}\\]")) %>%
    mutate(a3 = gsub(pattern = "\\[|\\]", "", a3)) %>%
    # bring a2
    left_join(select(maps::iso3166, a2, a3, country = ISOname), by = c("a3")) %>%
    tidyr::fill(a2, a3, country) %>%
    ungroup()
  
  # select columns
  data <- data %>%
    select(hdc, country, a2, osmid, name, admin_level, admin_level_ordered, everything(), -a3)
  
  # rename indicators
  colnames(data) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", ind_columns_new,"geom")
  
  
  
  # simplify data
  data <- st_make_valid(data)
  data <- st_simplify(data)
  # to polygons
  data <- st_cast(data, "MULTIPOLYGON")
  
  
  
 
  
  dir.create(sprintf("data/sample3/ghsl_%s", ghsl), recursive = TRUE)
  
  readr::write_rds(data,             sprintf("data/sample3/ghsl_%s/indicators_%s.rds", ghsl, ghsl))

  # if (nrow(overlay_lines) > 0) readr::write_rds(overlay_lines, sprintf("data/sample2_prep/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  
}


purrr::walk(c("0088", "0200", "0561", "0621", "1406", "1445",
              "0014", "0154", "0634"), 
            prep_data)


prep_data("0088")
prep_data("0200")
prep_data("0561")
prep_data("0621")
prep_data("1406")
prep_data("1445")
# prep_data("0014")
prep_data("0154")
prep_data("0634")




# prep overlays -----------------------------------------------------------

# ghsl <- "0014"
prep_overlays <- function(ghsl) {
  
  
  base_dir <- sprintf("data-raw/sample_3/ghsl_region_%s/", ghsl)
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = ".geojson$", recursive = TRUE)
  
  # filter only our overlays
  overlay_files1 <- overlay_files[overlay_files %like% c("allbike_latlon|h\\+slatlon|healthcarelatlon|pnablatlon|pnpblatlon|protectedbike|schools")]
  
  # the overlay files only have the shape geom, so we need to give them names based on the filename
  # read overlays
  open_overlay <- function(file) {
    # file <- overlay_files[[1]]
    
    a <- st_read(file)
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- a %>% mutate(year = 2019)
    
  }
  
  overlay <- lapply(overlay_files1, open_overlay)
  
  
  # open rapid_transit when available
  overlay_files_rapid <- overlay_files[overlay_files %like% c("rapid_transit")]
  overlay_files_rapid <- overlay_files_rapid[overlay_files_rapid %like% c("isochrones")]
  
  # fun
  open_overlay_rapid <- function(file) {
    # file <- overlay_files_rapid[[1]]
    
    # extract year
    year <- sub("(^.*)/(rapid_transit/)(\\d{4})/(.*$)", "\\3", file)
    
    a <- st_read(file)
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- a %>% mutate(year = year)
    
  }
  
  
  if (length(overlay_files_rapid) > 0) {
    
  overlay_rapid <- lapply(overlay_files_rapid, open_overlay_rapid)
  
  overlay <- c(overlay, overlay_rapid)
    
  }
  
  
  
  # identify geom type
  names(overlay) <- purrr::map_chr(overlay, function(x) as.character(st_geometry_type(x)))
  # separate between polygons and lines
  overlay_polygons <- overlay[grep("MULTIPOLYGON", names(overlay))] %>% rbindlist()
  overlay_lines <- overlay[grep("MULTILINESTRING", names(overlay))] %>% rbindlist()
  
  
  # juntar objeto dos overlays sem a geom
  overlay_df <- rbind(overlay_polygons %>% select(-geometry), overlay_lines %>% select(-geometry)) %>% select(ind, geom_type, year)
  
  
  # create aux table with the overlay ids
  overlay_id <- tibble::tribble( 
    
    ~ind, ~indicator,
    "pnpblatlon.geojson",            "bike_pnpb",
    "pnablatlon.geojson",            "bike_pnab",
    "allbike_latlon.geojson",        "bike_abikeways",
    "protectedbike_latlon.geojson",  "bike_pbikeways",
    "healthcarelatlon.geojson",      "walk_pnh",
    "schoolslatlon.geojson",         "walk_pne",
    "h+slatlon.geojson",             "walk_pns",
    "carfreelatlon.geojson",         "walk_cf",
    "all_isochrones_ll.geojson",     "transit_pnrtall",
    "lrt_isochrones_ll.geojson",     "transit_pnrtlrt",
    "mrt_isochrones_ll.geojson",     "transit_pnrtmrt",
    "brt_isochrones_ll.geojson",     "transit_pnrtbrt"
    
  )
  
  # bring indicator id to the overlays
  overlay_polygons <- left_join(overlay_polygons, overlay_id, by = "ind") %>% select(-ind) %>% 
    mutate(indicator = paste0(indicator, "_", year)) %>%
    # select(-year) %>%
    st_sf(crs = 4326)
  if (nrow(overlay_lines) > 0) {
    overlay_lines <- left_join(overlay_lines, overlay_id, by = "ind") %>% select(-ind) %>% 
      mutate(indicator = paste0(indicator, "_", year)) %>%
    # select(-year) %>%
      st_sf(crs = 4326)
    overlay_lines <- st_simplify(overlay_lines)
    # overlay_lines1 <- st_cast(overlay_lines, "LINESTRING")
    
  } 
  
  # simplify data
  overlay_polygons <- st_simplify(overlay_polygons)
  # to polygons
  # overlay_polygons1 <- st_cast(overlay_polygons, "POLYGON")
  
  # overlay_lines1 <- sfheaders::sf_cast(overlay_lines, to = "LINESTRING")
  # a <- st_union(overlay_lines1)
  # b <- overlay_lines1[1:2,]
  
  overlay_df <- left_join(overlay_df, overlay_id, by = "ind") %>% 
    mutate(indicator = paste0(indicator, "_", year)) %>%
    select(-ind)
  
  # we should overlays for all available indicators, so we need to check that
  overlay_missing <- setdiff(colnames(data)[8:(length(colnames(data)) - 1)], overlay_df$indicator)
  
  nrows <- length(overlay_missing)
  overlay_missing_polygons <- st_sf(geom_type = rep("MULTIPOLYGON", nrows),
                                    indicator = overlay_missing,
                                    geometry = st_sfc(lapply(1:nrows, function(x) st_multipolygon())),
                                    crs = 4326)
  overlay_missing_df <- data.frame(geom_type = rep("MULTIPOLYGON", nrows),
                                   indicator = overlay_missing)
  
  
  # join
  overlay_polygons <- rbind(overlay_polygons, overlay_missing_polygons)
  overlay_df <- rbind(overlay_df, overlay_missing_df)
  
  # save by year
  
  save_by_year <- function(year) {
    
    
    
  }
  
  
  readr::write_rds(overlay_df,       sprintf("data/sample3/ghsl_%s/overlays_%s.rds", ghsl, ghsl))
  readr::write_rds(overlay_polygons, sprintf("data/sample3/ghsl_%s/overlays_polygons_%s.rds", ghsl, ghsl))
  readr::write_rds(overlay_lines,    sprintf("data/sample3/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  
}








# calculate boundaries for the world view ---------------------
indicators_all <- purrr::map_dfr(dir("data/sample3", pattern = "^indicators_", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon and save the data
indicators_all %>% st_set_geometry(NULL) %>% 
  mutate(across(city_poptotal_1975:performance_bikep45_2019, round, 3)) %>%
  readr::write_rds("data/sample3/all_indicators/all_indicators.rds")


# select the admin level NA - the ghsl level
indicators_ghsl <- indicators_all %>% filter(admin_level == 0) %>% st_sf(crs = 4326)
# centroids
indicators_ghsl_centroids <- st_centroid(indicators_ghsl)
# save
readr::write_rds(indicators_ghsl_centroids, "data/sample3/atlas_city_markers.rds")





# calculate mean for each country -----------------

# extract the country
atlas_country <- indicators_all %>%
  st_set_geometry(NULL) %>%
  # select(-geom) %>%
  filter(admin_level == 0) %>%
  group_by(a2) %>%
  summarise(across(city_poptotal_1975:last_col(), mean, na.rm = TRUE)) %>%
  ungroup()

# bring the shapes
atlas_country <- atlas_country %>%
  left_join(select(spData::world, iso_a2, name_long), by = c("a2" = "iso_a2")) %>%
  st_sf(crs = 4326) %>%
  select(a2, name_long, everything())

# save
readr::write_rds(atlas_country, "data/sample3/atlas_country_polygons.rds")





# save indicators by each city by each admin level - for comparison --------

indicators_all <- purrr::map_dfr(dir("data/sample3", pattern = "^indicators_\\d{4}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL)

# ghsl <- "0634"
# ghsl <- "1406"

export_by_osmid <- function(ghsl) {
  
  indicators <- indicators_all_df %>% filter(hdc == ghsl)
  
  # save by each admin
  # ind <- "city_poptotal"
  # ind <- "bike_pnpb"
  save_ind <- function(ind) {
    
    indicators_ind <- indicators %>% 
      select(hdc, country, a2, osmid, name, admin_level, admin_level_ordered,
             starts_with(ind))
    # to long format
    colnames_compare <- colnames(indicators_ind)[8:ncol(indicators_ind)]
    # extract year
    years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                          replacement = "\\2",
                          x = colnames_compare)
    
    colnames(indicators_ind) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", 
                                  years_compare)
    
    indicators_ind <- tidyr::pivot_longer(indicators_ind,
                                          cols = matches("\\d{4}"),
                                          names_to = "year",
                                          values_to = "value")
    
    dir.create(sprintf("data/sample3/ghsl_%s/indicators_compare",
                       ghsl))
    
    # save
    readr::write_rds(indicators_ind, sprintf("data/sample3/ghsl_%s/indicators_compare/indicators_compare_%s_%s.rds",
                                             ghsl, ghsl, ind))
    
    
  }
  # to long format
  colnames_compare <- colnames(indicators)[8:ncol(indicators)]
  # extract year
  years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                        replacement = "\\1",
                        x = colnames_compare)
  ind_list <- unique(years_compare)
  # apply
  purrr::walk(ind_list, save_ind)
  
}

export_by_osmid("0088")
export_by_osmid("0200")
export_by_osmid("0561")
export_by_osmid("0621")
export_by_osmid("1406")
export_by_osmid("1445")
# prep_data("0014")
export_by_osmid("0154")
export_by_osmid("0634")


# export only for comparison
# ghsl <- "1406"
# ind <- "bike_pnpb"

export_comparison1 <- function(level) {
  
  
  # filter levels
  indicators_all_level <- indicators_all_df %>%
    filter(admin_level  == level)
  
  export_comparison <- function(ind) {
    
    
    indicators_ind <- indicators_all_level %>% 
      select(hdc, country, a2, osmid, name, admin_level, admin_level_ordered,
             starts_with(ind)) %>%
      mutate(admin_level = as.integer(admin_level)) %>%
      # delete the last level
      filter(admin_level < 10)
    
    
    # to long format
    colnames_compare <- colnames(indicators_ind)[8:ncol(indicators_ind)]
    # extract year
    years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                          replacement = "\\2",
                          x = colnames_compare)
    
    colnames(indicators_ind) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", 
                                  years_compare)
    
    indicators_ind <- tidyr::pivot_longer(indicators_ind,
                                          cols = matches("\\d{4}"),
                                          names_to = "year",
                                          values_to = "value")
    
    # save
    readr::write_rds(indicators_ind, sprintf("data/sample3/comp/indicators_compare_%s_%s.rds",
                                             level, ind))
    
    
  }
  
  # to long format
  colnames_compare <- colnames(indicators_all_level)[8:ncol(indicators_all_level)]
  # extract year
  years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                        replacement = "\\1",
                        x = colnames_compare)
  ind_list <- unique(years_compare)
  # apply
  purrr::walk(ind_list, export_comparison)
  
}


# filter level
levels <- unique(indicators_all_df)
purrr::walk(unique(indicators_all_df$admin_level), export_comparison1)


# list all osmid and names availables -------------------------------------

indicators_all <- purrr::map_dfr(dir("data/sample3", pattern = "^indicators_\\d{4}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL)




# create list with osmid --------------------------------------------------

count(indicators_all_df, hdc, country, osmid, name, admin_level, admin_level_ordered) %>%
  select(-n) %>%
  readr::write_rds("data/sample3/list_osmid_name.rds")



# create indicator availability for each city -----------------------------

# to long format
colnames_compare <- colnames(indicators_all_df)[8:ncol(indicators_all_df)]
# extract year
years_compare <- gsub(pattern = "(.*)_(.*)_(\\d{4}$)",
                      replacement = "\\2",
                      x = colnames_compare)

colnames(indicators_all_df) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", 
                                 years_compare)

indicators_all_df_long <- tidyr::pivot_longer(indicators_all_df,
                                              cols = 8:last_col(),
                                              names_to = "year",
                                              values_to = "value")

a1 <- distinct(indicators_all_df_long, hdc, year, .keep_all = TRUE) %>%
  filter(!is.na(value)) %>%
  group_by(hdc) %>%
  mutate(ind = paste0(year, collapse = "|")) %>%
  ungroup() %>%
  group_by(hdc) %>%
  summarise(ind = first(ind))

readr::write_rds(a1, "data/sample3/list_availability.rds")

