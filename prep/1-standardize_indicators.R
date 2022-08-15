library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)


prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0561"
  # ghsl <- "0088"
  
  base_dir <- sprintf("data-raw/sample_3/ghsl_region_%s/", ghsl)
  
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
  data_all <- lapply(world, open_data) %>% rbindlist(fill = TRUE)
  # ghsl to 4 characters
  data_all <- data_all %>% mutate(hdc = stringr::str_pad(hdc, width = 4, side = "left", pad = 0))
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
  
  # create new column names with new standardized id
  ind_columns_new <- fcase(
    ind_columns == "pnab",                  "bike_pnab_2019",
    ind_columns == "pnpb",                  "bike_pnpb_2019",
    ind_columns == "all_bikeways_km",       "bike_abikeways_2019",
    ind_columns == "protected_bikeways_km", "bike_pbikeways_2019",
    ind_columns == "healthcare",            "walk_pnh_2019",
    ind_columns == "schools",               "walk_pne_2019",
    ind_columns == "h.s",                   "walk_pns_2019",
    ind_columns == "carfree",               "walk_pncf_2019",
    ind_columns == "total_pop",             "city_poptotal_2019",
    ind_columns == "density",               "city_density_2019",
    
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
  data <- st_simplify(data)
  # to polygons
  # data <- st_cast(data, "POLYGON")
  
  
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = ".geojson$")
  # overlay_files <- dir("data/sample_2", full.names = TRUE, recursive = TRUE)
  # overlay_files <- overlay_files[grepl("ghsl_region_\\d{4}/geodata", overlay_files)]
  # overlay_files <- overlay_files[grepl(".geojson$", overlay_files)]
  
  # the overlay files only have the shape geom, so we need to give them names based on the filename
  # read overlays
  open_overlay <- function(file) {
    # file <- overlay_files[[1]]
    
    a <- st_read(file)
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    
  }
  
  overlay <- lapply(overlay_files, open_overlay)
  
  # identify geom type
  names(overlay) <- purrr::map_chr(overlay, function(x) as.character(st_geometry_type(x)))
  # separate between polygons and lines
  overlay_polygons <- overlay[grep("MULTIPOLYGON", names(overlay))] %>% rbindlist()
  overlay_lines <- overlay[grep("MULTILINESTRING", names(overlay))] %>% rbindlist()
  

  
  
  # juntar objeto dos overlays sem a geom
  overlay_df <- lapply(overlay_files, function(file) st_set_geometry(open_overlay(file), NULL)) %>% rbindlist()
  
  
  # create aux table with the overlay ids
  overlay_id <- tibble::tribble( 
    
    ~ind, ~indicator,
    "pnpblatlon.geojson",            "bike_pnpb_2019",
    "pnablatlon.geojson",            "bike_pnab_2019",
    "allbike_latlon.geojson",        "bike_abikeways_2019",
    "protectedbike_latlon.geojson",  "bike_pbikeways_2019",
    "healthcarelatlon.geojson",      "walk_pnh_2019",
    "schoolslatlon.geojson",         "walk_pne_2019",
    "h+slatlon.geojson",             "walk_pns_2019",
    "carfreelatlon.geojson",         "walk_cf_2019",
    
  )
  
  # bring indicator id to the overlays
  overlay_polygons <- left_join(overlay_polygons, overlay_id, by = "ind") %>% select(-ind) %>% st_sf(crs = 4326)
  if (nrow(overlay_lines) > 0) {
    overlay_lines <- left_join(overlay_lines, overlay_id, by = "ind") %>% select(-ind) %>% st_sf(crs = 4326)
    overlay_lines <- st_simplify(overlay_lines)
  } 
  
  # simplify data
  overlay_polygons <- st_simplify(overlay_polygons)
  # to polygons
  # overlay_polygons <- st_cast(overlay_polygons, "POLYGON")
  
  
  overlay_df <- left_join(overlay_df, overlay_id, by = "ind") %>% select(-ind)
  
  # we should overlays for all available indicators, so we need to check that
  overlay_missing <- setdiff(colnames(data)[6:(length(colnames(data)) - 1)], overlay_df$indicator)
  
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
  
  # data <- data %>%
  #   select(name, hdc, osmid, admin_level, 
  #          total_pop, 
  #          walk_healthcare_2019 = healthcare,
  #          walk_schools_2019 = schools,
  #          walk_hs_2019 = h.s,
  #          bike_pnab_2019 = pnab,
  #          bike_pnpb_2019 = pnpb
  #   )
  
  dir.create(sprintf("data/sample3/ghsl_%s", ghsl), recursive = TRUE)
  
  readr::write_rds(data,             sprintf("data/sample3/ghsl_%s/indicators_%s.rds", ghsl, ghsl))
  readr::write_rds(overlay_df,       sprintf("data/sample3/ghsl_%s/overlays_%s.rds", ghsl, ghsl))
  readr::write_rds(overlay_polygons, sprintf("data/sample3/ghsl_%s/overlays_polygons_%s.rds", ghsl, ghsl))
  readr::write_rds(overlay_lines,    sprintf("data/sample3/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  # if (nrow(overlay_lines) > 0) readr::write_rds(overlay_lines, sprintf("data/sample2_prep/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  
}


purrr::walk(c("0088", "0200", "0561", "0621", "1406", "1445"), prep_data)








# calculate boundaries for the world view ---------------------
indicators_all <- lapply(dir("data/sample3", pattern = "^indicators_", full.names = TRUE, recursive = TRUE),
                         readr::read_rds) %>%
  rbindlist(fill = TRUE)

# select the admin level NA - the ghsl level
indicators_ghsl <- indicators_all %>% filter(admin_level == 0) %>% st_sf(crs = 4326)
# centroids
indicators_ghsl_centroids <- st_centroid(indicators_ghsl)
# save
readr::write_rds(indicators_ghsl_centroids, "data/sample3/atlas_city_markers.rds")





# calculate mean for each country -----------------

# extract the country
atlas_country <- indicators_all %>%
  select(-geom) %>%
  filter(admin_level == 0) %>%
  group_by(a2) %>%
  summarise(across(walk_pnh_2019:last_col(), mean, na.rm = TRUE)) %>%
  ungroup()

# bring the shapes
atlas_country <- atlas_country %>%
  left_join(select(spData::world, iso_a2, name_long), by = c("a2" = "iso_a2")) %>%
  st_sf(crs = 4326) %>%
  select(a2, name_long, everything())

# save
readr::write_rds(atlas_country, "data/sample3/atlas_country_polygons.rds")


