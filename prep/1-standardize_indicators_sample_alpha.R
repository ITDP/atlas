library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
sf::sf_use_s2(FALSE)



# duplicate the pop data for 2022 as well -----------------------------------------------------
list_pop_2020 <- dir("data-raw/data_alpha/city_results", recursive = TRUE, pattern = "pop_2020.tif$", full.names = TRUE)
# reanem to 2022
list_pop_2022 <- stringr::str_replace(list_pop_2020, pattern = "2020(?=.tif$)", replacement = "2022")
# copy
purrr::walk2(list_pop_2020, list_pop_2022, file.copy)



# rename folder to 5 chars ------------------------------------------------
list_folders <- dir("data-raw/data_alpha/city_results", full.names = TRUE)
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
world <- dir("data-raw/data_alpha/city_results", full.names = TRUE, recursive = TRUE)
# world <- c(world, dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE))
world <- world[grepl("ghsl_region_\\d{5}/indicator_values.gpkg$", world)]
# fun to open all data
open_data <- function(file) {
  
  
  # file <- world[37]
  
  a <- st_read(file)
  a <- st_cast(a, "MULTIPOLYGON")
  a <- tidyr::fill(a, hdc)
  # a <- tidyr::fill(a, hdc)
  a <- a %>% mutate(osmid = as.character(osmid),
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
data_all <- purrr::map_dfr(world, open_data)
# ghsl to 5 characters
data_all <- data_all %>% mutate(hdc = stringr::str_pad(hdc, width = 5, side = "left", pad = 0))


# remove indicators that we wont use
data_all <- data_all %>%
  dplyr::select(-starts_with("rtr_")) %>%
  dplyr::select(-starts_with("stns_")) %>%
  dplyr::select(-starts_with("km_")) %>%
  dplyr::select(-starts_with("n_points_special")) %>%
  dplyr::select(-starts_with("performance")) %>%
  # select(-performance_walk_30, -performance_walk_60, -performance_bike_lts1_30, -performance_bike_lts1_45,
  # -performance_bike_lts1_60, -performance_bike_lts2_30, -performance_bike_lts2_60) %>%
  # select(-density) %>%
  # select(-blockmean_density) %>%
  dplyr::select(-recording_time)





prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- 1406
  # ghsl <- 1022
  # ghsl <- "0561"
  # ghsl <- "0088"
  # ghsl <- "0634"
  # ghsl <- "0014"
  # ghsl <- "01406"
  # ghsl <- "01445" # rec
  # ghsl <- "00021"
  # ghsl <- "12080"
  # ghsl <- "00154"
  
  # base_dir <- sprintf("data-raw/sample_3/ghsl_region_%s/", ghsl)
  
  # filter only city
  data <- data_all %>% filter(hdc == ghsl) %>% st_sf(crs = 4326)
  
  
  
  # identify the ghsl level as 0
  data <- data %>% mutate(admin_level = ifelse(is.na(osmid), 0, admin_level))
  # identify the ghsl number to the osmid
  data <- data %>% mutate(osmid = ifelse(is.na(osmid), hdc, osmid))
  # fill the name of the country
  data <- data %>% mutate(country = unique(data$country)[!is.na(unique(data$country))][1])
  # por enquanto, apagar o que esta com o nome NA
  data <- data %>% filter(!is.na(name))
  # it may be necessary to scale the admin_level - from 0 to max
  ordered_admin <- sort(as.numeric(unique(data$admin_level)))
  admin_level_oder <- data.frame(admin_level = as.character(ordered_admin), admin_level_ordered = seq(from = 1, to = length(ordered_admin)))
  data <- data %>% mutate(admin_level = as.character(admin_level))
  data <- left_join(data, admin_level_oder, by = "admin_level")
  
  # organize columns
  data <- data %>% dplyr::select(hdc, a3 = country, osmid, name, admin_level, admin_level_ordered, everything())
  
  
  # get available indicators for this city
  ind_columns <- colnames(data)[colnames(data) %nin% c("hdc", "a3", "osmid", "name", "admin_level", "admin_level_ordered", "geom")]
  
  
  # first, rename indicators that are divided by year
  ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                      replacement = "city_poptotal_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(density)_(\\d{4})",
                      replacement = "city_popdensity_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(block_density)",
                      replacement = "city_blockdensity_2022",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(pnab)_(\\d{4})",
                      replacement = "bike_pnab_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(pnpb)_(\\d{4})",
                      replacement = "bike_pnpb_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(all_bikeways_km)",
                      replacement = "bike_abikeways_2022",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(protected_bikeways_km)",
                      replacement = "bike_pbikeways_2022",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(bikeshare)_(\\d{4})",
                      replacement = "bike_bikeshare_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(healthcare)_(\\d{4})",
                      replacement = "walk_pnh_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(schools)_(\\d{4})",
                      replacement = "walk_pne_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(h.s)_(\\d{4})",
                      replacement = "walk_pns_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(carfree)_(\\d{4})",
                      replacement = "walk_pncf_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(pnft)_(\\d{4})",
                      replacement = "transit_pnft_\\2",
                      x = ind_columns)
  ind_columns <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
                      replacement = "transit_\\L\\1\\E\\2_\\3",
                      x = ind_columns,
                      perl = TRUE)
  ind_columns <- gsub(pattern = "(people_not_near_highways)",
                      replacement = "city_pnnhighways_2022",
                      x = ind_columns,
                      perl = TRUE)
  
  
  # create new column names with new standardized id
  ind_columns_new <- fcase(
    startsWith(ind_columns, "city_poptotal"), ind_columns,
    startsWith(ind_columns, "city_popdensity"), ind_columns,
    startsWith(ind_columns, "city_blockdensity"), ind_columns,
    startsWith(ind_columns, "transit_pnrt"), ind_columns,
    startsWith(ind_columns, "bike_pnab"),                  ind_columns,
    startsWith(ind_columns, "bike_pnpb"),                  ind_columns,
    startsWith(ind_columns, "bike_abikeways"),       ind_columns,
    startsWith(ind_columns, "bike_pbikeways"), ind_columns,
    startsWith(ind_columns, "bike_bikeshare"),             ind_columns,
    startsWith(ind_columns, "walk_pnh"),            ind_columns,
    startsWith(ind_columns, "walk_pne"),               ind_columns,
    startsWith(ind_columns, "walk_pns"),                   ind_columns,
    startsWith(ind_columns, "walk_pncf"),               ind_columns,
    startsWith(ind_columns, "transit_pnft"),                  ind_columns,
    startsWith(ind_columns, "city_pnnhighways"),                  ind_columns,
    
    ind_columns == "performance_bike_lts2_30",               "performance_bikep30_2022",
    ind_columns == "performance_bike_lts2_45",               "performance_bikep45_2022",
    ind_columns == "performance_bike_lts2_60",               "performance_bikep60_2022",
    ind_columns == "performance_walk_30",               "performance_walkp30_2022",
    ind_columns == "performance_walk_45",               "performance_walkp45_2022",
    ind_columns == "performance_walk_60",               "performance_walkp60_2022"
    
  )
  
  # # bring country name
  data <- data %>%
    # select(-geom) %>%
    # filter(admin_level == 0) %>%
    # mutate(a3 = stringr::str_extract(name, "\\[[:upper:]{3}\\]")) %>%
    # mutate(a3 = gsub(pattern = "\\[|\\]", "", a3)) %>%
    # bring a2
    left_join(dplyr::select(maps::iso3166, a3, country = ISOname) %>% distinct(a3, .keep_all = TRUE), by = c("a3")) %>%
    tidyr::fill(a3, country) %>%
    ungroup()
  
  # select columns
  data <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, everything())
  
  # rename indicators
  colnames(data) <- c("hdc", "country", "a3", "osmid", "name", "admin_level", "admin_level_ordered", ind_columns_new,"geom")
  
  # arrange data correctly
  data <- data %>%
    dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered,
                  # starts_with("city_poptotal"),
                  starts_with("city_popdensity"),
                  starts_with("city_blockdensity"),
                  starts_with("city_pnnhighways"),
                  starts_with("bike_pnab"),
                  starts_with("bike_pnpb"),
                  starts_with("bike_abikeways"),
                  starts_with("bike_pbikeways"),
                  starts_with("bike_bikeshare"),
                  starts_with("walk_pnh"),
                  starts_with("walk_pne"),
                  starts_with("walk_pns"),
                  starts_with("walk_pncf"),
                  starts_with("transit_pnft"),
                  starts_with("transit_pnrtall"),
                  starts_with("transit_pnrtbrt"),
                  starts_with("transit_pnrtlrt"),
                  starts_with("transit_pnrtmrt"),
                  starts_with("performance_bike"),
                  starts_with("performance_walk"))
  
  data$admin_level <- as.character(data$admin_level)
  data$admin_level_ordered <- as.character(data$admin_level_ordered)
  
  # exception for recife
  if (ghsl == "01445") {
    
    data$country <- "Brazil"
    data$a3 <- "BRA"
    
  }
  
  # simplify data
  
  # data_original <- data %>% filter(admin_level == 10)
  
  data <- rmapshaper::ms_simplify(data, drop_null_geometries = FALSE, keep_shapes = TRUE, keep = 0.8)
  # data2 <- data1 %>% filter(admin_level == 10)
  
  # data <- st_make_valid(data)
  # data <- st_simplify(data)
  # data1 <- st_cast(data, "MULTIPOLYGON")
  # to polygons
  # data <- st_cast(data, "MULTIPOLYGON")
  
  dir.create(sprintf("data/data_alpha/ghsl_%s", ghsl), recursive = TRUE)
  
  # save
  readr::write_rds(data, sprintf("data/data_alpha/ghsl_%s/indicators_%s.rds", ghsl, ghsl))
  
}


# apply to all cities
cities_available <- unique(data_all$hdc)
library(purrr)
walk(cities_available, prep_data)






# prep overlays -----------------------------------------------------------

indicators_all <- purrr::map_dfr(dir("data/data_alpha", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# ghsl <- "0014"
# ghsl <- "0088"
# ghsl <- "0154"
# ghsl <- "0634"
# ghsl <- "01406"
# ghsl <- "00021"
# ghsl <- "09691"
# ghsl <- cities_available[21]
prep_overlays <- function(ghsl) {
  
  
  base_dir <- sprintf("data-raw/data_alpha/city_results/ghsl_region_%s/", ghsl)
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = ".geojson$", recursive = TRUE)
  overlay_pop <- dir(paste0(base_dir, "geodata/population"), full.names = TRUE, pattern = ".tif$", recursive = TRUE)
  
  # filter only our overlays
  overlay_files1 <- overlay_files[overlay_files %like% c("allbike_latlon|h\\+slatlon|healthcarelatlon|pnablatlon|pnpblatlon|pnftlatlon|protectedbike|schools|block_densities_latlon|buffered_hwys_latlon|carfreelatlon")]
  
  # the overlay files only have the shape geom, so we need to give them names based on the filename
  # read overlays
  open_overlay <- function(file) {
    # file <- overlay_files1[[2]]
    
    a <- st_read(file)
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = as.character(st_geometry_type(.)))
    a <- a %>% mutate(year = 2022)
    
    # create value
    a <- a %>%
      mutate(value = ifelse(ind == "block_densities_latlon.geojson", block_count, NA_integer_)) %>%
      # select final variables
      dplyr::select(ind, geom_type, year, value)
    
    # a <- st_cast(a, "MULTIPOLYGON")
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    if (geom_type == "POLYGON") a <- st_cast(a, "MULTIPOLYGON")
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- st_transform(a, 4326)
    
  }
  
  overlay <- lapply(overlay_files1, open_overlay)
  
  # for the population overlay - open raster, convert to sf, and save on the same format
  open_overlay_pop <- function(file) {
    # file <- overlay_pop[[10]]
    
    # extract year
    year <- sub("(^.*)/(pop_)(\\d{4})(.*$)", "\\3", file)
    
    a <- stars::read_stars(file)
    # to sf
    a <- st_as_sf(a)
    a <- a %>% rename(value = 1)
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- a %>% mutate(year = year)
    a <- a %>% mutate(ind = "pop.tif")
    
    # select final variables
    a <- a %>% dplyr::select(ind, geom_type, year, value)
    
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    
    if (geom_type == "POLYGON") a <- st_cast(a, "MULTIPOLYGON")
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    
    a <- st_transform(a, 4326)
  }
  overlay_pop_open <- lapply(overlay_pop, open_overlay_pop)
  
  # bind
  overlay <- c(overlay, overlay_pop_open)
  
  # open rapid_transit when available
  overlay_files_rapid <- overlay_files[overlay_files %like% c("rapid_transit")]
  overlay_files_rapid <- overlay_files_rapid[overlay_files_rapid %like% c("isochrones")]
  
  # fun
  open_overlay_rapid <- function(file) {
    # file <- overlay_files_rapid[[223]]
    
    # extract year
    year <- sub("(^.*)/(rapid_transit/)(\\d{4})/(.*$)", "\\3", file)
    
    a <- st_read(file)
    # if the polygon is empty, create an empty mulitpolygon
    if(st_is_empty(a)) {
      
      a <- st_sf(geometry = st_sfc(st_multipolygon()), crs = 4326)
      
    }
    
    
    a <- a %>% mutate(ind = basename(file))
    # identify geom type
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- a %>% mutate(year = year) %>%
      mutate(value = NA)
    
    # select final variables
    a <- a %>% dplyr::select(ind, geom_type, year, value)
    
    # a <- st_cast(a, "MULTIPOLYGON")
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    if (geom_type %in% c("POLYGON")) a <- st_cast(a, "MULTIPOLYGON")
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- st_transform(a, 4326)
    
  }
  
  
  if (length(overlay_files_rapid) > 0) {
    
    overlay_rapid <- lapply(overlay_files_rapid, open_overlay_rapid)
    
    overlay <- c(overlay, overlay_rapid)
    
  }
  
  
  
  # identify geom type
  names(overlay) <- purrr::map_chr(overlay, function(x) unique(as.character(st_geometry_type(x))))
  # separate between polygons and lines
  overlay_polygons <- overlay[grep("MULTIPOLYGON|POLYGON", names(overlay))] %>% rbindlist()
  overlay_lines <- overlay[grep("MULTILINESTRING", names(overlay))] %>% rbindlist()
  
  
  
  # juntar objeto dos overlays sem a geom
  if (nrow(overlay_lines) > 0) {
    
    overlay_df <- rbind(overlay_polygons %>% dplyr::select(-geometry), 
                        overlay_lines %>% dplyr::select(-geometry)) %>% dplyr::select(ind, geom_type, year, value) %>%
      distinct(ind, year, .keep_all = TRUE)
    
    
  } else {
    
    
    overlay_df <- rbind(overlay_polygons %>% dplyr::select(-geometry))
    
  }
  
  
  # create aux table with the overlay ids
  overlay_id <- tibble::tribble( 
    
    ~ind, ~indicator,
    "pop.tif",            "city_popdensity",
    "buffered_hwys_latlon.geojson",            "city_pnnhighways",
    "block_densities_latlon.geojson",            "city_blockdensity",
    "pnpblatlon.geojson",            "bike_pnpb",
    "pnablatlon.geojson",            "bike_pnab",
    "allbike_latlon.geojson",        "bike_abikeways",
    "protectedbike_latlon.geojson",  "bike_pbikeways",
    "healthcarelatlon.geojson",      "walk_pnh",
    "schoolslatlon.geojson",         "walk_pne",
    "h+slatlon.geojson",             "walk_pns",
    "carfreelatlon.geojson",         "walk_pncf",
    "pnftlatlon.geojson",            "transit_pnft",
    "all_isochrones_ll.geojson",     "transit_pnrtall",
    "lrt_isochrones_ll.geojson",     "transit_pnrtlrt",
    "mrt_isochrones_ll.geojson",     "transit_pnrtmrt",
    "brt_isochrones_ll.geojson",     "transit_pnrtbrt"
    
  )
  
  # bring indicator id to the overlays
  overlay_polygons <- left_join(overlay_polygons, overlay_id, by = "ind") %>% dplyr::select(-ind) %>% 
    mutate(indicator = paste0(indicator, "_", year)) %>%
    # select(-year) %>%
    st_sf(crs = 4326)
  if (nrow(overlay_lines) > 0) {
    overlay_lines <- left_join(overlay_lines, overlay_id, by = "ind") %>% dplyr::select(-ind) %>% 
      mutate(indicator = paste0(indicator, "_", year)) %>%
      # select(-year) %>%
      st_sf(crs = 4326)
    overlay_lines <- st_simplify(overlay_lines, dTolerance = 0.01)
    # overlay_lines1 <- st_cast(overlay_lines, "LINESTRING")
    
  } 
  
  # simplify data
  overlay_polygons <- rmapshaper::ms_simplify(overlay_polygons, drop_null_geometries = FALSE, keep_shapes = TRUE)
  
  # to polygons
  # overlay_polygons1 <- st_cast(overlay_polygons, "POLYGON")
  
  # overlay_lines1 <- sfheaders::sf_cast(overlay_lines, to = "LINESTRING")
  # a <- st_union(overlay_lines1)
  # b <- overlay_lines1[1:2,]
  
  overlay_df <- left_join(overlay_df, overlay_id, by = "ind") %>% 
    mutate(indicator = paste0(indicator, "_", year)) %>%
    dplyr::select(-ind)
  
  # we should overlays for all available indicators, so we need to check that
  overlay_missing <- setdiff(colnames(indicators_all)[8:(length(colnames(indicators_all)) - 1)], overlay_df$indicator)
  
  if (!is.null(overlay_missing)) {
    
    
    nrows <- length(overlay_missing)
    
    overlay_missing_polygons <- st_sf(geom_type = rep("MULTIPOLYGON", nrows),
                                      indicator = overlay_missing,
                                      value = NA, 
                                      geometry = st_sfc(lapply(1:nrows, function(x) st_multipolygon())),
                                      crs = 4326) %>%
      mutate(year = sub("^(.*)_(.*)_(\\d{4})$", "\\3", indicator))
    overlay_missing_df <- data.frame(geom_type = rep("MULTIPOLYGON", nrows),
                                     value = NA,
                                     indicator = overlay_missing) %>%
      mutate(year = sub("^(.*)_(.*)_(\\d{4})$", "\\3", indicator))
    
    
    # join
    overlay_polygons <- rbind(overlay_polygons, overlay_missing_polygons)
    overlay_df <- rbind(overlay_df, overlay_missing_df)
    
    
  }
  
  # save by year
  
  save_by_ind <- function(ind) {
    # ind <- "pnrtlrt"
    # year1 <- "2022"
    
    # overlay_df_inds <- overlay_df %>% filter(year == year1) %>%
    #   filter(stringr::str_detect(indicator, ind))
    dir.create(sprintf("data/data_alpha/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
    
    overlay_polygons_inds <- overlay_polygons  %>%
      filter(stringr::str_detect(indicator, ind))
    
    if (nrow(overlay_lines) > 0) {
      
      overlay_lines_inds <- overlay_lines %>% 
        filter(stringr::str_detect(indicator, ind))
      
      readr::write_rds(overlay_lines_inds,    sprintf("data/data_alpha/ghsl_%s/overlays/%s/overlays_lines_%s_%s.rds", ghsl, ind, ghsl, ind))
      
    }
    
    
    
    # readr::write_rds(overlay_df_inds, sprintf("data/sample3/ghsl_%s/overlays/%s/overlays_df_%s_%s_%s.rds", ghsl, year1, ghsl, year1, ind))
    readr::write_rds(overlay_polygons_inds, sprintf("data/data_alpha/ghsl_%s/overlays/%s/overlays_polygons_%s_%s.rds", ghsl, ind, ghsl, ind))
  }
  
  inds <- unique(sub("^(.*)_(.*)_(\\d{4})$", "\\2", overlay_df$indicator))
  purrr::walk(inds, save_by_ind)
  
  
  readr::write_rds(overlay_df,       sprintf("data/data_alpha/ghsl_%s/overlays_%s.rds", ghsl, ghsl))
  # readr::write_rds(overlay_polygons, sprintf("data/sample3/ghsl_%s/overlays_polygons_%s.rds", ghsl, ghsl))
  # readr::write_rds(overlay_lines,    sprintf("data/sample3/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  
}

# apply to all cities
cities_available <- unique(data_all$hdc)
walk(cities_available, prep_overlays)









# calculate boundaries for the world view ---------------------
indicators_all <- purrr::map_dfr(dir("data/data_alpha", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
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
readr::write_rds(indicators_ghsl_centroids, "data/data_alpha/atlas_city_markers.rds")





# calculate mean for each country -----------------


atlas_country <- st_read("data-raw/data_alpha/country_results/country_results.geojson")
atlas_country <- rmapshaper::ms_simplify(atlas_country)

# bring the names
atlas_country <- atlas_country %>%
  filter(index != "-99") %>%
  left_join(
    countrycode::codelist %>% dplyr::select(country.name.en, iso3c),
    
    by = c("index" = "iso3c")) %>%
  rename(name = country.name.en)

# rename indicators
# get available indicators for this city
ind_columns <- colnames(atlas_country)[colnames(atlas_country) %nin% c("index", "name", "geometry")]


# first, rename indicators that are divided by year
ind_columns <- gsub(pattern = "(total_pop)_(\\d{4})",
                    replacement = "city_poptotal_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(density)_(\\d{4})",
                    replacement = "city_popdensity_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(block_density)",
                    replacement = "city_blockdensity_2022",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(pnab)_(\\d{4})",
                    replacement = "bike_pnab_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(pnpb)_(\\d{4})",
                    replacement = "bike_pnpb_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(all_bikeways_km)",
                    replacement = "bike_abikeways_2022",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(protected_bikeways_km)",
                    replacement = "bike_pbikeways_2022",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(bikeshare)_(\\d{4})",
                    replacement = "bike_bikeshare_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(healthcare)_(\\d{4})",
                    replacement = "walk_pnh_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(schools)_(\\d{4})",
                    replacement = "walk_pne_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(h.s)_(\\d{4})",
                    replacement = "walk_pns_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(carfree)_(\\d{4})",
                    replacement = "walk_pncf_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(pnft)_(\\d{4})",
                    replacement = "transit_pnft_\\2",
                    x = ind_columns)
ind_columns <- gsub(pattern = "(PNrT)_([[:lower:]]{3})_(\\d{4})",
                    replacement = "transit_\\L\\1\\E\\2_\\3",
                    x = ind_columns,
                    perl = TRUE)
ind_columns <- gsub(pattern = "(people_not_near_highways)",
                    replacement = "city_pnnhighways_2022",
                    x = ind_columns,
                    perl = TRUE)


# create new column names with new standardized id
ind_columns_new <- fcase(
  startsWith(ind_columns, "city_poptotal"), ind_columns,
  startsWith(ind_columns, "city_popdensity"), ind_columns,
  startsWith(ind_columns, "city_blockdensity"), ind_columns,
  startsWith(ind_columns, "transit_pnrt"), ind_columns,
  startsWith(ind_columns, "bike_pnab"),                  ind_columns,
  startsWith(ind_columns, "bike_pnpb"),                  ind_columns,
  startsWith(ind_columns, "bike_abikeways"),       ind_columns,
  startsWith(ind_columns, "bike_pbikeways"), ind_columns,
  startsWith(ind_columns, "bike_bikeshare"),             ind_columns,
  startsWith(ind_columns, "walk_pnh"),            ind_columns,
  startsWith(ind_columns, "walk_pne"),               ind_columns,
  startsWith(ind_columns, "walk_pns"),                   ind_columns,
  startsWith(ind_columns, "walk_pncf"),               ind_columns,
  startsWith(ind_columns, "transit_pnft"),                  ind_columns,
  startsWith(ind_columns, "city_pnnhighways"),                  ind_columns,
  
  ind_columns == "performance_bike_lts2_30",               "performance_bikep30_2022",
  ind_columns == "performance_bike_lts2_45",               "performance_bikep45_2022",
  ind_columns == "performance_bike_lts2_60",               "performance_bikep60_2022",
  ind_columns == "performance_walk_30",               "performance_walkp30_2022",
  ind_columns == "performance_walk_45",               "performance_walkp45_2022",
  ind_columns == "performance_walk_60",               "performance_walkp60_2022"
  
)


colnames(atlas_country) <- c("a3", ind_columns_new, "name", "geometry")

# arrange data correctly
atlas_country <- atlas_country %>%
  dplyr::select(a3, name,
                starts_with("city_popdensity"),
                starts_with("city_blockdensity"),
                starts_with("city_pnnhighways"),
                starts_with("bike_pnab"),
                starts_with("bike_pnpb"),
                starts_with("bike_abikeways"),
                starts_with("bike_pbikeways"),
                starts_with("bike_bikeshare"),
                starts_with("walk_pnh"),
                starts_with("walk_pne"),
                starts_with("walk_pns"),
                starts_with("walk_pncf"),
                starts_with("transit_pnft"),
                starts_with("transit_pnrtall"),
                starts_with("transit_pnrtbrt"),
                starts_with("transit_pnrtlrt"),
                starts_with("transit_pnrtmrt"),
                starts_with("performance_bike"),
                starts_with("performance_walk"))

# select only the indicators that we have in the indicators
atlas_country <- atlas_country %>%
  dplyr::select(a3, name,
                any_of(colnames(indicators_all))   
  )

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
    
    readr::write_rds(atlas_country_ind, sprintf("data/data_alpha/countries/atlas_country_%s.rds",
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

indicators_all <- purrr::map_dfr(dir("data/data_alpha", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL)




# create list with osmid --------------------------------------------------

count(indicators_all_df, hdc, country, osmid, name, admin_level, admin_level_ordered) %>%
  dplyr::select(-n) %>%
  readr::write_rds("data/data_alpha/list_osmid_name.rds")



# create indicator availability for each city -----------------------------

# to long format
colnames_compare <- colnames(indicators_all_df)[8:ncol(indicators_all_df)]
# extract year
# years_compare <- gsub(pattern = "(.*)_(.*)_(\\d{4}$)",
#                       replacement = "\\2_\\3",
#                       x = colnames_compare)

# colnames(indicators_all_df) <- c("hdc", "country", "a2", "osmid", "name", "admin_level", "admin_level_ordered", 
#                                  years_compare)

indicators_all_df_long <- tidyr::pivot_longer(indicators_all_df,
                                              cols = 8:last_col(),
                                              names_sep = "_",
                                              names_to = c("ind_type", "ind", "year"),
                                              values_to = "value")

a1 <- distinct(indicators_all_df_long, hdc, ind, year, .keep_all = TRUE) %>%
  filter(!is.na(value)) %>%
  group_by(country, name, hdc, ind_type, ind) %>%
  summarise(availability = paste0(year, collapse = "|")) %>%
  ungroup()
# ungroup() %>%
# group_by(hdc) %>%
# summarise(ind = first(ind))
# select(hdc, ind, year)

# add recife manually

readr::write_rds(a1, "data/data_alpha/list_availability.rds")

