library(sf)
library(dplyr)
library(mapview)

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}


# first, extract the reaches in the grid format ---------------------------

extract_grid <- function(ghsl) {
  
  # ghsl <- "00200"
  # ghsl <- "00561"
  # ghsl <- "01445"
  
  ghsl_new <- ghsl
  
  a <- st_read(sprintf("data-raw/sample_3/ghsl_region_%s/geodata/connections.gpkg", ghsl)) %>%
    select(id, cumsum_bike_lts2_45, cumsum_walk_45) %>%
    # create hdc
    mutate(hdc = ghsl) %>%
    # rename indicators
    select(hdc, osmid = id, performance_bikep45_2022 = cumsum_bike_lts2_45,  performance_walkp45_2022 = cumsum_walk_45)
  
  readr::write_rds(a, sprintf("data/sample5/ghsl_%s/grid_%s.rds", ghsl_new, ghsl_new))
  
}

# check cities with grids
# cities_grid <- dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE)
# cities_grid <- cities_grid[grepl("connections.gpkg", x = cities_grid)]
# cities_grid <- stringr::str_extract(cities_grid, pattern = "(\\d{5})")

extract_grid("00088")
extract_grid("00200")
extract_grid("00561")
extract_grid("00621")
extract_grid("01406")
extract_grid("01445")



# extract reaches

extract_reaches <- function(ghsl) {
  # ghsl <- "0200"
  # ghsl <- "0561"
  # ghsl <- "01445"
  
  ghsl_new <- paste0("0", ghsl)
  
  a <- st_read(sprintf("data-raw/sample_3/ghsl_region_%s/geodata/connections.gpkg", ghsl)) %>%
    # get cetroid
    st_centroid(.) %>%
    sfc_as_cols()
  
  
  
  a1 <- a %>%
    select(id, reach_walk_45, reach_bike_lts2_45, lon, lat)
  
  
  # reaches for walk
  a1_walk <- a1 %>%
    filter(!is.na(reach_walk_45)) %>%
    select(id, reach_walk_45)
  
  reaches_walk <- st_as_sf(a1_walk, wkt = "reach_walk_45", crs = 4326)
  st_geometry(reaches_walk) <- "geometry"
  # bring the empty ones
  empty <- setdiff(a1$id, a1_walk$id)
  reaches_walk_empty <- st_sf(id = empty,
                              geometry = st_sfc(lapply(1:length(empty), function(x) st_multipolygon())),
                              crs = 4326)
  # join
  reaches_walk_end <- rbind(reaches_walk, reaches_walk_empty) %>% arrange(id)
  # bring centroid
  # reaches_walk_end <- left_join(reaches_walk_end, a1 %>% select(osmid = id, lon, lat))
  # simplify
  reaches_walk_end <- rmapshaper::ms_simplify(reaches_walk_end, drop_null_geometries = FALSE, keep_shapes = TRUE)
  
  # save for each id
  save_by_id <- function(variables) {
    
    reaches_walk_end1 <- reaches_walk_end %>% filter(id == variables)
    readr::write_rds(reaches_walk_end1, sprintf("data/sample5/ghsl_%s/reaches/reaches_%s_%s_walkp45.rds", ghsl, ghsl, variables))
    
  }
  # get list of id
  ids <- reaches_walk_end$id %>% unique()
  purrr::walk(ids, save_by_id)
  
  
  
  
  a1_bike <- a1 %>%
    filter(!is.na(reach_bike_lts2_45)) %>%
    select(id, reach_bike_lts2_45)
  
  reaches_bike <- st_as_sf(a1_bike, wkt = "reach_bike_lts2_45", crs = 4326)
  st_geometry(reaches_bike) <- "geometry"
  # bring the empty ones
  empty <- setdiff(a1$id, a1_bike$id)
  reaches_bike_empty <- st_sf(id = empty,
                              geometry = st_sfc(lapply(1:length(empty), function(x) st_multipolygon())),
                              crs = 4326)
  # join
  reaches_bike_end <- rbind(reaches_bike, reaches_bike_empty) %>% arrange(id)
  
  # simplify
  reaches_bike_end <- rmapshaper::ms_simplify(reaches_bike_end,  drop_null_geometries = FALSE, keep_shapes = TRUE)
  
  # save for each id
  save_by_id <- function(variables) {
    
    reaches_bike_end1 <- reaches_bike_end %>% filter(id == variables)
    readr::write_rds(reaches_bike_end1, sprintf("data/sample5/ghsl_%s/reaches/reaches_%s_%s_bikep45.rds", ghsl, ghsl, variables))
    
  }
  # get list of id
  ids <- reaches_bike_end$id %>% unique()
  purrr::walk(ids, save_by_id)
  
}


extract_reaches("01445")