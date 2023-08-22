library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
sf::sf_use_s2(FALSE)



# start by getting the overlays table ---------------------------------------------------------

library(googlesheets4)
overlay_table <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/194T-zZZRhwAAvon7lOeyjT49jzij3SKhDM-2HyBi3Hc/edit?usp=sharing") %>%
  mutate(overlay = basename(overlay_dir)) %>%
  mutate(overlay = sub(pattern = ".geojson", replacement = "", x = overlay)) %>%
  mutate(overlay = sub(pattern = ".tif", replacement = "", x = overlay))



# prep overlays -----------------------------------------------------------

indicators_all <- purrr::map_dfr(dir("data/data_july2023", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# ghsl <- "0014"
# ghsl <- "0088"
# ghsl <- "0154"
# ghsl <- "0634"
# ghsl <- "01406" # fortaleza
# ghsl <- "00021"
# ghsl <- "09691"
# ghsl <- "00816"
# ghsl <- unique(indicators_all$hdc)[20]
prep_overlays <- function(ghsl) {
  
  
  base_dir <- sprintf("data-raw/atlas_data_july_31/cities_out/ghsl_region_%s/", ghsl)
  # 
  # a1 <- dir(paste0(base_dir, "geodata"), pattern = ".geojson")
  # a1 <- a1[!(a1 %like% "population|rapid_transit")]
  # a2 <- sub(pattern = ".geojson", replacement = "", x = a1)
  # 
  # lapply(paste0(base_dir, "geodata/", a2), dir.create)
  # 
  # before <- paste0(base_dir, "geodata/", a1)
  # after <- paste0(base_dir, "geodata/", a2, "/", a2, "_2022.geojson")
  # 
  # purrr::walk2(before, after, file.copy)
  # walk(before, file.remove)
  
  # # change name of folder h+s
  # folder_hs <- dir(paste0(base_dir, "geodata")
  #                  , pattern = "h\\+slatlon"
  #                  ,full.names = TRUE
  # )
  # folder_hs_new <- paste0(paste0(base_dir, "/geodata/hslatlon"))
  # # rename
  # walk2(folder_hs, folder_hs_new, file.rename)
  # files_hs <- dir(paste0(base_dir, "geodata/hslatlon")
  #                 , pattern = ".geojson"
  #                 ,full.names = TRUE
  # )
  # files_hs_new <- stringr::str_remove(files_hs, "\\+")
  # walk2(files_hs, files_hs_new, file.rename)
  # # pop
  # folder_pop <- dir(paste0(base_dir, "geodata")
  #                  , pattern = "population"
  #                  ,full.names = TRUE
  # )
  # folder_pop_new <- paste0(paste0(base_dir, "/geodata/pop"))
  # # rename
  # walk2(folder_pop, folder_pop_new, file.rename)
  
  
  
  
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = ".geojson$", recursive = TRUE)
  overlay_pop <- dir(paste0(base_dir, "geodata/pop"), full.names = TRUE, pattern = ".tif$", recursive = TRUE)
  
  # filter only our overlays
  # overlay_files1 <- overlay_files[overlay_files %like% c("allbike_latlon|h\\+slatlon|healthcarelatlon|pnablatlon|pnpblatlon|pnftlatlon|protectedbike|schools|block_densities_latlon|buffered_hwys_latlon|carfreelatlon")]
  overlay_files1 <- overlay_files[overlay_files %like% paste(overlay_table$overlay, collapse  = "|")]
  overlay_files1 <- overlay_files1[!(overlay_files1 %like% "rapid_transit")]
  
  
  
  # the overlay files only have the shape geom, so we need to give them names based on the filename
  # read overlays
  open_overlay <- function(file) {
    # file <- overlay_files1[[13]]
    
    a <- st_read(file)
    
    
    # extract year
    year1 <- sub("(^.*)(\\d{4})(.geojson$)", "\\2", basename(file))
    # extract 
    
    
    if (nrow(a) == 0) {
      
      a <- st_sf(ind = basename(file), geometry = st_sfc(st_point()), crs = st_crs(4326))
      # identify geom type
      a <- a %>% mutate(geom_type = as.character(st_geometry_type(.)))
      a <- a %>% mutate(year = year1)
      # remove year
      a <- mutate(a, ind = sub(pattern = "_\\d{4}.geojson", replacement = "", x = ind))
      
    } else {
      
      a <- a %>% mutate(ind = basename(file))
      # identify geom type
      a <- a %>% mutate(geom_type = as.character(st_geometry_type(.)))
      a <- a %>% mutate(year = year1)
      # remove year
      a <- mutate(a, ind = sub(pattern = "_\\d{4}.geojson", replacement = "", x = ind))
      
    }
    
    # create value
    a <- a %>%
      mutate(value = ifelse(ind == "block_densities_latlon.geojson", block_count, NA_integer_)) %>%
      # select final variables
      dplyr::select(ind, geom_type, year, value)
    
    # a <- st_cast(a, "MULTIPOLYGON")
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    if (isTRUE(geom_type == "POLYGON")) a <- st_cast(a, "MULTIPOLYGON")
    if (isTRUE(geom_type == "MULTIPOINT")) a <- st_cast(a, "POINT")
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
    a <- mutate(a, ind = sub(pattern = "_\\d{4}.tif", replacement = "", x = ind))
    
    # select final variables
    a <- a %>% dplyr::select(ind, geom_type, year, value)
    
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    
    if (geom_type == "POLYGON") a <- st_cast(a, "MULTIPOLYGON")
    if (geom_type == "MULTIPOINT") a <- st_cast(a, "POINT")
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    
    a <- st_transform(a, 4326)
  }
  overlay_pop_open <- lapply(overlay_pop, open_overlay_pop)
  
  # bind
  overlay <- c(overlay, overlay_pop_open)
  
  # open rapid_transit when available
  overlay_files_rapid <- overlay_files[overlay_files %like% c("rapid_transit")]
  # overlay_files_rapid <- overlay_files_rapid[overlay_files_rapid %like% c("isochrones")]

  # fun
  open_overlay_rapid <- function(file) {
    # file <- overlay_files_rapid[[5]]

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
    a <- mutate(a, ind = sub(pattern = ".geojson", replacement = "", x = ind))

    # select final variables
    a <- a %>% dplyr::select(ind, geom_type, year, value)

    # a <- st_cast(a, "MULTIPOLYGON")
    geom_type <- st_geometry_type(a) %>% unique() %>% as.character()
    if (geom_type %in% c("POLYGON")) a <- st_cast(a, "MULTIPOLYGON")
    if (isTRUE(geom_type == "MULTIPOINT")) a <- st_cast(a, "POINT")
    a <- a %>% mutate(geom_type = st_geometry_type(.))
    a <- st_transform(a, 4326)

  }

  
  if (length(overlay_files_rapid) > 0) {
    
    overlay_rapid <- lapply(overlay_files_rapid, open_overlay_rapid)
    
    overlay <- c(overlay, overlay_rapid)
    
  }
  
  
  
  # identify geom type
  names(overlay) <- purrr::map_chr(overlay, function(x) unique(as.character(st_geometry_type(x)))) 
  # separate between polygons and lines and points
  overlay_polygons <- overlay[grep("MULTIPOLYGON|POLYGON", names(overlay))] %>% rbindlist()
  overlay_lines <- overlay[grep("MULTILINESTRING", names(overlay))] %>% rbindlist()
  overlay_points <- overlay[grep("POINT", names(overlay))] %>% rbindlist()
  
  
  
  # juntar objeto dos overlays sem a geom
  if (nrow(overlay_lines) > 0 & nrow(overlay_points) > 0) {
    
    overlay_df <- rbind(overlay_polygons %>% dplyr::select(-geometry), 
                        overlay_lines %>% dplyr::select(-geometry),
                        overlay_points %>% dplyr::select(-geometry)) %>% 
      dplyr::select(ind, geom_type, year, value) %>%
      distinct(ind, year, .keep_all = TRUE)
    
    
  } else {
    
    
    overlay_df <- rbind(overlay_polygons %>% dplyr::select(-geometry))
    
  }
  
  
  # create aux table with the overlay ids
  # overlay_id <- tibble::tribble( 
  #   
  #   ~ind, ~indicator,
  #   "pop.tif",            "city_popdensity",
  #   "buffered_hwys_latlon.geojson",            "city_pnnhighways",
  #   "block_densities_latlon.geojson",            "city_blockdensity",
  #   "pnpblatlon.geojson",            "bike_pnpb",
  #   "pnablatlon.geojson",            "bike_pnab",
  #   "allbike_latlon.geojson",        "bike_abikeways",
  #   "protectedbike_latlon.geojson",  "bike_pbikeways",
  #   "healthcarelatlon.geojson",      "walk_pnh",
  #   "schoolslatlon.geojson",         "walk_pne",
  #   "h+slatlon.geojson",             "walk_pns",
  #   "carfreelatlon.geojson",         "walk_pncf",
  #   "pnftlatlon.geojson",            "transit_pnft",
  #   "all_isochrones_ll.geojson",     "transit_pnrtall",
  #   "lrt_isochrones_ll.geojson",     "transit_pnrtlrt",
  #   "mrt_isochrones_ll.geojson",     "transit_pnrtmrt",
  #   "brt_isochrones_ll.geojson",     "transit_pnrtbrt"
  #   
  # )
  
  overlay_id <- overlay_table %>%
    rename(ind = overlay) %>%
    select(ind, indicator)
    # mutate(ind = ifelse(ind == "pop_[year].tif","pop.tif", ind)) %>%
    # mutate(ind = ifelse(ind == "pop_2020.tif","pop.tif", ind))
  
  # # which years we have for population?
  # pop_years <- overlay_polygons %>%
  #   filter(ind == "pop.tif") %>%
  #   mutate(ind = "pop_[year].tif") %>%
  #   distinct(ind, year)
  # 
  # overlay_id <- overlay_id %>%
  #   left_join(pop_years, by = "ind") %>%
  #   mutate(ind = ifelse(ind == "pop_[year].tif","pop.tif", ind)
  
  # select(-year) %>%
  if (nrow(overlay_polygons) > 0) {
    overlay_polygons <- overlay_polygons %>%
      # left_join(overlay_lines, overlay_id, by = "ind") %>% dplyr::select(-ind) %>% 
      # mutate(indicator = paste0(indicator, "_", year)) %>%
      # select(-year) %>%
      st_sf(crs = 4326)
    overlay_polygons <- rmapshaper::ms_simplify(overlay_polygons, drop_null_geometries = FALSE, keep_shapes = TRUE)
    # overlay_lines1 <- st_cast(overlay_lines, "LINESTRING")
    
  } 
  if (nrow(overlay_lines) > 0) {
    overlay_lines <- overlay_lines %>%
      # left_join(overlay_lines, overlay_id, by = "ind") %>% dplyr::select(-ind) %>% 
      # mutate(indicator = paste0(indicator, "_", year)) %>%
      # select(-year) %>%
      st_sf(crs = 4326)
    overlay_lines <- st_simplify(overlay_lines, dTolerance = 0.01)
    # overlay_lines1 <- st_cast(overlay_lines, "LINESTRING")
    
  } 
  if (nrow(overlay_points) > 0) {
    overlay_points <- overlay_points %>%
      st_sf(crs = 4326)
    
  } 
  
  
  overlay_df1 <- left_join(overlay_id, overlay_df, by = "ind") %>% 
    mutate(indicator = paste0(indicator, "_", year)) %>%
    dplyr::select(-ind)
  
  # # we should overlays for all available indicators, so we need to check that
  # overlay_missing <- setdiff(colnames(indicators_all)[8:(length(colnames(indicators_all)) - 1)], overlay_df$indicator)
  # 
  # if (!is.null(overlay_missing)) {
  #   
  #   
  #   nrows <- length(overlay_missing)
  #   
  #   overlay_missing_polygons <- st_sf(geom_type = rep("MULTIPOLYGON", nrows),
  #                                     indicator = overlay_missing,
  #                                     value = NA, 
  #                                     geometry = st_sfc(lapply(1:nrows, function(x) st_multipolygon())),
  #                                     crs = 4326) %>%
  #     mutate(year = sub("^(.*)_(.*)_(\\d{4})$", "\\3", indicator))
  #   overlay_missing_df <- data.frame(geom_type = rep("MULTIPOLYGON", nrows),
  #                                    value = NA,
  #                                    indicator = overlay_missing) %>%
  #     mutate(year = sub("^(.*)_(.*)_(\\d{4})$", "\\3", indicator))
  #   
  #   
  #   # join
  #   overlay_polygons <- rbind(overlay_polygons, overlay_missing_polygons)
  #   overlay_df <- rbind(overlay_df, overlay_missing_df)
  #   
  #   
  # }
  
  # save by year
  
  # ind1 <- "pnpb"
  save_by_ind <- function(ind1) {
    
    dir.create(sprintf("data/data_july2023/ghsl_%s/overlays/%s", ghsl, ind1), recursive = TRUE)
    
    # which overlays to use with this indicator?
    overlays_thisind <- overlay_id %>%
      dplyr::filter(indicator %in% ind1)
    
    if (nrow(overlay_polygons) > 0) {
      
      overlay_polygons_inds <- overlay_polygons  %>%
        filter(ind %in% overlays_thisind$ind)
      
    }
    
    if (nrow(overlay_lines) > 0) {
      
      overlay_lines_inds <- overlay_lines %>% 
        filter(ind %in% overlays_thisind$ind)
      
    }
    
    if (nrow(overlay_points) > 0) {
      
      overlay_points_inds <- overlay_points %>% 
        filter(ind %in% overlays_thisind$ind)
      
    }
    
    # year1 <- 2022
    
    save_by_year <- function(year1) {
      
      if (nrow(overlay_polygons) > 0) {
        
        overlay_polygons_inds <- overlay_polygons_inds  %>%
          filter(year == year1) %>%
          mutate(indicator = paste0(ind1, "_", year))
        
        if (nrow(overlay_polygons_inds) > 0) {
          
          readr::write_rds(overlay_polygons_inds, sprintf("data/data_july2023/ghsl_%s/overlays/%s/overlays_polygons_%s_%s_%s.rds", ghsl, ind1, ghsl, ind1, year1))
          
        }
      }
      
      if (nrow(overlay_lines) > 0) {
        
        overlay_lines_inds <- overlay_lines_inds %>% 
          filter(year == year1) %>%
          mutate(indicator = paste0(ind1, "_", year))
        
        
        if (nrow(overlay_lines_inds) > 0) {
          readr::write_rds(overlay_lines_inds,    sprintf("data/data_july2023/ghsl_%s/overlays/%s/overlays_lines_%s_%s_%s.rds", ghsl, ind1, ghsl, ind1, year1))
        }
        
      }
      
      if (nrow(overlay_points) > 0) {
        
        overlay_points_inds <- overlay_points_inds %>% 
          filter(year == year1) %>%
          mutate(indicator = paste0(ind1, "_", year))
        
        
        if (nrow(overlay_points_inds) > 0) {
          readr::write_rds(overlay_points_inds,    sprintf("data/data_july2023/ghsl_%s/overlays/%s/overlays_points_%s_%s_%s.rds", ghsl, ind1, ghsl, ind1, year1))
        }
      }
      
    }
    
    # run for all yeras from that indicator
    years <- unique(c(overlay_polygons_inds$year, overlay_lines_inds$year, overlay_points_inds$year))
    purrr::walk(years, save_by_year)
    
  }
  
  # run for all indicators
  inds <- unique(sub("^(.*)_(.*)_(\\d{4})$", "\\2", overlay_id$indicator))
  purrr::walk(inds, save_by_ind)
  
  
  readr::write_rds(overlay_df,       sprintf("data/data_july2023/ghsl_%s/overlays_%s.rds", ghsl, ghsl))
  # readr::write_rds(overlay_polygons, sprintf("data/sample3/ghsl_%s/overlays_polygons_%s.rds", ghsl, ghsl))
  # readr::write_rds(overlay_lines,    sprintf("data/sample3/ghsl_%s/overlays_lines_%s.rds", ghsl, ghsl))
  
  return("a")
  
}

# apply to all cities
cities_available <- unique(indicators_all$hdc)
walk(cities_available, prep_overlays)
lapply <- lapply(cities_available, safely(prep_overlays))

prep_overlays("01406")



