library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
library(stars)
sf::sf_use_s2(FALSE)

folder <- "/media/kauebraga/data/pedestriansfirst"

# start by getting the overlays table ---------------------------------------------------------

library(googlesheets4)
overlay_table <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/194T-zZZRhwAAvon7lOeyjT49jzij3SKhDM-2HyBi3Hc/edit?usp=sharing") %>%
  mutate(overlay = basename(overlay_dir)) %>%
  mutate(overlay = sub(pattern = ".geojson", replacement = "", x = overlay)) %>%
  mutate(overlay = sub(pattern = ".tif", replacement = "", x = overlay))


indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# to export to .geojson, we will not export several rapid transit files



prep_overlays1 <- function(ghsl, year) {
  
  # ghsl <- "01406"
  # ghsl <- "01165"
  # ghsl <- "00017"
  # ghsl <- "05402"; year = 2023
  
  # base_dir <- sprintf("data-raw/atlas_data_july_31/cities_out/ghsl_region_%s/", ghsl)
  # base_dir <- sprintf("data-raw/atlas_data_july_31/cities_out/ghsl_region_%s/", ghsl)
  
  base_dir <- sprintf("%s/cities_out/ghsl_region_%s/", folder, ghsl)
  dir.create(sprintf("data/data_final/ghsl_%s/overlays/temp", ghsl), recursive = TRUE)
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = "(.geojson|.tif)$", recursive = TRUE)
  overlay_files <- overlay_files[overlay_files %like% paste(overlay_table$overlay, collapse  = "|")]
  # filter year
  overlay_files <- overlay_files[overlay_files %like% sprintf("_%s.", year)]
  
  # we will include population for 2025, which will be the proxy for 2023/2024
  overlay_files <- c(overlay_files, sprintf("%s/geodata/population/pop_2025.tif", base_dir))
  
  # save overlay for each indicator
  save_overlay <- function(file) {
    # file <- overlay_files[overlay_files %like% "allhwys_latlon_2024"]
    # file <- overlay_files[overlay_files %like% "rapid_transit"][1]

    print(file)
    
    # year1 <- if (file %like% "rapid_transit") sub("(^.*)/(rapid_transit/)(\\d{4})/(.*$)", "\\3", file)   else  sub("(^.*)(\\d{4})((.geojson|.tif)$)", "\\2", basename(file))
    # extract  name
    ind <- basename(file)
    ind <-  if (file %like% "rapid_transit") sub(pattern = "(.geojson|.tif)", replacement = "", x = ind) else sub(pattern = "_\\d{4}(.geojson|.tif)", replacement = "", x = ind)
    
    # extract the format
    format <- overlay_table %>%
      filter(overlay == ind) %>%
      pull(format) %>% unique()
    
    # format
    if (file %like% "population") {
      
      
      dir.create(sprintf("data/data_final/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
      
      out_name1 <- sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.tif", ghsl, ind, ghsl, year)
      out_name2 <- sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.tif", ghsl, ind, ind, ghsl, year)
      file.copy(from = file,
                out_name1)
      file.copy(from = file,
                out_name2)
      
      # if (year == 2025) {
      #   
      #   # duplicate 2025 to 2024
      #   file.copy(from = out_name2,
      #             to = sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.tif", ghsl, ind, ind, ghsl, '2023'))
      #   
      # }
      
      
      
    } else if(file %like% "block_densities_latlon") {
      
      dir.create(sprintf("data/data_final/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
      a <- st_read(file)
      a <- a %>% dplyr::filter(density > 0)
      a <- stars::st_rasterize(a %>% select(density))
      # mapview(a)
      stars::write_stars(a, sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.tif", ghsl, ind, ind, ghsl, year))
      
      
      
      
      
      
    } else if(format == "fgb") {
      
      dir.create(sprintf("data/data_final/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
      
      a <- st_read(file)
      
      # export fgb
      out12 <- sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)
      out <- sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)
      out1 <- sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl,  ind, ghsl, year)
      if (file.exists(out12)) {
        
        file.remove(out)
        file.remove(out12)
        file.remove(out1)
      }
      
      a <- a %>% mutate(a = 1)
      
      file.remove(sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year))
      file.remove(sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year))
      
      try(st_write(a, sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)))
      
      # export the geojson (to go to the .zip)
      if (!(file %in% c("brt_lines_ll", "brt_stations_ll", "lrt_lines_ll", "lrt_stations_ll", "mrt_lines_ll", "mrt_stations_ll"))) {
        
        st_write(a, sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year),
                 append = FALSE)
        
      }
      
      # a <- a %>% mutate(a = 1)
      
      # st_write(a, sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year1))
      # st_write(a, sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year1),
      #          append = FALSE)
      
    } 
    
    
  }
  
  
  message("done for ", ghsl)
  walk(overlay_files, save_overlay)
  
  return("ok")
}

cities_available <- unique(indicators_all$hdc)

purrr::walk(cities_available, prep_overlays1, year = 2023)
purrr::walk(cities_available, prep_overlays1, year = 2024)

results <- purrr::map(cities_available, possibly(prep_overlays1, otherwise = "erro"), year = 2024)





# export PNRT separately  -------------------------------------------------


# ghsl <- "00039"
prep_overlays_pnrt <- function(ghsl) {
  
  
  base_dir <- sprintf("%s/cities_out/ghsl_region_%s/", folder, ghsl)
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata/rapid_transit"), full.names = TRUE, pattern = "(.geojson|.tif)$", recursive = TRUE)
  overlay_files <- overlay_files[overlay_files %like% paste(overlay_table$overlay, collapse  = "|")]
  
  # save overlay for each indicator
  save_overlay <- function(file) {
    # file <- overlay_files[overlay_files %like% "rapid_transit"][66]
    
    # extract year
    year <- sub("(^.*)/(rapid_transit/)(\\d{4})/(.*$)", "\\3", file)
    # extract  name
    ind <- basename(file)
    ind <-  if (file %like% "rapid_transit") sub(pattern = "(.geojson|.tif)", replacement = "", x = ind) else sub(pattern = "_\\d{4}(.geojson|.tif)", replacement = "", x = ind)
    
    # extract the format
    format <- overlay_table %>%
      filter(overlay == ind) %>%
      pull(format) %>% unique()
    
    # format
    if(format == "fgb") {
      
      dir.create(sprintf("data/data_final/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
      
      a <- st_read(file)
      
      # export fgb
      out12 <- sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)
      out <- sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)
      out1 <- sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl,  ind, ghsl, year)
      if (file.exists(out12)) {
        
        file.remove(out)
        file.remove(out12)
        file.remove(out1)
      }
      
      a <- a %>% mutate(a = 1)
      
      file.remove(sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year))
      file.remove(sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year))
      
      try(st_write(a, sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year)))
      
      # export the geojson (to go to the .zip)
      if (!(file %in% c("brt_lines_ll", "brt_stations_ll", "lrt_lines_ll", "lrt_stations_ll", "mrt_lines_ll", "mrt_stations_ll"))) {
        
        st_write(a, sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year),
                 append = FALSE)
        
      }
      
      # a <- a %>% mutate(a = 1)
      
      # st_write(a, sprintf("data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year1))
      # st_write(a, sprintf("data/data_final/ghsl_%s/overlays/temp/%s_%s_%s.geojson", ghsl, ind, ghsl, year1),
      #          append = FALSE)
      
    } 
    
    
  }
  
  
  message("done for ", ghsl)
  walk(overlay_files, save_overlay)
  
  return("ok")
}




cities_available <- unique(indicators_all$hdc)

results <- purrr::map(cities_available, possibly(prep_overlays_pnrt, otherwise = "erro"))







# evaluate which cities are left
left <- dir("data/data_final", recursive = TRUE, full.names = TRUE)
left <- left[left %like% "overlays"]
# remove
walk(left, file.remove)

left <- stringr::str_extract(left, "\\d{5}") %>% unique()
left <- setdiff(cities_available, left)
# 
# library(furrr)
# plan(multisession)
# future_walk(left, prep_overlays)


# old <- dir("data/data_final", pattern = "pnrtall", recursive = TRUE, full.names = TRUE)
# new <- stringr::str_replace(old, pattern = "pnrtall", replacement = "pnrt")
# 
# purrr::walk2(old, new, file.rename)



# zip overlays files to share (download) ------------------------------------------------------

list_availability <- readRDS("data/data_final/list_availability.rds")


# ghsl <- "08154"; over <- "pnpb"; year1 <- 2023

zip_pop <- function(ghsl) {
  
  
  
  dir1 <- function(pattern, ...) {
    
    dir(pattern = pattern, ...)
  }
  
  # # zip pop separately
  # file <- lapply(sprintf("%s_%s", "pop", ghsl), 
  #                dir1, 
  #                path = sprintf("data/data_final/ghsl_%s/overlays/temp", ghsl), full.names = TRUE) 
  # file <- do.call(c, file)
  # 
  # # zip those files
  # zip::zip(zipfile = sprintf("data/data_final/ghsl_%s/overlays/%s_%s.zip", ghsl, "pop", ghsl), files = file,
  #          mode = "cherry-pick")
  
  # zip pop separately
  file <- lapply(sprintf("%s_%s", "block_densities_latlon", ghsl), 
                 dir1, 
                 path = sprintf("data/data_final/ghsl_%s/overlays", ghsl), full.names = TRUE,
                 recursive = TRUE) 
  file <- do.call(c, file)
  
  # zip those files
  zip::zip(zipfile = sprintf("data/data_final/ghsl_%s/overlays/%s_%s.zip", ghsl, "blockdensity", ghsl), files = file,
           mode = "cherry-pick")
  
}

cities_available <- unique(indicators_all$hdc)
library(furrr)
plan(multisession)
furrr::future_walk(cities_available, zip_pop)


fim <- purrr::map(oi, possibly(zip_pop, otherwise = "buhh"), .progress = TRUE)


# ghsl <- "08154"; over <- "pnpb"

process_overlay <- function(over, ghsl) {
  
  dir1 <- function(pattern, ...) {
    
    dir(pattern = pattern, ...)
  }
  
  # remove pop
  if (over != "popdensity") {
    
    overlay_subset <- subset(overlay_table, indicator == over & overlay != "pop")
    
  } else {
    
    overlay_subset <- subset(overlay_table, indicator == over)
    
  }
  
  file <- lapply(sprintf("%s_%s", overlay_subset$overlay, ghsl), 
                 dir1, 
                 path = sprintf("data/data_final/ghsl_%s/overlays/temp", ghsl), full.names = TRUE) 
  file <- do.call(c, file)
  
  # zip those files
  zip::zip(zipfile = sprintf("data/data_final/ghsl_%s/overlays/%s_%s.zip", ghsl, over, ghsl), files = file,
           mode = "cherry-pick")
  
  
}

# create combinations of ghsl and overlay
combinations <- expand.grid(cities_available, unique(overlay_table$indicator))
# combinations <- filter(combinations, Var1 == "99999")

fim <- purrr::walk2(combinations$Var2, combinations$Var1, possibly(process_overlay, otherwise = "buhh"), .progress = TRUE)


# remove temp folder?

