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


indicators_all <- purrr::map_dfr(dir("data/data_beta", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)


prep_overlays <- function(ghsl) {
  
  # ghsl <- "01406"
  # ghsl <- "01165"
  # ghsl <- "00010"
  # ghsl <- "00456"
  
  # base_dir <- sprintf("data-raw/atlas_data_july_31/cities_out/ghsl_region_%s/", ghsl)
  # base_dir <- sprintf("data-raw/atlas_data_july_31/cities_out/ghsl_region_%s/", ghsl)
  
  start <- Sys.time()
  
  base_dir <- sprintf("data-raw/data_beta/cities/ghsl_region_%s/", ghsl)
  # # change name of folder h+s
  # folder_hs <- dir(paste0(base_dir, "geodata")
  #                  , pattern = "h\\+s"
  #                  ,full.names = TRUE
  # )
  # folder_hs_new <- paste0(paste0(base_dir, "geodata/hs"))
  # # rename
  # walk2(folder_hs, folder_hs_new, file.rename)
  # files_hs <- dir(paste0(base_dir, "geodata/hs")
  #                 , pattern = ".geojson"
  #                 ,full.names = TRUE
  # )
  # files_hs_new <- stringr::str_remove(files_hs, "\\+")
  # walk2(files_hs, files_hs_new, file.rename)
  
  # overlay files ------------------------------------
  overlay_files <- dir(paste0(base_dir, "geodata"), full.names = TRUE, pattern = "(.geojson|.tif)$", recursive = TRUE)
  overlay_files <- overlay_files[overlay_files %like% paste(overlay_table$overlay, collapse  = "|")]
  
  
  
  save_overlay <- function(file) {
    # file <- overlay_files[[103]]
    # file <- overlay_files[[10]]
    # file <- overlay_files[overlay_files %like% "block"]
    # file <- overlay_files[overlay_files %like% "population"]
    # file <- overlay_files[overlay_files %like% "grid_pop_evaluated"]
    
    # open file
    if (file %like% ".tif$") {
      
      a <- stars::read_stars(file)
      
    } else {
      
      a <- st_read(file, quiet = TRUE)
      
      
    }
    # extract year
    year1 <- if (file %like% "rapid_transit") sub("(^.*)/(rapid_transit/)(\\d{4})/(.*$)", "\\3", file)   else  sub("(^.*)(\\d{4})((.geojson|.tif)$)", "\\2", basename(file))
    # extract  name
    ind <- basename(file)
    ind <-  if (file %like% "rapid_transit") sub(pattern = "(.geojson|.tif)", replacement = "", x = ind) else sub(pattern = "_\\d{4}(.geojson|.tif)", replacement = "", x = ind)
    # save
    dir.create(sprintf("data/data_beta/ghsl_%s/overlays/%s", ghsl, ind), recursive = TRUE)
    
    # format
    if (file %like% "population") {
      
      readr::write_rds(a, sprintf("data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.rds", ghsl, ind,  ind, ghsl, year1))
      
    } else if (file %like% "block_densities|grid_pop_evaluated") {
      
      # convert to raster
      if (file %like% "block_densities") {
        
      a <- stars::st_rasterize(a %>% select(density))
        
      } else if (file %like% "grid_pop_evaluated") {
        
      a <- stars::st_rasterize(a %>% select(journey_gap_unweighted))
        
      }
      
      readr::write_rds(a, sprintf("data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.rds", ghsl, ind,  ind, ghsl, year1))
     
      
      } else {
      
      out <- sprintf("data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year1)
      if (file.exists(out)) {
        
        file.remove(out)
        
      }
      
      a <- a %>% mutate(a = "teste")  
      
      st_write(a, sprintf("data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.fgb", ghsl, ind,  ind, ghsl, year1))
      
    }
    
    
  }
  
  
  message("done for ", ghsl)
  # overlay_files <- overlay_files[overlay_files %like% "grid_pop_evaluated"]
  walk(overlay_files, save_overlay)
  message("time", round(Sys.time() - start), 2)
}


cities_available <- unique(indicators_all$hdc)
library(furrr)
plan(multisession)
future_walk(cities_available, prep_overlays)
walk(cities_available, prep_overlays)

prep_overlays("00456")
prep_overlays("00574")
prep_overlays("04608")
prep_overlays("10076")
prep_overlays("13039")

# # evaluate which cities are left
# left <- dir("data/data_beta", recursive = TRUE, full.names = TRUE)
# left <- left[left %like% "overlays"]
# left <- stringr::str_extract(left, "\\d{5}") %>% unique()
# left <- setdiff(cities_available, left)
# 
# library(furrr)
# plan(multisession)
# future_walk(left, prep_overlays)
