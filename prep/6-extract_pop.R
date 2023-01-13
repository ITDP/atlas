library(sf)
library(dplyr)
library(mapview)
library(raster)



extract_pop <- function(ghsl) {
  
  # ghsl <- "00200"
  # ghsl <- "00561"
  # ghsl <- "01445"
  # ghsl <- "00021"
  
  # salvar por ano
  
  save_year <- function(year) {
    
    # year <- 2020
    
    a <- raster(sprintf("data-raw/data_nov18/city_results/ghsl_region_%s/geodata/population/pop_%s.tif", ghsl, year))
    
    dir.create(sprintf("data/sample5/ghsl_%s/overlays/population", ghsl))
    
    readr::write_rds(a, sprintf("data/sample5/ghsl_%s/overlays/population/overlay_population_%s_%s.rds", ghsl, ghsl, year))
  }
  
  # aplly years
  years_all <- dir(sprintf("data-raw/data_nov18/city_results/ghsl_region_%s/geodata/population", ghsl))
  years_all <- stringr::str_extract(years_all, "\\d{4}")
  purrr::walk(years_all, save_year)
  
  
}

# apply to every city
cities <- stringr::str_extract(dir("data-raw/data_nov18/city_results"), "\\d{5}")
purrr::walk(cities, extract_pop)
