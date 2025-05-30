library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
library(googlesheets4)
sf::sf_use_s2(FALSE)

indicators_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
                               sheet = "Indicators")

# save indicators by each city by each admin level - for comparison --------

indicators_all <- purrr::map_dfr(dir("data/data_final", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)
# indicators to select
ind_to_select <- paste0(indicators_sheet$indicator_type, "_", indicators_sheet$indicator_code, "_")

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL) %>%
  # filter only the essential indicators
  dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered,
                # starts_with("city_popdensity_"),
                # starts_with("city_blockdensity_"),
                # starts_with("city_journeygap_"),
                # starts_with("bike_pnpb_"),
                # starts_with("walk_pns_"),
                # starts_with("walk_pncf_"),
                # starts_with("walk_pnnhighways_"),
                # starts_with("transit_pnft_"),
                # starts_with("transit_pnrt_"),
                # starts_with("transit_pnst")
                matches(ind_to_select)
                )

# ghsl <- "0634"
# ghsl <- "08154"

export_by_osmid <- function(ghsl) {
  
  indicators <- indicators_all_df %>% dplyr::filter(hdc == ghsl)
  
  # save by each admin
  # ind <- "city_poptotal"
  # ind <- "bike_pnpb"
  save_ind <- function(ind) {
    
    indicators_ind <- indicators %>% 
      dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered,
             starts_with(ind))
    # to long format
    colnames_compare <- colnames(indicators_ind)[8:ncol(indicators_ind)]
    # extract year
    years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                          replacement = "\\2",
                          x = colnames_compare)
    
    colnames(indicators_ind) <- c("hdc", "country", "a3", "osmid", "name", "admin_level", "admin_level_ordered", 
                                  years_compare)
    
    indicators_ind <- tidyr::pivot_longer(indicators_ind,
                                          cols = matches("\\d{4}"),
                                          names_to = "year",
                                          values_to = "value")
    
    dir.create(sprintf("data/data_final/ghsl_%s/indicators_compare",
                       ghsl))
    
    # save
    readr::write_rds(indicators_ind, sprintf("data/data_final/ghsl_%s/indicators_compare/indicators_compare_%s_%s.rds",
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

# apply to all cities
cities_available <- unique(indicators_all$hdc)
purrr::walk(cities_available, export_by_osmid)

# export_by_osmid("01156") # trujillo
# export_by_osmid("05472") # jakarta

# export only for comparison
# ghsl <- "08154"
# ind <- "bike_pnpb"
# level <- 8

export_comparison1 <- function(level) {
  
  
  # filter levels
  indicators_all_level <- indicators_all_df %>%
    dplyr::filter(admin_level  == level)
  
  export_comparison <- function(ind) {
    
    
    indicators_ind <- indicators_all_level %>% 
      dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered,
             starts_with(ind)) %>%
      mutate(admin_level = as.integer(admin_level))
    # delete the last level
    # filter(admin_level < 10)
    
    
    # to long format
    colnames_compare <- colnames(indicators_ind)[8:ncol(indicators_ind)]
    # extract year
    years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                          replacement = "\\2",
                          x = colnames_compare)
    
    colnames(indicators_ind) <- c("hdc", "country", "a3", "osmid", "name", "admin_level", "admin_level_ordered", 
                                  years_compare)
    
    indicators_ind <- tidyr::pivot_longer(indicators_ind,
                                          cols = matches("\\d{4}"),
                                          names_to = "year",
                                          values_to = "value")
    
    # save
    readr::write_rds(indicators_ind, sprintf("data/data_final/comp/indicators_compare_%s_%s.rds",
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
dir.create('data/data_final/comp')
purrr::walk(unique(indicators_all_df$admin_level), export_comparison1)





