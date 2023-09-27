library(sf)
library(dplyr)
library(mapview)
library(leaflet)
library(data.table)
library(Hmisc)
library(purrr)
sf::sf_use_s2(FALSE)

# save indicators by each city by each admin level - for comparison --------

indicators_all <- purrr::map_dfr(dir("data/data_july2023", pattern = "^indicators_\\d{5}", full.names = TRUE, recursive = TRUE),
                                 readr::read_rds)

# remove polygon
indicators_all_df <- indicators_all %>% st_set_geometry(NULL) %>%
  # filter only the essential indicators
  dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered,
                # starts_with("city_poptotal"),
                starts_with("city_popdensity_"),
                starts_with("city_blockdensity_"),
                starts_with("city_journeygap_"),
                starts_with("bike_pnpb_"),
                starts_with("walk_pns_"),
                starts_with("walk_pncf_"),
                starts_with("walk_pnnhighways_"),
                starts_with("transit_pnft_"),
                starts_with("transit_pnrtall_"))

# ghsl <- "0634"
# ghsl <- "01406"

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
    
    dir.create(sprintf("data/data_july2023/ghsl_%s/indicators_compare",
                       ghsl))
    
    # save
    readr::write_rds(indicators_ind, sprintf("data/data_july2023/ghsl_%s/indicators_compare/indicators_compare_%s_%s.rds",
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


# export only for comparison
# ghsl <- "1406"
# ind <- "bike_pnpb"
# level <- 10

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
    readr::write_rds(indicators_ind, sprintf("data/data_july2023/comp/indicators_compare_%s_%s.rds",
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
purrr::walk(unique(indicators_all_df$admin_level), export_comparison1)







# # do the same thing for the countries! ------------------------------------
# 
# 
# # save indicators by each city by each admin level - for comparison --------
# 
# # open data
# indicators_all <- readRDS("data/data_july2023/atlas_country_polygons.rds")
# 
# # remove countties without data
# # indicators_all <- indicators_all %>% dplyr::filter(!is.na(bike_pnpb_2022))
# 
# # remove polygon
# indicators_all_df <- indicators_all %>% st_set_geometry(NULL)
# 
# # country_code <- "BRA"
# # ind <- "city_poptotal"
# 
# save_ind <- function(ind) {
#   
#   indicators_ind <- indicators_all_df %>% 
#     select(a3, name, 
#            starts_with(ind))
# 
#   
#   # save
#   readr::write_rds(indicators_ind, sprintf("data/data_july2023/indicators_compare_country/indicators_compare_country_%s.rds",
#                                            ind))
#   
#   
# }
# # to long format
# colnames_compare <- colnames(indicators)[3:ncol(indicators)]
# # extract year
# years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
#                       replacement = "\\1",
#                       x = colnames_compare)
# ind_list <- unique(years_compare)
# # apply
# purrr::walk(ind_list, save_ind)
# 
# 
# 
