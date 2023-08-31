library(sf)
library(dplyr)
library(data.table)
library(readr)
library(googlesheets4)

# indicators
indicators_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
                               sheet = "Indicators")

# open overall data
world <- dir("data/data_july2023", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{5}.rds")
data_world <- lapply(world, function(x) st_set_geometry(read_rds(x), NULL)) %>% rbindlist(fill = TRUE)
data_world <- data_world %>% mutate(admin_level = as.numeric(admin_level)) %>%
  mutate(across(9:last_col(), as.numeric)) %>%
  # filter only the essential indicators
  dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name,
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
  
  
  
  
  # create ranks for countries -------------------------------
# ind <- "bike_pnpb"
ranks_countries <- function(ind ) {
  
  if(file.exists(sprintf("data/data_july2023/countries/atlas_country_%s.rds",
                         ind))) {
    
    
    atlas_country <- readRDS(sprintf("data/data_july2023/countries/atlas_country_%s.rds",
                                     ind))
    
    # calculate size of each group
    country_ranks <- atlas_country %>%
      select(a3, name, starts_with(sprintf("%s_", ind))) %>%
      st_set_geometry(NULL) %>%
      mutate(across(3:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
      arrange(across(3)) %>%
      # create totals - NEED FIX
      mutate(n = n()) 
    
    readr::write_rds(country_ranks, sprintf("data/data_july2023/countries/ranks/atlas_country_rank_%s.rds", ind))
    
  }
  
}

# to long format
colnames_compare <- colnames(data_world)[9:ncol(data_world)]
# extract year
years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                      replacement = "\\1",
                      x = colnames_compare)
ind_list <- unique(years_compare)
# apply
purrr::walk(ind_list, ranks_countries)



# ranks!!! ----------------------------------------------------------------


# in the world
rank_world <- data_world %>%
  # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
  filter(admin_level < 10) %>%
  # delete indicators that are NA
  group_by(admin_level) %>%
  # calculate size of each group
  mutate(across(9:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
  # create totals - NEED FIX
  mutate(n = n()) %>%
  mutate(type_rank = "world") %>%
  ungroup()




prep_data <- function(ghsl) {
  # ghsl <- 5134
  # ghsl <- "01406"
  # ghsl <- 1022
  # ghsl <- "0621"
  # ghsl <- "1445" # recife
  # ghsl <- "01445" # recife
  # ghsl <- "0634"
  # ghsl <- "12080"
  # ghsl <- "00021"
  # ghsl <- "01105"
  # ghsl <- "01361"
  # ghsl <- "00010"
  
  # calculate ranks for admin level 8 (cities for fortaleza - test)
  # compare to: other cities in the world, in the country, in the metro
  
  dir.create(sprintf("data/data_july2023/ghsl_%s/ranks", ghsl))
  
  data <- data_world %>% filter(hdc == ghsl)
  
  rank_hdc_world <- rank_world %>%
    # filter(hdc == ghsl) %>%
    mutate(type_rank = "world")
  
  # in the country
  rank_hdc_country <- data_world %>%
    # filter only the coyuntry
    filter(country == unique(data$country)) %>%
    # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
    filter(admin_level < 10) %>%
    # delete indicators that are NA
    group_by(admin_level) %>%
    # calculate size of each group
    mutate(across(9:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
    # create totals - NEED FIX
    mutate(n = n()) %>%
    mutate(type_rank = "country") %>%
    # filter(hdc == ghsl) %>%
    ungroup()
  
  # metro
  rank_hdc_metro <- data %>%
    filter(admin_level != 0) %>%
    group_by(admin_level) %>%
    # calculate size of each grou
    mutate(across(9:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
    # create totals - NEED FIX
    mutate(n = n()) %>%
    # create type of rank
    mutate(type_rank = "metro")
  
  # bind the comparions
  rank_complete <- rbind(rank_hdc_world, rank_hdc_country, rank_hdc_metro)
  
  # level <- "Agglomeration"
  # level <- 8
  # level <- 6
  
  # save by osm level
  filter_by_level <- function(level) {
    
    rank_complete_level <- rank_complete %>% filter(admin_level == level)
    
    # ind <- "bike_pnpb"
    # ind <- "city_poptotal"
    # ind <- "transit_pnrtmrt"
    # ind <- "walk_pns"
    # ind <- "bike_abikeways"
    # ind <- "walk_pnh"
    # ind <- "walk_pncf"
    # ind <- "bike_bikeshare"
    # ind <- "city_pnnhighways"
    
    filter_by_ind <- function(ind) {
      
      
      
      # create the rankins for the hdc only / filter the indicator
      rank_complete_level_ind <- rank_complete_level %>%
        filter(hdc == ghsl) %>%
        dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, n, type_rank, starts_with(paste0("rank_", ind)), starts_with(ind)) %>%
        # rename the indicators ranks/values to account for the years
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), ""), starts_with("rank")) %>%
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), "value_"), starts_with(ind)) %>%
        # to long format
        tidyr::pivot_longer(cols = 10:last_col(),
                            # cols = starts_with("rank_"),
                            names_sep = "_",
                            names_to = c("type1", "year"),
                            values_to = "count") %>%
        tidyr::pivot_wider(names_from = "type1",
                           values_from = "count",
                           names_glue = "{type1}") %>%
        # create text
        mutate(text = sprintf('<div class="text_compare" style="font-size: 14px";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                              rank, n, type_rank))
      
      # get the indicators transformations
      indicators_sheet <- indicators_sheet %>% mutate(ind1 = paste0(tolower(indicator_type), "_", indicator_code ))
      format_indicator_name <- subset(indicators_sheet, ind1 == ind)$indicator_name
      format_indicator_unit <- subset(indicators_sheet, ind1 == ind)$indicator_unit
      indicator_transformation <- subset(indicators_sheet, ind1 == ind)$indicator_transformation
      
      
      # create the full list of rankings
      rank_complete_level_ind_full <- rank_complete_level %>%
        dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, n, type_rank, starts_with(paste0("rank_", ind)), starts_with(ind)) %>%
        # rename the indicators ranks/values to account for the years
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), ""), starts_with("rank")) %>%
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), "value_"), starts_with(ind)) %>%
        # to long format
        tidyr::pivot_longer(cols = 10:last_col(),
                            # cols = starts_with("rank_"),
                            names_sep = "_",
                            names_to = c("type1", "year"),
                            values_to = "count") %>%
        tidyr::pivot_wider(names_from = "type1",
                           values_from = "count",
                           names_glue = "{type1}") %>%
        # create text
        arrange(type_rank, year, rank) %>%
        # format indicator value
        mutate(value = case_when(indicator_transformation %in% "percent" ~ as.character(round(value * 100)), 
                                 indicator_transformation %in% "thousands" & value >= 1000000 ~ scales::comma(value, accuracy = 0.1, scale = 0.000001, suffix = "M"),
                                 indicator_transformation %in% "thousands" & value < 1000000 ~ scales::comma(value, accuracy = 1, scale = 0.001, suffix = "k"),
                                 TRUE ~ as.character(round(value)))) %>%
        # mutate(value = paste0(value, format_indicator_unit)) %>%
        # create text
        group_by(type_rank, year) %>%
        mutate(text = sprintf("<div class = \"text_compare\" style = \"padding-bottom: 0px; padding-top: 0px; font-size: 14px\"><span style=\"font-size: 17px;\">%s </span>&nbsp;%s <span  style=\"float:right; font-size: 12px; color: #B1B5B9 \">&nbsp;%s</span><span style=\"float:right; font-size: 17px;\">&nbsp;%s</span></div>", 
                              1:n(), name, format_indicator_unit, value)) %>%
        ungroup() %>%
        group_by(admin_level, type_rank, year) %>%
        summarise(text = paste(
          text,
          collapse = "\n"
        )) %>%
        ungroup() %>%
        mutate(text = paste("<div id=\"\" style=\"overflow-y:scroll; height:150px;\">",
                            text,
                            "</div>",
                            sep = "\n"
        ))
      
      rank_complete_level_ind_full <- rank_complete_level_ind_full %>% mutate(text = purrr::map_chr(text, htmltools::HTML))
      
      
      # save
      write_rds(rank_complete_level_ind, sprintf("data/data_july2023/ghsl_%s/ranks/ranks_%s_%s_%s.rds", ghsl, ghsl, level, ind))
      
      # save the full list html code for this indicator
      write_rds(rank_complete_level_ind_full, sprintf("data/data_july2023/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", ghsl, ghsl, level, ind))
      
    }
    
    # see indicators availabilty
    data1 <- janitor::remove_empty(data, which = "cols")
    colnames_compare <- colnames(data1)[9:ncol(data1)]
    # extract year
    years_compare <- gsub(pattern = "(.*)_(\\d{4}$)",
                          replacement = "\\1",
                          x = colnames_compare)
    # years_compare <- years_compare[-length(years_compare)]
    ind_list <- unique(years_compare)
    # apply to every indicator
    purrr::walk(ind_list, filter_by_ind)
    
    # uiui <- lapply(ind_list, possibly(filter_by_ind, otherwise = "error"))
    
  }
  
  levels <- unique(data$admin_level)
  # apply to every indicator
  purrr::walk(levels, filter_by_level)
  
  # uiui <- lapply(levels, possibly(filter_by_level, otherwise = "error"))
  
  
}

# apply to every city
cities_available <- unique(data_world$hdc)
purrr::walk(cities_available, 
            prep_data)
# results <- lapply(cities_available, 
            # possibly(prep_data, "error"))

# prep_data("01406")
# prep_data("01361")


