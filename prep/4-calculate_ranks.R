library(sf)
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(googlesheets4)

# indicators
indicators_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/13LZoiy0RcQ_ivc8SOoiU9ctHq5GQY48dpNbCpU9GzKk/edit#gid=0",
                               sheet = "Indicators")
# indicators to select
ind_to_select <- paste0(indicators_sheet$indicator_type, "_", indicators_sheet$indicator_code, "_")

# open overall data
world <- dir("data/data_final", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{5}.rds")
data_world <- lapply(world, function(x) st_set_geometry(read_rds(x), NULL)) %>% rbindlist(fill = TRUE)
data_world <- data_world %>% mutate(admin_level = as.numeric(admin_level)) %>%
  mutate(across(9:last_col(), as.numeric)) %>%
  # filter only the essential indicators
  dplyr::select(hdc, country, a3, osmid, name, admin_level, admin_level_ordered, admin_level_name,
                # starts_with("city_popdensity_"),
                # starts_with("city_blockdensity_"),
                # starts_with("bike_pnpb_"),
                # starts_with("walk_pns_"),
                # starts_with("walk_pncf_"),
                # starts_with("walk_pnnhighways_"),
                # starts_with("transit_pnft_"),
                # starts_with("transit_pnrt_"),
                # starts_with("transit_pnst_")
                starts_with(ind_to_select)
                
                )
  
  
  
  
  # create ranks for countries -------------------------------
# ind <- "transit_pnft"
# ind <- "bike_pnpb"
ranks_countries <- function(ind ) {
  
  if(file.exists(sprintf("data/data_final/countries/atlas_regions_%s.rds",
                         ind))) {
    
    
    atlas_country <- readRDS(sprintf("data/data_final/countries/atlas_regions_%s.rds",
                                     ind))
    
    # remove the world
    atlas_country <- atlas_country %>% filter(name != "The World")
    
    # calculate size of each group
    country_ranks <- atlas_country %>%
      select(a3, name, region_type, starts_with(sprintf("%s_", ind))) %>%
      # need to put the year in long format
      tidyr::pivot_longer(starts_with(sprintf("%s_", ind)),
                          names_to = c(".value", "year"),
                          names_pattern = "(.*)_(\\d{4}$)") %>%
      st_set_geometry(NULL) %>%
      # proble with n/a
      mutate(across(4, ~ ifelse(.x == "n/a", NA, .x))) %>%
      mutate(across(4, ~ as.numeric(.x))) %>%
      
      group_by(year, region_type) %>%  
      mutate(across(3:last_col(), ~rank(-.x, ties = "first", na.last = "keep"))) %>%
      arrange(across(c(region_type, year,5))) %>%
      ungroup() %>%
      # back to the wide format
      tidyr::pivot_wider(names_from = year,
                         names_glue = "{.value}_{year}",
                         values_from = starts_with(sprintf("%s", ind))) %>%
      # create totals - NEED FIX
      group_by(region_type) %>%
      mutate(n = n()) %>%
      ungroup()
    
    readr::write_rds(country_ranks, sprintf("data/data_final/countries/ranks/atlas_country_rank_%s.rds", ind))
    
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
dir.create('data/data_final/countries/ranks')
purrr::walk(ind_list, ranks_countries)



# ranks!!! ----------------------------------------------------------------


# in the world
rank_world <- data_world %>%
  # somente o ultimo nivel (geralmente de jurisdiction) nao va ser passivel de comparacao com o resto do mundo
  filter(admin_level < 10) %>%
  # delete indicators that are NA
  group_by(admin_level) %>%
  # calculate size of each group
  mutate(across(8:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
  # create totals - NEED FIX
  mutate(n = n()) %>%
  mutate(type_rank = "world") %>%
  ungroup()

# pre-compute country ranks once (avoids recomputing per city for cities sharing the same country)
country_ranks_cache <- data_world %>%
  filter(admin_level < 10) %>%
  group_by(country, admin_level) %>%
  mutate(across(8:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
  mutate(n = n()) %>%
  mutate(type_rank = "country") %>%
  ungroup()

# pre-compute indicator metadata lookup (avoids repeated subset() inside the inner loop)
indicators_meta <- indicators_sheet %>%
  mutate(ind1 = paste0(tolower(indicator_type), "_", indicator_code)) %>%
  select(ind1, indicator_name, indicator_unit, indicator_transformation) %>%
  split(.$ind1)


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
  # ghsl <- "05472"
  # ghsl <- cities_available[16]

  # calculate ranks for admin level 8 (cities for fortaleza - test)
  # compare to: other cities in the world, in the country, in the metro

  dir.create(sprintf("data/data_final/ghsl_%s/ranks", ghsl))

  data <- data_world %>% filter(hdc == ghsl)

  # reuse pre-computed world ranks (type_rank already set to "world")
  rank_hdc_world <- rank_world

  # reuse pre-computed country ranks instead of recomputing per city
  rank_hdc_country <- country_ranks_cache %>%
    filter(country == unique(data$country))

  # metro
  rank_hdc_metro <- data %>%
    filter(admin_level != 0) %>%
    group_by(admin_level) %>%
    # calculate size of each grou
    mutate(across(8:last_col(), ~rank(-.x, ties = "first", na.last = "keep"), .names = "rank_{.col}")) %>%
    # create totals - NEED FIX
    mutate(n = n()) %>%
    # create type of rank
    mutate(type_rank = "metro")

  # bind the comparions
  rank_complete <- rbind(rank_hdc_world, rank_hdc_country, rank_hdc_metro)

  # extract the agglomeration name once (reused across all levels and indicators)
  rank_complete <- rank_complete %>%
    group_by(hdc) %>%
    mutate(agglomeration = unique(name[admin_level == 0])) %>%
    ungroup()

  # compute available indicators once per city (not once per level)
  data1 <- janitor::remove_empty(data, which = "cols")
  ind_list <- unique(gsub("(.*)_(\\d{4}$)", "\\1", colnames(data1)[9:ncol(data1)]))

  # level <- "Agglomeration"
  # level <- 4
  # level <- 10
  # level <- 6
  # level <- 8

  # save by osm level
  filter_by_level <- function(level) {

    rank_complete_level <- rank_complete %>% filter(admin_level == level)

    # ind <- "bike_pnpb"
    # ind <- "city_popdensity"
    # ind <- "transit_pnrtall"
    # ind <- "walk_pns"
    # ind <- "bike_abikeways"
    # ind <- "walk_pnh"
    # ind <- "walk_pncf"
    # ind <- "bike_bikeshare"
    # ind <- "city_pnnhighways"
    # ind <- "transit_pnrt"

    filter_by_ind <- function(ind) {

      filter_compare <- if(level == 0) c("world", "country") else if (level <= 10) c("country", "metro") else c("metro")

      rank_complete_level1 <- rank_complete_level %>% filter(type_rank %in% filter_compare)


      # create the rankins for the hdc only / filter the indicator
      rank_complete_level_ind <- rank_complete_level1 %>%
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
        select(rank, n, type_rank, year, osmid)

        # # create text
        # mutate(text = sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
        #                       rank, n, type_rank))

      # get the indicators transformations from pre-computed lookup
      meta <- indicators_meta[[ind]]
      format_indicator_unit    <- meta$indicator_unit
      indicator_transformation <- meta$indicator_transformation


      # create the full list of rankings
      rank_complete_level_ind_full <- rank_complete_level1 %>%
        dplyr::select(hdc, country, a3, osmid, name, agglomeration, admin_level, admin_level_ordered, n, type_rank, starts_with(paste0("rank_", ind)), starts_with(ind)) %>%
        # rename the indicators ranks/values to account for the years
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), ""), starts_with("rank")) %>%
        rename_with(~stringr::str_replace(.x, paste0(ind, "_"), "value_"), starts_with(ind)) %>%
        # to long format
        tidyr::pivot_longer(cols = 11:last_col(),
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
                                 indicator_transformation %in% "round1" ~ as.character(round(value, 1)),
                                 TRUE ~ as.character(round(value)))) %>%
        # mutate(value = paste0(value, format_indicator_unit)) %>%
        # create rank value
        group_by(type_rank, year) %>%
        mutate(n = 1:n()) %>%
        ungroup() %>%
        mutate(format_indicator_unit = format_indicator_unit) %>%
        select(n, name, agglomeration, format_indicator_unit, value, year, type_rank, osmid)



      # save
      write_rds(rank_complete_level_ind, sprintf("data/data_final/ghsl_%s/ranks/ranks_%s_%s_%s.rds", ghsl, ghsl, level, ind))

      # save the full list html code for this indicator
      write_rds(rank_complete_level_ind_full, sprintf("data/data_final/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", ghsl, ghsl, level, ind))

    }

    # apply to every indicator
    purrr::walk(ind_list, filter_by_ind)

    # uiui <- lapply(ind_list, possibly(filter_by_ind, otherwise = "error"))

  }

  levels <- unique(data$admin_level)
  # apply to every level
  purrr::walk(levels, filter_by_level)

  return("ok")

  # uiui <- lapply(levels, possibly(filter_by_level, otherwise = "error"))


}

# apply to every city
cities_available <- unique(data_world$hdc)

library(furrr)
plan(multisession, workers = availableCores() - 1)
# furrr::future_walk(cities_available, purrr::possibly(prep_data, "error"), .options = furrr_options(seed = TRUE))
ui <- lapply(cities_available, purrr::possibly(prep_data, "error"))


# prep_data("01406")
# prep_data("01361")
# prep_data("05472") # jakarta
# prep_data("01156") # trujillo
# prep_data("01289") # lagos


# prep_data("01406")
# prep_data("01361")
# prep_data("05472") # jakarta
# prep_data("01156") # trujillo
# prep_data("01289") # lagos


