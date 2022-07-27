library(rvest)



osm_levels <- read_html("https://wiki.openstreetmap.org/wiki/Tag:boundary%3Dadministrative#10_admin_level_values_for_specific_countries")


osm_levels_filter <- osm_levels %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/div[3]/small/table') %>%
  html_table()



table <- osm_levels_filter[[1]]


# make colnames
colnames_table <- c("country", paste0("admin_level", seq(3, 10, 1)), "out")


colnames(table) <- colnames_table

table_filter <- table %>%
  slice(-1)
  