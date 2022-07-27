library(sf)
library(dplyr)
library(data.table)
library(readr)





world <- dir("data/sample2_prep", recursive = TRUE, full.names = TRUE, pattern = "indicators_\\d{4}.rds")
data_world <- lapply(world, read_rds) %>% rbindlist(fill = TRUE)


data_world <- tidyr::fill(data_world, hdc) %>% setDT()

spatial_levels <- unique(data_world, by = c("hdc", "admin_level"))

spatial_levels <- spatial_levels[, .(levels = .(admin_level)), by = hdc]

# export
readr::write_rds(spatial_levels, "data/spatial_levels.rds")
