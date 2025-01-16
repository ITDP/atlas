Sys.setenv(PATH=paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep=":"))
library(mapboxapi)
library(tools)
library(sf)
library(mapview)
library(dplyr)
library(data.table)
library(purrr)

mapboxapi::mb_access_token("sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNscjhjdmoydzJxd3Qya21zd2t5aHN0ZmoifQ.NcKleHf_-d4buaEmcTT_Lg")

# for 2024 - we are only running partially
hdcs = fread('../pedestriansfirst/input_data/hdc_to_run.csv', colClasses = c("character", "character", "character", "character", "logical"))

# 1) Run only the population data collection for the cities we already have data from the previous collection
hdcs_run = hdcs[run_again==TRUE]
hdcs_run = hdcs_run$hdc_new

# gather overlay from one city
files <- dir("../pedestriansfirst/cities_out",
             full.names = TRUE, recursive = TRUE)

# remove zip
files <- files[grepl("/geodata", files)]
files <- files[!grepl(".zip", files)]
files <- files[!grepl("/temp", files)]

# only 2024
files_2024 <- files[grepl("2024", files)]

# only the selected cities
files_2024 <- files_2024[grepl(paste0(hdcs_run, collapse = "|"), files_2024)]

# # rename
# files_hs <- files[grepl(pattern = "\\++s", x = files)]
# files_hs_new <- stringr::str_replace_all(files_hs, pattern = "h\\+s(?=_)", replacement = "hs")
# # rename
# purrr::map2(.x = files_hs, .y = files_hs_new, file.rename)

# save for each indicator
# ind <- "protectedbike_latlon"
# ind <- "hs_latlon"
# ind <- "healthcare_points_latlon"
# ind <- "schools_points_latlon"
# ind <- "healthcare_latlon"
# ind <- "schools_latlon"
# ind <- "carfree_latlon"
# ind <- "buffered_hwys_latlon"
# ind <- "allhwys_latlon"
# ind <- "pbpb_latlon"
# ind <- "pbab_latlon"
# ind <- "allbike_latlon"
# ind <- "pnft_latlon"
# ind <- "pnft_points_latlon"
# ind <- "pnst"




save_ind <- function(ind) {
  
  # open files
  # files_ind <- files_2024[grepl(ind, files_2024)]
  jakarta <- dir("../pedestriansfirst/cities_out/ghsl_region_05472/geodata", full.names = TRUE, recursive = TRUE)
  files_ind <- jakarta[grepl(ind, jakarta)]
  
  if (file_ext(files_ind[1]) %in% c("rds")) {
    
    data <- purrr::map_dfr(files_ind, readRDS)
    
  } else if(file_ext(files_ind[1]) %in% c("tif"))  {
    
    library(stars)
    data <- lapply(files_ind, function(x) st_as_sf(read_stars(x)))
  
    library(stars)  
    data1 <- stars::st_rasterize(data)
    mapview(data1)
    
  } else {
    
    
    # a <- st_read(files_ind[144])
    data <- purrr::map_dfr(files_ind, possibly(st_read))
    
    
  }
  
  # teste
  # st_write(data[1:200,], sprintf("data/%s.geojson", ind))
  max_zoom1 <- ifelse(ind == "block_densities_latlon", 12, 9)
  
  # export to mapbox
  tippecanoe(input = data,
             # output = sprintf("data-raw/data_final/mbtiles/%s_new.mbtiles", ind),
             output = sprintf("data-raw/data_final/mbtiles/%s_jakarta.mbtiles", ind),
             layer_name = paste0(ind, "_jakarta"),
             min_zoom = 8,
             max_zoom = max_zoom1,
             overwrite = TRUE
             # max_zoom = 16
  )
  
  
  upload_tiles(input = sprintf("data-raw/data_final/mbtiles/%s_jakarta.mbtiles", ind),
               access_token = "sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNscjhjdmoydzJxd3Qya21zd2t5aHN0ZmoifQ.NcKleHf_-d4buaEmcTT_Lg",
               username = "kauebraga",
               # tileset_id = paste0(ind, "_new"),
               tileset_id = paste0(ind, "_jakarta"),
               # tileset_name = paste0(ind, "_new"))
               tileset_name = paste0(ind, "_jakarta"))
  
}



# save_ind("protectedbike_latlon")
save_ind("hs_latlon")
save_ind("healthcare_points_latlon") # ok
save_ind("schools_points_latlon") # ok
save_ind("healthcare_latlon") # ok
save_ind("schools_latlon") # ok
save_ind("carfree_latlon")
save_ind("buffered_hwys_latlon")
save_ind("allhwys_latlon")
save_ind("pnpb_latlon")
save_ind("pnab_latlon")
save_ind("allbike_latlon")
save_ind("pnft_latlon")
save_ind("pnft_points_latlon")
save_ind("pnst_latlon")



library(mapdeck)

mapdeck(token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ",
        style = "mapbox://styles/kauebraga/clr8e5aqx001u01meen77bl13/draft",
        zoom = 2,
        location = c(-98.7382803, 31.7678448))
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addMapboxTiles(style_id = "clvyj4nre078b01qlabyh187f",
                 username = "kauebraga",
                 access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")


