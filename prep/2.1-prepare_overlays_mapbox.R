library(mapboxapi)
library(tools)
library(sf)
library(mapview)

mapboxapi::mb_access_token("sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNscjhjdmoydzJxd3Qya21zd2t5aHN0ZmoifQ.NcKleHf_-d4buaEmcTT_Lg")

# gather overlay from one city
files <- dir("",
             full.names = TRUE, recursive = TRUE)

# remove zip
files <- files[grepl("/geodata", files)]
files <- files[!grepl(".zip", files)]
files <- files[!grepl("/temp", files)]

# only 2022
files_2022 <- files[grepl("2024", files)]

# rename
# files_hs <- files_2022[grepl(pattern = "\\+s", x = files_2022)]
# files_hs_new <- stringr::str_replace_all(files_hs, pattern = "\\+", replacement = "")
# rename
# purrr::map2(.x = files_hs, .y = files_hs_new, file.rename)

# save for each indicator
# ind <- "protectedbike_latlon"
# ind <- "hs_latlon"
# ind <- "population"
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
# ind <- "/block_densities_latlon"




save_ind <- function(ind) {
  
  # open files
  files_ind <- files_2022[grepl(ind, files_2022)]
  
  if (file_ext(files_ind[1]) %in% c("rds")) {
    
    data <- purrr::map_dfr(files_ind, readRDS)
    
  } else if(file_ext(files_ind[1]) %in% c("tif"))  {
    
    library(stars)
    data <- lapply(files_ind, function(x) st_as_sf(read_stars(x)))
    
  } else {
    
    
    # a <- st_read(files_ind[144])
    data <- purrr::map(files_ind, possibly(st_read))
    
    
  }
  
  # teste
  # st_write(data[1:200,], sprintf("data/%s.geojson", ind))
  max_zoom1 <- ifelse(ind == "block_densities_latlon", 12, 9)
  
  # export to mapbox
  tippecanoe(input = data,
             output = sprintf("data-raw/data_beta/mbtiles/%s.mbtiles", ind),
             layer_name = ind,
             min_zoom = 8,
             max_zoom = max_zoom1,
             overwrite = TRUE
             # max_zoom = 16
  )
  
  upload_tiles(input = sprintf("data-raw/data_beta/mbtiles/%s.mbtiles", ind),
               access_token = "sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNscjhjdmoydzJxd3Qya21zd2t5aHN0ZmoifQ.NcKleHf_-d4buaEmcTT_Lg",
               username = "kauebraga",
               tileset_id = ind,
               tileset_name = ind)
  
}



# save_ind("protectedbike_latlon")
save_ind("hs_latlon")
save_ind("pop")
# save_ind("healthcare_points_latlon")
# save_ind("schools_points_latlon")
# save_ind("healthcare_latlon")
# save_ind("schools_latlon")
# save_ind("carfree_latlon")
# save_ind("buffered_hwys_latlon")
# save_ind("allhwys_latlon")
# save_ind("pnpb_latlon")
# save_ind("pnab_latlon")
# save_ind("allbike_latlon")
# save_ind("pnft_latlon")
# save_ind("pnft_points_latlon")
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


