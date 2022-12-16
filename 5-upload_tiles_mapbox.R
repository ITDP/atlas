
library(mapboxapi)
library(mapdeck)
library(httr)
library(data.table)
library(stringr)
library(purrr)
library(sf)

# get all cities with blockdensity indicator
files <- dir("data-raw/sample_3", full.names = TRUE, recursive = TRUE)
files <- files[files %like% "blockslatlon.geojson"]
# extract the codes
codes <- stringr::str_extract(string = files, pattern = "\\d{5}")

a <- st_read("data-raw/sample_3/ghsl_region_00621/geodata/blockslatlon.geojson")
mapview::mapview(a)


# Use tippecanoe to make a dynamic .mbtiles file that visualizes large data appropriately
# at any zoom level.  sf objects can also be used as input!
# (requires installing tippecanoe on your machine separately first)
pmap(list(files, sprintf("blocks_%s.mbtiles", codes), layer_name = sprintf("block_density_%s", codes)), tippecanoe)

# tippecanoe(input = "Texas.geojson",
#            output = "Texas.mbtiles",
#            layer_name = "texas_buildings")

# Upload the generated tileset to your Mapbox account (requires a Mapbox secret access token
# to be set as an environment variable)
upload_tiles(input = "Texas.mbtiles", username = "kauebraga",
             tileset_id = "TX_buildings",
             multipart = TRUE,
             access_token = )

upload_tiles1 <- function(input1, tileset_id1, username1, ...) {
  
  upload_tiles(input = input1, tileset_id = tileset_id1, username = username1, ...)
  
}

map2(sprintf("blocks_%s.mbtiles", codes), sprintf("blocks_%s", codes), upload_tiles1,
     username1 = "kauebraga", multipart = TRUE,
     access_token = "sk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNsYnBvcnVzcTA2cXIzb3F0YWEwZ2dkZWoifQ.0AEsn0QCTPue4U0dwNZ21Q")


# Head over to Mapbox Studio when the upload is done (check the status with
# `check_upload_status()`) and add it to a style.  When you've styled it, bring it back
# into R with mapdeck by referencing the style ID:
mapdeck(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
        style = "mapbox://styles/kwalkertcu/ckaf9qxim1pyk1io7r2e8exj2/draft",
        zoom = 6,
        location = c(-98.7382803, 31.7678448))