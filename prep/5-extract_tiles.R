library(dplyr)
library(sf)
library(raster)
library(mapview)
library(leaflet)
library(sf)
library(raster)

# open test tile


library(tiler)
tiler_options(osgeo4w = "C:/OSGeo4W64/OSGeo4W.bat")


a <- raster("data-raw/sample_3/ghsl_region_00014/geodata/population/pop_2010.tif")
mapview(a)


tile(file = "data-raw/sample_3/ghsl_region_00014/geodata/population/pop_2010.tif", 
     tiles = "tiles/tiles/tile1", zoom = "12-14", crs = 4326)




library(leaflet)
tiles <- "https://github.com/ITDP/atlas/tree/main/tiles/test/{z}/{x}/{y}.png"

leaflet() %>%
  addTiles() %>%
  addTiles("temp/{z}/{x}/{y}.png", options = tileOptions(opacity = 0.8))
  
leaflet(
  options = leafletOptions(minZoom = 0, maxZoom = 7), width = "100%") %>%
  addProviderTiles("Stamen.Toner") %>%
  addTiles(tiles, options = tileOptions(opacity = 0.8)) %>% setView(-100, 40, 3)


# -------------------------------------------------------------------------


a <- st_read("data-raw/sample_3/ghsl_region_00014/geodata/blockslatlon.geojson")
st_write(a, "hu.geotif")
r <- raster(a, res=0.01)


b <- rasterize(a, r, "density")

plot(b)

leaflet() %>%
  addTiles() %>%
  addRasterImage(x = b)

# save


plot(b)
mapview(b)

mapview(a)


library(leafgl)

tictoc::tic()
leaflet() %>%
  addTiles() %>%
  addGlPolygons(data = a)
tictoc::toc()


b <- brick(system.file("external/rlogo.grd", package="raster"))
plotRGB(b)
