

data_ind <- reactive({
  
  # print(input$city)
  req(city$city_code)
  
  
  a <- readRDS(sprintf("../data/data_alpha/ghsl_%s/indicators_%s.rds", city$city_code, city$city_code))
  # readRDS(sprintf("data/atlas_%s_indicators.rds", city$city_code))
  # print(head(a))
  spatial_level_value$last <- length(unique(a$admin_level))
  return(a)
  
  
})


# # calculate number of spatial levels
# spatial_levels_number <- reactive({
#   
#   a
#   
# })

# reactive values to identify overlay geoms
overlay_geom <- reactiveValues(polygon = NULL, line = NULL)


data_overlays <- reactive({
  
  req(city$city_code)
  
  # if (indicator$mode != "popdensity") {
    
    
    a <- readRDS(sprintf("../data/data_alpha/ghsl_%s/overlays_%s.rds", city$city_code, city$city_code))
    
    return(a)
  # }
  
  
  
  
})


# filter the first indicator
data_ind1 <- reactive({
  
  req(city$city_code)
  
  # print("ui")
  pattern <- sprintf("^%s", indicator$type)
  cols <- c('country', 'osmid', 'admin_level', 'admin_level_ordered', 'name', grep(pattern, colnames(data_ind()), ignore.case = TRUE, value = TRUE), 'geom')
  a <- data_ind()[cols]
  
  # print(head(a))
  return(a)
  
})

# data_overlays1 <- reactive({
#   
#   # print(data_overlays())
#   # print(class(indicator$type))
#   # print(class(data_overlays()))
#   # print(indicator$type)
#   ui <- indicator$type
#   a <- subset(data_overlays(), startsWith(indicator, ui))
#   # a <- subset(data_overlays(), indicator %like% indicator$type)
#   # a <- subset(data_overlays(), startsWith(indicator, indicator$type))
#   # a <- data_overlays()[grepl(indicator$type, indicator)]
#   return(a)
#   
# })




# second level: for each 'mode' 
data_ind2 <- reactive({
  
  req(indicator$mode)
  
  # print(indicator$mode)
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  cols <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'geom')
  a <- data_ind1()[cols]
  
  indicator$mode <-indicator$mode
  
  # print(a)
  
  return(a)
  
})
data_overlays2 <- reactive({
  
  req(indicator$mode, input$year)
  
  if (indicator$mode != "density") {

    pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
    # pattern <- sprintf("%s_%s_2019", indicator$type, indicator$mode)
    a <- subset(data_overlays(), indicator == pattern)

    # print("ai")
    # print(a)


    # open geommetries
    geom_type <- unique(a$geom_type)

    if(geom_type %in% c("MULTIPOLYGON", "POLYGON")) {

      overlay_geom$polygon  <- readRDS(sprintf("../data/data_alpha/ghsl_%s/overlays/%s/overlays_polygons_%s_%s.rds",
                                               city$city_code, indicator$mode, city$city_code, indicator$mode))
      
      overlay_geom$polygon <- subset(overlay_geom$polygon, year == input$year)
      
      

    } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {

      overlay_geom$line <- readRDS(sprintf("../data/data_alpha/ghsl_%s/overlays/%s/overlays_lines_%s_%s.rds",
                                           city$city_code, indicator$mode, city$city_code, indicator$mode))

    }


    # print("overlays2")
    # print(overlay_geom$polygon)

    return(a)

  }
  
  
  
})


# filter year when available
data_ind3 <- reactive({
  
  req(indicator$mode, input$year, indicator$type)
  
  # print(indicator$mode)
  pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
  
  if (indicator$type == "performance" & isTRUE(input$regions_grid == "Grid")) {
    
    a <- readRDS(sprintf("../data/data_alpha/ghsl_%s/grid_%s.rds", city$city_code, city$city_code))
    a$country <- NA
    a$admin_level <- NA
    a$admin_level_ordered <- NA
    a$name <- NA
    
    cols <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', colnames(data_ind2())[startsWith(colnames(data_ind2()), pattern)], 'geom')
    
    a <- a[cols]
    colnames(a) <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', 'value', 'geom')
    
    
  } else {
    
    
    # print(indicator$mode)
    pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
    cols <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', colnames(data_ind2())[startsWith(colnames(data_ind2()), pattern)], 'geom')
    a <- data_ind1()[cols]
    colnames(a) <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', 'value', 'geom')
    # print(a)
    
    
  }
  
  return(a)
  
})


# reactive to create the filtered overlay with the geom -------------------

data_overlays_sf <- reactive({
  
  req(indicator$mode)
  
  if (indicator$mode != "density") {
  
  req(data_overlays2())
  # print(head(data_overlays2()))
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  # print("geom_type")
  # print(geom_type)
  
  
  # select data tahat will be used for the overlay
  if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    # print("a")
    # print(data_overlays2())
    
    data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$polygon, by = "indicator") %>% sf::st_sf()
    
  } else data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$line, by = "indicator") %>% sf::st_sf() 
  
  } 
  # else {
  #   
  #   data_overlays_sf <- raster::raster(sprintf("../data/data_alpha/ghsl_%s/overlays/population/overlay_population_%s_%s.tif", city$city_code, city$city_code, input$year))
  #   
  # }
  
  
  # print("head(data_overlays_sf)")
  # print(head(data_overlays_sf))
  
  return(data_overlays_sf)
  
})

