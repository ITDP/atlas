data_ind <- reactive({
  
  # print(input$city)
  req(city$city_code)
  
  a <- readRDS(sprintf("../data/sample3/ghsl_%s/indicators_%s.rds", city$city_code, city$city_code))
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
  
  # open geommetries
  overlay_geom$polygon  <- readRDS(sprintf("../data/sample3/ghsl_%s/overlays_polygons_%s.rds", city$city_code, city$city_code))
  overlay_geom$line <- readRDS(sprintf("../data/sample3/ghsl_%s/overlays_lines_%s.rds", city$city_code, city$city_code))
  
  
  readRDS(sprintf("../data/sample3/ghsl_%s/overlays_%s.rds", city$city_code, city$city_code))
  # print(head(a))
  
  
})


# filter the first indicator
data_ind1 <- reactive({
  
  req(city$city_code)
  
  # print("ui")
  pattern <- sprintf("^%s", indicator$type)
  cols <- c('osmid', 'admin_level_ordered', 'name', grep(pattern, colnames(data_ind()), ignore.case = TRUE, value = TRUE), 'geom')
  a <- data_ind()[cols]
  
  # print(head(a))
  return(a)
  
})

data_overlays1 <- reactive({
  
  # print(data_overlays())
  # print(class(indicator$type))
  # print(class(data_overlays()))
  # print(indicator$type)
  ui <- indicator$type
  a <- subset(data_overlays(), startsWith(indicator, ui))
  # a <- subset(data_overlays(), indicator %like% indicator$type)
  # a <- subset(data_overlays(), startsWith(indicator, indicator$type))
  # a <- data_overlays()[grepl(indicator$type, indicator)]
  # print(head(a))
  return(a)
  
})




# second level: for each 'mode' 
data_ind2 <- reactive({
  
  req(indicator_mode())
  
  # print(indicator_mode())
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  cols <- c('osmid','admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'geom')
  a <- data_ind1()[cols]
  colnames(a) <- c('osmid','admin_level_ordered', 'name', 'valor', 'geom')
  a <- a %>% dplyr::mutate(teste = "teste")
  
  # print(a)
  
  return(a)
  
})
data_overlays2 <- reactive({
  
  req(indicator_mode())
  pattern <- sprintf("%s_%s_2019", indicator$type, indicator_mode())
  # print(pattern)
  # print(head(data_overlays1()))
  a <- subset(data_overlays1(), indicator == pattern)
  
  # print(a)
  return(a)
  
  # print(sprintf("spatial level: %s", input$spatial_level))
  
})


# reactive to create the filtered overlay with the geom -------------------

data_overlays_sf <- reactive({
  
  req(indicator_mode())
  req(data_overlays2())
  # print(head(data_overlays2()))
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  
  print(data_overlays2())
  # print(geom_type)
  
  # select data tahat will be used for the overlay
  if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$polygon, by = "indicator") %>% sf::st_sf()
    
  } else data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$line, by = "indicator") %>% sf::st_sf() 
  
  
  # print(head(data_overlays_sf))
  
  return(data_overlays_sf)
  
})