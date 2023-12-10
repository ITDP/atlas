

data_ind <- reactive({
  
  # print(input$city)
  req(city$city_code)
  
  message("Open city data")
  
  a <- readRDS(sprintf("../data/data_beta/ghsl_%s/indicators_%s.rds", city$city_code, city$city_code))
  # readRDS(sprintf("data/atlas_%s_indicators.rds", city$city_code))
  # print(head(a))
  spatial_level_value$last <- length(unique(a$admin_level))
  return(a)
  
  
})



# reactive values to identify overlay geoms
overlay_geom <- reactiveValues(polygon = NULL, line = NULL)


# filter the first indicator
data_ind1 <- reactive({
  
  
  req(city$city_code)
  message("Filter indicator #1")
  # print("ui")
  pattern <- sprintf("^%s", indicator$type)
  cols <- c('country', 'osmid', 'admin_level', 'admin_level_ordered', 'name', grep(pattern, colnames(data_ind()), ignore.case = TRUE, value = TRUE), 'geom')
  a <- data_ind()[cols]
  # print(head(a))
  return(a)
  
})



# second level: for each 'mode' 
data_ind2 <- reactive({
  
  req(indicator$mode, data_ind1())
  message("Filter indicator #2")  
  # print("year$ok")
  # print(year$ok)
  # print(indicator$mode)
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  cols <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'geom')
  a <- data_ind1()[cols]
  # indicator$mode <-indicator$mode
  
  # print(a)
  
  return(a)
  
})



# filter year when available
data_ind3 <- reactive({
  
  req(data_ind2())
  
  # print("pq")
  # take dependcy on year and the previous dataset
  input$year
  
  isolate ({
    
    
  message("Filter indicator #3")
  
  # print(indicator$mode)
  pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok)
  
  if (indicator$type == "performance" & isTRUE(input$regions_grid == "Grid")) {
    
    a <- readRDS(sprintf("../data/data_beta/ghsl_%s/grid_%s.rds", city$city_code, city$city_code))
    a$country <- NA
    a$admin_level <- NA
    a$admin_level_ordered <- NA
    a$name <- NA
    
    cols <- c('country', 'osmid', 'admin_level','admin_level_ordered',  'name', colnames(data_ind2())[startsWith(colnames(data_ind2()), pattern)], 'geom')
    
    a <- a[cols]
    colnames(a) <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', 'value', 'geom')
    
    
  } else {
    
    pattern <- sprintf("_%s", year$ok)
    cols <- c('country', 'osmid', 'admin_level','admin_level_ordered', 'name', colnames(data_ind2())[endsWith(colnames(data_ind2()), pattern)], 'geom')
    a <- data_ind2()[cols]
    colnames_new <- sub(pattern = sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok), replacement = "value",  colnames(a))
    # remove the year
    colnames_new <- sub(pattern = sprintf("_%s$", year$ok), replacement = "",  colnames_new)
    colnames(a) <- colnames_new
    
  }
  
  # print("aqui origial")
  # print(a)
  return(a)
    
  })
  
  
  
})


