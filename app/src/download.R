


# download data in the world view -------------------------------------------------------------


# data
output$downloadData_countries_gpkg <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_countries_%s.gpkg", indicator$mode)
    
  },
  content = function(file) {
    
    pattern <-  sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
    
    sf::st_write(world_view$a_country, file)
    
  }
  
)
output$downloadData_countries_csv <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_countries_%s.csv", indicator$mode)
    
  },
  content = function(file) {
    
    write.csv(sf::st_set_geometry(world_view$a_country, NULL), file, row.names = FALSE, sep = ",")
    
  }
  
)
output$downloadData_cities_gpkg <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_cities_%s.gpkg", indicator$mode)
    
  },
  content = function(file) {
    
    sf::st_write(world_view$a_available[, c("name", "hdc", "value")], file)
    
  }
  
)
output$downloadData_cities_csv <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_cities_%s.csv", indicator$mode)
    
  },
  content = function(file) {
    
    write.csv(sf::st_set_geometry(world_view$a_available[, c("name", "hdc", "value")], NULL), file, row.names = FALSE, sep = ",")
    
  }
  
)






# download data for the selected region  for the indicator ---------------------


# data
output$downloadData1_gpkg <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_indicators_%s_%s.gpkg", city$city_code, indicator$mode)
    
  },
  content = function(file) {
    
    sf::st_write(data_ind2(), file)
    
  }
  
)
output$downloadData1_csv <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_indicators_%s_%s.csv", city$city_code, indicator$mode)
    
  },
  content = function(file) {
    
    write.csv(sf::st_set_geometry(data_ind2(), NULL), file, row.names = FALSE, sep = ",")
    
  }
  
)


# download data for the whole world for the indicator  ----------------------------------------------

output$downloadData2_gpkg <- downloadHandler(
  
  
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_indicators_%s.gpkg", city$city_code)
    
  },
  content = function(file) {
    
    sf::st_write(data_ind(), file)
    
  }
  
)
output$downloadData2_csv <- downloadHandler(
  
  
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_indicators_%s.csv", city$city_code)
    
  },
  content = function(file) {
    
    write.csv(sf::st_set_geometry(data_ind(), NULL), file, row.names = FALSE)
    
  }
  
)

output$download_overlay <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_overlay_%s_%s.gpkg", city$city_code, indicator$mode)
    
  },
  content = function(file) {
    
    sf::st_write(data_overlays_sf(), file)
    
  }
  
)