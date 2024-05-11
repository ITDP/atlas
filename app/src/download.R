


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
    
    
    sprintf("atlas_overlays_%s_%s.zip", city$city_code, indicator$mode)
    
  },
  content = function(file) {
    
    dir1 <- function(pattern, ...) {
      
      dir(pattern = pattern, ...)
    }
    
    bu <- indicator$mode
    overlay_subset <- subset(overlay_table, indicator == bu)
    file1 <- lapply(sprintf("%s_%s", overlay_subset$overlay, city$city_code), 
                   dir1, 
                   path = sprintf("../data/data_final/ghsl_%s/overlays/temp", city$city_code), full.names = TRUE) 
    file1 <- do.call(c, file1)
    
    # zip those files
    zip::zip(zipfile = file, files = file1,
             mode = "cherry-pick")
    
    # print("deu certo")
    # print(file)
    # 
    # file.copy(sprintf("../data/data_beta/ghsl_%s/overlays/%s_%s.zip", city$city_code, bu, city$city_code), file)
  },
  contentType = "application/zip"
  
)