


# download data in the world view -------------------------------------------------------------


# data
output$downloadData_countries_gpkg <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_countries_%s.gpkg", indicator$mode)
    
  },
  content = function(file) {
    
    pattern <-  sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
    
    # Separate the fixed columns
    fixed_cols <- c("a3", "name", "region_type", "geom")
    # Get only the indicator column names
    indicator_cols <- setdiff(names(atlas_country()), fixed_cols)
    # Extract year from indicator column names
    years <- as.numeric(sub(".*_(\\d{4})$", "\\1", indicator_cols))
    # Order indicator columns by year
    ordered_indicators <- indicator_cols[order(years)]
    # Reconstruct the data frame with sorted indicator columns
    a <- atlas_country()[c("a3", "name", "region_type", ordered_indicators, "geom")]
    
    sf::st_write(a, file)
    
  }
  
)
output$downloadData_countries_csv <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_countries_%s.csv", indicator$mode)
    
  },
  content = function(file) {
    
    # Separate the fixed columns
    fixed_cols <- c("a3", "name", "region_type", "geom")
    # Get only the indicator column names
    indicator_cols <- setdiff(names(atlas_country()), fixed_cols)
    # Extract year from indicator column names
    years <- as.numeric(sub(".*_(\\d{4})$", "\\1", indicator_cols))
    # Order indicator columns by year
    ordered_indicators <- indicator_cols[order(years)]
    # Reconstruct the data frame with sorted indicator columns
    a <- atlas_country()[c("a3", "name", "region_type", ordered_indicators, "geom")]
    
    write.csv(sf::st_set_geometry(a, NULL), file, row.names = FALSE, sep = ",")
    
  }
  
)
output$downloadData_cities_gpkg <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_cities_%s.gpkg", indicator$mode)
    
  },
  content = function(file) {
    
    pattern <-  sprintf("%s_%s", indicator$type, indicator$mode)
    # pattern <- "transit_pnrt"
    
    cols <- c("hdc", "country", "name", grep(pattern, colnames(atlas_city_markers), ignore.case = TRUE, value = TRUE), "geom")
    a <- atlas_city_markers[cols]
    # Separate the fixed columns
    fixed_cols <- c("hdc", "country", "name", "geom")
    # Get only the indicator column names
    indicator_cols <- setdiff(names(a), fixed_cols)
    # Extract year from indicator column names
    years <- as.numeric(sub(".*_(\\d{4})$", "\\1", indicator_cols))
    # Order indicator columns by year
    ordered_indicators <- indicator_cols[order(years)]
    # Reconstruct the data frame with sorted indicator columns
    a <- a[c("hdc", "country", "name", ordered_indicators, "geom")]
    
    sf::st_write(a, file)
    
  }
  
)
output$downloadData_cities_csv <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_cities_%s.csv", indicator$mode)
    
  },
  content = function(file) {
    
    
    pattern <-  sprintf("%s_%s", indicator$type, indicator$mode)
    
    cols <- c("hdc", "country", "name", grep(pattern, colnames(atlas_city_markers), ignore.case = TRUE, value = TRUE), "geom")
    a <- atlas_city_markers[cols]
    # Separate the fixed columns
    fixed_cols <- c("hdc", "country", "name", "geom")
    # Get only the indicator column names
    indicator_cols <- setdiff(names(a), fixed_cols)
    # Extract year from indicator column names
    years <- as.numeric(sub(".*_(\\d{4})$", "\\1", indicator_cols))
    # Order indicator columns by year
    ordered_indicators <- indicator_cols[order(years)]
    # Reconstruct the data frame with sorted indicator columns
    a <- a[c("hdc", "country", "name", ordered_indicators, "geom")]
    
    write.csv(sf::st_set_geometry(a, NULL), file, row.names = FALSE, sep = ",")
    
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
    
    bu <- indicator$mode
    file_ind <- sprintf("../data/data_final/ghsl_%s/overlays/%s_%s.zip", city$city_code, bu, city$city_code)
    file_pop <- sprintf("../data/data_final/ghsl_%s/overlays/%s_%s.zip", city$city_code, "pop", city$city_code)
    
    # zip those files
    zip::zip(zipfile = file, files = c(file_ind, file_pop),
             mode = "cherry-pick")
    
    # print("deu certo")
    # print(file)
    # 
    # file.copy(sprintf("../data/data_beta/ghsl_%s/overlays/%s_%s.zip", city$city_code, bu, city$city_code), file)
  },
  contentType = "application/zip"
  
)