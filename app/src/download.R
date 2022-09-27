output$download_button_maps <- renderUI({
  
  tagList(
    
      absolutePanel(class = "spatial_level", 
                    fixed = TRUE, draggable = FALSE,
                    style = "background: #00AE42",
                    top = 20, right = 570, height = 'auto', width = 140,
                    dropdown(
                      tagList(
                        downloadButton("downloadData1", "Download indicator for this region", icon = NULL),
                        downloadButton("downloadData2", "Download all indicators for this region", icon = NULL)
                      ),
                      hr(),
                      actionButton("downloadDic", "Download Data Dictionary", 
                                   onclick = "location.href='https://www.ipea.gov.br/acessooportunidades/dados';"),
                      circle = FALSE, 
                      # status = "danger",
                      label = "DOWNLOAD",
                      right = TRUE,
                      up = FALSE,
                      # icon = icon("download"), 
                      width = "350px",
                      # tooltip = tooltipOptions(title = "Click to see inputs !"),
                      inputId = "download_dropdown_maps"
                      
                    )
      )
  )
  
})  





# data
output$downloadData1 <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_%s.gpkg", input$cidade)
    
  },
  content = function(file) {
    
    sf::st_write(cidade_filtrada() %>% dplyr::left_join(hex_filtrado(), by = 'id_hex') %>% st_sf(crs = 4326), file)
    
  }
  
)

output$downloadData2 <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("acess_%s.gpkg", input$cidade)
    
  },
  content = function(file) {
    
    sf::st_write(cidade_filtrada() %>% dplyr::left_join(hex_filtrado(), by = 'id_hex') %>% st_sf(crs = 4326), file)
    
  }
  
)