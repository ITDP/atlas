output$download_button_maps <- renderUI({
  
  tagList(
    
    absolutePanel(class = "about_button", 
                  style = "background: #00AE42",
                  top = 40, right = 450, height = 40, width = 130,
                  dropdown(
                    tagList(
                      downloadButton("downloadData1", "Download indicator for this region", icon = NULL),
                      downloadButton("downloadData2", "Download all indicators for this region", icon = NULL)
                    ),
                    hr(),
                    tagList(
                      downloadButton("download_overlay", "Download overlay for this indicator", icon = NULL)
                    ),
                    hr(),
                    actionButton("downloadDic", "Download Data Dictionary", 
                                 onclick = "location.href='https://www.ipea.gov.br/acessooportunidades/dados';"),
                    circle = FALSE, 
                    # status = "danger",
                    label = HTML("&nbsp;&nbsp;Download"),
                    icon = icon("download"),
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
    
    
    sprintf("atlas_indicators_%s_%s.gpkg", city$city_code, indicator_mode())
    
  },
  content = function(file) {
    
    sf::st_write(data_ind2(), file)
    
  }
  
)

output$downloadData2 <- downloadHandler(
  
  
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_indicators_%s.gpkg", city$city_code)
    
  },
  content = function(file) {
    
    sf::st_write(data_ind(), file)
    
  }
  
)

output$download_overlay <- downloadHandler(
  
  # generate button with data
  filename = function() {
    
    
    sprintf("atlas_overlay_%s_%s.gpkg", city$city_code, indicator_mode())
    
  },
  content = function(file) {
    
    sf::st_write(data_overlays2(), file)
    
  }
  
)