# BOTAO PARA VOLTAR !!!!!!!!!! --------------------------------------------



observeEvent(c(input$back_to_world), {
  
  
  # print("back to world")
  # print(input$back_to_world)
  
  # if (isTRUE(input$back_to_workd != 0)) {
  
  req(input$back_to_world >= 1)
  
  # print("back to world")
  
  # reset the admin level
  rank$admin_level <- NULL
  # reset the indicator
  # indicator$mode <- "pnpb"
  
  # reset the city values
  shinyWidgets::updatePickerInput(session = session, inputId = "city",
                                  selected = character(0))
  city$city_code <- ""
  
  # session$sendCustomMessage(type = "resetValue", message = "city")
  # session$sendCustomMessage(type = "resetValue", message = "map_marker_click")
  
  # print(paste0("pa: ", input$map_marker_click))
  
  map <- leafletProxy("map", data = world_view$a_available) %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    setView(lng = 0, lat = 0, zoom = 3) %>%
    addMapPane("countries", zIndex = 410) %>% # shown below ames_circles
    addMapPane("markers_navailable", zIndex = 420) %>% # shown above ames_lines
    addMapPane("markers_available", zIndex = 430) %>% # shown above ames_lines
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 8,
      fillColor = ~world_view$pal(value),
      stroke = TRUE, fillOpacity = 0.9, color = "black",
      opacity = 0.9,
      # weight = 1,
      layerId = ~hdc,
      label = lapply(world_view$labels_markers1, htmltools::HTML),
      options = pathOptions(clickable = TRUE, pane = "markers_available"),
      labelOptions = labelOptions(
        style = list(
          # "color" = "red",
          "font-family" = "'Fira Sans', sans-serif",
          # "font-style" = "italic",
          # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
          "font-size" = "12px"
          # "border-color" = "rgba(0,0,0,0.5)"
        ))
    ) %>%
    addCircleMarkers(data = world_view$a_notavailable,
                     # radius = ~ifelse(type == "ship", 6, 10),
                     radius = 8,
                     # fillColor = ~pal(valor), 
                     stroke = TRUE, fillOpacity = 0.9, color = "#808080",
                     opacity = 0.8,
                     weight = 1,
                     layerId = ~hdc,
                     label = lapply(world_view$labels2, htmltools::HTML),
                     # label = ~htmltools::htmlEscape(name),
                     options = pathOptions(clickable = FALSE, pane = "markers_navailable")
    ) %>%
    addPolygons(data = world_view$a_country,
                layerId = ~name,
                fillColor = ~world_view$pal_countries(value), color = "black",  weight = 0, # erro nao eh aqui
                fillOpacity = 0.7,
                options = pathOptions(clickable = TRUE, pane = "countries"),
                group = "Countries",
                label = lapply(world_view$labels_country, htmltools::HTML)
    ) %>%
    # add polygons with the country color
    # addPolygons(fillColor = ~pal(pnpb), color = "black", layerId = ~code_metro) %>%
    addLegend("bottomright", pal = world_view$pal_countries, values = ~world_view$a_country$value,
              title = world_view$legend_title,
              # bins = 7,
              labFormat = world_view$legend_value,
              layerId = "legend_country")
  
  
  
  
  # display the ranks on the right side -------------------------------------
  
  # print("rank$rank_text_world ")
  # print(rank$rank_text_world)
  
  rank$rank_text <- rank$rank_text_world  
  rank$rank_value <- rank$rank_value_world  
  
  

  # remove the compare button (not available at the world view) ---------------------------------
  # delay(2, runjs("$('#compare_panel').css('display', 'none');"))

  
  
}) 





