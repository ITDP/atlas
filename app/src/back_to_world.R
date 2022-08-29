# BOTAO PARA VOLTAR !!!!!!!!!! --------------------------------------------



observeEvent(c(input$back_to_world), {
  
  
  # print("back to world")
  # print(input$back_to_world)
  
  # if (isTRUE(input$back_to_workd != 0)) {
  
  req(input$back_to_world >= 1)
  
  print("back to world")
  
  # reset the admin level
  rank$admin_level <- NULL
  
  # reset the city values
  shinyWidgets::updatePickerInput(session = session, inputId = "city",
                                  selected = character(0))
  city$city_code <- NULL
  
  # session$sendCustomMessage(type = "resetValue", message = "city")
  # session$sendCustomMessage(type = "resetValue", message = "map_marker_click")
  
  # print(paste0("pa: ", input$map_marker_click))
  
  
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  # print(pattern)
  cols <- c('name', 'hdc', 'osmid','admin_level_ordered', 'name', colnames(atlas_city_markers)[startsWith(colnames(atlas_city_markers), pattern)])
  a <- atlas_city_markers[cols]
  colnames(a) <- c('name', 'hdc', 'osmid', 'admin_level_ordered', 'name', 'valor', 'geom')
  
  # print(class(atlas_country))
  cols_country <- c('a2', colnames(atlas_country)[startsWith(colnames(atlas_country), pattern)])
  a_country <- atlas_country[cols_country]
  colnames(a_country) <- c('a2', 'valor', 'geom')
  
  pal <- colorNumeric(
    palette = "viridis",
    # palette = "YlGnBu",
    domain = a$valor)
  
  
  pal_countries <- colorNumeric(
    palette = "viridis",
    # palette = "YlGnBu",
    domain = a_country$valor)
  
  legend_title <- subset(list_indicators, indicator_code == indicator_mode())$indicator_name
  legend_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
  # format legend value
  legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = " km", transform = function(x) as.integer(x))
  
  # print(a)
  
  leafletProxy("map", data = a) %>%
    clearMarkers() %>%
    clearControls() %>%
    removeLayersControl() %>%
    clearShapes() %>%
    setView(lng = 0, lat = 0, zoom = 3) %>%
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 8,
      # fillColor = ~pal(valor), 
      stroke = TRUE, fillOpacity = 0.9, color = "#00AE42",
      opacity = 0.8,
      weight = 1,
      layerId = ~hdc,
      label = ~htmltools::htmlEscape(name)
    ) %>%
    addPolygons(data = a_country, 
                fillColor = ~pal_countries(valor), color = "black",  weight = 0,
                options = pathOptions(clickable = FALSE)) %>%
    # add polygons with the country color
    # addPolygons(fillColor = ~pal(pnpb), color = "black", layerId = ~code_metro) %>%
    addLegend("bottomright", pal = pal_countries, values = ~a_country$valor,
              title = legend_title,
              # bins = 7,
              labFormat = legend_value,
              layerId = "legend_country") %>%
    addLayersControl(baseGroups = c("Dark", "Light", "Satellite"),
                     # overlayGroups = c("Overlay"),
                     options = layersControlOptions(collapsed = FALSE),
                     position = "topright")
}) 





# content of the right panel ----------------------------------------------


observeEvent(c(input$back_to_world), {
  
  
  
  req(input$back_to_world >= 1)
  
  
  # value
  print(paste0("type: ", indicator$type))
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  # print(pattern)
  cols <- c('name_long', colnames(atlas_country)[startsWith(colnames(atlas_country), pattern)], "geom")
  # print(cols)
  a <- atlas_country[cols]
  colnames(a) <- c('name_long', 'valor', 'geom')
  # print(a)
  # only top five
  a <- a[order(-a$valor),]
  a <- a[1:3,]
  # mean for the world
  rank_indicator <- mean(a$valor)
  
  # print("oooia")
  # print(rank_indicator)
  
  # print(head(filter_rank()))
  # print(spatial_level_value$last)
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
  
  # format_indicator_name <- switch (indicator_mode(),
  #                                  "pnpb" = "People Near Protected Bike Lanes",
  #                                  "pnab" = "People Near Bike Lanes",
  #                                  "pnh" = "People Near pnh",
  #                                  "pne" = "People Near pne",
  #                                  "hs" = "People Near Services"
  # )
  
  format_indicator_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
    scales::percent(rank_indicator)
  } else round(rank_indicator)
  
  
  format_indicator_value_countries1 <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
    scales::percent(a$valor[1])
  } else round(a$valor[1])
  
  format_indicator_value_countries2 <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
    scales::percent(a$valor[2])
  } else round(a$valor[2])
  
  format_indicator_value_countries3 <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
    scales::percent(a$valor[3])
  } else round((a$valor[3]))
  
  
  # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
  rank$rank_value <- paste0('<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 20px">THIS INDICATOR IN </div>', 
                            '<div class="title_indicator" style="font-size: 20px;">', 
                            'THE WORLD', '</div>',
                            div(class = "value_indicator_rightpanel", format_indicator_value))
  
  rank$rank_value_world <- rank$rank_value
  
  
  # ranking
  
  text_title <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "RANKING")
  text1 <- sprintf("%s (%s)", 
                   filter_rank_country()$name_long[1], 
                   format_indicator_value_countries1)
  text2 <- sprintf("%s (%s)", 
                   filter_rank_country()$name_long[2], 
                   format_indicator_value_countries2)
  text3 <- sprintf("%s (%s)", 
                   filter_rank_country()$name_long[3], 
                   format_indicator_value_countries3)
  
  flag1 <- tags$img(src = sprintf("https://flagicons.lipis.dev/flags/4x3/%s.svg", tolower(filter_rank_country()$a2[1])), width = "25",
                    style = "float:left")
  flag2 <- tags$img(src = sprintf("https://flagicons.lipis.dev/flags/4x3/%s.svg", tolower(filter_rank_country()$a2[2])), width = "25",
                    style = "float:left")
  flag3 <- tags$img(src = sprintf("https://flagicons.lipis.dev/flags/4x3/%s.svg", tolower(filter_rank_country()$a2[3])), width = "25",
                    style = "float:left")
  
  # print(flag1)
  
  rank$rank_text <- paste0(text_title, "<br>",
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; font-size: 20px; float: left", "1º" ),
                           flag1,
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; float: left", text1),
                           div(style = "clear:both;"),
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; font-size: 20px; float: left", "2º" ),
                           flag2,
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; float: left", text2),
                           div(style = "clear:both;"),
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; font-size: 20px; float: left", "3º" ),
                           flag3,
                           div(class = "text_compare", style = "padding-bottom: 0px; padding-top: 0px; float: left", text3))
  
  # print(rank$rank_text)
  # print(rank$rank_value)
  
  rank$rank_text_world <- rank$rank_text_value
  
  
  
})

