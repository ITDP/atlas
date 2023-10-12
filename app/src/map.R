


# store the map config to go back when reseting ---------------------------

world_view <- reactiveValues(a_available = NULL,
                             pal = NULL,
                             labels_markers1 = NULL,
                             a_notavailable = NULL,
                             labels2 = NULL,
                             a_country = NULL,
                             pal_countries = NULL,
                             labels_country = NULL,
                             map = NULL)


# open country data -------------------------------------------------------

atlas_country <- reactive({
  
  req(indicator$mode, indicator$type)
  
  
  
  # print("oiiiiiiiiiiiiii")
  
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  # open data
  a <- readRDS(sprintf("../data/data_beta/countries/atlas_country_%s.rds", pattern))
  
  # print("aagsagas")
  # print(a)
  
  
})


osm_selected <- reactiveValues(oi = NULL)

admin_level_previous <- reactiveValues(a = NULL)


tl <- reactiveValues(transition = NULL)


# initial map
output$map <- renderLeaflet({
  
  # input$print
  
  shinyjs::logjs("Map: observer to default map")
  
  # https://stackoverflow.com/questions/43194575/leaflet-plugin-and-leafletproxy
  # https://stackoverflow.com/questions/48054756/overlay-an-image-in-a-shiny-leaflet-observe-event
  # https://stackoverflow.com/questions/70286037/r-leaflet-how-to-bind-a-client-side-event-to-a-polygon
  
  map <- leaflet(data = atlas_city_markers, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light", layerId = "epa") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 5,
      fillColor = "#00AE42",
      stroke = TRUE, fillOpacity = 0.9, color = "black",
      opacity = 0.9,
      weight = 2,
      # weight = 1,
      options = pathOptions(clickable = FALSE)
    ) %>%
    # registerPlugin1(spinPlugin) %>% 
    # registerPlugin1(leafletspinPlugin) %>% 
    addLayersControl(baseGroups = c("Light", "Dark", "Satellite"),
                     # overlayGroups = c("Overlay"),
                     options = layersControlOptions(collapsed = FALSE),
                     position = "bottomright") %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    leaflet.extras2::addSpinner() %>%
    
    # onRender(spin_event)
    
    htmlwidgets::onRender(
      "function(el, x) {
      L.control.zoom({position:'topright'}).addTo(this);
    }")
  
  
  
  map
  
  # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')                     
  # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>Test</label>" );')  
  
})

# create counter
counter <- reactiveValues(obs1 = NULL,
                          obs2 = NULL)

# update world values when indicator is changed -------
observeEvent(c(indicator$mode, input$year,  input$back_to_world), {
  
  req(indicator$mode, input$year, indicator_info$transformation)
  
  shinyjs::logjs("Map: update world data when indicator is changed")
  
  
  
  pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
  cols <- c('name', 'hdc', 'osmid','admin_level_ordered', 'name', colnames(atlas_city_markers)[startsWith(colnames(atlas_city_markers), pattern)], 'geom')
  a <- atlas_city_markers[cols]
  colnames(a) <- c('name', 'hdc', 'osmid', 'admin_level_ordered', 'name', 'value', 'geom')
  
  # print("a")
  # print(a)
  
  a_available <- subset(a, !is.na(value))
  a_notavailable <- subset(a, is.na(value))
  if(nrow(a_notavailable) > 0) {
    
    a_notavailable$name <- paste0(a_notavailable$name, "<br>Not available for this city")
    
  }
  
  # print(class(atlas_country))
  cols_country <- c('a3', 'name', colnames(atlas_country())[startsWith(colnames(atlas_country()), pattern)], 'geometry')
  a_country <- atlas_country()[cols_country]
  colnames(a_country) <- c('a3', 'name', 'value', 'geometry')
  
  # print("A CONOCN")
  # print(a_country)
  
  
  a_available$value <- as.numeric(a_available$value)
  
  pal <- colorNumeric(
    palette = "viridis",
    na.color = "#808080",
    # palette = "YlGnBu",
    domain = a_available$value)
  
  
  pal_countries <- colorNumeric(
    palette = "viridis",
    # palette = "YlGnBu",
    domain = a_country$value)
  
  
  
  legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  # format legend value
  legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = "", transform = function(x) as.integer(x))
  
  # create the label for the markers
  format_indicator_value_marker <- format_indicator_values(a_available$value, transformation = indicator_info$transformation)
  
  # format_indicator_value_marker <- paste0(format_indicator_value_marker, indicator_info$unit)
  
  # print("data_ind3_spatial()$name")
  # print(data_ind3_spatial()$name)
  # print(data_ind3_spatial()$name)
  
  labels_markers1 <- paste0("<b>", a_available$name, "</b><br/>", 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value_marker), 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                            "<br/><i>Click to go to the region</i>")
  
  # print("format_indicator_value_marker")
  # print(labels_markers1)
  
  # print(a)
  # labels <- paste0("<b>", a_available$name,  "</b><br/> <i>Click to go to the region</i>")
  labels2 <- paste0("<b>", a_notavailable$name,  "</b><br/> <i>Indicator not available for this region</i>")
  
  
  # tooltip for the countries
  format_indicator_value_country <- format_indicator_values(a_country$value, transformation = indicator_info$transformation)
  
  
  labels_country <- paste0("<b>", a_country$name, "</b><br/>", 
                           sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 20px; padding-bottom: 0px\"> %s</span>", format_indicator_value_country),
                           sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit)
  )  
  
  world_view$a_available <- a_available
  world_view$pal <- pal
  world_view$labels_markers1 <- labels_markers1
  world_view$a_notavailable <- a_notavailable
  world_view$labels2 <- labels2
  world_view$a_country <- a_country
  world_view$pal_countries <- pal_countries
  world_view$labels_country <- labels_country
  world_view$legend_title <- legend_title
  world_view$legend_value <- legend_value
  
  
  
})


# update the world map when the indicators is changed ---------------------
observeEvent(c(indicator$mode, input$year, input$back_to_world), {
  
  req(indicator$mode, is.null(rank$admin_level), input$year, indicator_info$transformation)
  
  shinyjs::logjs("Map: update world map when indicator is changed")
  
  # print("obs1")
  
  # pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
  # # print("pattern")
  # # print(pattern)
  # cols <- c('name', 'hdc', 'osmid','admin_level_ordered', 'name', colnames(atlas_city_markers)[startsWith(colnames(atlas_city_markers), pattern)], 'geom')
  # a <- atlas_city_markers[cols]
  # colnames(a) <- c('name', 'hdc', 'osmid', 'admin_level_ordered', 'name', 'value', 'geom')
  # 
  # a_available <- subset(a, !is.na(value))
  # a_notavailable <- subset(a, is.na(value))
  # if(nrow(a_notavailable) > 0) {
  #   
  #   a_notavailable$name <- paste0(a_notavailable$name, "<br>Not available for this city")
  #   
  # }
  # 
  # # print(class(atlas_country))
  # cols_country <- c('a3', 'name', colnames(atlas_country())[startsWith(colnames(atlas_country()), pattern)], 'geometry')
  # a_country <- atlas_country()[cols_country]
  # colnames(a_country) <- c('a3', 'name', 'value', 'geometry')
  # 
  # # print("A CONOCN")
  # # print(a_country)
  # 
  # pal <- colorNumeric(
  #   palette = "viridis",
  #   na.color = "#808080",
  #   # palette = "YlGnBu",
  #   domain = a$value)
  # 
  # 
  # pal_countries <- colorNumeric(
  #   palette = "viridis",
  #   # palette = "YlGnBu",
  #   domain = a_country$value)
  # 
  # 
  # 
  # legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  # legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  # # format legend value
  # legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = "", transform = function(x) as.integer(x))
  # 
  # # create the label for the markers
  # format_indicator_value_marker <- format_indicator_values(a$value, transformation = indicator_info$transformation)
  # 
  # 
  # # format_indicator_value_marker <- paste0(format_indicator_value_marker, indicator_info$unit)
  # 
  # # print("data_ind3_spatial()$name")
  # # print(data_ind3_spatial()$name)
  # # print(data_ind3_spatial()$name)
  # 
  # labels_markers1 <- paste0("<b>", a_available$name, "</b><br/>", 
  #                           sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value_marker), 
  #                           sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
  #                           "<br/><i>Click to go to the region</i>")
  # 
  # 
  # # print(a)
  # # labels <- paste0("<b>", a_available$name,  "</b><br/> <i>Click to go to the region</i>")
  # labels2 <- paste0("<b>", a_notavailable$name,  "</b><br/> <i>Indicator not available for this region</i>")
  # 
  # 
  # # tooltip for the countries
  # format_indicator_value_country <- format_indicator_values(a_country$value, transformation = indicator_info$transformation)
  # 
  # 
  # labels_country <- paste0("<b>", a_country$name, "</b><br/>", 
  #                          sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 20px; padding-bottom: 0px\"> %s</span>", format_indicator_value_country),
  #                          sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit)
  # )  
  # 
  # world_view$a_available <- a_available
  # world_view$pal <- pal
  # world_view$labels_markers1 <- labels_markers1
  # world_view$a_notavailable <- a_notavailable
  # world_view$labels2 <- labels2
  # world_view$a_country <- a_country
  # world_view$pal_countries <- pal_countries
  # world_view$labels_country <- labels_country
  # world_view$legend_title <- legend_title
  # world_view$legend_value <- legend_value
  
  
  map <- leafletProxy("map", data = world_view$a_available) %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    flyTo(lng = 0, lat = 0, zoom = 3) %>%
    addMapPane("countries", zIndex = 410) %>% # shown below ames_circles
    addMapPane("markers_navailable", zIndex = 420) %>% # shown above ames_lines
    addMapPane("markers_available", zIndex = 430) %>% # shown above ames_lines
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 5,
      fillColor = ~world_view$pal(value),
      stroke = TRUE, fillOpacity = 0.9, color = "black",
      opacity = 0.9,
      weight = 2,
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
                     radius = 5,
                     # fillColor = ~pal(value), 
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
                fillColor = ~world_view$pal_countries(value), color = "black",  
                weight = 1, # erro nao eh aqui
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
  
  map
  
  
}) 


# reactive values to store the indicator values
indicator <- reactiveValues(values = NULL, format = NULL, unit = NULL)

# overlay
overlay_layers <- reactiveValues(groups = NULL)




# observer to travel between cities (for the first time) ---------------------------------------

observeEvent(c(city$city_code), {
  
  req(city$city_code != "")
  
  
  shinyjs::logjs("Map: go to city view when coming from world view")
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  # print("obs - switch cities initial")
  
  # req(input$city)
  bbox <- sf::st_bbox(data_ind())
  
  # subset for the metro region polygons
  # data_metro <- subset(data_ind3_spatial())
  data_metro <- subset(data_ind3(), admin_level_ordered == 1)
  
  # print("data_metro")
  # print(data_metro)
  
  
  # if (isTRUE(input$admin_level == 1)) {
  
  osm_selected$oi <- data_metro$osmid
  
  fix <- indicator$mode
  overlay_labels <- subset(overlay_table, indicator == fix)$overlay_label
  # extract geom type of this indicator
  # geom_type <- unique(data_overlays2()$geom_type)
  # print("geom_type")
  # print(geom_type)
  
  # print(data_metro$osmid)
  # print("obs2")
  
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = data_metro$value)
  
  
  
  # format_indicator_value <- if(indicator_info$transformation == "percent") {
  #   round(data_metro$value * 100) 
  #   
  # } else if(indicator_info$transformation %in% "thousands") {
  #   
  #   if (data_metro$value >= 1000000) scales::comma(data_metro$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data_metro$value, accuracy = 1, scale = 0.001, suffix = "k")
  #   
  #   
  # } else round(data_metro$value)
  format_indicator_value <- format_indicator_values(data_metro$value, indicator_info$transformation)
  
  # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
  
  indicator$values <- format_indicator_value
  indicator$unit <- indicator_info$unit
  
  
  labels <- paste0("<b>", data_metro$name, "</b><br/>", 
                   span(style="font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px", indicator$values), 
                   span(style="font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 16px; padding-bottom: 0px; color: #B1B5B9;", indicator$unit), 
                   "<br/><i>Click to see more info</i>")
  
  
  waiter_hide()
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
    removeMarker(layerId = data_metro$osmid) %>%
    clearShapes() %>%
    removeControl(layerId = c("legend_country")) %>%
    removeControl(layerId = c("legend_city")) %>%
    removeLayersControl() %>%
    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
    
    
    # add map panes
    addMapPane("basemap", zIndex = 410) %>%
    addMapPane("Population density", zIndex = 415) %>%
    addMapPane("Average Block Density", zIndex = 416) %>%
    addMapPane("Journey Gap analysis grid", zIndex = 417) %>%
    addMapPane("People Near Services",zIndex = 420) %>%
    addMapPane("Healthcare locations", zIndex = 425) %>%
    addMapPane("Education locations", zIndex = 425) %>%
    addMapPane("People near healthcare", zIndex = 420) %>%
    addMapPane("People near education", zIndex = 420) %>%
    addMapPane("People Near Car-Free Places", zIndex = 420) %>%
    addMapPane("Areas near highways", zIndex = 420) %>%
    addMapPane("Grade-separated highways", zIndex = 425) %>%
    addMapPane("People Near Protected Bikeways", zIndex = 420) %>%
    addMapPane("People Near All Bikeways", zIndex = 420) %>%
    addMapPane("Protected bikeways", zIndex = 425) %>%
    addMapPane("All bikeways", zIndex = 425) %>%
    addMapPane("People Near Rapid Transport", zIndex = 420) %>%
    addMapPane("Metro rail (point)", zIndex = 425) %>%
    addMapPane("Metro rail (line)", zIndex = 425) %>%
    addMapPane("Light rail (point)", zIndex = 425) %>%
    addMapPane("Light rail (line)",zIndex = 425) %>%
    addMapPane("Bus rapid transport (point)", zIndex = 425) %>%
    addMapPane("Bus rapid transport (line)", zIndex = 425) %>%
    addMapPane("People Near Frequent Transport", zIndex = 420) %>%
    addMapPane("Frequent transport stops", zIndex = 425) %>%
    
    
    # add polygon with metro data
    addPolygons(data = data_metro,
                fillColor = ~pal(value), fillOpacity = 0.1,
                color = "#00AE42",  weight = 2, layerId = ~osmid,
                group = "Regions",
                label = lapply(labels, htmltools::HTML),
                labelOptions = labelOptions(
                  style = list(
                    # "color" = "red",
                    # "font-family" = "serif",
                    # "font-style" = "italic",
                    # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px"
                    # "border-color" = "rgba(0,0,0,0.5)"
                  )),
                options = pathOptions(pane = "basemap"),
                highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, weight = 6, color = "black"))
  
  
  # identify overlays to be opened
  fix <- indicator$mode
  overlays_to_open <- subset(overlay_table, indicator == fix)
    
  for(i in overlays_to_open$overlay){

    # data1 <- subset(data_overlays()[["lines"]], ind == i)

    fix <- indicator$mode
    overlay_subset <- subset(overlay_table, indicator == fix & overlay == i)
    overlay_name <- unique(overlay_subset$overlay_label)
    overlay_status <- unique(overlay_subset$overlay_show)
    
    file <- sprintf("../data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.%s", 
                               city$city_code, overlay_subset$overlay, overlay_subset$overlay, city$city_code, input$year, overlay_subset$format)

    if (file.exists(file)) {
      
      if (i %in% c("pop", "block_densities_latlon", "grid_pop_evaluated")) {
        
        data <- readRDS(file)
        
        map <- map %>%
          leafem::addGeoRaster(x = data,
                               opacity = 0.8,
                               group = overlay_name,
                               options = pathOptions(clickable = FALSE, pane = overlay_name),
                               autozoom = FALSE)
        
      } else {
        
        # print(file)
        
        should_fill <- if(grepl("_lines|protectedbike_latlon|allbike_latlon", i)) {
          
          FALSE
          
        } else {TRUE}
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_name,
                                     # color = "#00AE42",
                                     # color = "#000000",
                                     color = overlay_subset$fill,
                                     weight = 1,
                                     opacity = 0.5,
                                     fillColor = overlay_subset$fill,
                                     fill = should_fill,
                                     fillOpacity = 0.7,
                                     radius = 3,
                                     # pane = overlay_name,
                                     # gl = TRUE,
                                     # options = pathOptions(pane = overlay_name),
                                     options = pathOptions(clickable = FALSE, pane = overlay_name)
                                     # layerId = "overlay_layer",
        )
        
        
      }
      
      
    }
  }
  
  
  
  fix <- indicator$mode
  overlay_labels <- subset(overlay_table, indicator == fix)$overlay_label
  # identify groups to hide
  overlay_show <- subset(overlay_table, indicator == fix & overlay_show == "yes")$overlay_label
  overlay_hide <- subset(overlay_table, indicator == fix & overlay_show == "no")$overlay_label
  
  # # identify overlays
  overlay_layers$groups <- overlay_labels
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Satellite"),
      overlayGroups = c("Regions", overlay_labels),
      options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(overlay_hide) %>%
    showGroup(overlay_show)
  
  map %>% stopSpinner() 
  
  
  tl$transition <- 1
  # admin_level_previous$a <- 1
  
  
  # waiter_hide()
  
  
})



# observer to keep selected element highlited -----------------------------

# first, we should create a vector with the selected elements
element <- reactiveValues(selected = NULL,
                          selected1 = NULL,
                          indicator = NULL)
data <- reactiveValues(selected = NULL)


# when the admin level is changed, the vector should be restarted to avoid duplciation
observeEvent(c(rank$admin_level, indicator$mode), {
  element$selected <- NULL
  data$selected <- NULL
  
})



observeEvent(c(input$map_shape_click), {
  
  # waiter_show(html = tagList(
  #   spin_loaders(id = 3, color = "black"
  #                # style = "right: 150px; top: 200px"
  #                )),
  #   color = "rgba(233, 235, 240, .1)")
  ui <- input$map_shape_click$id
  
  # we have a problem that the overlay is being clickable, regardless of what we set
  # so we need to make this overlay un-reactable from here
  ui_ok <- grepl("^\\d{3,}", ui)
  
  # print("ui")
  # print(ui)
  # print(ui_ok)
  
  # this will only happen when we are beyond the city level
  req(isTRUE(input$admin_level >= 1),  isTRUE(input$regions_grid == "Regions"),
      req(data_ind3_spatial()),
      req(ui_ok))
  # req(isTRUE(input$admin_level >= 1),  isTRUE(input$regions_grid == "Regions"))
  
  # first, we should create a vector with the selected elements
  element$selected <- c(element$selected, ui)
  
  if(isTRUE(length(element$selected) == 1) | 
     # isTRUE(variables$indicator[length(variables$indicator)] != variables$indicator[length(variables$indicator)-1]) |
     isTRUE(element$selected[length(element$selected)] != element$selected[length(element$selected)-1])
  ) {
    
    
    # filter only the selected polygon
    data <- subset(data_ind3_spatial(), osmid == ui)
    
    # then select the previous polygon (if exists)
    if (length(element$selected) > 1) {
      
      data_previous <- subset(data_ind3_spatial(), osmid == tail(element$selected, 2)[1])
      # print("data_previous")
      # print(data_previous)
      
      # format_indicator_value <- if(indicator_info$transformation == "percent") {
      #   round(data_previous$value * 100) 
      #   
      # } else if(indicator_info$transformation %in% "thousands") {
      #   
      #   if (data_previous$value >= 1000000) scales::comma(data_previous$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data_previous$value, accuracy = 1, scale = 0.001, suffix = "k")
      #   
      #   
      # } else round(data_previous$value)
      
      format_indicator_value <- format_indicator_values(data_previous$value, indicator_info$transformation)
      
      
      # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
      
      
      
      labels1 <- paste0("<b>", data_previous$name, "</b><br/>", 
                        sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value), 
                        sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                        "<br/><i>Click to see more info</i>")    
      
      
      
    }
    
    # format_indicator_value <- if(indicator_info$transformation == "percent") {
    #   round(data$value * 100) 
    #   
    # } else if(indicator_info$transformation %in% "thousands") {
    #   
    #   if (data$value >= 1000000) scales::comma(data$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data$value, accuracy = 1, scale = 0.001, suffix = "k")
    #   
    #   
    # } else round(data$value)
    
    
    format_indicator_value <- format_indicator_values(data$value, indicator_info$transformation)
    
    # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
    
    
    labels <- paste0("<b>", data$name, "</b><br/>", 
                     sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value), 
                     sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                     "<br/><i>Click to see more info</i>")
    
    
    
    # update the map
    map <- leafletProxy("map", session) %>%
      # 1) delete the selected polygon
      removeShape(layerId = ui) %>%
      # 2) create the selected polygon with the stronger stroke
      addPolygons(data = data,
                  fillColor = data$fill, fillOpacity = 1,
                  # fillColor = ~pal(value),
                  # fillOpacity = 0.5,
                  color = "black",  weight = 8, layerId = ~osmid, opacity = 1,
                  group = "Regions",
                  label = lapply(labels, htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list(
                      # "color" = "red",
                      # "font-family" = "serif",
                      # "font-style" = "italic",
                      # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "font-size" = "12px"
                      # "border-color" = "rgba(0,0,0,0.5)"
                    )),
                  options = pathOptions(pane = "basemap"))
    
    # if there was a previous element, revert it to the oringial color and stroke
    if (length(element$selected) > 1) {
      map <- map %>%
        addPolygons(data = data_previous,
                    group = "Regions",
                    # fis theses colors
                    fillColor = data_previous$fill, fillOpacity = 0.5,
                    label = lapply(labels1, htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list(
                        # "color" = "red",
                        # "font-family" = "serif",
                        # "font-style" = "italic",
                        # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                        "font-size" = "12px"
                        # "border-color" = "rgba(0,0,0,0.5)"
                      )),
                    color = "black",  weight = 1, layerId = ~osmid
        )
    }
    
    
    map
    
  }
  
  # waiter_hide()
  
})

# # if I change the indicator while a region is selected, it should keep the region highlighted
# observeEvent(c(indicator$mode), {
#   
#   # this will only happen when we are beyond the city level
#   req(isTRUE(input$admin_level >= 1),  isTRUE(input$regions_grid == "Regions"))
#   
#   # filter only the selected polygon
#   data <- subset(data_ind3_spatial(), osmid == tail(element$selected, 1))
#   
#   # print("DATA")
#   # print(data)
#   # print(tail(element$selected, 1))
#   
#   # update the map
#   map <- leafletProxy("map", session) %>%
#     # 1) delete the selected polygon
#     removeShape(layerId = tail(element$selected, 1)) %>%
#     # 2) create the selected polygon with the stronger stroke
#     addPolygons(data = data,
#                 fillColor = data$fill, fillOpacity = 1,
#                 # fillColor = ~pal(value),
#                 # fillOpacity = 0.5,
#                 color = "black",  weight = 8, layerId = ~osmid, opacity = 1,
#                 group = "Regions",
#                 # label = lapply(labels, htmltools::HTML),
#                 # labelOptions = labelOptions(
#                 #   style = list(
#                 #     "font-size" = "12px"
#                 #   )),
#                 # options = pathOptions(pane = "basemap")
#     )
#   
#   
#   
#   map
#   
#   # waiter_hide()
#   
# })



# update overlay only when indicator/year is changed --------------------------------

observeEvent(c(indicator$mode, input$year), {
  
  # it will run only when we are at the city level
  req(!is.null(input$admin_level))
  # if (isTRUE(admin_level_previous$a >= 1)) {
  # if (isTRUE(input$admin_level >= 1)) {
  shinyjs::logjs("Map: update overlay only when indicator/year is changed")
  
  # print("obs3----")
  # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
  #             color = "rgba(233, 235, 240, .2)")
  
  # extract geom type of this indicator
  # geom_type <- unique(data_overlays2()$geom_type)
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
    clearGroup("Population density") %>%
    clearGroup("Average Block Density") %>%
    clearGroup("Journey Gap analysis grid" ) %>%
    clearGroup("People Near Services") %>%
    clearGroup("Healthcare locations" ) %>%
    clearGroup("Education locations" ) %>%
    clearGroup("People near healthcare" ) %>%
    clearGroup("People near education" ) %>%
    clearGroup("People Near Car-Free Places" ) %>%
    clearGroup("Areas near highways" ) %>%
    clearGroup("Grade-separated highways" ) %>%
    clearGroup("People Near Protected Bikeways" ) %>%
    clearGroup("People Near All Bikeways" ) %>%
    clearGroup("Protected bikeways" ) %>%
    clearGroup("All bikeways" ) %>%
    clearGroup("People Near Rapid Transport" ) %>%
    clearGroup("Metro rail (point)" ) %>%
    clearGroup("Metro rail (line)" ) %>%
    clearGroup("Light rail (point)" ) %>%
    clearGroup("Light rail (line)") %>%
    clearGroup("Bus rapid transport (point)" ) %>%
    clearGroup("Bus rapid transport (line)" ) %>%
    clearGroup("People Near Frequent Transport" ) %>%
    clearGroup("Frequent transport stops" ) %>%
    
    
    addMapPane("basemap", zIndex = 410) %>%
    addMapPane("Population density", zIndex = 415) %>%
    addMapPane("Average Block Density", zIndex = 416) %>%
    addMapPane("Journey Gap analysis grid", zIndex = 417) %>%
    addMapPane("People Near Services",zIndex = 420) %>%
    addMapPane("Healthcare locations", zIndex = 425) %>%
    addMapPane("Education locations", zIndex = 425) %>%
    addMapPane("People near healthcare", zIndex = 420) %>%
    addMapPane("People near education", zIndex = 420) %>%
    addMapPane("People Near Car-Free Places", zIndex = 420) %>%
    addMapPane("Areas near highways", zIndex = 420) %>%
    addMapPane("Grade-separated highways", zIndex = 425) %>%
    addMapPane("People Near Protected Bikeways", zIndex = 420) %>%
    addMapPane("People Near All Bikeways", zIndex = 420) %>%
    addMapPane("Protected bikeways", zIndex = 425) %>%
    addMapPane("All bikeways", zIndex = 425) %>%
    addMapPane("People Near Rapid Transport", zIndex = 420) %>%
    addMapPane("Metro rail (point)", zIndex = 425) %>%
    addMapPane("Metro rail (line)", zIndex = 425) %>%
    addMapPane("Light rail (point)", zIndex = 425) %>%
    addMapPane("Light rail (line)",zIndex = 425) %>%
    addMapPane("Bus rapid transport (point)", zIndex = 425) %>%
    addMapPane("Bus rapid transport (line)", zIndex = 425) %>%
    addMapPane("People Near Frequent Transport", zIndex = 420) %>%
    addMapPane("Frequent transport stops", zIndex = 425) %>%
    removeLayersControl()
  
  
  
  # print("geom_type")
  # print(geom_type)
  
  # add overlay
  # identify overlays to be opened
  fix <- indicator$mode
  overlays_to_open <- subset(overlay_table, indicator == fix)
  # print("overlays_to_open")
  # print(overlays_to_open)
  
  for(i in overlays_to_open$overlay){
    
    # data1 <- subset(data_overlays()[["lines"]], ind == i)
    
    fix <- indicator$mode
    overlay_subset <- subset(overlay_table, indicator == fix & overlay == i)
    overlay_name <- unique(overlay_subset$overlay_label)
    overlay_status <- unique(overlay_subset$overlay_show)
    
    file <- sprintf("../data/data_beta/ghsl_%s/overlays/%s/%s_%s_%s.%s", 
                    city$city_code, overlay_subset$overlay, overlay_subset$overlay, city$city_code, input$year, overlay_subset$format)
    
    if (file.exists(file)) {
      
      if (i %in% c("pop", "block_densities_latlon", "grid_pop_evaluated")) {
        
        data <- readRDS(file)
        
        map <- map %>%
          leafem::addGeoRaster(x = data,
                               opacity = 0.8,
                               group = overlay_name,
                               options = pathOptions(clickable = FALSE, pane = overlay_name),
                               autozoom = FALSE)
        
      } else {
        
        # print(file)
        
        should_fill <- if(grepl("_lines|protectedbike_latlon|allbike_latlon", i)) {
          
          FALSE
          
        } else {TRUE}
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_name,
                                     # color = "#00AE42",
                                     # color = "#000000",
                                     color = overlay_subset$fill,
                                     weight = 1,
                                     opacity = 0.5,
                                     fillColor = overlay_subset$fill,
                                     fill = should_fill,
                                     fillOpacity = 0.7,
                                     radius = 3,
                                     # pane = overlay_name,
                                     # gl = TRUE,
                                     # options = pathOptions(pane = overlay_name),
                                     options = pathOptions(clickable = FALSE, pane = overlay_name)
                                     # layerId = "overlay_layer",
        )
        
        
      }
      
      
    }
  }
  
  
  # # identify overlays
  # overlay_layers
  fix <- indicator$mode
  overlay_labels <- subset(overlay_table, indicator == fix)$overlay_label
  # identify groups to hide
  overlay_show <- subset(overlay_table, indicator == fix & overlay_show == "yes")$overlay_label
  overlay_hide <- subset(overlay_table, indicator == fix & overlay_show == "no")$overlay_label
  
  # print("overlay_show")
  # print(overlay_show)
  # print(overlay_hide)
  
  
  # # identify overlays
  overlay_layers$groups <- overlay_labels
  
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Satellite"),
      overlayGroups = c("Regions"
                        , overlay_labels),
      options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(overlay_hide) %>%
    showGroup(overlay_show)
  
  
  map %>% stopSpinner()
  # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')                     
  # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')                       
  
  
  # }
  # waiter_hide()
  
  
  
})


rv <- reactiveValues(prev_city = 1)  

observeEvent(c(city$city_code, rank$admin_level), {
  
  # print("pus")
  # print(rank$admin_level)
  
  req(rank$admin_level > 1)
  
  rv$prev_city <- c(rv$prev_city, rep(city$city_code, 2))
  
  print("comeh")
  print(rv$prev_city)
  
  
}, ignoreInit = TRUE, priority = 2)

observeEvent(c(indicator$mode), {
  
  # print("pus")
  # print(rank$admin_level)
  
  req(rank$admin_level >= 1)
  
  rv$prev_city <- c(rv$prev_city, rep(city$city_code, 2))
  
  
}, ignoreInit = TRUE, priority = 2)



# update the regions basemap  --------------------------------

# reactive to filter data and add column with colors
data_ind3_spatial <- reactive({
  
  req(city$city_code != "", indicator$mode,  input$regions_grid)
  # req(data_ind3(), input$admin_level, rank$admin_level)
  
  
  rank$admin_level
  input$admin_level
  
  isolate({
    
    if (indicator$type == "performance"  & input$regions_grid == "Grid") {
      
      
      a <- data_ind3() 
      
    } else {
      
      
      if (length(data_ind3()$admin_level) == 1) {
        
        
        a <- subset(data_ind3(), admin_level_ordered == 1)
        
      } else {
        
        # a <- subset(data_ind3(), admin_level_ordered ==  input$admin_level)
        a <- subset(data_ind3(), admin_level_ordered ==  rank$admin_level)
      }
      
      
    }
    
    
    # create the color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = a$value)
    
    # create a column with the colors
    a$fill <- pal(a$value)
    
    # print(a)
    return(a)
    
  })
  
})


# update basemap when anything changes at the city level
observeEvent(c(
  rank$admin_level,
  indicator$mode,
  input$year,
  input$regions_grid
), {
  
  
  # this observer will only run if whe aren't switching cities
  previous_city <- rv$prev_city[length(rv$prev_city)-1]
  
  
  # print(city$city_code)
  # print(rv$prev_city)
  
  # it will only run if the actual city is equal to the previous city
  req(city$city_code == previous_city,
      isTRUE(rank$admin_level >= 1),
      data_ind3_spatial())
  
  shinyjs::logjs("hello")
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  
  
  # filter legend title and values
  legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  # format legend value
  legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = " ", transform = function(x) as.integer(x))
  
  
  format_indicator_value <- format_indicator_values(data_ind3_spatial()$value, indicator_info$transformation)
  
  # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
  
  indicator$values <- format_indicator_value
  indicator$unit <- indicator_info$unit

  print(indicator$values)
  print(indicator$unit)
    
  
  labels <- paste0("<b>", data_ind3_spatial()$name, "</b><br/>", 
                   sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", indicator$values), 
                   sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9;\"> %s</span>", indicator$unit), 
                   "<br/><i>Click to see more info</i>")
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
    # clearMarkers() %>%
    # removeShape(layerId =  osm_selected$oi) %>%
    # removeTiles(layerId =  "tile") %>%
    removeShape(layerId = "reach") %>%
    clearGroup(group = "Regions") %>%
    # clearShapes() %>%
    # removeShape(layerId = city$osmid ) %>%
    clearControls() %>%
    addMapPane("basemap", zIndex = 410) %>%
    addMapPane("Population density", zIndex = 415) %>%
    addMapPane("Average Block Density", zIndex = 416) %>%
    addMapPane("Journey Gap analysis grid", zIndex = 417) %>%
    addMapPane("People Near Services", zIndex = 420) %>%
    addMapPane("Healthcare locations", zIndex = 425) %>%
    addMapPane("Education locations", zIndex = 425) %>%
    addMapPane("People near healthcare", zIndex = 420) %>%
    addMapPane("People near education", zIndex = 420) %>%
    addMapPane("People Near Car-Free Places", zIndex = 420) %>%
    addMapPane("Areas near highways", zIndex = 420) %>%
    addMapPane("Grade-separated highways", zIndex = 425) %>%
    addMapPane("People Near Protected Bikeways", zIndex = 430) %>%
    addMapPane("People Near All Bikeways", zIndex = 420) %>%
    addMapPane("Protected bikeways", zIndex = 425) %>%
    addMapPane("All bikeways", zIndex = 425)
  # addMapPane("People Near Rapid Transport", zIndex = 420) %>%
  # addMapPane("Metro rail (point)", zIndex = 425) %>%
  # addMapPane("Metro rail (line)", zIndex = 425) %>%
  # addMapPane("Light rail (point)", zIndex = 425) %>%
  # addMapPane("Light rail (line)",zIndex = 425) %>%
  # addMapPane("Bus rapid transport (point)", zIndex = 425) %>%
  # addMapPane("Bus rapid transport (line)", zIndex = 425) %>%
  # addMapPane("People Near Frequent Transport", zIndex = 420) %>%
  # addMapPane("Frequent transport stops", zIndex = 425)
  
  
  # record the values in a list
  leaflet_params <- list(
    label1 = if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) "Click to see the reach" else lapply(labels, htmltools::HTML),
    bring_to_front = if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) TRUE else FALSE,
    weigth1 = if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) 2 else 6,
    weigth2 = if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) 0.01 else 2
    
  )
  
  
  # opacity
  if (input$admin_level == 1) {
    
    color_stroke <- "#00AE42"
    weight_stroke <- 2
    opacity <- 0.1
    
  } else {
    color_stroke <- "black"
    weight_stroke <- leaflet_params$weigth2
    opacity <- 0.5
  }

  
  map <- map %>%
    addPolygons(data = data_ind3_spatial(), 
                fillColor = data_ind3_spatial()$fill,
                fillOpacity = opacity,
                color = color_stroke,  weight = weight_stroke, 
                group = "Regions",
                label = leaflet_params$label1,
                labelOptions = labelOptions(
                  style = list(
                    # "color" = "red",
                    # "font-family" = "serif",
                    # "font-style" = "italic",
                    # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px"
                    # "border-color" = "rgba(0,0,0,0.5)"
                  )),
                layerId = ~osmid,
                options = pathOptions(pane = "basemap", 
                                      clickable = TRUE),
                highlightOptions = highlightOptions(bringToFront = leaflet_params$bring_to_front, opacity = 1, 
                                                    weight = leaflet_params$weigth1, color = "black"
                                                    # fillColor = 'yellow'
                )
                # label = ~(label)
    )
  
  
  if (input$admin_level != 1)  {
    
    map <- map %>%
      addLegend(data = data_ind3_spatial(), "bottomright",
                pal = colorNumeric(
                  palette = "YlOrRd",
                  domain = NULL),
                values = ~value,
                title = legend_title,
                # bins = c(0, 0.25, 0.50, 0.75, 1),
                labFormat = legend_value,
                layerId = "legend_city"
      )
  } else map <- map
  
  
  if (!(indicator$mode %in% c("popdensity"))) map <- map %>% showGroup("Regions")
  
  
  map %>% stopSpinner()
  
  osm_selected$oi <- data_ind3_spatial()$osmid
  
  waiter_hide()
  
}, priority = 1)



# display reaches when the map is clicked (grid only) ---------------------

observeEvent(c(input$map_shape_click), {
  
  # this will only happen when we are beyond the city level
  req(isTRUE(input$admin_level >= 1), isTRUE(input$regions_grid == "Grid"))
  
  # get id of clicked element
  ui <- input$map_shape_click$id
  
  # open file
  reach <- readRDS(sprintf("../data/data_alpha/ghsl_%s/reaches/reaches_%s_%s_%s.rds", city$city_code, city$city_code, ui, indicator$mode))
  
  leafletProxy("map", session) %>%
    removeShape(layerId = "reach") %>%
    addPolygons(data = reach, 
                # group = "Overlay",
                opacity = 0.8,
                options = pathOptions(clickable = FALSE, pane = "overlay"),
                layerId = "reach",
                fillColor = "#00AE42", fillOpacity = 0.6,
                weight = 1, color = "black"
    ) 
  
  
  
  
})

