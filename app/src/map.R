# update map --------------------------------------------------------------


osm_selected <- reactiveValues(oi = NULL)

admin_level_previous <- reactiveValues(a = NULL)


tl <- reactiveValues(transition = NULL)


# initial map
output$map <- renderLeaflet({
  
  
  map <- leaflet(data = atlas_city_markers, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light", layerId = "epa") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addLayersControl(baseGroups = c("Dark", "Light", "Satellite"),
                     # overlayGroups = c("Overlay"),
                     options = layersControlOptions(collapsed = FALSE),
                     position = "topright") %>%
    # htmlwidgets::onRender("
    #                       function() {
    #                     L.control.layers({'Light'}).addTo(this);
    #                       }
    #                       ") %>%
    #   htmlwidgets::onRender("function(el, x) {
    #            var overlayMaps = {'Towns': groups.Light};
    #     L.control.layers(overlayMaps).addTo(this);
    # }") %>%
    # hideGroup("Overlay") %>%
    
  # addControl(html = "<h3> QUEEEEEEEEEE </h3>") %>%
  
  # addCircleMarkers(
  #   # radius = ~ifelse(type == "ship", 6, 10),
  #   radius = 10,
  #   # color = ~pal(bike_pnpb_2019),
  #   stroke = TRUE, fillOpacity = 0.5,
  #   layerId = ~hdc
  # ) %>%
  # addPolygons(data = atlas_country,
  #             fillColor = ~pal_countries(bike_pnpb_2019), color = "black",  weight = 1,
  #             options = pathOptions(clickable = FALSE)) %>%
  # # addPolygons(fillColor = ~pal(pnpb), color = "black", layerId = ~code_metro) %>%
  # addLegend("bottomleft", pal = pal_countries, values = ~atlas_country$bike_pnpb_2019) %>%
  # htmlwidgets::onRender("
  #     function(el, x) {
  #         $('.leaflet-control-layers').prepend('<label>My Epic Title</label>');
  #     }
  # ") %>%
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


# update the world map when the indicators is changed ---------------------
observeEvent(c(indicator_mode(), input$year), {
  
  req(indicator_mode(), is.null(rank$admin_level), input$year)               
  
  
  print("obs1")
  
  # print(indicator_mode())
  
  # this will only runs if we are at the wold view (admin level = null)
  # if(is.null(input$admin_level)) {
  
  # print(atlas_city_markers)
  
  pattern <- sprintf("%s_%s_%s", indicator$type, indicator_mode(), input$year)
  # print("pattern")
  # print(pattern)
  cols <- c('name', 'hdc', 'osmid','admin_level_ordered', 'name', colnames(atlas_city_markers)[startsWith(colnames(atlas_city_markers), pattern)], 'geom')
  a <- atlas_city_markers[cols]
  colnames(a) <- c('name', 'hdc', 'osmid', 'admin_level_ordered', 'name', 'valor', 'geom')
  
  # print(class(atlas_country))
  cols_country <- c('a2', colnames(atlas_country)[startsWith(colnames(atlas_country), pattern)], 'geom')
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
    clearShapes() %>%
    flyTo(lng = 0, lat = 0, zoom = 3) %>%
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
              layerId = "legend_country") 
  
  # addLayersControl(baseGroups = c("Dark", "Light", "Satellite"),
  #                  options = layersControlOptions(collapsed = FALSE),
  #                  position = "topright") %>%
  
  
  
  #   htmlwidgets::onRender("
  #     function(el, x) {
  #         $('.leaflet-control-layers').prepend('<label>My Epic Title</label>');
  #     }
  # ")
  
  
  # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')                     
  # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')                       
  
  # }
  
  
}) 






# observer to travel between cities ---------------------------------------


observeEvent(c(city$city_code), {
  
  # req(input$city)
  bbox <- sf::st_bbox(data_ind())
  
  # subset for the metro region polygons
  data_metro <- subset(data_ind3(), admin_level_ordered == 1)
  
  
  # if (isTRUE(input$admin_level == 1)) {
  
  osm_selected$oi <- data_metro$osmid
  
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  
  # print(data_metro$osmid)
  print("obs2")
  
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = data_metro$valor)
  
  
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    removeMarker(layerId = data_metro$osmid) %>%
    clearShapes() %>%
    removeControl(layerId = "legend_country") %>%
    removeLayersControl() %>%
    # clearControls() %>%
    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
    addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
    addMapPane("overlay", zIndex = 420) %>% # shown above ames_lines
    # flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]],
    #             options = list(duration = 1.5,
    #                            animate = TRUE,
    #                            easeLinearity = 2,
    #                            noMoveStart = TRUE)) %>%
    
    
    addPolygons(data = data_metro,
                fillColor = ~pal(valor), fillOpacity = 0.5,
                color = "black",  weight = 1, layerId = ~osmid,
                label = ~htmltools::htmlEscape(name),
                options = pathOptions(pane = "basemap"),
                highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, weight = 6, color = "black")) %>%
    # addLegend("bottomleft", pal = pal, values = ~valor) %>%
    addLayersControl(
      overlayGroups = c("Overlay"),
      baseGroups = c("Dark", "Light", "Satellite"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright") 
  
  
  #   htmlwidgets::onRender('
  #     function() {
  #         $(".leaflet-control-layers").prepend("<label>My Epic Title</label>");
  #     }
  # ')
  
  
  
  # add overlay
  if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    
    map <- map %>%
      addPolygons(data = data_overlays_sf(), 
                  group = "Overlay",
                  opacity = 0.8,
                  options = pathOptions(clickable = FALSE, pane = "overlay"),
                  layerId = "overlay_layer",
                  fillColor = "#00AE42", fillOpacity = 0.6,
                  weight = 1, color = "black"
      ) 
    
    
    
    
    
  } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
    
    
    map <- map %>%
      addPolylines(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                   layerId = "overlay_layer")
    
    
  }
  
  
  map
  
  tl$transition <- 1
  # admin_level_previous$a <- 1
  
  
  
  
})



# observer to keep selected element highlited -----------------------------

# first, we should create a vector with the selected elements
element <- reactiveValues(selected = NULL)


# when the admin level is changed, the vector should be restarted to avoid duplciation
observeEvent(c(input$admin_level), {
  element$selected <- NULL
  
})


observeEvent(c(input$map_shape_click), {
  
  # this will only happen when we are beyond the city level
  req(isTRUE(input$admin_level >= 1))
  
  # first, we should create a vector with the selected elements
  ui <- input$map_shape_click$id
  element$selected <- c(element$selected, ui)
  print("element$selected")
  print(input$map_shape_click)
  
  
  # thigs to do here:
  # 1) delete the selected polygon
  # 2) create the selected polygon with the stronger stroke
  # 3) "diselect" when another polygon is selected
  
  
  # filter only the selected polygon
  data <- subset(data_ind3_spatial(), osmid == ui)
  # then select the previous polygon (if exists)
  if (length(element$selected) > 1) {
    
    data_previous <- subset(data_ind3_spatial(), osmid == tail(element$selected, 2)[1])
    # print("data_previous")
    # print(data_previous)
    
    
  }
  
  
  
  # update the map
  map <- leafletProxy("map", session) %>%
    # 1) delete the selected polygon
    removeShape(layerId = ui) %>%
    # 2) create the selected polygon with the stronger stroke
    addPolygons(data = data,
                fillColor = data$fill, fillOpacity = 1,
                # fillColor = ~pal(valor), 
                # fillOpacity = 0.5,
                color = "black",  weight = 8, layerId = ~osmid, opacity = 1,
                label = ~htmltools::htmlEscape(name),
                options = pathOptions(pane = "basemap"))
  
  # if there was a previous element, revert it to the oringial color and stroke
  if (length(element$selected) > 1) {
    map <- map %>%
      addPolygons(data = data_previous, 
                  # fis theses colors
                  fillColor = data_previous$fill, fillOpacity = 0.5,
                  label = ~htmltools::htmlEscape(name),
                  color = "black",  weight = 1, layerId = ~osmid
      )
  }
  
  
  map
  
})



# update overlay only when indicator is changed --------------------------------

observeEvent(c(indicator_mode()), {
  
  # it will run only when we are at the city level
  req(!is.null(input$admin_level))
  # if (isTRUE(admin_level_previous$a >= 1)) {
  # if (isTRUE(input$admin_level >= 1)) {
  
  print("obs3")
  
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  # print("geom_type")
  # print(geom_type)
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    removeShape(layerId = "overlay_layer") %>%
    addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
    addMapPane("overlay", zIndex = 420) %>% # shown above ames_lines
    removeLayersControl()
  
  
  
  # add overlay
  if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    
    map <- map %>%
      addPolygons(data = data_overlays_sf(), group = "Overlay", opacity = 0.8,
                  options = pathOptions(clickable = FALSE, pane = "overlay"),
                  layerId = "overlay_layer",
                  fillColor = "#00AE42", fillOpacity = 0.6,
                  weight = 1, color = "black"
      ) %>%
      # addLegend("bottomleft", pal = pal, values = ~valor) %>%
      addLayersControl(overlayGroups = c("Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE))
    
    
  } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
    
    
    map <- map %>%
      addPolylines(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                   layerId = "overlay_layer") %>%
      # addLegend("bottomleft", pal = pal, values = ~valor) %>%
      addLayersControl(overlayGroups = c("Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE))
    
    
    
  }
  
  map
  # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')                     
  # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')                       
  
  
  # }
  
  
  
})


rv <- reactiveValues(prev_city = 1)  

observeEvent(c(city$city_code,
               input$admin_level,
               input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_city), {
                 
                 # rv$prev_city <- if (length(rv$prev_city)== 1) city$city_code else rv$prev_city
                 # print(rv$prev_city)
                 rv$prev_city <- c(rv$prev_city, city$city_code)
                 # a <- if(city$city_code == "") NULL
                 # print(rv$prev_city)
                 
                 # print(rv$prev_city)
                 # print("vai")
                 # print(city$city_code)
                 # print(rv$prev_city[length(rv$prev_city)-1])
                 
                 
               }, ignoreInit = TRUE)


# update the basemap  --------------------------------

# reactive to filter data and add column with colors
data_ind3_spatial <- reactive({
  
  req(data_ind3(), input$admin_level)
  
  a <- subset(data_ind3(), admin_level_ordered == input$admin_level)
  
  
  # create the color palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = a$valor)
  
  # create a column with the colors
  a$fill <- pal(a$valor)
  
  return(a)
  
})



observeEvent(c(input$admin_level,
               indicator_mode()), {
                 
                 
                 
                 # this observer will only run if whe aren't switching cities
                 previous_city <- rv$prev_city[length(rv$prev_city)-1]
                 
                 req(city$city_code == previous_city,
                     isTRUE(input$admin_level >= 1),
                     data_ind3_spatial())
                 
                 
                 
                 # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
                 #             color = "rgba(233, 235, 240, .4)")
                 
                 
                 print("obs4")
                 
                 # extract geom type of this indicator
                 geom_type <- unique(data_overlays2()$geom_type)
                 
                 # filter legend title and values
                 legend_title <- subset(list_indicators, indicator_code == indicator_mode())$indicator_name
                 legend_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
                 # format legend value
                 legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = " km", transform = function(x) as.integer(x))
                 
                 
                 
                 map <- leafletProxy("map", session) %>%
                   # clearMarkers() %>%
                   removeShape(layerId =  osm_selected$oi) %>%
                   removeTiles(layerId =  "tile") %>%
                   # clearShapes() %>%
                   clearControls() %>%
                   addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
                   addMapPane("overlay", zIndex = 420)# shown above ames_lines
                 
                 
                 if (isTRUE(input$indicator_bike == "pnabb")) {
                   
                   
                   # create the color palette
                   pal <- colorNumeric(
                     palette = "YlOrRd",
                     domain = seq(0, 1000, 50))
                   
                   map <- map %>%
                     mapboxapi::addMapboxTiles(style_id = "cl1b8ovgb001b14nuzqlt28sz",
                                               username = "kauebraga",
                                               layerId = "tile",
                                               options = tileOptions(zIndex = 9999,  opacity = 0.8),
                                               access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ") %>%
                     addLegend(pal = pal,  "bottomright",values = seq(0, 1000, 50), title = "teste")
                   
                   
                 } else  {
                   
                   
                   map <- map %>%
                     addPolygons(data = data_ind3_spatial(), 
                                 fillColor = data_ind3_spatial()$fill,
                                 fillOpacity = 0.5,
                                 color = "black",  weight = 1, 
                                 label = ~htmltools::htmlEscape(name),
                                 layerId = ~osmid,
                                 options = pathOptions(pane = "basemap"),
                                 highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, 
                                                                     weight = 6, color = "black"
                                                                     # fillColor = 'yellow'
                                 )
                                 # label = ~(label)
                     ) %>%
                     addLegend(data = data_ind3_spatial(), "bottomright",
                               pal = colorNumeric(
                                 palette = "YlOrRd",
                                 domain = NULL),
                               values = ~valor,
                               title = legend_title,
                               # bins = c(0, 0.25, 0.50, 0.75, 1),
                               labFormat = legend_value
                     )
                   
                   
                 } 
                 
                 map
                 
                 osm_selected$oi <- data_ind3_spatial()$osmid
                 
                 # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')
                 # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')
                 
                 # }
                 
                 waiter_hide()
                 # v$rng1 <- city$city_code
                 
                 
               })

# observeEvent(c(input$indicator), {
#   
#   # filter to selected spatial_level
#   # the highest visualiztion will be aggregated at cities
#   # create label
#   # data_ind2_spatial <- data_ind2_spatial %>% dplyr::mutate(label = sprintf("<h3>%s</h3><br/>Click for more information", name))
#   # data_ind2_spatial$label <- purrr::map_chr(data_ind2_spatial$label, ~htmltools::HTML)
#   
#   pal <- colorNumeric(
#     palette = "viridis",
#     domain = data_ind2()$valor)
#   
#   
#   
#   leafletProxy("map", session) %>%
#     clearMarkers() %>%
#     clearShapes() %>%
#     clearControls() %>%
#     addPolygons(data = data_ind2(), fillColor = ~pal(valor), color = "black",  weight = 1
#                 # , layerId = ~name
#                 # label = ~(label)
#     ) %>%
#     addLegend(data = data_ind2(), "bottomleft", pal = pal, values = ~valor) %>%
#     addPolygons(data = data_overlays2(), group = "Overlay") %>%
#     addLayersControl(overlayGroups = c("Overlay"),
#                      # position = c("topleft"),
#                      options = layersControlOptions(collapsed = FALSE))
#   
#   
# })