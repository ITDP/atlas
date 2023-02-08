# # update map --------------------------------------------------------------
# 
# # https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
# # https://stackoverflow.com/questions/52846472/leaflet-plugin-and-leafletproxy-with-polylinedecorator-as-example
# spinPlugin <- htmlDependency(
#   "spin.js",
#   "2.3.2",
#   src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/spin.js/2.3.2"),
#   script = "spin.min.js") # there's no spin.css
# 
# leafletspinPlugin <- htmlDependency(
#   "Leaflet.Spin",
#   "1.1.2",
#   src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/Leaflet.Spin/1.1.2"),
#   script = "leaflet.spin.min.js")
# 
# registerPlugin1 <- function(map, plugin) {
#   map$dependencies <- c(map$dependencies, list(plugin))
#   map
# }
# 
# # Note: Ctrl-Shift-J opens the javascript console in the browser
# spin_event <- "function(el, x) {
#   console.log('spin event added');
#   var mymap = this;
#   mymap.on('layerremove', function(e) {
#     console.log('layerremove fired');
#     mymap.spin(true);
#   });
#   mymap.on('layeradd', function(e) {
#     console.log('layeradd fired');
#     mymap.spin(false);
#   });
# }"



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
  
  req(indicator$mode, indicator$type, is.null(rank$admin_level))
  
  # print("oiiiiiiiiiiiiii")
  
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  # open data
  a <- readRDS(sprintf("../data/data_alpha/countries/atlas_country_%s.rds", pattern))
  
  # print("aagsagas")
  # print(a)
  
  
})


osm_selected <- reactiveValues(oi = NULL)

admin_level_previous <- reactiveValues(a = NULL)


tl <- reactiveValues(transition = NULL)


# initial map
output$map <- renderLeaflet({
  
  # input$print
  
  map <- leaflet(data = atlas_city_markers, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light", layerId = "epa") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    # registerPlugin1(spinPlugin) %>% 
    # registerPlugin1(leafletspinPlugin) %>% 
    addLayersControl(baseGroups = c("Dark", "Light", "Satellite"),
                     # overlayGroups = c("Overlay"),
                     options = layersControlOptions(collapsed = FALSE),
                     position = "topright") %>%
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


# update the world map when the indicators is changed ---------------------
observeEvent(c(indicator$mode, input$year, input$back_to_world), {
  
  req(indicator$mode, is.null(rank$admin_level), input$year, indicator_info$transformation)
  
  
  # print("obs1")
  
  # print(indicator$mode)
  
  # this will only runs if we are at the wold view (admin level = null)
  # if(is.null(input$admin_level)) {
  
  # print(atlas_city_markers)
  # at <- Sys.time()
  
  pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, input$year)
  # print("pattern")
  # print(pattern)
  cols <- c('name', 'hdc', 'osmid','admin_level_ordered', 'name', colnames(atlas_city_markers)[startsWith(colnames(atlas_city_markers), pattern)], 'geom')
  a <- atlas_city_markers[cols]
  colnames(a) <- c('name', 'hdc', 'osmid', 'admin_level_ordered', 'name', 'value', 'geom')
  
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
  
  pal <- colorNumeric(
    palette = "viridis",
    na.color = "#808080",
    # palette = "YlGnBu",
    domain = a$value)
  
  
  pal_countries <- colorNumeric(
    palette = "viridis",
    # palette = "YlGnBu",
    domain = a_country$value)
  
  
  
  legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  # format legend value
  legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = "", transform = function(x) as.integer(x))
  
  # create the label for the markers
  format_indicator_value_marker <- format_indicator_values(a$value, transformation = indicator_info$transformation)
  
  
  # format_indicator_value_marker <- paste0(format_indicator_value_marker, indicator_info$unit)
  
  # print("data_ind3_spatial()$name")
  # print(data_ind3_spatial()$name)
  # print(data_ind3_spatial()$name)
  
  labels_markers1 <- paste0("<b>", a_available$name, "</b><br/>", 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value_marker), 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                            "<br/><i>Click to go to the region</i>")
  
  
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
  
  
  map <- leafletProxy("map", data = a_available) %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    flyTo(lng = 0, lat = 0, zoom = 3) %>%
    addMapPane("countries", zIndex = 410) %>% # shown below ames_circles
    addMapPane("markers_navailable", zIndex = 420) %>% # shown above ames_lines
    addMapPane("markers_available", zIndex = 430) %>% # shown above ames_lines
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 8,
      fillColor = ~pal(value),
      stroke = TRUE, fillOpacity = 0.9, color = "black",
      opacity = 0.9,
      # weight = 1,
      layerId = ~hdc,
      label = lapply(labels_markers1, htmltools::HTML),
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
    addCircleMarkers(data = a_notavailable,
                     # radius = ~ifelse(type == "ship", 6, 10),
                     radius = 8,
                     # fillColor = ~pal(value), 
                     stroke = TRUE, fillOpacity = 0.9, color = "#808080",
                     opacity = 0.8,
                     weight = 1,
                     layerId = ~hdc,
                     label = lapply(labels2, htmltools::HTML),
                     # label = ~htmltools::htmlEscape(name),
                     options = pathOptions(clickable = FALSE, pane = "markers_navailable")
    ) %>%
    addPolygons(data = a_country,
                layerId = ~name,
                fillColor = ~pal_countries(value), color = "black",  weight = 0, # erro nao eh aqui
                fillOpacity = 0.7,
                options = pathOptions(clickable = TRUE, pane = "countries"),
                group = "Countries",
                label = lapply(labels_country, htmltools::HTML)
    ) %>%
    # add polygons with the country color
    # addPolygons(fillColor = ~pal(pnpb), color = "black", layerId = ~code_metro) %>%
    addLegend("bottomright", pal = pal_countries, values = ~a_country$value,
              title = legend_title,
              # bins = 7,
              labFormat = legend_value,
              layerId = "legend_country")
  
  map
  
  
}) 


# reactive values to store the indicator values
indicator <- reactiveValues(values = NULL, format = NULL, unit = NULL)



# observer to travel between cities ---------------------------------------


observeEvent(c(city$city_code), {
  
  req(city$city_code != "")
  
  waiter_show(
    html = tagList(spin_loaders(id = 2, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  print("obs - switch cities initial")
  
  # req(input$city)
  bbox <- sf::st_bbox(data_ind())
  
  # subset for the metro region polygons
  # data_metro <- subset(data_ind3_spatial())
  data_metro <- subset(data_ind3(), admin_level_ordered == 1)
  
  # print("data_metro")
  # print(data_metro)
  
  
  # if (isTRUE(input$admin_level == 1)) {
  
  osm_selected$oi <- data_metro$osmid
  
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  # print("geom_type")
  # print(geom_type)
  
  # print(data_metro$osmid)
  # print("obs2")
  
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = data_metro$value)
  
  waiter_hide()
  
  
  format_indicator_value <- if(indicator_info$transformation == "percent") {
    round(data_metro$value * 100) 
    
  } else if(indicator_info$transformation %in% "thousands") {
    
    if (data_metro$value >= 1000000) scales::comma(data_metro$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data_metro$value, accuracy = 1, scale = 0.001, suffix = "k")
    
    
  } else round(data_metro$value)
  
  # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
  
  indicator$values <- format_indicator_value
  indicator$unit <- indicator_info$unit
  
  
  labels <- paste0("<b>", data_metro$name, "</b><br/>", 
                   span(style="font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px", indicator$values), 
                   span(style="font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 16px; padding-bottom: 0px; color: #B1B5B9;", indicator$unit), 
                   "<br/><i>Click to see more info</i>")
  
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "white")) %>%
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
                fillColor = ~pal(value), fillOpacity = 0.5,
                color = "black",  weight = 1, layerId = ~osmid,
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
                highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, weight = 6, color = "black")) %>%
    # addLegend("bottomleft", pal = pal, values = ~value) %>%
    addLayersControl(
      overlayGroups = c("Regions", "Overlay"),
      baseGroups = c("Dark", "Light", "Satellite"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright")
  
  
  
  
  
  # add overlay
  if(indicator$mode %in% c("popdensity", "blockdensity")) {
    
    # print("gooooo")
    # print(head(data_overlays_sf()))
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = overlay_geom$polygon$value)
    
    map <- map %>%
      addPolygons(data = overlay_geom$polygon,
                  group = "Overlay",
                  options = pathOptions(clickable = FALSE, pane = "overlay"),
                  # layerId = "overlay_layer",
                  fillOpacity = 0.5,
                  color =  ~pal(value),
                  weight = 0) %>%
      addLayersControl(overlayGroups = c("Regions", "Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Regions")
    
    
    
    
  } else if(geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    # print("gooooo")
    # print(head(data_overlays_sf()))
    
    
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
    
    ai <- sfheaders::st_cast(data_overlays_sf(), "LINESTRING")
    
    
    map <- map %>%
      addGlPolylines(data = ai, group = "Overlay", options = pathOptions(clickable = FALSE, pane = "overlay"),
                     color = "#00AE42",
                     layerId = "overlay_layer")
    
    
  }
  
  
  # print("data_overlays_sf()")
  # print(data_overlays_sf())
  
  map %>% stopSpinner()
  
  tl$transition <- 1
  # admin_level_previous$a <- 1
  
  
  # waiter_hide()
  
  
})



# observer to keep selected element highlited -----------------------------

# first, we should create a vector with the selected elements
element <- reactiveValues(selected = NULL)


# when the admin level is changed, the vector should be restarted to avoid duplciation
observeEvent(c(input$admin_level), {
  element$selected <- NULL
  
})



observeEvent(c(input$map_shape_click), {
  
  waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
                          color = "rgba(233, 235, 240, .2)")
  
  # this will only happen when we are beyond the city level
  req(isTRUE(input$admin_level >= 1),  isTRUE(input$regions_grid == "Regions"))
  
  # first, we should create a vector with the selected elements
  ui <- input$map_shape_click$id
  element$selected <- c(element$selected, ui)
  # print("element$selected")
  # print(input$map_shape_click)
  
  
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
    
    format_indicator_value <- if(indicator_info$transformation == "percent") {
      round(data_previous$value * 100) 
      
    } else if(indicator_info$transformation %in% "thousands") {
      
      if (data_previous$value >= 1000000) scales::comma(data_previous$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data_previous$value, accuracy = 1, scale = 0.001, suffix = "k")
      
      
    } else round(data_previous$value)
    
    
    # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
    
    
    
    labels1 <- paste0("<b>", data_previous$name, "</b><br/>", 
                      sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value), 
                      sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                      "<br/><i>Click to see more info</i>")    
    
    
    
  }
  
  format_indicator_value <- if(indicator_info$transformation == "percent") {
    round(data$value * 100) 
    
  } else if(indicator_info$transformation %in% "thousands") {
    
    if (data$value >= 1000000) scales::comma(data$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(data$value, accuracy = 1, scale = 0.001, suffix = "k")
    
    
  } else round(data$value)
  
  
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
  
  waiter_hide()
  
})



# update overlay only when indicator is changed --------------------------------

observeEvent(c(indicator$mode, input$year), {
  
  # it will run only when we are at the city level
  req(!is.null(input$admin_level))
  # if (isTRUE(admin_level_previous$a >= 1)) {
  # if (isTRUE(input$admin_level >= 1)) {
  
  # print("obs3----")
  # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
  #             color = "rgba(233, 235, 240, .2)")
  
  # extract geom type of this indicator
  geom_type <- unique(data_overlays2()$geom_type)
  # print("geom_type")
  # print(geom_type)
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "white")) %>%
    # clearMarkers() %>%
    clearGroup(group =  "Overlay") %>%
    addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
    addMapPane("overlay", zIndex = 420) %>% # shown above ames_lines
    removeLayersControl()
  
  
  # print("geom_type")
  # print(geom_type)
  
  # add overlay
  if(indicator$mode %in% c("popdensity", "blockdensity")) {
    
    # print("gooooo")
    # print(head(data_overlays_sf()))
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = overlay_geom$polygon$value)
    
    map <- map %>%
      addPolygons(data = overlay_geom$polygon,
                  group = "Overlay",
                  options = pathOptions(clickable = FALSE, pane = "overlay"),
                  # layerId = "overlay_layer",
                  fillOpacity = 0.5,
                  color =  ~pal(value),
                  weight = 0) %>%
      addLayersControl(overlayGroups = c("Regions", "Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Regions")
    
    
    
    
  } else if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
    
    
    
    
    
    map <- map %>%
      addPolygons(data = data_overlays_sf(), 
                  group = "Overlay", opacity = 0.8,
                  options = pathOptions(clickable = FALSE, pane = "overlay"),
                  layerId = "overlay_layer",
                  fillColor = "#00AE42", fillOpacity = 0.6,
                  weight = 1, color = "black"
      ) %>%
      # addLegend("bottomleft", pal = pal, values = ~value) %>%
      addLayersControl(overlayGroups = c("Regions","Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE))
    
    
  } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
    
    
    ai <- sfheaders::sf_cast(data_overlays_sf(), "LINESTRING")
    
    map <- map %>%
      addGlPolylines(data = ai, group = "Overlay", 
                     # options = pathOptions(clickable = TRUE, pane = "overlay"),
                     layerId = "overlay_layer",
                     color = "#00AE42"
                     # pane = "overlay"
      ) %>%
      # addLegend("bottomleft", pal = pal, values = ~value) %>%
      addLayersControl(overlayGroups = c("Regions", "Overlay"),
                       baseGroups = c("Dark", "Light", "Satellite"),
                       options = layersControlOptions(collapsed = FALSE))
    
    
    
  }
  
  map %>% stopSpinner()
  # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')                     
  # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')                       
  
  
  # }
  # waiter_hide()
  
  
  
})


rv <- reactiveValues(prev_city = 1)  

observeEvent(c(city$city_code,
               input$admin_level,
               input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_city), {
                 
                 rv$prev_city <- c(rv$prev_city, rep(city$city_code, 2))
                 
                 
               }, ignoreInit = TRUE)


# update the basemap  --------------------------------

# reactive to filter data and add column with colors
data_ind3_spatial <- reactive({
  
  # print("aaaaaaaaaaaaa")
  # print(data_ind3())
  # print(input$admin_level)
  
  req(city$city_code != "", indicator$mode,  input$regions_grid)
  # req(data_ind3(), input$admin_level, rank$admin_level)
  
  
  # print("pera")
  # print(indicator$mode)
  
  # rank$admin_level
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
    
    print("QUEEEEEEEEEEEE")
    # print(a)
    
    
    
  }
  
  
  # create the color palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = a$value)
  
  # create a column with the colors
  a$fill <- pal(a$value)
  
  return(a)
  
  })
  
})



observeEvent(c(input$admin_level,
               indicator$mode,
               input$year,
               input$regions_grid), {
                 
                 
                 # this observer will only run if whe aren't switching cities
                 previous_city <- rv$prev_city[length(rv$prev_city)-1]
                 
                 
                 req(city$city_code == previous_city,
                     isTRUE(input$admin_level >= 1),
                     data_ind3_spatial())
                 
                 # print("previous_city")
                 # print(previous_city)
                 # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
                 #             color = "rgba(233, 235, 240, .2)")
                 
                 # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
                 #             color = "rgba(233, 235, 240, .4)")
                 
                 
                 # print("obs - switch cities new")
                 # print(Sys.time())
                 
                 # extract geom type of this indicator
                 geom_type <- unique(data_overlays2()$geom_type)
                 
                 # filter legend title and values
                 legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
                 legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
                 # format legend value
                 legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = " ", transform = function(x) as.integer(x))
                 
                 
                 # print("indicator_info$transformation")
                 # print(data_ind3_spatial())
                 
                 format_indicator_value <- if(indicator_info$transformation == "percent") {
                   round(data_ind3_spatial()$value * 100) 
                   
                 } else if(indicator_info$transformation %in% "thousands") {
                   
                   ifelse(data_ind3_spatial()$value >= 1000000, 
                          scales::comma(data_ind3_spatial()$value, accuracy = 0.1, scale = 0.000001, suffix = "M"), 
                          scales::comma(data_ind3_spatial()$value, accuracy = 1, scale = 0.001, suffix = "k"))
                   
                   
                 } else round(data_ind3_spatial()$value)
                 
                 
                 # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
                 
                 indicator$values <- format_indicator_value
                 indicator$unit <- indicator_info$unit
                 
                 # print("data_ind3_spatial()$name")
                 # print(data_ind3_spatial()$name)
                 # print(data_ind3_spatial()$name)
                 
                 # labels <- paste0("<b>", data_ind3_spatial()$name, "</b><br/>", 
                 #                  sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", indicator$values), 
                 #                  "<br/><i>Click to see more info</i>")
                 
                 labels <- paste0("<b>", data_ind3_spatial()$name, "</b><br/>", 
                                  sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", indicator$values), 
                                  sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9;\"> %s</span>", indicator$unit), 
                                  "<br/><i>Click to see more info</i>")
                 
                 # labels <- paste0("<b>", data_ind3_spatial()$name,  "</b><br/> <i>Click to see more info</i>")
                 
                 # print("ahahah")
                 # print(osm_selected$oi)
                 
                 
                 
                 
                 map <- leafletProxy("map", session) %>%
                   # clearMarkers() %>%
                   startSpinner(list("lines" = 10, "length" = 10,
                                     "width" = 5, "radius" = 5,
                                     color = "white")) %>%
                   # clearMarkers() %>%
                   # removeShape(layerId =  osm_selected$oi) %>%
                   # removeTiles(layerId =  "tile") %>%
                   removeShape(layerId = "reach") %>%
                   clearGroup(group = "Regions") %>%
                   # clearShapes() %>%
                   # removeShape(layerId = city$osmid ) %>%
                   clearControls() %>%
                   addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
                   addMapPane("overlay", zIndex = 420)# shown above ames_lines
                 
                 
                 # record the values in a list
                 leaflet_params <- list(
                   label1 <- if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) "Click to see the reach" else lapply(labels, htmltools::HTML),
                   bring_to_front <- if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) TRUE else FALSE,
                   weigth1 <- if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) 2 else 6,
                   weigth2 <- if (isTRUE(indicator$type == "performance") & isTRUE(input$regions_grid == "Grid")) 0.01 else 2
                   
                 )
                 
                 
                 # print("pegouuuu")
                 
                 # print("AHHHHHHHH")
                 # print(data_ind3_spatial())
                 
                 map <- map %>%
                   addPolygons(data = data_ind3_spatial(), 
                               fillColor = data_ind3_spatial()$fill,
                               fillOpacity = 0.5,
                               color = "black",  weight = weigth2, 
                               group = "Regions",
                               label = label1,
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
                               highlightOptions = highlightOptions(bringToFront = bring_to_front, opacity = 1, 
                                                                   weight = weigth1, color = "black"
                                                                   # fillColor = 'yellow'
                               )
                               # label = ~(label)
                   ) %>%
                   addLegend(data = data_ind3_spatial(), "bottomright",
                             pal = colorNumeric(
                               palette = "YlOrRd",
                               domain = NULL),
                             values = ~value,
                             title = legend_title,
                             # bins = c(0, 0.25, 0.50, 0.75, 1),
                             labFormat = legend_value
                   )
                 
                 if (!(indicator$mode %in% c("popdensity", "blockdensity"))) map <- map %>% showGroup("Regions")
                 
                 
                 map %>% stopSpinner()
                 
                 osm_selected$oi <- data_ind3_spatial()$osmid
                 
                 
                 
               })



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

