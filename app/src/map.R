


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
  a <- readRDS(sprintf("../data/data_final/countries/atlas_country_%s.rds", pattern))
  
  # print("aagsagas")
  # print(a)
  
  
})


osm_selected <- reactiveValues(oi = NULL, before = NULL)

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
    addSpinner() %>%
    addScaleBar(position = "bottomleft") %>%
    # onRender(spin_event)
    
    htmlwidgets::onRender(
      "function(el, x) {
      L.control.zoom({position:'topright'}).addTo(this);
      var myMap = this;
      myMap.on('baselayerchange',
      function (e) {
      Shiny.onInputChange('my_map_tile', e.layer.groupname)
        })
      
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
  
  req(indicator$mode, input$year, indicator_info$transformation, is.null(rank$admin_level))
  
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
  
  # print("atlas_country()")
  # print(atlas_country())
  
  cols_country <- c('a3', 'name', colnames(atlas_country())[startsWith(colnames(atlas_country()), pattern)], 'geometry')
  a_country <- atlas_country()[cols_country]
  colnames(a_country) <- c('a3', 'name', 'value', 'geometry')
  
  # print("A CONOCN")
  # print(a_country)
  
  
  a_available$value <- as.numeric(a_available$value)
  # we should truncate popdensity to 20000
  if (indicator$mode == "popdensity") {
    
    a_available$value_legend <- ifelse(a_available$value > 20000, 20000, a_available$value)
    a_country$value_legend <- ifelse(a_country$value > 20000, 20000, a_country$value)
    
  } else {
    
    a_available$value_legend <- a_available$value
    a_country$value_legend <- a_country$value
  }
  
  
  pal <- colorBin(
    palette = "viridis",
    na.color = "#808080",
    bins = 4,
    # palette = "YlGnBu",
    domain = a_available$value_legend,
    pretty = TRUE
    
  )
  
  # print("buuumm")
  # print(a_available$value_legend)
  # print(a_available$value)
  
  
  pal_countries <- colorBin(
    palette = "viridis",
    bins = 4,
    # palette = "YlGnBu",
    domain = a_country$value_legend,
    pretty = TRUE
  )
  
  
  
  legend_title <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  legend_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  # format legend value
  # legend_value <- if(legend_value == "%") scales::percent else labelFormat(suffix = "", transform = function(x) as.integer(x))
  # legend_value <- if(legend_value == "%") labelFormat(suffix = "%", transform = function (x) x * 100) else labelFormat(suffix = " ", transform = function(x) as.integer(x))
  legend_value <- if(legend_value == "%") {
    
    # labelFormat(suffix = "%", transform = function (x) x * 100)
    function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(cuts * 100), '%')
      paste(p[-n], p[-1], sep = ' - ')
    }
    
  } else {
    
    #we should cap pop density to 20k 
    function(type, cuts, p) {
      n = length(cuts)
      p = round(cuts)
      
      p <- ifelse(p >= 1000, scales::comma(p, accuracy = 0.1, scale = 0.001, suffix = "k"), p)
      
      if (indicator$mode == "popdensity") {
        p = ifelse(p == "20.0k", "20.0k+", p)
        
      }
      
      
      paste(p[-n], p[-1], sep = ' - ')
    }
    
    # labelFormat(suffix = " ", 
    #             transform = function(x) as.integer(x)
    # )
    
  }
  
  # create the label for the markers
  format_indicator_value_marker <- format_indicator_values(a_available$value, transformation = indicator_info$transformation)
  
  # format_indicator_value_marker <- paste0(format_indicator_value_marker, indicator_info$unit)
  
  # print("data_ind3_spatial()$name")
  # print(data_ind3_spatial()$name)
  # print(data_ind3_spatial()$name)
  
  labels_markers1 <- paste0("<b>", a_available$name, "</b><br/>", 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value_marker), 
                            sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                            "<br/><i>Click for detail</i>")
  
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
  
  # print(world_view$a_available)
  # print(world_view$a_country)
  
  # value only for the legend
  
  
  map <- leafletProxy("map", data = world_view$a_available) %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    # flyTo(lng = 0, lat = 0, zoom = 3) %>%
    addMapPane("countries", zIndex = 410) %>% # shown below ames_circles
    addMapPane("markers_navailable", zIndex = 420) %>% # shown above ames_lines
    addMapPane("markers_available", zIndex = 430) %>% # shown above ames_lines
    # markers available
    addCircleMarkers(
      # radius = ~ifelse(type == "ship", 6, 10),
      radius = 5,
      fillColor = ~world_view$pal(value_legend),
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
                fillColor = ~world_view$pal_countries(value_legend), color = "black",  
                weight = 1, # erro nao eh aqui
                fillOpacity = 0.7,
                options = pathOptions(clickable = TRUE, pane = "countries"),
                group = "Countries",
                label = lapply(world_view$labels_country, htmltools::HTML)
    ) %>%
    # add polygons with the country color
    # addPolygons(fillColor = ~pal(pnpb), color = "black", layerId = ~code_metro) %>%
    addLegend("bottomright", pal = world_view$pal_countries, values = ~world_view$a_country$value_legend,
              title = world_view$legend_title,
              bins = 4,
              labFormat = world_view$legend_value,
              layerId = "legend_country")
  
  map
  
  
}) 


# reactive values to store the indicator values
indicator <- reactiveValues(values = NULL, format = NULL, unit = NULL)

# overlay
overlay_layers <- reactiveValues(groups = NULL)




# observer to travel between cities (for the first time) ---------------------------------------

# map_start <- reactive({
observeEvent(c(city$city_code), {
  
  # print("boiiii")
  # print(rank$admin_level_name)
  
  req(city$city_code != "")
  
  
  # shinyjs::logjs("Map: go to city view when coming from world view")
  
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  
  rank$admin_level_name <- unique(subset(data_ind(), admin_level_ordered == rank$admin_level)$admin_level_name)
  
  # print("obs - switch cities initial")
  
  # req(input$city)
  bbox <- st_bbox(data_ind())
  
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
  
  pal <- colorBin(
    palette = "YlGnBu",
    bins = 7,
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
                   span(style="font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 16px; padding-bottom: 0px; color: #B1B5B9;", indicator$unit)) 
  # "<br/><i>Click to see more info</i>")
  
  
  waiter_hide()
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
    # removeMarker(layerId = data_metro$osmid) %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls() %>%
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
    addMapPane("People Near Bikeways + Public Transport", zIndex = 420) %>%
    addMapPane("Frequent transport stops", zIndex = 425)
  
  # remove groups
  for(i in 1:length(overlay_table$overlay_group)){
    map = map %>% clearGroup(overlay_table$overlay_group)
  }
  
  map <- map %>%
    # add the city markers without the one selected
    addCircleMarkers(
      data = world_view$a_available,
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
    removeMarker(layerId = data_metro$osmid) %>%
    
    
    # add polygon with metro data
    addPolygons(data = data_metro,
                fillColor = ~pal(value), fillOpacity = 0.1,
                color = "#00AE42",  weight = 2, layerId = ~osmid,
                group = sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>"),
                label = lapply(labels, htmltools::HTML),
                labelOptions = labelOptions(
                  interactive = TRUE, clickable = TRUE, permanent = FALSE,
                  style = list(
                    # "color" = "red",
                    # "font-family" = "serif",
                    # "font-style" = "italic",
                    # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px"
                    # "border-color" = "rgba(0,0,0,0.5)"
                  )),
                options = pathOptions(interactive = TRUE, pane = "basemap", clickable = TRUE),
                highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, weight = 6, color = "grey"))
  
  
  # identify overlays to be opened
  fix <- indicator$mode
  overlays_to_open <- subset(overlay_table, indicator == fix)
  
  for(i in overlays_to_open$overlay){
    
    # data1 <- subset(data_overlays()[["lines"]], ind == i)
    
    fix <- indicator$mode
    overlay_subset <- subset(overlay_table, indicator == fix & overlay == i)
    overlay_name <- unique(overlay_subset$overlay_label)
    overlay_status <- unique(overlay_subset$overlay_show)
    overlay_mapboxid <- unique(overlay_subset$mapbox_id)
    overlay_group <- unique(overlay_subset$overlay_group)
    
    
    file <- sprintf("../data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.%s", 
                    city$city_code, overlay_subset$overlay, overlay_subset$overlay, city$city_code, input$year, overlay_subset$format)
    
    # if (file.exists(file)) {
    
    if (i %in% c("pop", "grid_pop_evaluated")) {
      
      # data <- readRDS(file)
      # instead it would be
      data <- stars::read_stars(file)
      
      map <- map %>%
        leafem::addGeoRaster(x = data,
                             opacity = 0.6,
                             group = overlay_group,
                             options = list(clickable = FALSE, pane = overlay_name),
                             colorOptions = colorOptions(palette = viridis::viridis(n = 9)),
                             autozoom = FALSE)
      
    } else if (i %in% c("protectedbike_latlon", "hs_latlon", "healthcare_latlon", "schools_latlon",
                        "pnst_latlon", "pnft_points_latlon", "pnft_latlon", "allbike_latlon",
                        "pnab_latlon", "pnpb_latlon", "allhwys_latlon", "buffered_hwys_latlon",
                        "carfree_latlon", "schools_points_latlon", "healthcare_points_latlon"
                        )) {
      
      
      
      map <- map %>%
        mapboxapi::addMapboxTiles(access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ",
                                  style_id = overlay_mapboxid,
                                  username = "kauebraga",
                                  group = overlay_group,
                                  # group = paste0('<svg height="15" width="15" xmlns="http://www.w3.org/2000/svg"><circle r="7.5" cx="7.5" cy="7.5" stroke="green" stroke-width="1" fill="red" /></svg>', overlay_name),
                                  options = tileOptions(pane = overlay_name))
      
    } else {
      
      # print(file)
      
      should_fill <- if(grepl("_lines|protectedbike_latlon|allbike_latlon", i)) {
        
        FALSE
        
      } else {TRUE}
      
      if (i == "block_densities_latlon") {
        
        print("hererererer")
        print(file)
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_group,
                                     color = "black",
                                     fill = TRUE,
                                     weight = 0,
                                     opacity = 0.6,
                                     fillOpacity = 0.8,
                                     options = pathOptions(interactive = FALSE, pane = overlay_name)
        )
        
        
        
      } else {
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_group,
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
    
    
    # }
  }
  
  
  
  fix <- indicator$mode
  overlay_labels <- subset(overlay_table, indicator == fix)$overlay_group
  # overlay_labels <- paste0('<svg height="15" width="15" xmlns="http://www.w3.org/2000/svg"><circle r="7.5" cx="7.5" cy="7.5" stroke="green" stroke-width="1" fill="red" /></svg>', overlay_labels)
  # overlay_labels <- as.list(overlay_labels)
  # names(overlay_labels) <- subset(overlay_table, indicator == fix)$overlay_label
  # print(overlay_labels)
  # identify groups to hide
  overlay_show <- subset(overlay_table, indicator == fix & overlay_show == "yes")$overlay_group
  overlay_hide <- subset(overlay_table, indicator == fix & overlay_show == "no")$overlay_group
  
  # # identify overlays
  overlay_layers$groups <- overlay_group
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Satellite"),
      overlayGroups = c(sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>"),
                        overlay_labels),
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
  
  ui <- input$map_shape_click$id
  
  # we have a problem that the overlay is being clickable, regardless of what we set
  # so we need to make this overlay un-reactable from here
  ui_ok <- grepl("^\\d{3,}", ui)
  
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
      
      format_indicator_value <- format_indicator_values(data_previous$value, indicator_info$transformation)
      
      
      
      labels1 <- paste0("<b>", data_previous$name, "</b><br/>", 
                        sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value), 
                        sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                        "<br/><i>Click to select</i>")    
      
      
      
    }
    
    
    format_indicator_value <- format_indicator_values(data$value, indicator_info$transformation)
    
    
    
    labels <- paste0("<b>", data$name, "</b><br/>", 
                     sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", format_indicator_value), 
                     sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9\"> %s</span>", indicator_info$unit), 
                     "<br/><i>Click to select</i>")
    
    
    
    # update the map
    map <- leafletProxy("map", session) %>%
      # 1) delete the selected polygon
      removeShape(layerId = ui) %>%
      # 2) create the selected polygon with the stronger stroke
      addPolygons(data = data,
                  fillColor = data$fill, fillOpacity = 0.5,
                  # fillColor = ~pal(value),
                  # fillOpacity = 0.5,
                  color = "grey",  weight = 8, layerId = ~osmid, opacity = 1,
                  group = sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>"),
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
                    group = sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>"),
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



# update overlay only when indicator/year is changed --------------------------------

observeEvent(c(indicator$mode, input$year), {
  
  # it will run only when we are at the city level
  req(!is.null(input$admin_level))
  # if (isTRUE(admin_level_previous$a >= 1)) {
  # if (isTRUE(input$admin_level >= 1)) {
  shinyjs::logjs("Map: update overlay only when indicator/year is changed")
  
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
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
    addMapPane("People Near Bikeways + Public Transport", zIndex = 420) %>%
    removeLayersControl()
  
  # remove groups
  for(i in 1:length(overlay_table$overlay_group)){
    map = map %>% clearGroup(overlay_table$overlay_group)
  }
  
  
  
  
  # add overlay
  # identify overlays to be opened
  fix <- indicator$mode
  overlays_to_open <- subset(overlay_table, indicator == fix)
  
  for(i in overlays_to_open$overlay){
    
    
    fix <- indicator$mode
    overlay_subset <- subset(overlay_table, indicator == fix & overlay == i)
    overlay_name <- unique(overlay_subset$overlay_label)
    overlay_status <- unique(overlay_subset$overlay_show)
    overlay_mapboxid <- unique(overlay_subset$mapbox_id)
    overlay_group <- unique(overlay_subset$overlay_group)
    
    file <- sprintf("../data/data_final/ghsl_%s/overlays/%s/%s_%s_%s.%s", 
                    city$city_code, overlay_subset$overlay, overlay_subset$overlay, city$city_code, input$year, overlay_subset$format)
    
    # if (file.exists(file)) {
    
    if (i %in% c("pop", "grid_pop_evaluated")) {
      
      data <- stars::read_stars(file)
      
      map <- map %>%
        leafem::addGeoRaster(x = data,
                             opacity = 0.8,
                             group = overlay_group,
                             options = pathOptions(clickable = FALSE, pane = overlay_name),
                             colorOptions = colorOptions(palette = viridis::viridis(n = 9)),
                             autozoom = FALSE)
      
      
    } else if (i %in% c(c("protectedbike_latlon", "hs_latlon", "healthcare_latlon", "schools_latlon",
                          "pnst_latlon", "pnft_points_latlon", "pnft_latlon", "allbike_latlon",
                          "pnab_latlon", "pnpb_latlon", "allhwys_latlon", "buffered_hwys_latlon",
                          "carfree_latlon", "schools_points_latlon", "healthcare_points_latlon"))) {
      
      
      # print(i)
      # print("iiiii")
      
      map <- map %>%
        mapboxapi::addMapboxTiles(access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ",
                                  style_id = overlay_mapboxid,
                                  username = "kauebraga",
                                  group = overlay_group,
                                  # group = paste0('<svg height="15" width="15" xmlns="http://www.w3.org/2000/svg"><circle r="7.5" cx="7.5" cy="7.5" stroke="green" stroke-width="1" fill="red" /></svg>', overlay_name),
                                  options = tileOptions(pane = overlay_name))
      
    } else {
      
      # print(file)
      
      should_fill <- if(grepl("_lines|protectedbike_latlon|allbike_latlon", i)) {
        
        FALSE
        
      } else {TRUE}
      
      if (i == "block_densities_latlon") {
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_group,
                                     # color = "#00AE42",
                                     # color = "#000000",
                                     color = "black",
                                     fill = TRUE,
                                     weight = 0,
                                     opacity = 0.6,
                                     fillOpacity = 0.8,
                                     options = pathOptions(interactive = FALSE, pane = overlay_name)
        )
        
        
        
      } else {
        
        map = map %>% leafem::addFgb(file = file,
                                     group = overlay_group,
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
                                     options = pathOptions(interactive = FALSE, pane = overlay_name)
                                     # layerId = "overlay_layer",
        )
      }
      
    }
    
    
  }
  # }
  
  
  # # identify overlays
  # overlay_layers
  fix <- indicator$mode
  overlay_labels <- subset(overlay_table, indicator == fix)$overlay_group
  # identify groups to hide
  overlay_show <- subset(overlay_table, indicator == fix & overlay_show == "yes")$overlay_group
  overlay_hide <- subset(overlay_table, indicator == fix & overlay_show == "no")$overlay_group
  
  # print("overlay_show")
  # print(overlay_show)
  # print(overlay_hide)
  
  
  # # identify overlays
  overlay_layers$groups <- overlay_labels
  
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Satellite"),
      overlayGroups = c(sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>")
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

observeEvent(c(city$city_code, rank$admin_level, input$year), {
  
  # print("pus")
  # print(rank$admin_level)
  
  req(rank$admin_level > 1)
  
  rv$prev_city <- c(rv$prev_city, rep(city$city_code, 2))
  
  # print("comeh")
  # print(rv$prev_city)
  
  
}, ignoreInit = TRUE, priority = 2)

observeEvent(c(indicator$mode, input$year), {
  
  # print("pus")
  # print(rank$admin_level)
  
  req(rank$admin_level >= 1)
  
  rv$prev_city <- c(rv$prev_city, rep(city$city_code, 2))
  
  
}, ignoreInit = TRUE, priority = 2)



# update the regions basemap  --------------------------------

# reactive to filter data and add column with colors
data_ind3_spatial <- reactive({
  
  # req(city$city_code != "", indicator$mode,  input$regions_grid)
  # req(data_ind3(), input$admin_level, rank$admin_level)
  
  
  rank$admin_level
  indicator$mode
  # input$admin_level
  year$ok
  
  isolate({
    
    # print("pera0")
    # print(data_ind3())
    print("pererere")
    print(length(unique(data_ind3()$admin_level)))
    
    
    if (length(unique(data_ind3()$admin_level)) == 1) {
      
      
      
      a <- subset(data_ind3(), admin_level_ordered == 1)
      
    } else {
      
      # a <- subset(data_ind3(), admin_level_ordered ==  input$admin_level)
      print("AQUIIIII")
      print(rank$admin_level)
      a <- subset(data_ind3(), admin_level_ordered ==  rank$admin_level)
    }
    
    
    
    
    # we should truncate popdensity to 20000
    if (indicator$mode == "popdensity") {
      
      a$value_legend <- ifelse(a$value > 20000, 20000, a$value)
      
    } else a$value_legend <- a$value
    
    print("pera1")
    print(a)
    
    # create the color palette
    # the number of bins will depend on the number of units on the map
    total_units <- nrow(a)
    bins_map <- ifelse(total_units == 1, 2, ifelse(total_units <= 4, total_units, 4))
    pal <- colorBin(
      palette = "YlGnBu",
      bins = bins_map,
      domain = a$value_legend,
      pretty = TRUE)
    
    # create a column with the colors
    # it brakes if the value is zero
    a$value_legend <- ifelse(a$value_legend == 0, 0.00001, a$value_legend)
    print("a$value_legend")
    print(a$value_legend)
    a$fill <- pal(a$value_legend)
    
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
  legend_value <- if(legend_value == "%") {
    
    # labelFormat(suffix = "%", transform = function (x) x * 100)
    function(type, cuts, p) {
      n = length(cuts)
      p = paste0(round(cuts * 100), '%')
      paste(p[-n], p[-1], sep = ' - ')
    }
    
  } else {
    
    #we should cap pop density to 20k 
    function(type, cuts, p) {
      n = length(cuts)
      p = round(cuts)
      
      p <- ifelse(p >= 1000, scales::comma(p, accuracy = 0.1, scale = 0.001, suffix = "k"), p)
      
      if (indicator$mode == "popdensity") {
        p = ifelse(p == "20.0k", "20.0k+", p)
        
      }
      
      paste(p[-n], p[-1], sep = ' - ')
    }
    
    # labelFormat(suffix = " ", 
    #             transform = function(x) as.integer(x)
    # )
    
  }
  
  
  # print("pera2")
  # print(data_ind3_spatial())
  
  
  format_indicator_value <- format_indicator_values(data_ind3_spatial()$value, indicator_info$transformation)
  
  # format_indicator_value <- paste0(format_indicator_value, indicator_info$unit)
  
  indicator$values <- format_indicator_value
  indicator$unit <- indicator_info$unit
  
  # print(indicator$values)
  # print(indicator$unit)
  
  
  labels <- paste0("<b>", data_ind3_spatial()$name, "</b><br/>", 
                   sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 22px; padding-bottom: 0px\"> %s</span>", indicator$values), 
                   sprintf("<span style=\"font-family: 'Fira Sans', sans-serif;font-style: normal;font-weight: 600; font-size: 12px; padding-bottom: 0px; color: #B1B5B9;\"> %s</span>", indicator$unit), 
                   "<br/><i>Click to select</i>")
  
  
  map <- leafletProxy("map", session) %>%
    # clearMarkers() %>%
    startSpinner(list("lines" = 10, "length" = 10,
                      "width" = 5, "radius" = 5,
                      color = "black")) %>%
    # clearMarkers() %>%
    # removeShape(layerId =  osm_selected$oi) %>%
    # removeTiles(layerId =  "tile") %>%
    removeShape(layerId = "reach") %>%
    clearGroup(group = sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>")) %>%
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
                group = sprintf("<span id = \"teste_map\"><img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Agglomeration</img></span>"),
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
                                                    weight = leaflet_params$weigth1, color = "grey"
                                                    # fillColor = 'yellow'
                )
                # label = ~(label)
    )
  
  
  if (input$admin_level != 1)  {
    
    total_units <- nrow(data_ind3_spatial())
    bins_map <- ifelse(total_units == 1, 2, ifelse(total_units <= 4, total_units, 4))
    pal <- colorBin(
      palette = "YlGnBu",
      bins = bins_map,
      domain = data_ind3_spatial()$value_legend,
      pretty = TRUE)
    
    # print(pal)
    # print(data_ind3_spatial()$value_legend)
    
    map <- map %>%
      addLegend(data = data_ind3_spatial(), "bottomright",
                pal = pal,
                values = ~value_legend,
                title = legend_title,
                labFormat = legend_value,
                layerId = "legend_city"
      )
  } else map <- map
  
  
  if (!(indicator$mode %in% c("popdensity"))) map <- map %>% showGroup("<img src=\"icons/icon_MULTIPOLYGON_.png\" height=\"30\" width = \"30\">Regions</img>")
  
  
  map %>% stopSpinner()
  
  osm_selected$oi <- data_ind3_spatial()$osmid
  
  waiter_hide()
  
}, priority = -1)



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

