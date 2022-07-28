# open boundaries
atlas_city_markers <- readRDS("../data/sample3/atlas_city_markers.rds")
atlas_country <- readRDS("../data/sample3/atlas_country_polygons.rds")
# country rank
atlas_country_ranks <- readRDS("../data/sample3/ranks/rank_country.rds")


function(input, output, session) {
  
  
  
  # ui----------------------------------------------------------------------
  
  output$city_selection <- renderUI({
    
    
    tagList(
      
      
      
      # 1) CITY SELECTION -------------------------------------------------------
      # https://www.rapidtables.com/code/text/unicode-characters.html
      
      
      # tags$button(id = "back_to_world",
      #             class = "btn btn-default",
      #             type = "reset",
      #             style = "background: transparent; font-size: 26px; color: #00AE42;",
      #             "ATLAS"),
      # actionButton(inputId = "back_to_world",
      #              label = "ATLAS",
      #              style = "background: transparent; font-size: 26px; color: #00AE42;"
      # ),
      h3(style = "color: #00AE42; display: inline-block; font-size: 28px", strong("ATLAS")),
      div(style = "display: inline-block;",
          pickerInput(inputId = "city",
                      label = NULL,
                      choices = list(
                        'Brazil' = c("Fortaleza" = "1406",
                                     "Recife" = "1445"),
                        # 'USA' = c("Boston" = "1022"),
                        # 'Ethiopia' = c("Addis Ababa" = "5134")
                        'Mexico' = c("Guadalajara" = "0088",
                                     "Monterrey" = "0200"),
                        'Colombia' = c("Bogota" = "0621",
                                       "Medellin" = "0561")
                      ),
                      options = pickerOptions(size = 15,
                                              iconBase = "fa",
                                              tickIcon = "fa-check",
                                              title = "Search for a metro region ...",
                                              liveSearch = TRUE)
          )
      )
    )
    
  })
  
  output$left_panel <- renderUI({
    
    # Create lists that will give the select options to the respective language
    list_indicators <- structure(c("bike", "walk",  "transit", "performance",  "built_env"), 
                                 .Names = c("Bicycle", "Walk",  "Transit", "Performance", "Built Env"))
    
    list_bike <- structure(c("pnpb", "pnab", "abikeways", "pbikeways"), 
                           .Names = c("People Near Protected Bikelanes", "People Near All Bikelanes",
                                      "Bikeways", "Protected Bikeways"))
    
    list_walk <- structure(c("healthcare", "schools", "hs"), 
                           .Names = c("People Near Healthcare", "People Near Schools", "People Near Services"))
    
    list_transit <- structure(c("pntt", "etct"), 
                              .Names = c("People Near Transit&nbsp;&nbsp;&nbsp;", "ETC"))
    
    list_performance <- structure(c("bikep", "walkp"), 
                                  .Names = c("Bicycle&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Walk"))
    
    list_built_env <- structure(c("schoolsbe", "etcbe"), 
                                .Names = c("Schools&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;","ETC"))
    
    # list_spatial_levels <- structure(c("adminstrative_area2(city)", "adminstrative_area3(juris)"),
    #                                  .Names = c("city", "jurisdiction"))
    
    
    # Start proper UI here 
    tagList(
      
      tags$div(class = "title_left_panel", "INDICATORS"),
      
      
      
      # THIS CONDITIONAL PANEL WILL UNFOLD NICELY WITH THE REMAINING SELECTIONS WHEN A CITY IS SELECTED
      
      
      # absolutePanel(
      #   id = "controls_animated1",
      #   class = "panel panel-default", 
      #   # class = "w3-container w3-animate-opacity", 
      #   fixed = TRUE, draggable = FALSE,
      #   bottom = 100, left = 10, width = 300,
      
      
      # 2) INDICATOR SELECTION --------------------------------------------------
      
      # radioGroupButtons(inputId = "indicator",
      #                   label = "Indicator",
      #                   choices = c(list_indicators),
      #                   individual = TRUE,
      #                   justified = TRUE,
      #                   selected = "bike"),
      
      # Create the left side panel  
      absolutePanel(class = "left_panel_indicators", 
                    bottom = 30, left = -1, width = 250, height = 320,
                    # by type of indicator
                    # conditionalPanel(
                    # condition = "input.indicator == 'bike'",
                    accordion_input(inputId = "indicator_bike",
                                    label = "Bike",
                                    choices = c(list_bike),
                                    selected = "pnpb"),
                    # includeHTML("test_accordion2.html"),
                    # pickerInput(inputId = "indicator_bike",
                    #             label = "Bike",
                    #             choices = c(list_bike),
                    #             selected = "PNB")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.indicator == 'walk'",
                    accordion_input(inputId = "indicator_walk",
                                    label = "Walk",
                                    choices = c(list_walk),
                                    selected = character(0)),
                    #   pickerInput(inputId = "indicator_walk",
                    #               label = "Walk",
                    #               choices = c(list_walk),
                    #               selected = "PN Healthcare")
                    # ),
                    # conditionalPanel(
                    # condition = "input.indicator == 'transit'",
                    accordion_input(inputId = "indicator_transit",
                                    label = "Transit",
                                    choices = c(list_transit),
                                    selected = character(0)),
                    #   pickerInput(inputId = "indicator_transit",
                    #               label = "Transit",
                    #               choices = c(list_transit),
                    #               selected = "PNT")
                    # ),
                    # conditionalPanel(
                    # condition = "input.indicator == 'built_env'",
                    accordion_input(inputId = "indicator_performance",
                                    label = "Performance",
                                    choices = c(list_performance),
                                    selected = character(0)),
                    
                    accordion_input(inputId = "indicator_built_env",
                                    label = "Built Env",
                                    choices = c(list_built_env),
                                    selected = character(0))
                    #   pickerInput(inputId = "indicator_built_env",
                    #               label = "Built Env",
                    #               choices = c(list_built_env),
                    #               selected = "Schools")
                    # )
                    
                    
                    
      )
      
      
    )
    
    
    
  })
  
  output$left_panel_filter <- renderUI({
    
    
    tagList(
      conditionalPanel(
        condition = "ind_cum.indexOf(input.indicator_performance) > -1",
        # condition = "typeof input.indicator_performance != ''",
        absolutePanel(
          # id = "controls",
          class = "spatial_level",
          # fixed = TRUE, draggable = FALSE,
          bottom = 30, left = 300, height = 115,
          # 'typeof undefined' identifies when is null 
          sliderInput(inputId = "time_cutoff",
                      min = 30, max = 60, step = 15,
                      label = "TIME CUTOFF",
                      value = 30
                      # selected = character(0)
          )
          
        )
        
      )
    )
    
    
  })
  
  
  spatial_level_value <- reactiveValues(last = NULL)
  
  
  
  output$spatial_level <- renderUI({
    
    # CALCULATE the spatial levels for each city
    
    # print(paste0("last", spatial_level$last))
    go <- length(unique(data_ind()$admin_level))
    
    
    # list_spatial_levels <- structure(c("0", "6", "7", "8", "9", "10"),
    #                                  .Names = c("Mais", "6", "7", "8", "9", "Menos"))
    
    tagList(
      conditionalPanel(
        condition = "input.city != '' || typeof input.map_marker_click !== 'undefined'",
        absolutePanel(
          # id = "controls", 
          class = "spatial_level",
          # fixed = TRUE, draggable = FALSE,
          bottom = 30, right = 530, height = 115,
          # 'typeof undefined' identifies when is null 
          sliderTextInput(inputId = "admin_level",
                          choices = seq(1, go),
                          label = "LEVEL OF DETAIL",
                          selected = 1,
                          grid = TRUE
                          # selected = character(0)
          )
          
        )
        
      )
    )
    
  })
  
  
  output$right_panel <- renderUI({
    
    tagList(
      
      conditionalPanel(
        condition = "typeof input.indicator_bike !== 'undefined'",
        absolutePanel(
          class = "right_panel",
          # class = "w3-container w3-animate-opacity", 
          # class = "panel panel-default",
          # fixed = TRUE, draggable = FALSE,
          top = 0, right = 0, width = 300, height = "100%",
          absolutePanel(
            class = "right_panel_textbox",
            top = 105, right = 10, width = 280,
            htmlOutput("text_indicator"),
            htmlOutput("rank_value"),
            htmlOutput("rank_text"),
            actionButton(inputId = "back_to_world",
                         label = "Back to World View"
                         # selected = character(0)
            )
          )
          
          
        )
      )
    )
    
  })
  
  
  
  # server ------------------------------------------------------------------
  
  
  # v_city <- reactive({
  #   
  #   req(input$city)
  #   print(input$city)
  #   if(input$city != "") input$city else NULL
  #   
  # })
  
  
  
  indicator <- reactiveValues(type = NULL)
  
  
  observeEvent(c(input$indicator_bike), {
    
    indicator$type <- "bike"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_built_env", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    print("performance :", input$indicator_performance)
    print(input$city)
    
  })
  
  observeEvent(c(input$indicator_walk), {
    
    print("ai!")
    indicator$type <- "walk"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_built_env", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
    
  })
  
  observeEvent(c(input$indicator_transit), {
    
    indicator$type <- "transit"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_built_env", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
  })
  
  observeEvent(c(input$indicator_performance), {
    
    indicator$type <- "performance"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_built_env", selected = character(0))
    
    # print("performance :", input$indicator_performance)
    
  })
  
  observeEvent(c(input$indicator_built_env), {
    
    indicator$type <- "built_env"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
  })
  
  # observeEvent(c(input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
  # 
  #   print(paste0("last input: ", input$indicator_bike))
  #   # print(paste0("last input: ", input$last_input))
  #   
  #   if (is.null(input$last_input)) {
  #     
  #     indicator$type <- 'bike'
  #     
  #   } else indicator$type <- ifelse(startsWith(input$last_input, "bike"), "bike",
  #                                              ifelse(startsWith(input$last_input, "walk"), "walk",
  #                                                                ifelse(startsWith(input$last_input, "transit"), "transit",
  #                                                                                  ifelse(startsWith(input$last_input, "built_env"), "built_env"))))
  #   
  # 
  #   
  #   })
  
  
  
  
  # reactive to select the type of indicator --------------------------------
  indicator_mode <- reactive({
    
    if (indicator$type == "bike") {
      
      input$indicator_bike
      
    } else if (indicator$type == "walk"){
      
      input$indicator_walk
      
    } else if (indicator$type == "transit"){
      
      input$indicator_transit
      
    } else if (indicator$type == "built_env"){
      
      input$indicator_built_env
      
    } else if (indicator$type == "performance"){
      
      input$indicator_performance
      
    }
    
  })
  
  
  output$text_indicator <- renderUI({
    
    # str1 <- paste("You have selected", input$var)
    # str2 <- paste("You have chosen a range that goes from",
    #               input$range[1], "to", input$range[2])
    # HTML(paste(str1, str2, sep = '<br/>'))
    # 
    #   h3("People Near Protected Bikelanes")
    #   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
    
    req(indicator_mode())
    # print(indicator_mode())
    
    includeHTML(sprintf("www/text/text_indicator_%s.html", indicator_mode()))
    
  })
  
  # initial map
  output$map <- renderLeaflet({
    
    
    
    
    
    pal <- colorNumeric(
      palette = "BuGn",
      # palette = "YlGnBu",
      domain = atlas_city_markers$bike_pnpb_2019)
    
    pal_countries <- colorNumeric(
      palette = "BuGn",
      # palette = "YlGnBu",
      domain = atlas_country$bike_pnpb_2019)
    
    map <- leaflet(data = atlas_city_markers, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addLayersControl(baseGroups = c("Dark", "Light"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
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
    htmlwidgets::onRender(
      "function(el, x) {
        L.control.zoom({position:'topright'}).addTo(this);
      }")
    
    
    map
    
  })
  
  
  # update the world map when the indicators is changed ---------------------
  observeEvent(c(input$indicator, input$back_to_world,
                 input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
                   
                   req(indicator_mode())
                   
                   
                   # this will only runs if we are at the wold view (admin level = null)
                   if(is.null(input$admin_level)) {
                     
                     # print(atlas_city_markers)
                     
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
                     
                     # create legend title
                     legend_title <- fcase(
                       indicator_mode() %like% "pnpb", "% of the population within a 300m walk of a protected bikelane",
                       default = "teste"
                       
                     )
                     
                     
                     # format legend value
                     legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
                       
                       scales::percent
                       
                     } else labelFormat(suffix = " km", transform = function(x) as.integer(x))
                     
                     # print(a)
                     
                     leafletProxy("map", data = a) %>%
                       # clearMarkers() %>%
                       # clearControls() %>%
                       # clearShapes() %>%
                       flyTo(lng = 0, lat = 0, zoom = 3) %>%
                       addCircleMarkers(
                         # radius = ~ifelse(type == "ship", 6, 10),
                         radius = 10,
                         # fillColor = ~pal(valor), 
                         stroke = TRUE, fillOpacity = 0.9, color = "black",
                         weight = 0.5,
                         layerId = ~hdc,
                         label = ~htmlEscape(name)
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
                   }
                   
                   
                 }) 
  
  
  
  # BOTAO PARA VOLTAR !!!!!!!!!! --------------------------------------------
  
  
  
  observeEvent(c(input$back_to_world), {
    
    
    updatePickerInput(session = session, inputId = "city",
                      selected = character(0))
    
    city$city_code <- NULL
    
    session$sendCustomMessage(type = "resetValue", message = "city")
    session$sendCustomMessage(type = "resetValue", message = "map_marker_click")
    
    print(paste0("pa: ", input$map_marker_click))
    
    
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
    
    # create legend title
    legend_title <- fcase(
      indicator_mode() %like% "pnpb", "% of the population within a 300m walk of a protected bikelane",
      default = "teste"
      
    )
    
    
    # format legend value
    legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
      
      scales::percent
      
    } else labelFormat(suffix = " km", transform = function(x) as.integer(x))
    
    # print(a)
    
    leafletProxy("map", data = a) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes() %>%
      setView(lng = 0, lat = 0, zoom = 3) %>%
      addCircleMarkers(
        # radius = ~ifelse(type == "ship", 6, 10),
        radius = 10,
        # fillColor = ~pal(valor), 
        stroke = TRUE, fillOpacity = 0.9, color = "black",
        weight = 0.5,
        layerId = ~hdc,
        label = ~htmlEscape(name)
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
    
  }) 
  
  # observeEvent(input$map_shape_click, {
  #   
  #   print(input$map_shape_click)
  #   
  # })
  
  # first, define the city input
  city <- reactiveValues(code = NULL)
  
  observeEvent(c(input$city), {city$city_code <- input$city})
  observeEvent(c(input$map_marker_click), {
    
    city$city_code <- input$map_marker_click$id
    
    updatePickerInput(session = session, inputId = "city",
                      selected = city$city_code)
    
    
  }
  )
  
  data_ind <- reactive({
    
    # print(input$city)
    req(city$city_code)
    
    a <- readRDS(sprintf("../data/sample3/ghsl_%s/indicators_%s.rds", city$city_code, city$city_code))
    # readRDS(sprintf("data/atlas_%s_indicators.rds", city$city_code))
    # print(head(a))
    spatial_level_value$last <- length(unique(a$admin_level))
    return(a)
    
  })
  
  # # calculate number of spatial levels
  # spatial_levels_number <- reactive({
  #   
  #   a
  #   
  # })
  
  # reactive values to identify overlay geoms
  overlay_geom <- reactiveValues(polygon = NULL, line = NULL)
  
  
  data_overlays <- reactive({
    
    req(city$city_code)
    
    # open geommetries
    overlay_geom$polygon  <- readRDS(sprintf("../data/sample3/ghsl_%s/overlays_polygons_%s.rds", city$city_code, city$city_code))
    overlay_geom$line <- readRDS(sprintf("../data/sample3/ghsl_%s/overlays_lines_%s.rds", city$city_code, city$city_code))
    
    
    readRDS(sprintf("../data/sample3/ghsl_%s/overlays_%s.rds", city$city_code, city$city_code))
    # print(head(a))
    
    
  })
  
  
  # filter the first indicator
  data_ind1 <- reactive({
    
    req(city$city_code)
    
    # print("ui")
    pattern <- sprintf("^%s", indicator$type)
    cols <- c('osmid', 'admin_level_ordered', 'name', grep(pattern, colnames(data_ind()), ignore.case = TRUE, value = TRUE))
    a <- data_ind()[cols]
    
    # print(head(a))
    return(a)
    
  })
  
  data_overlays1 <- reactive({
    
    # print(data_overlays())
    # print(class(indicator$type))
    # print(class(data_overlays()))
    print(indicator$type)
    ui <- indicator$type
    a <- subset(data_overlays(), startsWith(indicator, ui))
    # a <- subset(data_overlays(), indicator %like% indicator$type)
    # a <- subset(data_overlays(), startsWith(indicator, indicator$type))
    # a <- data_overlays()[grepl(indicator$type, indicator)]
    # print(head(a))
    return(a)
    
  })
  
  
  
  
  # second level: for each 'mode' 
  data_ind2 <- reactive({
    
    req(indicator_mode())
    
    # print(indicator_mode())
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    cols <- c('osmid','admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)])
    a <- data_ind1()[cols]
    colnames(a) <- c('osmid','admin_level_ordered', 'name', 'valor', 'geom')
    a <- a %>% dplyr::mutate(teste = "teste")
    
    # print(a)
    
    return(a)
    
  })
  data_overlays2 <- reactive({
    
    req(indicator_mode())
    pattern <- sprintf("%s_%s_2019", indicator$type, indicator_mode())
    # print(pattern)
    # print(head(data_overlays1()))
    a <- subset(data_overlays1(), indicator == pattern)
    
    # print(a)
    return(a)
    
    # print(sprintf("spatial level: %s", input$spatial_level))
    
  })
  

# additional filter for other indicators ----------------------------------

  
  
  
  # show ranks on the right panel -------------------------------------------
  
  get_rank <- reactive({
    
    # filter rank from rank files
    a <- readRDS(sprintf("../data/sample3/ranks/rank_%s.rds", city$city_code)) %>% setDT()
    
    
  })
  
  filter_rank <- reactive({
    
    
    req(indicator_mode())
    # print(paste0("pattern: ", indicator_mode()))
    # print(head(get_rank()))
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    cols <- c('osmid', 'admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'rank_type', 'n')
    a <- get_rank()[, ..cols]
    colnames(a) <- c('osmid','admin_level_ordered', 'name', 'rank', 'rank_type', 'n')
    return(a)
    
  })  
  
  filter_rank_country <- reactive({
    
    
    req(indicator_mode())
    # print(head(get_rank()))
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    # print(paste0("pattern: ", indicator_mode()))
    cols <- c('name_long', 'a2', colnames(atlas_country_ranks)[startsWith(colnames(atlas_country_ranks), pattern)])
    # print(cols)
    a <- atlas_country_ranks[, ..cols]
    colnames(a) <- c('name_long', 'a2', 'rank')
    # only top five
    a <- setorder(a, rank)
    a <- a[1:3,]
    # print(a)
    return(a)
    
  })  
  
  # observer to watch the click on the polygons to update the right panel ----
  
  rank <- reactiveValues(rank_value = NULL, rank_text = NULL,
                         rank_value_initial = NULL, rank_text_initial = NULL,
                         admin_level = NULL)
  
  
  # display initial rank with indicators - in the world view
  observeEvent(c(input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
    
    
    if(is.null(input$admin_level)) {
      
      # value
      atlas_country1 <- setDT(copy(atlas_country))
      pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
      # print(pattern)
      cols <- c('name_long', colnames(atlas_country1)[startsWith(colnames(atlas_country1), pattern)])
      # print(cols)
      a <- atlas_country1[, ..cols]
      colnames(a) <- c('name_long', 'valor')
      # only top five
      a <- setorder(a, -valor)
      a <- a[1:3,]
      # mean for the world
      rank_indicator <- mean(a$valor)
      
      # print(rank_indicator)
      
      # print(head(filter_rank()))
      # print(spatial_level_value$last)
      
      
      format_indicator_name <- switch (indicator_mode(),
                                       "pnpb" = "People Near Protected Bike Lanes",
                                       "pnab" = "People Near Bike Lanes",
                                       "healthcare" = "People Near Healthcare",
                                       "schools" = "People Near Schools",
                                       "hs" = "People Near Services"
      )
      
      format_indicator_value <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
        
        scales::percent(rank_indicator)
        
      } else round(rank_indicator)
      
      
      format_indicator_value_countries1 <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
        
        scales::percent(a$valor[1])
        
      } else round(a$valor[1])
      
      
      
      format_indicator_value_countries2 <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
        
        
        scales::percent(a$valor[2])
        
      } else round(a$valor[2])
      
      
      format_indicator_value_countries3 <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
        
        scales::percent(a$valor[3])
        
      } else round((a$valor[3]))
      
      
      
      # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
      rank$rank_value <- paste0('<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 20px">THIS INDICATOR IN </div>', 
                                '<div class="title_indicator" style="font-size: 20px;">', 
                                'THE WORLD', '</div>',
                                div(class = "value_indicator_rightpanel", format_indicator_value))
      
      
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
      
    }
    
    
  })
  
  observeEvent(c(input$admin_level), {
    
    rank$admin_level <- input$admin_level
    
  })
  
  observeEvent(c(city$city_code), {
    
    rank$admin_level <- 1
    
  })
  
  
  # display rank when region or map is clicked
  observeEvent(c(input$map_shape_click, input$map_marker_click, city$city_code,
                 input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
                   
                   ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
                   
                   
                   # print(paste0("ui: :", ui))
                   
                   # keep to osm_id selected
                   # osm_selected$oi <- ui$id
                   
                   
                   rank_indicator <- subset(data_ind2(), osmid == ui)
                   
                   # print(rank_indicator)
                   
                   # print(head(filter_rank()))
                   # print(spatial_level_value$last)
                   
                   
                   format_indicator_name <- switch (indicator_mode(),
                                                    "pnpb" = "People Near Protected Bike Lanes",
                                                    "pnab" = "People Near Bike Lanes",
                                                    "healthcare" = "People Near Healthcare",
                                                    "schools" = "People Near Schools",
                                                    "hs" = "People Near Services"
                   )
                   
                   # print(rank_indicator$valor)
                   
                   format_indicator_value <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
                     
                     scales::percent(rank_indicator$valor)
                     
                   } else round(rank_indicator$valor)
                   
                   
                   
                   # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
                   rank$rank_value <- paste0('<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 20px">THIS INDICATOR IN </div>', 
                                             '<div class="title_indicator" style="font-size: 20px;">', 
                                             rank_indicator$name, '</div>',
                                             div(class = "value_indicator_rightpanel", format_indicator_value))
                   # print(rank_indicator)
                   
                   # print(ui$id)
                   
                   # print(input$map_marker_click)
                   # print(input$admin_level)
                   # print(!is.null(print(input$map_marker_click)))
                   
                   # print(rank$rank_value)
                   # print(paste0("admin: ", input$admin_level))
                   # if (isTRUE(is.na(input$admin_level))) rank$rank_value_initial <- rank$rank_value
                   # print(rank$rank_value_initial)
                   # print(input$admin_level == 1 | !is.null(input$map_marker_click))
                   
                   # print(spatial_level_value$last)
                   print(paste0("gua; ", rank$admin_level))
                   # print(ui$id)
                   # print(filter_rank())
                   # print(input$admin_level == spatial_level_value$last)
                   # print(subset(filter_rank(), osmid == ui$id & rank_type == "metro"))
                   # print(!is.null(input$map_marker_click))
                   
                   # the number of ranks will depend on the admin level
                   
                   # this first condition will show the indicator ranks as soon as the city marker is clicked
                   base_text <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "COMPARED TO OTHER REGIONS")
                   
                   if (!is.null(city$city_code) & is.null(input$admin_level)) {
                     
                     a <- subset(filter_rank(), osmid == ui & rank_type == "world")
                     rank$rank_text <- sprintf('%s <div class="text_compare"> Ranks <strong>%s</strong> out of <strong>%s</strong> in the world</div>', 
                                               base_text, a$rank, a$n)
                     
                     rank$rank_text_initial <- rank$rank_text
                     rank$rank_value_initial <- rank$rank_value
                     
                   } else if (rank$admin_level == 1) {
                     
                     a <- subset(filter_rank(), osmid == ui & rank_type == "world")
                     
                     rank$rank_text <- sprintf('%s <div class="text_compare"> Ranks <strong>%s</strong> out of <strong>%s</strong> in the world</div>', 
                                               base_text, a$rank, a$n)
                     rank$rank_value <- paste0('<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 20px">THIS INDICATOR IN </div>', 
                                               '<div class="title_indicator" style="font-size: 20px;">', 
                                               rank_indicator$name, '</div>',
                                               div(class = "value_indicator_rightpanel", format_indicator_value))
                     rank$rank_text_initial <- rank$rank_text
                     rank$rank_value_initial <- rank$rank_value
                     print(paste0("teste: ", rank$rank_text_initial))
                     
                     
                   } else if (input$admin_level == spatial_level_value$last) {
                     
                     a <- subset(filter_rank(), osmid == ui & rank_type == "metro")
                     # print(a)
                     
                     rank$rank_text <- sprintf('%s  <div class="text_compare"> Ranks <strong>%s</strong> out of <strong>%s</strong> in the metro</div>', 
                                               base_text, a$rank, a$n)
                     
                   } else {
                     
                     a1 <- subset(filter_rank(), osmid == ui & rank_type == "world")
                     a2 <- subset(filter_rank(), osmid == ui & rank_type == "metro")
                     
                     
                     text1 <- sprintf('%s  <div class="text_compare"  style="padding-bottom: 5px">Ranks <strong>%s</strong> out of <strong>%s</strong> in the world</div>', 
                                      base_text, a1$rank, a1$n)
                     text2 <- sprintf('<div class="text_compare" style="padding-top: 0px";>Ranks <strong>%s</strong> out of <strong>%s</strong> in the metro</div>', 
                                      a2$rank, a2$n)
                     
                     rank$rank_text <- paste0(text1, text2)
                     
                   }
                   
                   
                   # print(rank$rank_text)
                   
                   
                   # print(input$map_shape_click$id)
                   # filter for the select shape
                   
                   # rank$rank_text <- sprintf("<h3><strong>%s</strong></h3> ranks <strong>%s</strong> out of <strong>%s</strong> in the world", a1$name, a1$rank, a1$n)
                   # rank$rank_text_metro <- sprintf("<h3><strong>%s</strong></h3> ranks <strong>%s</strong> out of <strong>%s</strong> in the metro", a2$name, a2$rank, a2$n)
                   # print(rank$rank_text)
                   # return(rank$rank_text)
                   
                 })
  
  
  
  # if I change the spatial_level, the right panel should inform the user
  # that they should click on a region to see more things
  observeEvent(c(input$admin_level, city$city_code), {
    
    
    # it will run only when we are at the city level
    
    if (isTRUE(rank$admin_level == 1)) {
      
      
      rank$rank_value <- rank$rank_value_initial
      rank$rank_text <- rank$rank_text_initial
      
      
    } else if (isTRUE(rank$admin_level != 1)) {
      
      rank$rank_value <- '<div class="text_compare"> Click on the map to see more info </div>'
      rank$rank_text <- ""
      
    }
    
    
  })
  
  
  output$rank_value <- renderUI({
    req(indicator_mode())
    
    HTML(rank$rank_value)
    
  })
  
  # create chart with indicator value
  
  
  
  output$rank_text <- renderUI({
    req(indicator_mode())
    
    HTML(rank$rank_text)
    
  })
  
  
  # reactive to create the filtered overlay with the geom -------------------
  
  data_overlays_sf <- reactive({
    
    req(indicator_mode())
    req(data_overlays2())
    # print(head(data_overlays2()))
    
    # extract geom type of this indicator
    geom_type <- unique(data_overlays2()$geom_type)
    
    print(data_overlays2())
    # print(geom_type)
    
    # select data tahat will be used for the overlay
    if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
      
      data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$polygon, by = "indicator") %>% st_sf()
      
    } else data_overlays_sf <- dplyr::left_join(data_overlays2(), overlay_geom$line, by = "indicator") %>% st_sf() 
    
    
    # print(head(data_overlays_sf))
    
    return(data_overlays_sf)
    
  })
  
  
  # update map --------------------------------------------------------------
  
  
  osm_selected <- reactiveValues(oi = NULL)
  
  admin_level_previous <- reactiveValues(a = NULL)
  
  observeEvent(c(input$city, input$map_marker_click), {
    
    # req(input$city)
    bbox <- sf::st_bbox(data_ind())
    
    # subset for the metro region polygons
    data_metro <- subset(data_ind2(), admin_level_ordered == 1)
    
    
    # if (isTRUE(input$admin_level == 1)) {
    
    osm_selected$oi <- data_metro$osmid
    
    # }
    
    
    # print(sprintf("Data ind raw: %s", head(data_ind2())))
    # print(sprintf("Data ind raw: %s", class(data_ind2())))
    
    
    # print(sprintf("Data ind: %s", head(data_ind2_cities)))
    # print(colnames(data_ind2_cities))
    # print(nrow(data_ind2_cities))
    
    # extract geom type of this indicator
    geom_type <- unique(data_overlays2()$geom_type)
    
    # print(data_overlays_sf())
    
    # print(paste0("geom type: ", geom_type))
    # print(head(data_overlays_sf))
    
    
    # print(data_metro$valor)
    print(data_metro$osmid)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data_metro$valor)
    
    
    
    
    map <- leafletProxy("map", session) %>%
      # clearMarkers() %>%
      removeMarker(layerId = data_metro$osmid) %>%
      clearShapes() %>%
      removeControl(layerId = "legend_country") %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
      # flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]],
      #             options = list(duration = 1.5,
      #                            animate = TRUE,
      #                            easeLinearity = 2,
      #                            noMoveStart = TRUE)) %>%
      
      addPolygons(data = data_metro, fillColor = ~pal(valor), color = "black",  weight = 1, layerId = ~osmid,
                  highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, weight = 6, color = "black")) %>%
      addLayersControl(baseGroups = c( "Dark", "Light"),
                       options = layersControlOptions(collapsed = TRUE)) 
    
    # print(head(map))
    
    # add overlay
    if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
      
      
      map <- map %>%
        addPolygons(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                    layerId = "overlay_layer") %>%
        # addLegend("bottomleft", pal = pal, values = ~valor) %>%
        addLayersControl(overlayGroups = c("Overlay"),
                         baseGroups = c("Dark", "Light"),
                         options = layersControlOptions(collapsed = FALSE))
      
      
    } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
      
      
      map <- map %>%
        addPolylines(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                     layerId = "overlay_layer") %>%
        # addLegend("bottomleft", pal = pal, values = ~valor) %>%
        addLayersControl(overlayGroups = c("Overlay"),
                         baseGroups = c("Dark", "Light"),
                         options = layersControlOptions(collapsed = FALSE))
      
      
      
    }
    
    map
    
    admin_level_previous$a <- 1
    
    
  })
  
  
  
  # update overlay only when indicator is changed --------------------------------
  
  observeEvent(c(input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
    
    # it will run only when we are at the city level
    if (isTRUE(input$admin_level >= 1)) {
      
      # print("ui")
      
      
      # extract geom type of this indicator
      geom_type <- unique(data_overlays2()$geom_type)
      # print(geom_type)
      
      map <- leafletProxy("map", session) %>%
        # clearMarkers() %>%
        removeShape(layerId = "overlay_layer")
      # clearControls()
      
      
      
      # add overlay
      if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
        
        
        map <- map %>%
          addPolygons(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                      layerId = "overlay_layer") %>%
          # addLegend("bottomleft", pal = pal, values = ~valor) %>%
          addLayersControl(overlayGroups = c("Overlay"),
                           baseGroups = c("Light", "Dark"),
                           options = layersControlOptions(collapsed = FALSE))
        
        
      } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
        
        
        map <- map %>%
          addPolylines(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE),
                       layerId = "overlay_layer") %>%
          # addLegend("bottomleft", pal = pal, values = ~valor) %>%
          addLayersControl(overlayGroups = c("Overlay"),
                           baseGroups = c("Light", "Dark"),
                           options = layersControlOptions(collapsed = FALSE))
        
        
        
      }
      
      map
      
      
    }
    
    
    
  })
  
  
  
  
  # update the basemap  --------------------------------
  observeEvent(c(input$admin_level,
                 input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_built_env), {
                   
                   
                   admin_level_previous$a <-admin_level_previous$a + 1
                   # print(isTRUE(is.null(admin_level_previous$a)))
                   print(admin_level_previous$a)
                   # it will run only when we are at the city level
                   # if (isTRUE(input$admin_level > 1)) {
                   if (isTRUE(input$admin_level >= 1)) {
                     
                     
                     # print("ui")
                     
                     # filter to selected spatial_level
                     # the highest visualiztion will be aggregated at cities
                     data_ind2_spatial <- subset(data_ind2(), admin_level_ordered == input$admin_level)
                     # create label
                     # data_ind2_spatial <- data_ind2_spatial %>% dplyr::mutate(label = sprintf("<h3>%s</h3><br/>Click for more information", name))
                     # data_ind2_spatial$label <- purrr::map_chr(data_ind2_spatial$label, ~htmltools::HTML)
                     
                     
                     # extract geom type of this indicator
                     geom_type <- unique(data_overlays2()$geom_type)
                     # print(geom_type)
                     
                     # create the color palette
                     pal <- colorNumeric(
                       palette = "YlOrRd",
                       domain = data_ind2_spatial$valor)
                     
                     # create legend title
                     legend_title <- fcase(
                       data_overlays2()$indicator %like% "pnpb", "% of the population within a 300m walk of a protected bikelane",
                       default = "teste"
                       
                     )
                     
                     
                     # format legend value
                     legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "healthcare", "schools", "hs")) {
                       
                       scales::percent
                       
                     } else labelFormat(suffix = " km", transform = function(x) as.integer(x))
                     
                     # print(paste0("legend value: ", data_ind2_spatial$valor))
                     
                     
                     
                     map <- leafletProxy("map", session) %>%
                       # clearMarkers() %>%
                       removeShape(layerId =  osm_selected$oi) %>%
                       # clearShapes() %>%
                       clearControls() %>%
                       addPolygons(data = data_ind2_spatial, fillColor = ~pal(valor), color = "black",  weight = 1
                                   , layerId = ~osmid,
                                   highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, 
                                                                       weight = 6, color = "black"
                                                                       # fillColor = 'yellow'
                                   )
                                   # label = ~(label)
                       ) %>%
                       addLegend(data = data_ind2_spatial, "bottomright",
                                 pal = pal,
                                 values = ~valor,
                                 title = legend_title,
                                 # bins = c(0, 0.25, 0.50, 0.75, 1),
                                 labFormat = legend_value
                       )
                     # 
                     
                     
                     # # add overlay
                     # if (geom_type %in% c("MULTIPOLYGON", "POLYGON")) {
                     #   
                     #   
                     #   map <- map %>%
                     #     addPolygons(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE)) %>%
                     #     # addLegend("bottomleft", pal = pal, values = ~valor) %>%
                     #     addLayersControl(overlayGroups = c("Overlay"),
                     #                      baseGroups = c("Light", "Dark"),
                     #                      options = layersControlOptions(collapsed = FALSE))
                     #   
                     #   
                     # } else if (geom_type %in% c("MULTILINESTRING", "LINESTRING")) {
                     #   
                     #   
                     #   map <- map %>%
                     #     addPolylines(data = data_overlays_sf(), group = "Overlay", options = pathOptions(clickable = FALSE)) %>%
                     #     # addLegend("bottomleft", pal = pal, values = ~valor) %>%
                     #     addLayersControl(overlayGroups = c("Overlay"),
                     #                      baseGroups = c("Light", "Dark"),
                     #                      options = layersControlOptions(collapsed = FALSE))
                     #   
                     #   
                     #   
                     # }
                     
                     map
                     
                     osm_selected$oi <- data_ind2_spatial$osmid
                     
                   }
                   
                   
                   
                   
                   
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
  
  # change to be made to UI afterwards
  observeEvent(c(input$admin_level, input$indicator_performance), {
    
    # disable the button
    shinyjs::runjs('$(".irs-single").remove();')
    # change background color to white
    # runjs('$("#modo_ativo button:eq(0)").css("background-color", "#FFF");')
    # remove the reactivity only when the element is child of #modo_ativo
    # runjs('$("#modo_ativo > div > div:nth-child(1) > button > input[value=public_transport]").remove();')
    # runjs('$("#modo_ativo > input[value=public_transport]").remove();')
    
    # disable(selector = "#modo_ativo button:eq(0)")
    # disable the button (still reactive tough)
    # runjs('$("#modo_ativo button:eq(0)").prop("disabled", true).prop("onclick",null).off("click");')
    # delete the button (not wanted)
    # runjs('$("#modo_ativo button:eq(0)").remove();')
    # runjs("$('input[value=B]').parent().attr('disabled', true);")
    
    
  })
  
  
  
}