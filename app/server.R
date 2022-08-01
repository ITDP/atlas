# open boundaries
atlas_city_markers <- readRDS("../data/sample3/atlas_city_markers.rds")
atlas_country <- readRDS("../data/sample3/atlas_country_polygons.rds")
# country rank
atlas_country_ranks <- readRDS("../data/sample3/ranks/rank_country.rds")
# list indicators
list_indicators <- readRDS("../data/sample3/list_indicators.rds")


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
    # list_indicators <- structure(c("bike", "walk",  "transit", "performance",  "city"), 
    #                              .Names = c("Bicycle", "Walk",  "Transit", "Performance", "Built Env"))
    
    list_bike <- structure(c("pnpb", "pnab", "abikeways", "pbikeways"), 
                           .Names = c("People Near Protected Bikelanes", "People Near All Bikelanes",
                                      "Bikeways", "Protected Bikeways"))
    
    list_walk <- structure(c("pnh", "pne", "pns"), 
                           .Names = c("People Near Healthcare", "People Near Education", "People Near Services"))
    
    list_transit <- structure(c("pntt", "etct"), 
                              .Names = c("People Near Transit&nbsp;&nbsp;&nbsp;", "ETC"))
    
    list_performance <- structure(c("bikep", "walkp"), 
                                  .Names = c("Bicycle&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Walk"))
    
    list_city <- structure(c("poptotal", "density"), 
                           .Names = c("Population&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;","Population Density"))
    
    # list_spatial_levels <- structure(c("adminstrative_area2(city)", "adminstrative_area3(juris)"),
    #                                  .Names = c("city", "jurisdiction"))
    
    
    # Start proper UI here 
    tagList(
      
      tags$div(class = "title_left_panel", "INDICATORS", 
               actionButton("teste1", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                            class = "minimize")
               # tags$button(id = "teste1", type = "button", class = "btn", style= "float: right; padding: 0",
               #             icon("bus"))
               
      ),
      
      
      
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
                    bottom = 5, left = -2, width = 240, height = 'auto',
                    # by type of indicator
                    # conditionalPanel(
                    # condition = "input.indicator == 'bike'",
                    
                    accordion_input(inputId = "indicator_bike",
                                    label = "Bike",
                                    choices = c(list_bike),
                                    selected = "pnpb"),
                    
                    accordion_input(inputId = "indicator_city",
                                    label = "City",
                                    choices = c(list_city),
                                    selected = character(0)),
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
                    # condition = "input.indicator == 'city'",
                    accordion_input(inputId = "indicator_performance",
                                    label = "Performance",
                                    choices = c(list_performance),
                                    selected = character(0))
                    
                    
                    #   pickerInput(inputId = "indicator_city",
                    #               label = "Built Env",
                    #               choices = c(list_city),
                    #               selected = "pne")
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
          bottom = 30, left = 300, height = 'auto',
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
          bottom = 30, right = 530, height = 'auto',
          # 'typeof undefined' identifies when is null 
          tags$div(class = "title_left_panel", "LEVEL OF DETAIL", 
                   actionButton("teste2", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                                class = "minimize")),
          sliderTextInput(inputId = "admin_level",
                          choices = seq(1, go),
                          label = NULL,
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
          tabsetPanel(type = "tabs", id = "right_tabs",
                      tabPanel("OVERVIEW", value = "tab_overview",         
                               absolutePanel(
                                 class = "right_panel_textbox",
                                 top = 80, right = 0, width = 280,
                                 htmlOutput("text_indicator"),
                                 tags$button(
                                   id = "link_see_more",
                                   class = "btn btn-default action-button shiny-bound-input",
                                   div(class = "link_button", "Read more")
                                 ),
                                 htmlOutput("rank_value"),
                                 htmlOutput("rank_text")
                                 
                               )
                               
                               
                      ),
                      tabPanel("MORE INFO",  value = "tab_viewmore",                                
                               absolutePanel(
                                 class = "right_panel_textbox",
                                 top = 80, right = 0, width = 280,
                                 htmlOutput("text_indicator2")
                                 
                               )
                               )
                      
                      
          )
          
        )
        
      )
    )
    
  })
  
  
  
  observeEvent(input$link_see_more, {
    updateNavbarPage(session, "right_tabs", "tab_viewmore")
  })
  
  output$back_to_world_panel <- renderUI({
    
    tagList(
      
      conditionalPanel(
        condition = "input.city != '' || typeof input.map_marker_click !== 'undefined'",
        absolutePanel(
          class = "right_panel",
          # class = "w3-container w3-animate-opacity", 
          # class = "panel panel-default",
          # fixed = TRUE, draggable = FALSE,
          top = 20, right = 400, width = 90, height = 30,
          style = "background: black",
          actionButton(inputId = "back_to_world",
                       label = "Reset map"
                       # selected = character(0)
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
    updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    # print("performance :", input$indicator_performance)
    # print(input$city)
    # print(indicator$type)
    
  })
  
  observeEvent(c(input$indicator_walk), {
    
    indicator$type <- "walk"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
    
  })
  
  observeEvent(c(input$indicator_transit), {
    
    indicator$type <- "transit"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
  })
  
  observeEvent(c(input$indicator_performance), {
    
    indicator$type <- "performance"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    
    # print("performance :", input$indicator_performance)
    
  })
  
  observeEvent(c(input$indicator_city), {
    
    indicator$type <- "city"
    # update the others
    updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    # print("ai!")
    
  })
  
  # observeEvent(c(input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_city), {
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
  #                                                                                  ifelse(startsWith(input$last_input, "city"), "city"))))
  #   
  # 
  #   
  #   })
  
  
  
  # first, define the city input ----------------
  city <- reactiveValues(code = NULL)
  
  observeEvent(c(input$city), {city$city_code <- input$city})
  observeEvent(c(input$map_marker_click), {
    
    city$city_code <- input$map_marker_click$id
    
    updatePickerInput(session = session, inputId = "city",
                      selected = city$city_code)
    
    
  }
  )
  
  # reactive to select the type of indicator --------------------------------
  indicator_mode <- reactive({
    
    req(indicator$type)
    
    if (indicator$type == "bike") {
      
      a <- input$indicator_bike
      
    } else if (indicator$type == "walk"){
      
      a <- input$indicator_walk
      
    } else if (indicator$type == "transit"){
      
      a <- input$indicator_transit
      
    } else if (indicator$type == "city"){
      
      a <- input$indicator_city
      
    } else if (indicator$type == "performance"){
      
      a <- input$indicator_performance
      
    }
    
    # print("indicator mode")
    # print(a)
    return(a)
    
  })
  
  
  output$text_indicator <- renderUI({
    
    req(indicator_mode())
    
    includeHTML(sprintf("www/text/text_indicator_%s.html", indicator_mode()))
    
  })
  
  output$text_indicator2 <- renderUI({
    
    # str1 <- paste("You have selected", input$var)
    # str2 <- paste("You have chosen a range that goes from",
    #               input$range[1], "to", input$range[2])
    # HTML(paste(str1, str2, sep = '<br/>'))
    # 
    #   h3("People Near Protected Bikelanes")
    #   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
    
    req(indicator_mode())
    # print(indicator_mode())
    
    includeHTML(sprintf("www/text/text_indicator_more_%s.html", indicator_mode()))
    
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
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(baseGroups = c("Dark", "Light", "Satellite"),
                       # overlayGroups = c("Overlay"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright") %>%
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
  observeEvent(c(indicator_mode(), input$back_to_world), {
    
    req(indicator_mode(), is.null(input$admin_level))               
    
    
    print("obs1")
    
    # print(indicator_mode())
    
    # this will only runs if we are at the wold view (admin level = null)
    # if(is.null(input$admin_level)) {
    
    # print(atlas_city_markers)
    
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    # print("pattern")
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
    legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
      
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
        radius = 8,
        # fillColor = ~pal(valor), 
        stroke = TRUE, fillOpacity = 0.9, color = "#00AE42",
        opacity = 0.8,
        weight = 1,
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
  
  
  
  # BOTAO PARA VOLTAR !!!!!!!!!! --------------------------------------------
  
  
  
  observeEvent(c(input$back_to_world), {
    
    
    # print("back to world")
    # print(input$back_to_world)
    
    # if (isTRUE(input$back_to_workd != 0)) {
    
    req(input$back_to_world >= 1)
    
    print("back to world")
    
    
    updatePickerInput(session = session, inputId = "city",
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
    
    # create legend title
    legend_title <- fcase(
      indicator_mode() %like% "pnpb", "% of the population within a 300m walk of a protected bikelane",
      default = "teste"
      
    )
    
    
    # format legend value
    legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
      
      scales::percent
      
    } else labelFormat(suffix = " km", transform = function(x) as.integer(x))
    
    # print(a)
    
    leafletProxy("map", data = a) %>%
      clearMarkers() %>%
      # clearControls() %>%
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
    # htmlwidgets::onRender("function(el, x) {$('.leaflet-control-layers').prepend('<label class = \"control-label\">MAP DETAILS</label>');}")
    #   htmlwidgets::onRender('
    #     function() {
    #         $(".leaflet-control-layers").prepend("<label style=\'text-align:center\'>My Epic Title</label>");
    #     }
    # ')
    
    # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')
    # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')
    
    
    
    # }
    
  }) 
  
  # observeEvent(input$map_shape_click, {
  #   
  #   print(input$map_shape_click)
  #   
  # })
  
  
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
    # print(indicator$type)
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
    
    req(city$city_code)
    # filter rank from rank files
    a <- readRDS(sprintf("../data/sample3/ranks/rank_%s.rds", city$city_code)) %>% setDT()
    
    
  })
  
  filter_rank <- reactive({
    
    
    req(indicator_mode())
    # print(paste0("pattern: ", indicator_mode()))
    # print(head(get_rank()))
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    # print("pattern")
    # print(pattern)
    cols <- c('osmid', 'admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'rank_type', 'n')
    a <- get_rank()[, ..cols]
    colnames(a) <- c('osmid','admin_level_ordered', 'name', 'rank', 'rank_type', 'n')
    # print(head(a))
    return(a)
    
  })  
  
  filter_rank_country <- reactive({
    
    req(indicator$type)    
    
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    # print(paste0("pattern: ", indicator_mode()))
    cols <- c('name_long', 'a2', colnames(atlas_country_ranks)[startsWith(colnames(atlas_country_ranks), pattern)], 'n')
    # print(cols)
    a <- atlas_country_ranks[, ..cols]
    colnames(a) <- c('name_long', 'a2', 'rank', 'n')
    # only top five
    a <- setorder(a, rank)
    a <- a[1:3,]
    # print(a)
    return(a)
    
  })  
  
  # observer to watch the click on the polygons to update the right panel ----
  
  rank <- reactiveValues(rank_value = NULL, rank_text = NULL,
                         rank_value_initial = NULL, rank_text_initial = NULL,
                         rank_value_world = NULL, rank_text_world = NULL,
                         admin_level = NULL)
  
  
  # display initial rank with indicators - in the world view
  observeEvent(c(input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_city), {
    
    
    
    # req(indicator_mode())
    # req(indicator$type)
    
    print("agora vai!")
    # print(rank$admin_level)
    
    if(is.null(rank$admin_level)) {
      
      # print("queeeeeeeeeee")
      
      # value
      atlas_country1 <- setDT(copy(atlas_country))
      # print(paste0("type: ", indicator$type))
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
      
      # print("oooia")
      # print(rank_indicator)
      
      # print(head(filter_rank()))
      # print(spatial_level_value$last)
      
      format_indicator_name <- list_indicators[indicator_code == indicator_mode()]$indicador_name
      
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
      
      rank$rank_text_world <- rank$rank_text_value
      
    }
    
    
  })
  
  
  # store the admin level in this reactivevalue, so it behaves as it should
  observeEvent(c(input$admin_level), {
    
    rank$admin_level <- input$admin_level
    
  })
  
  observeEvent(c(city$city_code), {
    
    if (city$city_code != "") {
      
      rank$admin_level <- 1
      
    }
    
    
  })
  
  observeEvent(c(input$back_to_world), {
    
    rank$admin_level <- NULL
    
  })
  
  
  # display rank when region or map is clicked
  observeEvent(c(input$map_shape_click,
                 input$indicator_bike, input$indicator_walk, input$indicator_transit, input$indicator_city), {
                   
                   ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
                   
                   
                   # print(paste0("ui: :", ui))
                   
                   # keep to osm_id selected
                   # osm_selected$oi <- ui$id
                   
                   
                   rank_indicator <- subset(data_ind2(), osmid == ui)
                   
                   # print(rank_indicator)
                   
                   # print(head(filter_rank()))
                   # print(spatial_level_value$last)
                   
                   
                   # format_indicator_name <- switch (indicator_mode(),
                   #                                  "pnpb" = "People Near Protected Bike Lanes",
                   #                                  "pnab" = "People Near Bike Lanes",
                   #                                  "pnh" = "People Near Healthcare",
                   #                                  "pne" = "People Near pne",
                   #                                  "hs" = "People Near Services"
                   # )
                   format_indicator_name <- list_indicators[indicator_code == indicator_mode()]$indicador_name
                   
                   # print(rank_indicator$valor)
                   
                   format_indicator_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
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
                   # print(paste0("gua; ", rank$admin_level))
                   # print(ui$id)
                   # print(filter_rank())
                   # print(input$admin_level == spatial_level_value$last)
                   # print(subset(filter_rank(), osmid == ui$id & rank_type == "metro"))
                   # print(!is.null(input$map_marker_click))
                   
                   # the number of ranks will depend on the admin level
                   
                   # this first condition will show the indicator ranks as soon as the city marker is clicked
                   base_text <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "COMPARED TO OTHER REGIONS")
                   
                   if (!is.null(city$city_code) & isTRUE(is.null(rank$admin_level))) {
                     
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
                     # print(paste0("teste: ", rank$rank_text_initial))
                     
                     
                     
                     # print("olha")
                     # print(rank$rank_value)
                     # print(a$rank)
                     # print(a$n)
                     
                     
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
  observeEvent(c(input$admin_level, input$map_marker_click, city$city_code), {
    
    
    # print(paste0("rank admin level"))
    # print(rank$admin_level)
    
    # it will run only when we are at the city level
    
    if (isTRUE(rank$admin_level == 1)) {
      
      # print(city$city_code)
      rank_indicator <- subset(data_ind2(), osmid == city$city_code)
      
      format_indicator_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
        scales::percent(rank_indicator$valor)
      } else round(rank_indicator$valor)
      
      
      
      # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
      rank$rank_value <- paste0('<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 20px">THIS INDICATOR IN </div>', 
                                '<div class="title_indicator" style="font-size: 20px;">', 
                                rank_indicator$name, '</div>',
                                div(class = "value_indicator_rightpanel", format_indicator_value))
      # print(rank$rank_value)
      
      # this first condition will show the indicator ranks as soon as the city marker is clicked
      base_text <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "COMPARED TO OTHER REGIONS")
      
      a <- subset(filter_rank(), osmid == city$city_code & rank_type == "world")
      # print(a)
      
      rank$rank_text <- sprintf('%s <div class="text_compare"> Ranks <strong>%s</strong> out of <strong>%s</strong> in the world</div>', 
                                base_text, a$rank, a$n)
      
      
    } else
      
      if (isTRUE(rank$admin_level != 1)) {
        
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
  
  
  tl <- reactiveValues(transition = NULL)
  
  # observeEvent(c(input$city, input$map_marker_click), {
  observeEvent(c(city$city_code), {
    
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
                  label = ~htmlEscape(name),
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
        addPolygons(data = data_overlays_sf(), group = "Overlay", opacity = 0.8,
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
      addMapPane("overlay", zIndex = 420) # shown above ames_lines
    # clearControls()
    
    
    
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
                   # print(city$city_code)
                   # print(rv$prev_city[length(rv$prev_city)-1])
                   
                   
                 }, ignoreInit = TRUE)
  
  
  # update the basemap  --------------------------------
  observeEvent(c(input$admin_level,
                 indicator_mode()), {
                   
                   
                   # print("prev_city")
                   # print(rv$prev_city[length(rv$prev_city)-1])
                   # print("atual_city")
                   # print(city$city_code)
                   
                   req(city$city_code == rv$prev_city[length(rv$prev_city)-1],
                       isTRUE(input$admin_level >= 1))
                   
                   # admin_level_previous$a <-admin_level_previous$a + 1
                   
                   # print(isTRUE(is.null(admin_level_previous$a)))
                   # print("previous")
                   # print(admin_level_previous$a)
                   # print(rank$admin_level)
                   # it will run only when we are at the city level
                   # if (isTRUE(admin_level_previous$a >= 1)) {
                   # if (isTRUE(input$admin_level >= 1)) {
                   
                   
                   
                   
                   print("obs4")
                   
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
                   legend_value <- if(indicator_mode() %in% c("pnpb", "pnab", "pnh", "pne", "pns")) {
                     
                     scales::percent
                     
                   } else labelFormat(suffix = " km", transform = function(x) as.integer(x))
                   
                   # print(paste0("legend value: ", data_ind2_spatial$valor))
                   
                   
                   
                   map <- leafletProxy("map", session) %>%
                     # clearMarkers() %>%
                     removeShape(layerId =  osm_selected$oi) %>%
                     removeTiles(layerId =  "tile") %>%
                     # clearShapes() %>%
                     clearControls() %>%
                     addMapPane("basemap", zIndex = 410) %>% # shown below ames_circles
                     addMapPane("overlay", zIndex = 420)# shown above ames_lines
                   
                   
                   if (isTRUE(input$indicator_bike == "pnab")) {
                     
                     
                     # create the color palette
                     pal <- colorNumeric(
                       palette = "YlOrRd",
                       domain = seq(0, 1000, 50))
                     
                     map <- map %>%
                       addMapboxTiles(style_id = "cl1b8ovgb001b14nuzqlt28sz",
                                      username = "kauebraga",
                                      layerId = "tile",
                                      options = tileOptions(zIndex = 9999,  opacity = 0.8),
                                      access_token = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ") %>%
                       addLegend(pal = pal,  "bottomright",values = seq(0, 1000, 50), title = "teste")
                     
                     
                   } else  {
                     
                     
                     map <- map %>%
                       addPolygons(data = data_ind2_spatial, 
                                   fillColor = ~pal(valor), fillOpacity = 0.5,
                                   color = "black",  weight = 1, 
                                   label = ~htmlEscape(name),
                                   layerId = ~osmid,
                                   options = pathOptions(pane = "basemap"),
                                   highlightOptions = highlightOptions(bringToFront = FALSE, opacity = 1, 
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
                     
                     
                   } 
                   
                   map
                   
                   osm_selected$oi <- data_ind2_spatial$osmid
                   
                   # shinyjs::runjs('$( ".leaflet-control-layers > label" ).remove();')
                   # shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );')
                   
                   # }
                   
                   
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
  
  
  # change to be made to UI afterwards
  observeEvent(c(input$admin_level, city$city_code), {
    
    delay(1, shinyjs::runjs('$(".irs-single").remove();'))
    
    # a <- tags$div(class = "title_left_panel", "MAP DETAILS",
    #          actionButton("teste3", label = "", icon = icon("minus"), style= "float: right; padding: 0",
    #                       class = "minimize")
    # 
    # )
    
    a <- "<div class='title_left_panel'>  MAP DETAILS  <button class='btn btn-default action-button minimize' id='teste3' style='float: right; padding: 0' type='button'><i class='fa fa-minus' role='presentation' aria-label='minus icon'></i> </button></div>"
    
    delay(1, shinyjs::runjs('$( ".leaflet-control-layers > .title_left_panel" ).remove();'))
    delay(1, shinyjs::runjs(sprintf('$( ".leaflet-control-layers" ).prepend( "%s");', a)))
    # delay(1, shinyjs::runjs('$( ".leaflet-control-layers" ).prepend( "<label class = \'control-label\'>MAP DETAILS</label>" );'))
    
    # print("input$admin_level")
    # print(input$admin_level)
    
    # shinyjs::runjs("var today = new Date(); alert(today);")
    
    
    
    
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$teste1), {
    print("button")
    print(input$teste1)
  })
  
  observeEvent(c(input$teste1), {
    
    req(input$teste1 >= 1)
    
    # collpase the intire indicator panels
    # shinyjs::runjs("$('.left_panel_indicators').slideToggle('')")
    
    
    # collpase the the indicators inside each indicator header - and keep the header
    a <- "if(!$('#indicator_bike').hasClass('in'))
    {
    // alert('Collapsed');
    $('#indicator_bike, #indicator_walk, #indicator_transit, #indicator_city, #indicator_performance').collapse('show');
    }
    else {
    // alert('Collapsed');
    $('#indicator_bike, #indicator_walk, #indicator_transit, #indicator_city, #indicator_performance').not('active').collapse('hide');
    }
    "
    
    # keep only the selected category (bike, walk etc)
    
    
    shinyjs::runjs(a)
    
    
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$teste2), {
    
    req(input$teste2 >= 1)
    
    shinyjs::runjs("$('#spatial_level > div > div > div.form-group.shiny-input-container > span').slideToggle('')")
    
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$teste3), {
    
    req(input$teste3 >= 1)
    
    shinyjs::runjs("$('.leaflet-control-layers > .leaflet-control-layers-list').slideToggle('')")
    
  }, ignoreInit = TRUE)
  
  
  
}

