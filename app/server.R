# open boundaries
atlas_city_markers <- readRDS("../data/sample3/atlas_city_markers.rds")
atlas_country <- readRDS("../data/sample3/atlas_country_polygons.rds")
# country rank
atlas_country_ranks <- readRDS("../data/sample3/ranks/rank_country.rds")
# list indicators
list_indicators <- readRDS("../data/sample3/list_indicators.rds")
list_osmid_name <- readRDS("../data/sample3/list_osmid_name.rds")


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
          shinyWidgets::pickerInput(inputId = "city",
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
                                    options = shinyWidgets::pickerOptions(size = 15,
                                                                          iconBase = "fa",
                                                                          tickIcon = "fa-check",
                                                                          title = "Search for a metro region ...",
                                                                          liveSearch = TRUE)
          )
      )
    )
    
  })
  
  
  
  
  
  output$left_panel_filter <- renderUI({
    
    
    tagList(
      # conditionalPanel(
      # condition = "ind_cum.indexOf(input.indicator_performance) > -1",
      # condition = "typeof input.indicator_performance != ''",
      absolutePanel(
        # id = "controls",
        class = "spatial_level",
        # fixed = TRUE, draggable = FALSE,
        bottom = 30, left = 300, height = 'auto',
        # 'typeof undefined' identifies when is null 
        tags$div(class = "title_left_panel", "YEAR", 
                 actionButton("teste5", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                              class = "minimize")),
        shinyWidgets::pickerInput(inputId = "year",
                                  label = NULL,
                                  choices = 1980:2019,
                                  selected = 2019,
                                  options = shinyWidgets::pickerOptions(
                                    size = 5
                                  )
                                  # selected = character(0)
        )
        
      )
      
      # )
    )
    
    
  })
  
  
  spatial_level_value <- reactiveValues(last = NULL)
  
  
  output$comparison_panel <- renderUI({
    
    req(input$admin_level, ind_city())
    
    
    # get options to show in the comparison
    choices_comparison <- subset(list_osmid_name, admin_level_ordered == input$admin_level)
    choices_values <- choices_comparison$osmid
    choices_names <- choices_comparison$name
    names(choices_values) <- choices_names
    
    
    absolutePanel(
      class = "spatial_level",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      bottom = 30, right = 860, height = 'auto', width = 400,
      tags$div(class = "title_left_panel", "COMPARE", 
               actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
                            class = "minimize"),
               actionButton("teste4", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px;",
                            class = "minimize")
      ),
      
      shinyWidgets::pickerInput(inputId = "city_compare",
                                label = NULL,
                                choices = choices_values,
                                multiple = TRUE,
                                options = shinyWidgets::pickerOptions(size = 15,
                                                                      iconBase = "fa",
                                                                      tickIcon = "fa-check",
                                                                      title = "Search for a region ...",
                                                                      liveSearch = TRUE)
      ),
      highchartOutput('comparison', height = "150px")
      # actionButton(inputId = "about",
      #              label = "About",
      #              class = "about_button"
      #              # selected = character(0)
      # )
    )
    
  })
  
  
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
          shinyWidgets::sliderTextInput(inputId = "admin_level",
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
          top = 20, right = 500, width = 90, height = 30,
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
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    # print("performance :", input$indicator_performance)
    # print(input$city)
    # print(indicator$type)
    
  })
  
  observeEvent(c(input$indicator_walk), {
    
    indicator$type <- "walk"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
    
  })
  
  observeEvent(c(input$indicator_transit), {
    
    indicator$type <- "transit"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
  })
  
  observeEvent(c(input$indicator_performance), {
    
    indicator$type <- "performance"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    
    # print("performance :", input$indicator_performance)
    
  })
  
  observeEvent(c(input$indicator_city), {
    
    indicator$type <- "city"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    # print("ai!")
    
  })
  
  
  
  # first, define the city input ----------------
  city <- reactiveValues(code = NULL)
  
  observeEvent(c(input$city), {city$city_code <- input$city})
  observeEvent(c(input$map_marker_click), {
    
    city$city_code <- input$map_marker_click$id
    
    shinyWidgets::updatePickerInput(session = session, inputId = "city",
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
  
  
  
  
  
  
  # open files --------------------------------------------------------------
  
  source("src/filter_indicators.R", local = TRUE)  
  source("src/ranks.R", local = TRUE)  
  source("src/texts_rightpanel.R", local = TRUE)  
  source("src/map.R", local = TRUE)  
  source("src/popovers.R", local = TRUE)  
  source("src/changes_jquery.R", local = TRUE)  
  source("src/about.R", local = TRUE)  
  source("src/back_to_world.R", local = TRUE)  
  source("src/compare.R", local = TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
}

