# open boundaries
atlas_city_markers <- readRDS("../data/sample5/atlas_city_markers.rds")
atlas_country <- readRDS("../data/sample5/atlas_country_polygons.rds")
# filter only countries that have indidcators
atlas_country <- subset(atlas_country, !is.na(bike_pnpb_2022))
# country rank
atlas_country_ranks <- readRDS("../data/sample5/ranks/rank_country.rds")
# list indicators
list_indicators <- readRDS("../data/sample5/list_indicators.rds")
list_osmid_name <- readRDS("../data/sample5/list_osmid_name.rds")
list_availability <- readRDS("../data/sample5/list_availability.rds")
list_block <- readRDS("../data/sample5/list_block_density.rds")




function(input, output, session) {
  
  
  
  # ui----------------------------------------------------------------------
  
  output$city_selection <- renderUI({
    
    # make list
    oi <- split(list_availability, list_availability$country)
    pera <- function(variables) {
      
      name <- unique(variables$name)
      code <- unique(variables$hdc)
      
      names(code) <- name
      
      return(code)
      
      
    }
    list_selection <- lapply(oi, pera)
    
    
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
      h3(style = "color: #00AE42; display: inline-block; font-size: 28px; margin-right: 15px", strong("ATLAS")),
      div(style = "display: inline-block;",
          shinyWidgets::pickerInput(inputId = "city",
                                    label = NULL,
                                    width = "220px",
                                    choices = list_selection,
                                    options = shinyWidgets::pickerOptions(size = 15,
                                                                          iconBase = "fa",
                                                                          tickIcon = "fa-check",
                                                                          title = "Search for a metro region ...",
                                                                          liveSearch = TRUE)
          )
      )
    )
    
  })
  
  
  observeEvent(c(indicator$mode), {
    
    year_options <- subset(list_availability, ind == indicator$mode)$availability
    year_options <- unlist( strsplit(year_options, "[|]"))
    year_options <- unique(year_options)
    
    updatePickerInput(
      session = session,
      inputId = "year",
      choices = year_options,
      selected = 2022)
    
    
  })
  
  
  # output$left_panel_filter <- renderUI({
  #   
  #   # print("input$admin_level")
  #   # print(input$admin_level)
  #   
  #   req(indicator$mode)
  #   
  #   year_options <- subset(list_availability, ind == indicator$mode)$availability
  #   year_options <- unlist( strsplit(year_options, "[|]"))
  #   year_options <- unique(year_options)
  #   
  #   
  #   # hdc_available <-  subset(list_availability, ind = indicator$mode)$hdc
  #   # hdc_available <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))$hdc
  #   
  #   
  #   tagList(
  #     # conditionalPanel(
  #     # condition = "ind_cum.indexOf(input.indicator_performance) > -1",
  #     # condition = "typeof input.indicator_performance != ''",
  #     absolutePanel(
  #       # id = "controls",
  #       class = "spatial_level",
  #       # fixed = TRUE, draggable = FALSE,
  #       bottom = 45, left = 300, height = 'auto', width = 120,
  #       # 'typeof undefined' identifies when is null 
  #       tags$div(class = "title_left_panel", style = "padding: 10px 0", "YEAR" ,
  #                # actionButton("teste5", label = "", icon = icon("minus"), style= "float: right; padding: 0",
  #                # class = "minimize")
  #                tags$button(
  #                  id = "tooltip_year",
  #                  class="btn btn-light btn-xs",
  #                  style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
  #                  icon("circle-info")
  #                  
  #                ),
  #       ),
  #       div(
  #         bsPopover(id = "tooltip_year",
  #                   # title = sprintf("<strong>%s</strong>", "LEVEL OF DETAIL"),
  #                   title = "",
  #                   content = HTML(includeHTML('www/tooltips/tooltip_year.html')),
  #                   placement = "right",
  #                   trigger = "hover",
  #                   options = list(container = "body")
  #         )
  #       ),
  #       shinyWidgets::pickerInput(inputId = "year",
  #                                 label = NULL,
  #                                 choices = year_options,
  #                                 selected = 2019,
  #                                 options = shinyWidgets::pickerOptions(
  #                                   size = 5
  #                                 )
  #                                 # selected = character(0)
  #       )
  #       
  #     )
  #     
  #     # )
  #   )
  #   
  #   
  # })
  
  
  spatial_level_value <- reactiveValues(last = NULL)
  
  
  # output$comparison_button <- renderUI({
  #   
  #   req(input$admin_level)
  #   
  #   
  #   absolutePanel(
  #     
  #     
  #     class = "spatial_level",
  #     style = "background: #00AE42",
  #     # class = "w3-container w3-animate-opacity", 
  #     # class = "panel panel-default",
  #     # fixed = TRUE, draggable = FALSE,
  #     bottom = 45, left = 440, height = 'auto', width = 130,
  #     # tags$div(class = "title_left_panel", 
  #     #          # "COMPARE", 
  #     #          actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
  #     #                       class = "minimize"),
  #     #          actionButton("teste4", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px;",
  #     #                       class = "minimize")
  #     # ),
  #     actionButton(inputId = "comparison_button", 
  #                  label = "COMPARE",
  #                  style = "display: inline; padding-right: 2px;"),
  #     tags$button(
  #       id = "tooltip_compare",
  #       class="btn btn-light btn-xs",
  #       style = "display: inline; width: 5px; background: transparent; padding-left: 0; color: #1C1C1C; font-size: 14px",
  #       icon("circle-info")
  #       
  #     ),
  #     # label = label_with_info("COMPARE", tooltip_id = "tooltip_compare")
  #     # , onclick = '$("#comparison_panel").toggle("show");'
  #     
  #     div(
  #       bsPopover(id = "tooltip_compare",
  #                 title = "",
  #                 content = HTML(includeHTML('www/tooltips/tooltip_comparison.html')),
  #                 placement = "top",
  #                 trigger = "hover",
  #                 options = list(container = "body"))
  #     )
  #   )
  #   
  # })
  
  
  
  # observeEvent(c(input$comparison_button), {
  # 
  #   # print("input$comparison_button")
  #   # print(as.numeric(input$comparison_button))
  #   req(input$comparison_button >= 1)
  #   
  #   # toggle("lalala")
  # 
  #   # runjs("$( '#lalala' ).toggle();")
  # 
  #   print("parara")
  #   
  # 
  # })
  
  
  # onclick("comparison_button", runjs("$( '#lalala' ).toggle();"))
  
  
  # show the comparison panel when the user clicks on the compare panel -------------------------
  onclick("comparison_button", toggle("lalala"))
  
  
  
  # hides the comparison button when the user is in the global view -----------------------------
  observeEvent(c(city$city_code), {
    
    req(city$city_code != "", city$times == 0)
    
    # print("BEEEEEEEEEEEEEEEM")
    
    shinyjs::show("compare_panel")
    
    
  }, once = FALSE)
  
  
  # create regions names for the comparison 
  comparison_values <- reactive({
    
    req(input$admin_level, data_ind3_spatial(), indicator$mode)
    
    
    
    # get the admin level original
    al <- as.numeric(unique(data_ind3_spatial()$admin_level))
    
    # print("al")
    # print(al)
    
    # first, select only the ones that are available for the indicator in question
    hdc_available <-  subset(list_availability, ind == indicator$mode)$hdc
    # hdc_available <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))$hdc
    
    # get options to show in the comparison
    choices_comparison <- subset(list_osmid_name, admin_level == al)
    # filter hdc with the indicators available
    choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
    # if is in the neigbourhood level (level >= 10), only show for the city in question
    if (al >= 10) {
      
      # print("uhhhhhhhhhhhhhhhhhhhhhhhhh")
      
      choices_comparison <- subset(choices_comparison, hdc == city$city_code)
      
    }
    
    # remove the osmid that is already being shown
    # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
    # extract values
    choices_values <- choices_comparison$osmid
    choices_names <- choices_comparison$name
    names(choices_values) <- choices_names
    
    # print("choices_names")
    # print(choices_names)
    
    return(choices_values)
    
  })
  
  output$comparison_panel <- renderUI({
    
    
    
    absolutePanel(
      id = "lalala",
      class = "spatial_level",
      style = "z-index: 9999999999; opacity: 0.97",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      bottom = 115, left = 440, height = 'auto', width = 500,
      tags$div(class = "title_left_panel", "COMPARE", 
               actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
                            class = "minimize")
               # actionButton("teste4", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px;",
               #              class = "minimize")
      ),
      
      shinyWidgets::pickerInput(inputId = "city_compare",
                                label = NULL,
                                choices = comparison_values(),
                                multiple = TRUE,
                                options = shinyWidgets::pickerOptions(size = 15,
                                                                      iconBase = "fa",
                                                                      tickIcon = "fa-check",
                                                                      title = "Search for a region...",
                                                                      liveSearch = TRUE)
      ),
      highchartOutput('comparison_chart', height = "250px")
    ) %>% hidden()
    
    # }
    
    
  })
  
  
  
  
  output$spatial_level <- renderUI({
    
    
    # CALCULATE the spatial levels for each city
    
    req(input$city != "")
    
    # print(paste0("last", spatial_level$last))
    go <- length(unique(data_ind()$admin_level))
    
    
    
    tagList(
      conditionalPanel(
        condition = "input.city != '' || typeof input.map_marker_click !== 'undefined'",
        absolutePanel(
          # id = "controls",
          class = "spatial_level",
          # fixed = TRUE, draggable = FALSE,
          bottom = 45, right = 480, height = 'auto', width = 220,
          # 'typeof undefined' identifies when is null
          tags$div(class = "title_left_panel", style = "padding: 10px 0", "LEVEL OF DETAIL",
                   tags$button(
                     id = "tooltip_level",
                     class="btn btn-light btn-xs",
                     style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                     icon("circle-info")
                     
                   ),
                   actionButton("teste2", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                                class = "minimize")
                   
          ),
          
          
          
          # label = label_with_info("COMPARE", tooltip_id = "tooltip_compare")
          # , onclick = '$("#comparison_panel").toggle("show");'
          
          div(
            bsPopover(id = "tooltip_level",
                      # title = sprintf("<strong>%s</strong>", "LEVEL OF DETAIL"),
                      title = "",
                      content = HTML(includeHTML('www/tooltips/tooltip_level.html')),
                      placement = "top",
                      trigger = "hover"
                      # options = list(container = "body")
            )
          ),
          conditionalPanel(
            condition = "output.panelStatus",
            radioGroupButtons(inputId = "regions_grid", label = "", choices = c("Regions"), selected = "Regions", justified = TRUE),
          ),
          conditionalPanel(
            condition = "input.regions_grid == 'Regions'",
            shinyWidgets::sliderTextInput(inputId = "admin_level",
                                          choices = seq(1, go),
                                          label = NULL,
                                          selected = 1,
                                          grid = TRUE,
                                          dragRange = FALSE
                                          # selected = character(0)
            )
          )
          # actionButton("grid", label = "", icon = icon("table-cells-large"))
          
        )
        
      )
    )
    
    
    
  })
  
  observeEvent(c(indicator$mode), {
    
    req(city$city_code != "")
    
    # print("indicator$type")
    # print(indicator$type)
    
    if (indicator$type == "performance") {
      
      updateRadioGroupButtons(
        session = session,
        "regions_grid",
        choices = c("Regions", "Grid"),
        selected = "Regions",
        justified = TRUE
      )
      
      
    } else {
      
      
      updateRadioGroupButtons(
        session = session,
        "regions_grid",
        choices = "Regions",
        selected = "Regions"
      )
      
    }
    
    
    
  })
  
  
  
  
  # output$right_panel <- renderUI({
  #   
  #   tagList(
  #     
  #     # conditionalPanel(
  #     # condition = "typeof input.indicator_bike !== 'undefined'",
  #     absolutePanel(
  #       class = "right_panel",
  #       # class = "w3-container w3-animate-opacity", 
  #       # class = "panel panel-default",
  #       # fixed = TRUE, draggable = FALSE,
  #       top = 0, right = 0, width = 300, height = "calc(100vh - 15px)",
  #       tabsetPanel(type = "tabs", id = "right_tabs",
  #                   tabPanel("OVERVIEW", value = "tab_overview",         
  #                            absolutePanel(
  #                              class = "right_panel_textbox",
  #                              top = 65, right = 5, width = 280,
  #                              htmlOutput("text_indicator"),
  #                              tags$button(
  #                                id = "link_see_more",
  #                                class = "btn btn-default action-button shiny-bound-input",
  #                                div(class = "link_button", "Read more")
  #                              ),
  #                              uiOutput("rank_value"),
  #                              uiOutput("rank_text")
  #                              
  #                            )
  #                            
  #                            
  #                            
  #                   ),
  #                   tabPanel("MORE INFO",  value = "tab_viewmore",                                
  #                            absolutePanel(
  #                              class = "right_panel_textbox",
  #                              top = 65, right = 5, width = 280,
  #                              htmlOutput("text_indicator2")
  #                              
  #                            )
  #                   )
  #                   
  #                   
  #                   # )
  #                   
  #       )
  #       
  #     )
  #   )
  #   
  # })
  # 
  # 
  
  
  observeEvent(input$link_see_more, {
    updateNavbarPage(session, "right_tabs", "tab_viewmore")
  })
  
  output$panelStatus <- reactive({
    indicator$type == "performance"
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  # output$back_to_world_panel <- renderUI({
  #   
  #   tagList(
  #     
  #     conditionalPanel(
  #       condition = "input.city != '' || typeof input.map_marker_click !== 'undefined'",
  #       absolutePanel(
  #         class = "about_button",
  #         # class = "w3-container w3-animate-opacity", 
  #         # class = "panel panel-default",
  #         # fixed = TRUE, draggable = FALSE,
  #         top = 40, right = 730, width = 130, height = 40,
  #         actionButton(inputId = "back_to_world",
  #                      icon = icon("rotate-left"),
  #                      label = HTML("&nbsp;&nbsp;Reset map")
  #                      # selected = character(0)
  #         )
  #         
  #         
  #       )
  #     )
  #   )
  #   
  # })
  
  
  
  # download button ---------------------------------------------------------
  
  output$download_button <- renderUI({
    
    absolutePanel(
      
      
      class = "about_button",
      style = "background: #00AE42",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 20, right = 700, height = 40, width = 130,
      # tags$div(class = "title_left_panel", 
      #          # "COMPARE", 
      #          actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
      #                       class = "minimize"),
      #          actionButton("teste4", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px;",
      #                       class = "minimize")
      # ),
      actionButton(inputId = "download_button", 
                   icon = icon("download"),
                   label =  HTML("&nbsp;Download"))
      
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
  
  
  
  indicator <- reactiveValues(type = NULL, mode = NULL)
  
  
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
    indicator$mode <- input$indicator_bike
    
  })
  
  observeEvent(c(input$indicator_walk), {
    
    indicator$type <- "walk"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
    indicator$mode <- input$indicator_walk
    
    
  })
  
  observeEvent(c(input$indicator_transit), {
    
    indicator$type <- "transit"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
    
    indicator$mode <- input$indicator_transit
    
  })
  
  observeEvent(c(input$indicator_performance), {
    
    indicator$type <- "performance"
    # update the others
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
    shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
    
    indicator$mode <- input$indicator_performance
    
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
    indicator$mode <- input$indicator_city
    
  })
  
  
  
  
  
  # first, define the city input ----------------
  city <- reactiveValues(code = NULL, times = -1, osmid = NULL)
  
  observeEvent(c(input$city), {city$city_code <- input$city})
  observeEvent(c(input$map_marker_click), {
    
    city$city_code <- input$map_marker_click$id
    
    shinyWidgets::updatePickerInput(session = session, inputId = "city",
                                    selected = city$city_code)
    
    
  }
  )
  
  # reactive to select the type of indicator --------------------------------
  # indicator_mode <- reactive({
  #   
  #   req(indicator$type)
  #   
  #   if (indicator$type == "bike") {
  #     
  #     a <- input$indicator_bike
  #     
  #   } else if (indicator$type == "walk"){
  #     
  #     a <- input$indicator_walk
  #     
  #   } else if (indicator$type == "transit"){
  #     
  #     a <- input$indicator_transit
  #     
  #   } else if (indicator$type == "city"){
  #     
  #     a <- input$indicator_city
  #     
  #   } else if (indicator$type == "performance"){
  #     
  #     a <- input$indicator_performance
  #     
  #   }
  #   
  #   # print("indicator mode")
  #   # print(a)
  #   return(a)
  #   
  # })
  
  
  # if the user change cities on the top menu, will check if the same indicators is available
  # if not, it's gonna select other available
  observeEvent(c(input$city), {
    
    # print("a")
    # print(input$city)
    
    req(indicator$mode, input$city != "")
    
    ind <- subset(list_availability, hdc == input$city)
    ind_type <- ind$ind_type
    ind_mode <- ind$ind
    # print("ind_mode")
    # print(ind_mode)
    
    # check if it's available
    available <- indicator$mode %in% ind_mode
    # print("available")
    # print(available)
    
    if (!available) {
      
      # get first available
      ind_type1 <- ind_type[1]
      ind_mode1 <- ind_mode[1]
      
      # update values
      indicator$type <- "bike"
      indicator$mode <- "pnpb"
      
      if (indicator$type == "bike") {
        
        shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = indicator$mode)
        shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
        shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
        shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
        shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
        
        
      }
      
    }
    
    
    
    
    
  })
  
  
  setBookmarkExclude(c("bookmark"))
  
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
  
  # count how many times a city was selected --------------------------------
  
  
  observeEvent(c(city$city_code), {
    
    city$times <- city$times + 1
    
    # print("city$times")
    # print(city$times)
    
  })  
  
  
  
  # open files --------------------------------------------------------------
  
  source("src/filter_indicators.R", local = TRUE)  
  source("src/ranks.R", local = TRUE)  
  source("src/texts_rightpanel.R", local = TRUE)  
  source("src/map.R", local = TRUE)  
  source("src/popovers.R", local = TRUE)  
  source("src/popovers_cities_indicators.R", local = TRUE)  
  source("src/changes_jquery.R", local = TRUE)  
  source("src/about.R", local = TRUE)  
  source("src/back_to_world.R", local = TRUE)  
  source("src/compare.R", local = TRUE)  
  source("src/download.R", local = TRUE)  
  source("src/indicator_not_available.R", local = TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

