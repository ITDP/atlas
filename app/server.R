# open boundaries
atlas_city_markers <- readRDS("../data/data_july2023/atlas_city_markers.rds")
# atlas_country <- readRDS("../data/data_alpha/atlas_country_polygons.rds")
# # filter only countries that have indidcators
# atlas_country <- subset(atlas_country, !is.na(bike_pnpb_2022))
# country rank
# atlas_country_ranks <- readRDS("../data/data_alpha/ranks/rank_country.rds")
# list indicators
list_indicators <- readRDS("../data/data_july2023/list_indicators.rds")
list_osmid_name <- readRDS("../data/data_july2023/list_osmid_name.rds")
list_availability <- readRDS("../data/data_july2023/list_availability.rds")
overlay_table <- readRDS("../data/data_july2023/overlay_table.rds")
# list_block <- readRDS("../data/data_alpha/list_block_density.rds")

# define some credentials
credentials <- data.frame(
  user = c("taylor", "kaue"), # mandatory
  password = c("taylor_atlas", "kaue_atlas"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2023-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

function(input, output, session) {
  
  # password protection related
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # the modal at startup
  query_modal <- modalDialog1(
    # title = "Important message",
    icon("arrow-left"), HTML("&nbsp;&nbsp;"), "Start by selecting an indicator",
    easyClose = TRUE,
    # size = "m",
    footer = NULL,
    id1 = "modal_initial"
    # footer = tagList(
    #   actionButton("run", "Run query")
    # )
  )
  
  # Show the model on start up ...
  delay(200, showModal(query_modal))
  
  observe({
    
    delay(200, shinyjs::runjs('$( ".mfb-component--br" ).remove();'))
    
  })
  
  w <- Waiter$new(id = c("rank_final"),
                  html = tagList(
                    spin_loaders(id = 3, color = "#00AE42"
                                 # style = "right: 150px; top: 200px"
                    )),
                  # color = "rgba(233, 235, 240, .1)")
                  color = "#1C1C1C")
  
  # ui----------------------------------------------------------------------
  
  output$city_selection <- renderUI({
    
    # validate(
    #   need(input$indicator != "", '')
    # )
    
    
    # make list
    oi <- split(list_availability, list_availability$country)
    pera <- function(variables) {
      
      name <- unique(variables$name)
      code <- unique(variables$hdc)
      
      names(code) <- name
      
      return(code)
      
      
    }
    list_selection <- lapply(oi, pera)
    
    conditionalPanel(
      condition = "input.indicator != ''",
      tagList(
        
        
        
        # 1) CITY SELECTION -------------------------------------------------------
        # https://www.rapidtables.com/code/text/unicode-characters.html
        
        
        # h3(style = "color: #00AE42; display: inline-block; font-size: 28px; margin-right: 15px", strong("ATLAS")),
        div(style = "display: inline-block;",
            shinyWidgets::pickerInput(inputId = "city",
                                      label = "IN",
                                      width = "250px",
                                      choices = list_selection,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            iconBase = "fa",
                                                                            tickIcon = "fa-check",
                                                                            title = "Go to a region ...",
                                                                            liveSearch = TRUE,
                                                                            width = 'fit')
            )
        )
      )
    )
    
  })
  
  output$year <- renderUI({
    
    
    # validate(
    #   need(input$indicator, '')
    # )
    conditionalPanel(
      condition = "input.indicator != ''",
      shinyWidgets::pickerInput(inputId = "year",
                                label = NULL,
                                choices = 2022,
                                selected = 2022,
                                width = "250px",
                                options = shinyWidgets::pickerOptions(
                                  size = 5
                                )
                                # selected = character(0)
      )
    )
    
  })
  
  
  # update year according to the indicator
  year <- reactiveValues(ok = as.character(2022))
  
  observeEvent(c(indicator$mode), {
    
    year_options <- subset(list_availability, ind == indicator$mode)$availability
    year_options <- unlist( strsplit(year_options, "[|]"))
    year_options <- unique(year_options)
    
    # print("year_options")
    # print(year_options)
    
    updatePickerInput(
      session = session,
      inputId = "year",
      choices = year_options,
      selected = 2022)
    
    year$ok <- as.character(2022)
    
    
  })
  
  
  
  
  spatial_level_value <- reactiveValues(last = NULL)
  
  
  # onclick("comparison_button", runjs("$( '#lalala' ).toggle();"))
  
  
  # show the comparison panel when the user clicks on the compare panel -------------------------
  onclick("comparison_button", toggle("lalala", anim = TRUE, animType = "fade"))
  onclick("teste5", hide("lalala", anim = TRUE, animType = "fade"))
  observeEvent(c(city$city_code), { hide("lalala") })
  
  
  # hides the comparison button when the user is in the global view -----------------------------
  observeEvent(c(input$indicator), {
    
    req(input$indicator != "")
    # req(city$times == 0)
    # req(city$city_code != "", city$times == 0)
    
    shinyjs::show("compare_panel")
    
    
  }, once = FALSE)
  
  
  # create regions names for the comparison 
  compare_rv <- reactiveValues(test = NULL,
                               country = NULL,
                               countries = NULL,
                               hdc = NULL,
                               choices = NULL)
  
  
  # observeEvent(c(data_ind3_spatial(), ), {
  # comparison_values <- reactive({
  observeEvent(c(input$map_shape_click), {
    
    # input$comparison_button
    
    # req(input$comparison_button >= 1)
    # req(input$admin_level, data_ind3_spatial(), indicator$mode, rank$admin_level)
    
    req(city$city_code == "")
    
    
    
    compare_rv$country <- NULL
    compare_rv$countries <- NULL
    compare_rv$hdc <- NULL
    compare_rv$choices <- atlas_country()$name
    
    
    # print("quero comer")
    # print("al")
    
    
    
  })
  
  
  observeEvent(c(city$city_code, rank$admin_level), {
    
    
    req(input$admin_level, data_ind3_spatial(), indicator$mode, rank$admin_level, city$city_code != "")
    
    al <- unique(data_ind3_spatial()$admin_level)
    
    # print(al)
    
    # first, select only the ones that are available for the indicator in question
    hdc_available1 <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))
    hdc_available <- hdc_available1$hdc
    
    # fisr, filter the level
    choices_comparison <- subset(list_osmid_name, admin_level == al)
    # get current country
    country_current <- unique(data_ind3_spatial()$country)
    # hdc_current <- 
    # get current hdc
    # hdc_options <- subset(list_osmid_name, admin_level == 0)
    # print("hdc")
    # print(hdc_current)
    
    # filter hdc with the indicators available
    choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
    # get countries
    countries <- unique(choices_comparison$country)
    
    if (rank$admin_level != 1) {
      
      # get options to show in the comparison - final
      # get hdc from that country
      hdc_comparison <- subset(list_osmid_name, country == country_current & admin_level == 0 & hdc %in% hdc_available)
      choices_comparison <- subset(choices_comparison, hdc == city$city_code)
      
    } else {
      
      
      # get hdc from that country
      hdc_comparison <- subset(list_osmid_name, country == country_current & admin_level == 0 & hdc %in% hdc_available)
      choices_comparison <- subset(choices_comparison, country == country_current)
      # print("OOOOOHHHH")
      # print(choices_comparison)
      
    }
    
    # remove the osmid that is already being shown
    # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
    
    
    # extract values
    
    # for the hdc available
    hdc_comparison_values <- hdc_comparison$osmid
    hdc_comparison_names <- hdc_comparison$name
    names(hdc_comparison_values) <- hdc_comparison_names
    # for the final selection
    choices_comparison_values <- choices_comparison$osmid
    choices_comparison_names <- choices_comparison$name
    names(choices_comparison_values) <- choices_comparison_names
    
    compare_rv$country <- country_current
    compare_rv$countries <- countries
    compare_rv$hdc <- hdc_comparison_values
    compare_rv$choices <- choices_comparison_values
    
    
    
  })
  
  
  output$comparison_panel <- renderUI({
    
    # print("duuhhh")
    # print(compare_rv$test)
    # print(city$city_code)
    
    # req(compare_rv$hdc)
    req(compare_rv$choices)
    
    # print("quaqua")
    # print(compare_rv$countries)
    # print("compare_rv$choices")
    # print(compare_rv$choices)
    
    absolutePanel(
      id = "lalala",
      class = "spatial_level",
      style = "z-index: 9999999999; opacity: 0.97",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      bottom = 115, left = 330, height = 'auto', width = 500,
      tags$div(class = "title_left_panel", HTML("COMPARE<br>"), 
               # tags$i("Click on the map to update the chart", style = "font-size: 12px"), 
               actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
                            class = "minimize"),
               actionButton("teste5", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px",
                            class = "minimize")
      ),
      
      conditionalPanel("output.city", style = "display:inline-block",
                       div(style="display:inline-block",
                           shinyWidgets::pickerInput(inputId = "city_compare_country_initial",
                                                     label = NULL,
                                                     choices = compare_rv$countries,
                                                     selected = compare_rv$country,
                                                     multiple = FALSE,
                                                     width = "125px",
                                                     options = shinyWidgets::pickerOptions(size = 15,
                                                                                           title = "Country...",
                                                                                           liveSearch = TRUE,
                                                                                           liveSearchPlaceholder = "Search...")
                           )),
                       
                       conditionalPanel("output.admin_level", style = "display:inline-block",
                                        div(style="display:inline-block",
                                            shinyWidgets::pickerInput(inputId = "city_compare_hdc_initial",
                                                                      label = NULL,
                                                                      choices = compare_rv$hdc,
                                                                      selected = city$city_code,
                                                                      width = "150px",
                                                                      multiple = FALSE,
                                                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                                                            title = "Region...",
                                                                                                            liveSearch = TRUE,
                                                                                                            liveSearchPlaceholder = "Search...")
                                            )))),
      div(style="display:inline-block",
          shinyWidgets::pickerInput(inputId = "city_compare1_initial",
                                    label = NULL,
                                    choices = compare_rv$choices,
                                    width = "135px",
                                    multiple = TRUE,
                                    options = shinyWidgets::pickerOptions(size = 15,
                                                                          # iconBase = "fa",
                                                                          # tickIcon = "fa-check",
                                                                          title = "Add...",
                                                                          liveSearch = TRUE,
                                                                          liveSearchPlaceholder = "Search...",
                                                                          selectedTextFormat = "static"
                                    )
          )),
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
        # absolutePanel(
        #   # id = "controls",
        #   class = "spatial_level",
        # fixed = TRUE, draggable = FALSE,
        # bottom = 45, right = 180, height = 'auto', width = 220,
        # 'typeof undefined' identifies when is null
        tags$div(class = "title_left_panel", style = "padding: 10px 0", "AT THE LEVEL OF",
                 tags$button(
                   id = "tooltip_level",
                   class="btn btn-light btn-xs",
                   style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                   icon("circle-info")
                   
                 )
                 
        ),
        
        
        
        
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
          shinyWidgets::pickerInput(inputId = "admin_level",
                                    choices = seq(1, go),
                                    label = NULL,
                                    selected = 1,
                                    width = 250,
                                    
                                    # grid = TRUE,
                                    # dragRange = FALSE
                                    # selected = character(0)
          )
        )
        
      )
      
      # )
    )
    
    
    
  })
  
  observeEvent(c(indicator$mode), {
    
    req(city$city_code != "", indicator$type)
    
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
  
  
  observeEvent(input$teste_id_uh, {
    updateNavbarPage(session, "right_tabs", "tab_overview")
  })
  
  output$panelStatus <- reactive({
    indicator$type == "performance"
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  
  output$city <- reactive({
    city$city_code != ""
  })
  outputOptions(output, "city", suspendWhenHidden = FALSE)
  
  output$admin_level <- reactive({
    rank$admin_level > 1
  })
  outputOptions(output, "admin_level", suspendWhenHidden = FALSE)
  
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
  
  observeEvent(c(input$indicator), {
    
    indicator$mode <- input$indicator
    
    if (input$indicator %in% c(  "popdensity", "blockdensity", "pnnhighways")) {
      
      indicator$type <- "city"
      
    } else if (input$indicator %in% c("pnpb")) {
      
      indicator$type <- "bike"
      
    } else if (input$indicator %in% c("pns", "pncf")) {
      
      indicator$type <- "walk"
      
    } else if (input$indicator %in% c("pnft", "pnrtall", "pnrtlrt", "pnrtmrt", "pnrtbrt")) {
      
      indicator$type <- "transit"
      
    }
    
    
    
  })
  
  # observeEvent(c(input$indicator_bike), {
  #   
  #   
  #   indicator$type <- "bike"
  #   # update the others
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
  #   # print("performance :", input$indicator_performance)
  #   # print(input$city)
  #   # print(indicator$type)
  #   indicator$mode <- input$indicator_bike
  #   
  # })
  # 
  # observeEvent(c(input$indicator_walk), {
  #   
  #   indicator$type <- "walk"
  #   # update the others
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
  #   
  #   indicator$mode <- input$indicator_walk
  #   
  #   
  # })
  # 
  # observeEvent(c(input$indicator_transit), {
  #   
  #   indicator$type <- "transit"
  #   # update the others
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
  #   
  #   indicator$mode <- input$indicator_transit
  #   
  # })
  # 
  # observeEvent(c(input$indicator_performance), {
  #   
  #   indicator$type <- "performance"
  #   # update the others
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_city", selected = character(0))
  #   
  #   indicator$mode <- input$indicator_performance
  #   
  #   # print("performance :", input$indicator_performance)
  #   
  # })
  # 
  # observeEvent(c(input$indicator_city), {
  #   
  #   indicator$type <- "city"
  #   # update the others
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_bike", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_walk", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_transit", selected = character(0))
  #   shinyWidgets::updateRadioGroupButtons(inputId = "indicator_performance", selected = character(0))
  #   # print("ai!")
  #   indicator$mode <- input$indicator_city
  #   
  # })
  # 
  # 
  
  
  
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
  
  
  # setBookmarkExclude(c("shinyjs-delay"))
  # setBookmarkExclude(c("bookmark"))
  # 
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
  
  
  onBookmark(function(state) {
    state$values$city_code <- city$city_code
    state$values$admin_level <- rank$admin_level
    state$values$indicator <- indicator$mode
    # state$values$region_selected <- element$selected
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    city$city_code <- state$values$city_code
    # rank$admin_level <- state$values$admin_level
    indicator$mode <- state$values$indicator
    # element$selected <- state$values$region_selected
  })
  
  ExcludedIDs <- reactiveVal(value = NULL)
  
  observe({
    toExclude <- c("bookmarkBtn", "comparison_button", "map_center", "map_marker_mouseover", "map_zoom", 
                   "download_dropdown_maps", "back_to_world", "indicator_transit", "teste1", "bookmark",
                   "indicator_walk","indicator_bike",  "map_groups", "indicator_city", "map_bounds", "link_see_more", "map_marker_mouseout",
                   "about", "right_tabs", "waiter_shown", "waiter_hidden", "map_shape_mouseover", "map_shape_mouseout", "map_click", "map_marker_click", "teste2",
                   "regions_grid",
                   "map_shape_click", "admin_level",
                   "city_open", "city_code", "year", "indicator_open", "city", "indicator", "year_open"
    )
    
    delayExclude <- grep("delay", names(input), value = TRUE)
    if(length(delayExclude) > 0){
      toExclude <- c(toExclude, delayExclude)
    }
    
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
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
  source("src/indicator_not_available_countries.R", local = TRUE)  
  source("src/variables.R", local = TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

