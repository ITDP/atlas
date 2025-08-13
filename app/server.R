# open boundaries
atlas_city_markers <- readRDS("../data/data_final/atlas_city_markers.rds")
list_indicators <- readRDS("../data/data_final/list_indicators.rds")
list_osmid_name <- readRDS("../data/data_final/list_osmid_name.rds")
list_availability <- readRDS("../data/data_final/list_availability.rds")
overlay_table <- readRDS("../data/data_final/overlay_table.rds")
list_availability_cities <- readRDS("../data/data_final/list_availability_cities.rds")
brazil_cities <- readRDS("../data/data_final/brazil_cities.rds")

# use the list of indicators to set the names
ind_city <- subset(list_indicators, indicator_type == "city")
ind_bike <- subset(list_indicators, indicator_type == "bike")
ind_walk <- subset(list_indicators, indicator_type == "walk")
ind_transit <- subset(list_indicators, indicator_type == "transit")

list_bike <- structure(c(ind_bike$indicator_code), 
.Names = c(ind_bike$indicator_name)
)

list_walk <- structure(c(ind_walk$indicator_code), 
.Names = c(ind_walk$indicator_name)
)

list_transit <- structure(c(ind_transit$indicator_code), 
.Names = c(ind_transit$indicator_name)
)

list_city <- structure(c(ind_city$indicator_code), 
.Names = c(ind_city$indicator_name))

function(input, output, session) {
  
  
  debug_msg <- function(...) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
    txt <- toString(list(...))
    # if (is_local) message(txt)
    # if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
    message(txt)
    shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
  }
  
  #Keep App alive
  # https://stackoverflow.com/questions/63739812/why-does-a-shiny-app-get-disconnected-when-not-used-for-a-while
  keep_alive <- shiny::reactiveTimer(intervalMs = 10000, 
                                     session = shiny::getDefaultReactiveDomain())
  
  shiny::observe(
    {keep_alive()
      cat(".")
    })
  
  
  # # password protection related
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
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
  # observeEvent(input$modal_beta_close, {
  
  delay(200, showModal(query_modal))
  
  # })
  
  observe({
    
    delay(200, shinyjs::runjs('$( ".mfb-component--br" ).remove();'))
    
  })
  
  observeEvent(input$year, {
    
    year$ok <- input$year
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
    
    # make list
    oi <- split(list_availability, list_availability$country)
    pera <- function(variables) {
      
      name <- unique(variables$name)
      code <- unique(variables$hdc)
      
      names(code) <- name
      
      return(code)
      
      
    }
    list_selection <- lapply(oi, pera)
    
    # print(length(list_selection))
    
    conditionalPanel(
      condition = "input.indicator != ''",
      tagList(
        
        
        
        # 1) CITY SELECTION -------------------------------------------------------
        # https://www.rapidtables.com/code/text/unicode-characters.html
        
        
        div(style = "display: inline-block;",
            shinyWidgets::pickerInput(inputId = "city",
                                      label = "IN",
                                      width = "330px",
                                      choices = list_selection,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            iconBase = "fa",
                                                                            tickIcon = "fa-check",
                                                                            title = "Select an urban area",
                                                                            liveSearch = TRUE,
                                                                            showContent = FALSE,
                                                                            sanitize = FALSE)
            )
        )
      )
    )
    
  })
  
  output$year <- renderUI({
    
    
    conditionalPanel(
      condition = "input.indicator != ''",
      shinyWidgets::pickerInput(inputId = "year",
                                label = NULL,
                                choices = c(2023, 2024),
                                selected = 2024,
                                width = "330px",
                                options = shinyWidgets::pickerOptions(
                                  size = 5
                                  
                                )
                                # selected = character(0)
      )
    )
    
  })
  
  output$world_view_selection <- renderUI({
    
    req(is.null(rank$admin_level))
    
    conditionalPanel(
      condition = "input.indicator != ''",
      shinyWidgets::pickerInput(inputId = "world_view1",
                                label = "World View",
                                width = "330px",
                                choices = c("Countries" = "country", 
                                            "UN Continents" = "UN Continents", "UN Regions" = "UN Regions", 
                                            "World Bank Regions" = "World Bank Regions", "World Bank Income" = "World Bank Income"),
                                selected = "country"
      )
    )
    
  })
  
  
  # update year according to the indicator
  year <- reactiveValues(ok = as.character(2024))
  
  observeEvent(c(indicator$mode), {
    
    
    print("chega aqui?")
    
    year_options <- subset(list_availability, ind == indicator$mode)$availability
    year_options <- unlist( strsplit(year_options, "[|]"))
    year_options <- unique(year_options)
    year_options <- sort(year_options)
    # remove year 2025 (from now)
    year_options <- year_options[year_options != 2025]
    
    # print("year_options")
    # print(year_options)
    
    updatePickerInput(
      session = session,
      inputId = "year",
      choices = year_options,
      selected = 2024)
    
    year$ok <- as.character(2024)
    
    
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
    compare_rv$choices <- sort(subset(atlas_country(), region_type == input$world_view1)$name)
    
    
    # print("quero comer")
    # print("al")
    
    
    
  })
  
  
  observeEvent(c(city$city_code, rank$admin_level), {
    
    
    req(input$admin_level, data_ind3_spatial(), indicator$mode, rank$admin_level, city$city_code != "")
    
    al <- unique(data_ind3_spatial()$admin_level)
    
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
      tags$div(
        # class = "title_left_panel",
        style = "display: flex; justify-content: space-between;",
        span(class = "title_left_panel", "COMPARE"), 
        
        # tags$i("Click on the map to update the chart", style = "font-size: 12px"), 
        div(style = "padding-top: 10px; padding-right: 10px;",
            actionButton("maximize_comparison", label = "", icon = icon("glyphicon glyphicon-resize-full", lib = "glyphicon"), style= "float: right; padding: 0",
                         class = "minimize"),
            actionButton("teste5", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px",
                         class = "minimize")
        )
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
                                                                                           title = "Regions...",
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
    go1 <- unique(data_ind()$admin_level_name)
    list_levels <- as.list(seq(1,go))
    names(list_levels) <- go1
    
    tagList(
      conditionalPanel(
        condition = "input.city != ''",
        # condition = "input.city != '' || typeof input.map_marker_click !== 'undefined'",
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
        # conditionalPanel(
        #   condition = "output.panelStatus",
        #   radioGroupButtons(inputId = "regions_grid", label = "", choices = c("Regions"), selected = "Regions", justified = TRUE),
        # ),
        # conditionalPanel(
        #   condition = "input.regions_grid == 'Regions'",
          shinyWidgets::pickerInput(inputId = "admin_level",
                                    choices = list_levels,
                                    label = NULL,
                                    selected = 1,
                                    width = 330
                                    
                                    # grid = TRUE,
                                    # dragRange = FALSE
                                    # selected = character(0)
          )
        # )
        
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
  
  output$indicator <- reactive({
    indicator$mode == ""
  })
  outputOptions(output, "city", suspendWhenHidden = FALSE)
  
  output$admin_level <- reactive({
    rank$admin_level > 1
  })
  outputOptions(output, "admin_level", suspendWhenHidden = FALSE)
  
  
  # download button ---------------------------------------------------------
  
  output$download_button <- renderUI({
    
    
    if (!is.null(indicator$mode)) {
      
      format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
      
    }
    
    
    if (!is.null(city$city_code)) {
      
      city_name <- unique(subset(list_availability, hdc == city$city_code)$name)
      
    }
    
    
    # disabled(
    dropdown(
      # download for the "world" level
      conditionalPanel("!output.city",
                       tagList(
                         div(class = "text_indicator", style = "letter-spacing: 0.02em", 
                             # "Download country data for the selected indicator"),
                             sprintf("Download measurements of %s for all countries", format_indicator_name)
                             # "ai"
                         ),
                         downloadButton("downloadData_countries_csv", "data table (.csv)", icon = NULL),
                         downloadButton("downloadData_countries_gpkg", "geospatial (.gpkg)", icon = NULL),
                         hr(style = "margin-bottom: 0"),
                         div(class = "text_indicator", style = "letter-spacing: 0.02em", 
                             # "Download country data for the selected indicator"),
                             sprintf("Download measurements of %s for all cities", format_indicator_name)
                             # "ai"
                         ),
                         downloadButton("downloadData_cities_csv", "data table (.csv)", icon = NULL),
                         downloadButton("downloadData_cities_gpkg", "geospatial (.gpkg)", icon = NULL),
                       )
      ),
      # download for the city level
      conditionalPanel("output.city",
                       tagList(
                         div(id = "download_city",
                             div(class = "text_indicator", style = "letter-spacing: 0.02em; font-size: 15px;", 
                                 # "Download indicator for this region"
                                 sprintf("Download measurements of %s for all analysis areas in %s", format_indicator_name, city_name)
                             ),
                             downloadButton("downloadData1_csv", "data table (.csv)", icon = NULL),
                             downloadButton("downloadData1_gpkg", "geospatial (.gpkg)", icon = NULL),
                             div(class = "text_indicator", style = "letter-spacing: 0.02em; font-size: 15px;", 
                                 # "Download overlays for this region/indicator"
                                 sprintf("Download geospatial overlays for %s in %s", format_indicator_name, city_name)
                             ),
                             downloadButton("download_overlay", "geospatial (.zip)", icon = NULL),
                             hr(style = "margin-bottom: 0")
                         ),
                         div(class = "text_indicator", style = "letter-spacing: 0.02em; font-size: 15px;", 
                             # "Download all indicators for this region"
                             sprintf("Download measurements of all indicators for all analysis areas in %s", city_name)
                         ),
                         downloadButton("downloadData2_csv", "data table (.csv)", icon = NULL),
                         downloadButton("downloadData2_gpkg", "geospatial (.gpkg)", icon = NULL),
                       )
      ),
      tags$hr(style = "border-top: 2px solid #bbb; margin: 20px 0;"),
      
      tags$p(style = "letter-spacing: 0.02em; font-size: 15px;",
        "Check the ",
        tags$a("data dictionary", href = "https://docs.google.com/spreadsheets/d/1tSE4U_yu6wKSkFnwbJjcXT2IlSSCcFeijoERVua_UMM/edit?usp=sharing", target = "_blank"),
        style = "font-size: 14px; margin-top: 2px; margin-bottom: 0px;"
      ),
      circle = FALSE, 
      # status = "danger",
      label = HTML("&nbsp;&nbsp;Download"),
      icon = icon("download"),
      right = TRUE,
      up = FALSE,
      # icon = icon("download"), 
      width = "350px",
      # tooltip = tooltipOptions(title = "Click to see inputs !"),
      inputId = "download_dropdown_maps",
      class = "disabled"
      
    )
    # )
    
    
    
  })
  
  indicator <- reactiveValues(type = NULL, mode = NULL)
  
  observeEvent(c(input$indicator), {
    
    indicator$mode <- input$indicator
    
    if (input$indicator %in% c(  "popdensity", "blockdensity", "journeygap")) {
      
      indicator$type <- "city"
      
    } else if (input$indicator %in% c("pnpb")) {
      
      indicator$type <- "bike"
      
    } else if (input$indicator %in% c("pns", "pncf", "pnnhighways")) {
      
      indicator$type <- "walk"
      
    } else if (input$indicator %in% c("pnft", "pnrt", "pnrtlrt", "pnrtmrt", "pnrtbrt", "pnst")) {
      
      indicator$type <- "transit"
      
    }
    
    
    
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
  
  # set city availability in dropdown depending on the indicator --------------------------------
  observeEvent(c(indicator$mode), {
    
    
    req(indicator$mode)
    
    runjs("$(function () {$('[data-toggle=\"tooltip\"]').tooltip()})")
    # filter the indicator in the list
    list_availability_cities1 <- subset(list_availability_cities, ind == indicator$mode)
    
    # make list
    oi <- split(list_availability, list_availability$country)
    pera <- function(variables) {
      
      name <- unique(variables$name)
      code <- unique(variables$hdc)
      
      names(code) <- name
      
      return(code)
      
      
    }
    list_selection <- lapply(oi, pera)
    names <- unlist(lapply(names(list_selection), function(n) names(list_selection[[n]])))
    
    
    # update list of cities accordindly
    updatePickerInput(
      session = session,
      inputId = "city",
      choices = list_selection,
      selected = city$city_code,
      choicesOpt = list(
        content = ifelse(list_availability_cities1$available, names,
                         sprintf('<span style="width: 200px; display: inline-block">%s</span><div class="tooltip1"><i style="float: right; color: red" class="fa-solid fa-triangle-exclamation"></i><span class="tooltiptext1">Indicator not avaiable<br> for this region
                                 </span></div>', names)),
        
        
        disabled = !(list_availability_cities1$available)
        
      )
    )
    
    
  }, ignoreInit = TRUE)
  
  # set indicator availability in dropdown depending on the city --------------------------------
  
  observeEvent(c(indicator$mode, city$city_code), {
    
    req(city$city_code != "")
    
    
    list_availability_cities1 <- subset(list_availability_cities, hdc == city$city_code)
    
    
    list_indicators <- list("City" = list_city, "Bike" = list_bike, "Walk" = list_walk, "Transit" = list_transit)
    names <- unlist(lapply(names(list_indicators), function(n) names(list_indicators[[n]])))
    
    updatePickerInput(
      session = session,
      inputId = "indicator",
      choices = list_indicators,
      selected = indicator$mode,
      choicesOpt = list(
        disabled = !(list_availability_cities1$available)
        
      )
    )
    
    
  })
  
  # count how many times a city was selected --------------------------------
  
  
  observeEvent(c(city$city_code), {
    
    city$times <- city$times + 1
    
    # print("city$times")
    # print(city$times)
    
  })  
  
  
  
  observeEvent(c(input$teste12), {
    
    # print("boraaaaa")
    
    subset(a, x == 1)
    
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
  # source("src/beta_checkpoint/beta_checkpoint.R", local = TRUE)  
  source("src/modal_brazilian_cities/modal_brazilian_cities.R", local = TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

