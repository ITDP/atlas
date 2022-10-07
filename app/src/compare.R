data_all <- reactive({
  
  data_all <- readRDS("../data/sample3/all_indicators.rds")
  
})



# get the values from other cities

ind_city <- reactive({
  
  req(city$city_code)
  
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  
  # open data
  a <- readRDS(sprintf("../data/sample3/ghsl_%s/indicators_compare/indicators_compare_%s_%s.rds",
                       city$city_code, city$city_code, pattern))
  
  return(a)
  
  
})

ind_compare <- reactive({
  
  req(city$city_code, data_ind3_spatial())
  
  level <- unique(data_ind3_spatial()$admin_level)
  
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  
  # print("AHHHH")
  # print(level)
  # print(pattern)
  
  # open data
  a <- readRDS(sprintf("../data/sample3/comp/indicators_compare_%s_%s.rds",
                       level, pattern))
  
  return(a)
  
  
  
})



output$comparison <- renderHighchart({
  
  ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
  
  
  value_city <- subset(ind_city(), osmid == ui)
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_transformation
  
  # print(format_indicator_unit_value)
  # print("format_indicator_unit_value")
  
  value_city$value <- if(format_indicator_unit_value == "percent") {
    round(value_city$value * 100) 
    
  } else round(value_city$value)
  
  
  hchart(value_city, type = "line", hcaes(x = year, y = value)) %>%
    hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 15)),
             labels = list(style = list(fontSize = 14))) %>%
    hc_xAxis(title = list(text = "Year", style = list(fontSize = 15)),
             labels = list(style = list(fontSize = 14))) %>%
    hc_add_theme(hc_theme_darkunica(
      chart = list(backgroundColor = "#1C1C1C",
                   style = list(fontFamily = "Franklin Gothic Book"))
      ,
      title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                textTransform = "none"))
      
    ))
  
  
})

# create a reactiveValues to store the selected terms in the order of selection
reV_order <- reactiveValues(values = NULL, values_max = NULL)

# use reactive to get and sort the selected terms in the order of selection
ordered_colnames <- reactive({
  if (length(reV_order$values) > length(input$city_compare)) {
    reV_order$values <- reV_order$values[reV_order$values %in% input$city_compare]
  }else {
    reV_order$values <- c(reV_order$values, input$city_compare[!input$city_compare %in% reV_order$values])
  }
  reV_order$values
})

observe({ ordered_colnames() }) # use an observe to update the reactive function above



observeEvent(c(input$city_compare), {
  
  
  value_compare <- subset(ind_compare(), osmid == tail(ordered_colnames(), 1))
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_transformation
  
  # print(format_indicator_unit_value)
  # print("format_indicator_unit_value")
  
  value_compare$value <- if(format_indicator_unit_value == "percent") {
    round(value_compare$value * 100) 
    
  } else round(value_compare$value)
  
  # print("ind_compare()")
  # print(value_compare)
  
  # print(input$city_compare)
  # print(ordered_colnames())
  
  # add total
  highchartProxy("comparison") %>%
    # hcpxy_remove_series(id = "que") %>%
    hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                     id = "que",
                     type = "line"
                     # color = "white",
                     # name = ind_compare()$name,
                     # size = 5,
                     # marker = list(radius = 5, symbol = "circle"),
                     # dataLabels = list(enabled = TRUE,
                     #                   align = "center",
                     #                   y = -20,
                     #                   # format = "City: {point.y}",
                     #                   format = "{point.y}",
                     #                   style = list(fontSize = 12,
                     #                                color = "white",
                     #                                textOutline = "0.3px black",
                     #                                fontWeight = "bold"))
    )
  
  
  
})



# modal


output$comparison_max <- renderHighchart({
  
  ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
  
  # print("ui")
  # print(ordered_colnames())
  
  value_city <- subset(ind_city(), osmid %in% c(ui, ordered_colnames()))
  
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicator_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_transformation
  
  
  value_city$value <- if(format_indicator_unit_value == "percent") {
    round(value_city$value * 100)
    
  } else round(value_city$value)
  
  
  hchart(value_city, type = "line", hcaes(x = year, y = value, group = name),
         name = unique(value_city$name),
         tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                        valueDecimals = 0)) %>%
    hc_legend(verticalAlign = "top") %>%
    hc_title(text = format_indicator_name,
             align = "left", x = 10
    ) %>%
    hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
             labels = list(style = list(fontSize = 15))) %>%
    hc_xAxis(title = list(text = "Year", style = list(fontSize = 16)),
             labels = list(style = list(fontSize = 15))) %>%
    hc_add_theme(hc_theme_darkunica(
      chart = list(backgroundColor = "#1C1C1C",
                   style = list(fontFamily = "Franklin Gothic Book"))
      ,
      title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                textTransform = "none"))
      
    )) %>%
    hc_exporting(
      enabled = TRUE, # always enabled
      filename = sprintf("compare_%s", indicator_mode()),
      buttons = list(contextButton = list( enabled = FALSE),
                     exportButton = list(text = "DOWNLOAD",
                                         menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'downloadCSV'),
                                         x = -20))
      )
      
      # highchart() %>%
      #   hc_xAxis(categories = value_city$year) %>%
      #   hc_add_series(data = value_city$value,
      #                 type = "line",
      #                 # color = "white",
      #                 name = value_city$name,
      #                 size = 5) %>%
      #   hc_add_theme(hc_theme_db())
      
      
      })
  
  observeEvent(c(input$maximize_comparison), {
    
    req(input$maximize_comparison >= 1)
    
    
    # get the admin level original
    al <- unique(data_ind3_spatial()$admin_level)
    
    # print("al")
    # print(al)
    
    # first, select only the ones that are available for the indicator in question
    hdc_available <-  subset(list_availability, grepl(pattern = indicator_mode(), x = ind))$hdc
    
    # fisr, filter the level
    choices_comparison <- subset(list_osmid_name, admin_level == al)
    # get current country
    country_current <- unique(data_ind3_spatial()$country)
    
    # filter hdc with the indicators available
    choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
    # get countries
    countries <- unique(choices_comparison$country)
    # get options to show in the comparison
    choices_comparison <- subset(choices_comparison, country == country_current)
    # remove the osmid that is already being shown
    # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
    # extract values
    choices_values <- choices_comparison$osmid
    choices_names <- choices_comparison$name
    names(choices_values) <- choices_names
    
    
    
    showModal(modalDialog(
      title = "COMPARE",
      size = c("l"),
      easyClose = TRUE,
      footer = NULL,
      absolutePanel(
        class = "about_modal",
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_country",
                                      label = NULL,
                                      choices = countries,
                                      selected = country_current,
                                      multiple = FALSE,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Search for a country...",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare1",
                                      label = NULL,
                                      choices = choices_values,
                                      multiple = TRUE,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            # iconBase = "fa",
                                                                            # tickIcon = "fa-check",
                                                                            title = "Add a region...",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...",
                                                                            selectedTextFormat = "static"
                                      )
            )),
        # absolutePanel(class = "spatial_level", 
        #               fixed = TRUE, draggable = FALSE,
        #               style = "background: #00AE42;",
        #               top = 20, right = 20, height = 'auto', width = 140,
        #               dropdown(
        #                 tagList(
        #                   downloadButton("download_graph1", "Download graph", icon = NULL),
        #                   downloadButton("download_graph2", "Download graph2", icon = NULL)
        #                 ),
        #                 hr(),
        #                 actionButton("downloadDic", "Download Data Dictionary", 
        #                              onclick = "location.href='https://www.ipea.gov.br/acessooportunidades/dados';"),
        #                 circle = FALSE, 
        #                 # status = "danger",
        #                 label = "DOWNLOAD",
        #                 right = TRUE,
        #                 up = FALSE,
        #                 # icon = icon("download"), 
        #                 width = "320px",
        #                 # tooltip = tooltipOptions(title = "Click to see inputs !"),
        #                 inputId = "download_dropdown_graphs"
        #                 
        #               )
        # ),
        highchartOutput("comparison_max", height = "280px")
        # highchartProxy("comparison")
        
      )
    )
    
    )
    
  })
  
  observeEvent(c(input$city_compare_country), {
    
    # get the admin level original
    al <- unique(data_ind3_spatial()$admin_level)
    
    # print("al")
    # print(al)
    
    # first, select only the ones that are available for the indicator in question
    hdc_available <-  subset(list_availability, grepl(pattern = indicator_mode(), x = ind))$hdc
    
    # fisr, filter the level
    choices_comparison <- subset(list_osmid_name, admin_level == al)
    
    
    # fisr, filter the level
    choices_comparison <- subset(list_osmid_name, admin_level == al)
    
    # get options to show in the comparison
    choices_comparison <- subset(choices_comparison, country == input$city_compare_country)
    # filter hdc with the indicators available
    choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
    # remove the osmid that is already being shown
    # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
    # extract values
    choices_values <- choices_comparison$osmid
    choices_names <- choices_comparison$name
    names(choices_values) <- choices_names
    
    
    
    updatePickerInput(
      session = session,
      inputId = "city_compare1",
      choices = choices_values)
    
    
  })
  
  # use reactive to get and sort the selected terms in the order of selection
  ordered_colnames1 <- reactive({
    if (length(reV_order$values_max) > length(input$city_compare1)) {
      reV_order$values_max <- reV_order$values_max[reV_order$values_max %in% input$city_compare1]
    }else {
      reV_order$values_max <- c(reV_order$values_max, input$city_compare1[!input$city_compare1 %in% reV_order$values_max])
    }
    reV_order$values_max
  })
  
  observe({ ordered_colnames1() })
  
  
  
  
  observeEvent(c(input$city_compare1), {
    
    value_compare <- subset(ind_compare(), osmid == tail(ordered_colnames1(), 1))
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_transformation
    
    # print(format_indicator_unit_value)
    # print("format_indicator_unit_value")
    
    value_compare$value <- if(format_indicator_unit_value == "percent") {
      round(value_compare$value * 100) 
      
    } else round(value_compare$value)
    
    # print("ind_compare()")
    # print(value_compare)
    
    # print(input$city_compare)
    # print(ordered_colnames())
    
    # add total
    highchartProxy("comparison_max") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                       id = "que",
                       type = "line",
                       # color = "white",
                       name = unique(value_compare$name),
                       size = 5,
                       tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                                      valueDecimals = 0)
                       # marker = list(radius = 5, symbol = "circle"),
                       # dataLabels = list(enabled = TRUE,
                       #                   align = "center",
                       #                   y = -20,
                       #                   # format = "City: {point.y}",
                       #                   format = "{point.y}",
                       #                   style = list(fontSize = 12,
                       #                                color = "white",
                       #                                textOutline = "0.3px black",
                       #                                fontWeight = "bold"))
      )
    
    
    
  })
  
  
  
  output$download_button_compare <- renderUI({
    
    tagList(
      
      absolutePanel(class = "spatial_level", 
                    fixed = TRUE, draggable = FALSE,
                    style = "background: #00AE42",
                    top = 20, right = 570, height = 'auto', width = 140,
                    dropdown(
                      tagList(
                        downloadButton("downloadData1", "Download graph", icon = NULL),
                        downloadButton("downloadData2", "Download graph2", icon = NULL)
                      ),
                      hr(),
                      actionButton("downloadDic", "Download Data Dictionary", 
                                   onclick = "location.href='https://www.ipea.gov.br/acessooportunidades/dados';"),
                      circle = FALSE, 
                      # status = "danger",
                      label = "DOWNLOAD",
                      right = TRUE,
                      up = FALSE,
                      # icon = icon("download"), 
                      width = "350px",
                      # tooltip = tooltipOptions(title = "Click to see inputs !"),
                      inputId = "download_dropdown_maps"
                      
                    )
      )
    )
    
  })  
  
  
  observeEvent(c(input$admin_level, ind_city()), {
    
    highchartProxy("comparison_max") %>%
      hcpxy_remove_series(all = TRUE)
    
    
  })