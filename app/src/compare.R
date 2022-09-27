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
    hc_add_theme(hc_theme_db())
  
  
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


  format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator_mode())$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator_mode())$indicator_transformation

  # print(format_indicator_unit_value)
  # print("format_indicator_unit_value")

  value_city$value <- if(format_indicator_unit_value == "percent") {
    round(value_city$value * 100)

  } else round(value_city$value)
  
  
  hchart(value_city, type = "line", hcaes(x = year, y = value)) %>%
    hc_add_theme(hc_theme_db())
  
  
})

observeEvent(c(input$maximize_comparison), {
  
  req(input$maximize_comparison >= 1)
  
  
  # get the admin level original
  al <- unique(data_ind3_spatial()$admin_level)
  
  # print("al")
  # print(al)
  
  # first, select only the ones that are available for the indicator in question
  hdc_available <-  subset(list_availability, grepl(pattern = indicator_mode(), x = ind))$hdc
  
  # get options to show in the comparison
  choices_comparison <- subset(list_osmid_name, admin_level == al)
  # filter hdc with the indicators available
  choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
  # remove the osmid that is already being shown
  # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
  # extract values
  choices_values <- choices_comparison$osmid
  choices_names <- choices_comparison$name
  names(choices_values) <- choices_names
  # 
  
  showModal(modalDialog(
    title = "COMPARE",
    size = c("l"),
    easyClose = TRUE,
    footer = NULL,
    absolutePanel(
      class = "about_modal",
      shinyWidgets::pickerInput(inputId = "city_compare1",
                                label = NULL,
                                choices = choices_values,
                                multiple = TRUE,
                                options = shinyWidgets::pickerOptions(size = 15,
                                                                      iconBase = "fa",
                                                                      tickIcon = "fa-check",
                                                                      title = "Search for a region...",
                                                                      liveSearch = TRUE)
                                ),
      absolutePanel(class = "spatial_level", 
                    fixed = TRUE, draggable = FALSE,
                    style = "background: #00AE42",
                    top = 20, right = 20, height = 'auto', width = 140,
                    dropdown(
                      tagList(
                        downloadButton("download_graph1", "Download graph", icon = NULL),
                        downloadButton("download_graph2", "Download graph2", icon = NULL)
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
                      width = "320px",
                      # tooltip = tooltipOptions(title = "Click to see inputs !"),
                      inputId = "download_dropdown_graphs"
                      
                    )
      ),
      highchartOutput("comparison_max", height = "280px")
      # highchartProxy("comparison")
      
    )
  )
  
  )
  
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