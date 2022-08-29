data_all <- reactive({
  
  data_all <- readRDS("../data/sample3/all_indicators.rds")
  
})



# get the values from other cities

ind_city <- reactive({
  
  
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  cols <- c('osmid','admin_level_ordered', 'name', colnames(data_all())[startsWith(colnames(data_all()), pattern)])
  a <- data_all()[cols]
  colnames(a) <- c('osmid','admin_level_ordered', 'name', 'valor')
  return(a)
  
  
})

ind_compare <- reactive({
  
  
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  cols <- c('osmid','admin_level_ordered', 'name', colnames(data_all())[startsWith(colnames(data_all()), pattern)])
  a <- data_all()[cols]
  colnames(a) <- c('osmid','admin_level_ordered', 'name', 'valor')
  return(a)
  
  
})




output$comparison <- renderHighchart({
  
  ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
  
  
  value_city <- subset(ind_city(), osmid == ui) %>%
    dplyr::mutate(year = 2019)
  
  # value_compare$year <- 2019
  # value_city$year <- 2019
  
  
  highchart() %>%
    hc_chart(inverted = TRUE) %>%
    # hc_xAxis(reversed = TRUE) %>%
    # hc_title(text = "teste",
    #          align = "left", x = 25) %>% 
    # add bar
    # hc_add_series(data = data,
    #               type = "errorbar",
    #               color = "#95A5A6",
    #               lineWidth = 5,
    #               # opacity = 0.5,
    #               name = "",
    #               tooltip = list(enabled = TRUE,
  #                              valueDecimals = 0),
  #               whiskerWidth = 3
  # ) %>%
  
  # add median
  hc_add_series(data = value_city, hcaes(x = year, y = valor),
                type = "scatter",
                color = "white",
                name = value_city$name,
                # name = ifelse(input$graph_type == "dumbell_renda", i18n()$t("Pobres Q1"), i18n()$t("Negros")),
                marker = list(radius = 5, symbol = "marker"),
                dataLabels = list(enabled = TRUE,
                                  align = "center",
                                  y = -20,
                                  format = "City: {point.y}",
                                  style = list(fontSize = 12,
                                               color = "white",
                                               textOutline = "0.3px black",
                                               fontWeight = "regular"))
                
                # tooltip = list(pointFormat = sprintf("%s: {point.y}", i18n()$t("Valor")),
                #                valueDecimals = 0)
  ) %>%
    hc_yAxis( lineWidth = 0,
              minorGridLineWidth = 0,
              labels = list(enabled = FALSE),
              minorTickLength = 0,
              tickLength = 0,
              visible = FALSE)
  
  
  
})

# create a reactiveValues to store the selected terms in the order of selection
reV_order <- reactiveValues(values = NULL)

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
  
  
  value_compare <- subset(ind_compare(), osmid == tail(ordered_colnames(), 1)) %>%
  # value_compare <- subset(ind_compare(), osmid %in% ordered_colnames()) %>%
    dplyr::mutate(year = 2019)
  
  # todo: removing cities by selecting in the dropdown is not working
  
  
  # print(input$city_compare)
  print(ordered_colnames())
  
  # add total
  highchartProxy("comparison") %>%
    # hcpxy_remove_series(id = "que") %>%
    hcpxy_add_series(data = value_compare, hcaes(x = year, y = valor),
                     id = "que",
                     type = "scatter",
                     color = "white",
                     name = value_compare$name,
                     size = 5,
                     marker = list(radius = 5, symbol = "circle"),
                     dataLabels = list(enabled = TRUE,
                                       align = "center",
                                       y = -20,
                                       # format = "City: {point.y}",
                                       format = "{point.y}",
                                       style = list(fontSize = 12,
                                                    color = "white",
                                                    textOutline = "0.3px black",
                                                    fontWeight = "bold"))
                     # tooltip = list(pointFormat = sprintf("%s: {point.y}", i18n()$t("Valor")),
                     #                valueDecimals = 0)
    )
  
  
})