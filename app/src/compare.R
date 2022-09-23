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
  
  print("AHHHH")
  print(level)
  print(pattern)
  
  # open data
  a <- readRDS(sprintf("../data/sample3/comp/indicators_compare_%s_%s.rds",
                       level, pattern))
  
  return(a)
  


})



output$comparison <- renderHighchart({
  
  ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
  
  
  value_city <- subset(ind_city(), osmid == ui)
  
  
  print("value_city$year")
  # print(head(value_city$year))
  print(unique(value_city$osmid))
  
  hchart(value_city, type = "line", hcaes(x = year, y = value)) %>%
    hc_add_theme(hc_theme_db())
  
  
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
  
  
  value_compare <- subset(ind_compare(), osmid == tail(ordered_colnames(), 1))
  
  # todo: removing cities by selecting in the dropdown is not working
  
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



# modal


output$comparison_max <- renderHighchart({
  
  ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
  
  
  value_city <- subset(ind_city(), osmid == ui)
  
  
  hchart(value_city, type = "line", hcaes(x = year, y = value)) %>%
    hc_add_theme(hc_theme_db())
  
  
  
})

observeEvent(c(input$maximize_comparison), {
  
  req(input$maximize_comparison >= 1)
  
  # get options to show in the comparison
  choices_comparison <- subset(list_osmid_name, admin_level_ordered == input$admin_level)
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
      highchartOutput("comparison_max", height = "200px")
      
    )
  )
  
  )
  
})