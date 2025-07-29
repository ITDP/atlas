

# tooltip when the compare is disabled
observeEvent(c(indicator$mode, input$back_to_world), {
  
  req(city$city_code == "")
  
  disable("compare_panel")
  
  # runjs("$('#compare_panel').attr('title', 'This is the hover-over text');")
  
  delay(1, runjs('$("#compare_panel").attr({"title":"Select a country to enable comparison",  "data-toggle":"tooltip"})'))
  delay(2, runjs('$("#compare_panel").tooltip({"trigger": "hover", "animation": true, delay: {show: 1,hide: 10}, placement: "auto"})'))
  delay(5, runjs('$("#compare_panel").tooltip("enable")'))
  
  # disable tool
  runjs('$("#tooltip_compare").tooltip("disable");')
  
  # tooltip <- ('<div class="tooltip1"><i style="float: right; color: red" class="fa-solid fa-triangle-exclamation"></i><span class="tooltiptext1">Indicator not avaiable<br> for this region</div>')
  
})

# disable compare button when no country is selected
observeEvent(c(input$map_shape_click, input$map_marker_click), {

  enable("compare_panel")
  runjs('$("#compare_panel").tooltip("disable");')

})


# get the values from other cities
ind_city <- reactive({
  # print("foi")
  
  req(city$city_code != "", input$comparison_button >= 1)
  
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  message("Compare #1")
  # open data
  a <- readRDS(sprintf("../data/data_final/ghsl_%s/indicators_compare/indicators_compare_%s_%s.rds",
                       city$city_code, city$city_code, pattern))
  
  
  return(a)
  
  
  
  # }
  
  
})


ind_compare <- reactiveValues(ind_compare = NULL,
                              ind_compare_initial = NULL)

observeEvent(c(input$city_compare_level_analysis), {
  
  
  req(city$city_code != "", data_ind3_spatial(), input$city_compare_level_analysis != "", input$maximize_comparison >= 1)
  message("Compare #2")
  
  
  # level <- unique(subset(data_ind3_spatial(), admin_level == input$city_compare_level_analysis)$admin_level)
  # print("level")
  # print(input$city_compare_level_analysis)
  # print(data_ind3_spatial())
  level <- input$city_compare_level_analysis
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  
  # open data
  ui <- sprintf("../data/data_final/comp/indicators_compare_%s_%s.rds",
                level, pattern)
  
  a <- readRDS(ui)
  
  ind_compare$ind_compare <- a  
  
})  

observeEvent(c(input$city_compare1_initial), {
  
  req(city$city_code != "", data_ind3_spatial())
  message("Compare #2 - initial")
  
  
  level <- unique(data_ind3_spatial()$admin_level)
  # pattern <- sprintf("%s_%s", "bike", "pnab")
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  
  # open data
  ui <- sprintf("../data/data_final/comp/indicators_compare_%s_%s.rds",
                level, pattern)
  
  print("ind_compare")
  print(ui)
  a <- readRDS(ui)
  
  ind_compare$ind_compare_initial <- a  
  
  
  
})





ind_country <- reactive({
  
  req(city$city_code == "")
  
  
})

output$comparison_chart <- renderHighchart({
  
  # waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
  #             color = "rgba(233, 235, 240, .2)")
  
  # req(ind_city())
  
  
  
  # ind_city()
  input$map_shape_click
  input$map_marker_click
  rank$admin_level
  city$city_code
  indicator$mode
  
  isolate({
    
    
    # print("Agoraaaaaaaaa")
    # print(city$city_code)
    
    if (city$city_code == "") {
      
      
      # print("ui compare")
      # print(atlas_country())
      
      ui <- input$map_shape_click$id
      value_city <- subset(atlas_country(), region_type == input$world_view1 & name == ui)
      value_city <- st_sf(value_city)
      value_city <- st_set_geometry(value_city, NULL)
      value_city <- tidyr::pivot_longer(value_city,
                                        cols = 4:last_col(),
                                        names_sep = "_",
                                        names_to = c("ind_type", "ind", "year"),
                                        values_to = "value")
      
      # make sure we are filtering the right indicator
      value_city <- subset(value_city, ind == indicator$mode)
      value_city <- value_city[order(value_city$year),,drop=FALSE]
      
      
      # pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok)
      # filter
      # print("bububububu")
      # print(indicator$mode)
      # print(year$ok)
      
      if (indicator$mode %in% c("not")) {
        
        ano <- year$ok
        value_city <- subset(value_city, ind == indicator$mode & year == ano)
        
        
      }
      
      
      format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
      format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
      format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
      
      # print(format_indicator_unit_value)
      # print("format_indicator_unit_value")
      
      # value_city$value <- if(format_indicator_unit_value == "percent") {
      #   round(value_city$value * 100) 
      #   
      # } else round(value_city$value)
      
      if (indicator$mode != "popdensity") {
        
        value_city$value <- format_indicator_values(value_city$value, transformation = indicator_info$transformation)
        
        
      }
      
      if (indicator$mode %in% c("not")) {
        
        hchart(value_city, type = "column", hcaes(x = name, y = value, group = name),
               name = unique(value_city$name),
               tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                              valueDecimals = 0)) %>%
          hc_plotOptions(column = list(
            pointWidth = 30,
            grouping = FALSE
          )
          ) %>%
          hc_legend(verticalAlign = "top") %>%
          hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 15)),
                   labels = list(style = list(fontSize = 14)),
                   maxPadding = 0.001) %>%
          hc_xAxis(title = list(text = "", style = list(fontSize = 15)),
                   labels = list(enabled = FALSE, style = list(fontSize = 14))) %>%
          hc_add_theme(hc_theme_darkunica(
            chart = list(backgroundColor = "#1C1C1C",
                         style = list(fontFamily = "Franklin Gothic Book"))
            ,
            title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                      textTransform = "none"))
          ))
        
      } else {
        
        
        hchart(value_city, type = "line", hcaes(x = year, y = value, group = name),
               name = unique(value_city$name),
               tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                              valueDecimals = 0)) %>%
          hc_plotOptions(column = list(pointWidth = 10)) %>%
          hc_legend(verticalAlign = "top") %>%
          # hc_title(text = format_indicator_name,
          # align = "left", x = 10
          # ) %>%
          hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                   labels = list(style = list(fontSize = 15)),
                   maxPadding = 0.001) %>%
          hc_xAxis(title = list(text = "Year", style = list(fontSize = 16)),
                   labels = list(style = list(fontSize = 15))) %>%
          hc_add_theme(hc_theme_darkunica(
            chart = list(backgroundColor = "#1C1C1C",
                         style = list(fontFamily = "Franklin Gothic Book"))
            ,
            title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                      textTransform = "none"))
            
          ))
        
        
        
      }
      
      
      
    } else {
      
      # print("goooooooooo")
      # print(input$map_shape_click)
      # print(rank$admin_level)
      # print(is.null(input$map_shape_click))
      
      # print("TOINNNN")
      # print(rank$admin_level)
      
      ui <- if(isTRUE(input$map_shape_click$group == "Countries") | isTRUE(rank$admin_level == 1)) city$city_code else input$map_shape_click$id
      
      
      value_city <- subset(ind_city(), osmid == ui)
      value_city <- value_city[order(value_city$year),,drop=FALSE]
      
      format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
      format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
      format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
      
      # print(format_indicator_unit_value)
      # print("format_indicator_unit_value")
      
      # value_city$value <- if(format_indicator_unit_value == "percent") {
      #   round(value_city$value * 100) 
      #   
      # } else round(value_city$value)
      
      if (indicator$mode != "popdensity") {
        value_city$value <- format_indicator_values(value_city$value, transformation = indicator_info$transformation)
      }
      
      if (indicator$mode %in% c("not")) {
        
        hchart(value_city, type = "column", hcaes(x = name, y = value, group = name),
               name = unique(value_city$name),
               tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                              valueDecimals = 0)) %>%
          hc_plotOptions(column = list(
            pointWidth = 30,
            grouping = FALSE
          )
          ) %>%
          hc_legend(verticalAlign = "top") %>%
          hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 15)),
                   labels = list(style = list(fontSize = 14)),
                   maxPadding = 0.001) %>%
          hc_xAxis(title = list(text = "", style = list(fontSize = 15)),
                   labels = list(enabled = FALSE, style = list(fontSize = 14))) %>%
          hc_add_theme(hc_theme_darkunica(
            chart = list(backgroundColor = "#1C1C1C",
                         style = list(fontFamily = "Franklin Gothic Book"))
            ,
            title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                      textTransform = "none"))
          ))
        
      } else {
        
        
        # print("value_city")
        # print(value_city)
        
        hchart(value_city, type = "line", hcaes(x = year, y = value, group = name),
               name = unique(value_city$name),
               tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                              valueDecimals = 0)) %>%
          hc_plotOptions(column = list(pointWidth = 10)) %>%
          hc_legend(verticalAlign = "top") %>%
          # hc_title(text = format_indicator_name,
          # align = "left", x = 10
          # ) %>%
          hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                   labels = list(style = list(fontSize = 15)),
                   maxPadding = 0.001) %>%
          hc_xAxis(title = list(text = "Year", style = list(fontSize = 16)),
                   labels = list(style = list(fontSize = 15))) %>%
          hc_add_theme(hc_theme_darkunica(
            chart = list(backgroundColor = "#1C1C1C",
                         style = list(fontFamily = "Franklin Gothic Book"))
            ,
            title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                      textTransform = "none"))
            
          ))
        
        
        
      }
      
      
    }
    
    
  })
  
  
  # }
  
  # waiter_hide()
  
})



# if I change the spatial level, I want to clean the grapH
# i WILL NOT ACTIVE THIS SO FAR - STILL NEEDS TESTING
# I WANT TO ADD A PLACEHOLDER SAYING THE USER CAN CLICK ON THE MAP
observeEvent(c(input$admin_level), {
  
  req(input$admin_level != 1)
  
  hide("lalala")
  
  # print("kakakak")
  
  # delay(500,   highchartProxy("comparison_chart") %>%
  #         hcpxy_remove_series(all = TRUE) %>%
  #         hcpxy_add_series(plotBackgroundImage = "https://media1.giphy.com/media/lJ88OkVp8NdOP74ucu/giphy.gif")
  # )
  
  # placeholder
  
  
  
  
  
})

# create a reactiveValues to store the selected terms in the order of selection
reV_order <- reactiveValues(values = NULL, values_max = NULL)

# if the city is selected, it will take the value
observeEvent(c(city$city_code, rank$admin_level), {
  
  req(ind_city())
  
  # print("FONFON")
  # print(rank$admin_level)
  # print(input$map_shape_click$id)
  
  reV_order$values <-  if(isTRUE(is.null(input$map_shape_click)) | isTRUE(rank$admin_level == 1)) city$city_code else input$map_shape_click$id
  
})

# use reactive to get and sort the selected terms in the order of selection
ordered_colnames <- reactive({
  
  # req(input$city_compare1_initial)
  # print("que1")
  # print(input$city_compare1_initial)
  
  if (length(reV_order$values) > length(input$city_compare1_initial)) {
    reV_order$values <- reV_order$values[reV_order$values %in% input$city_compare1_initial]
    # print("que1")
    
  }else {
    reV_order$values <- c(reV_order$values, input$city_compare1_initial[!input$city_compare1_initial %in% reV_order$values])
    # print("aqui")
    # print(reV_order$values)
  }
  reV_order$values
})

observe({ ordered_colnames() }) # use an observe to update the reactive function above


# update the graph with the new selection
observeEvent(c(input$city_compare1_initial), {
  
  # it will only run if the selection is diferente from the previous selection
  if (city$city_code == "") {
    
    
    
    value_compare <- subset(atlas_country(), region_type == input$world_view1 & name == tail(ordered_colnames(), 1))
    value_compare <- st_sf(value_compare)
    value_compare <- st_set_geometry(value_compare, NULL)
    value_compare <- tidyr::pivot_longer(value_compare,
                                         cols = 4:last_col(),
                                         names_sep = "_",
                                         names_to = c("ind_type", "ind", "year"),
                                         values_to = "value")
    value_compare <- subset(value_compare, ind == indicator$mode)
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    
    # 
    # value_compare$value <- if(format_indicator_unit_value == "percent") {
    #   round(value_compare$value * 100) 
    #   
    # } else round(value_compare$value)
    
    if (indicator$mode != "popdensity") {
      value_compare$value <- format_indicator_values(value_compare$value, transformation = indicator_info$transformation)
    }
    
    
    if (indicator$mode %in% c("not")) {
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      # add total
      highchartProxy("comparison_chart") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = name, y = value),
                         id = tail(ordered_colnames(), 1),
                         type = "column",
                         # color = "white",
                         name = unique(value_compare$name),
                         # size = 5,
                         tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                                        valueDecimals = 0),
                         pointWidth = 30
                         # pointPadding = "on"
                         
        )
      
    } else {
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      # add total
      highchartProxy("comparison_chart") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                         id = tail(ordered_colnames(), 1),
                         type = "line",
                         # color = "white",
                         name = unique(value_compare$name),
                         size = 5,
                         tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                                        valueDecimals = 0)
        )
      
    }
    
    
  } else {
    
    value_compare <- subset(ind_compare$ind_compare_initial, osmid == tail(ordered_colnames(), 1))
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    # print(format_indicator_unit_value)
    # print("format_indicator_unit_value")
    
    # value_compare$value <- if(format_indicator_unit_value == "percent") {
    #   round(value_compare$value * 100) 
    #   
    # } else round(value_compare$value)
    
    if (indicator$mode != "popdensity") {
      value_compare$value <- format_indicator_values(value_compare$value, transformation = indicator_info$transformation)
    }
    
    if (indicator$mode %in% c("not")) {
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      # add total
      highchartProxy("comparison_chart") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = name, y = value),
                         id = tail(ordered_colnames(), 1),
                         type = "column",
                         # color = "white",
                         name = unique(value_compare$name),
                         # size = 5,
                         tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                                        valueDecimals = 0),
                         pointWidth = 30
                         # pointPadding = "on"
                         
        )
      
    } else {
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      # add total
      highchartProxy("comparison_chart") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                         id = tail(ordered_colnames(), 1),
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
      
    }
    
    
    
  }
  
  
  
})



previous_selection <- reactiveVal(character(0))

# to remove series from the comparision
observeEvent(c(input$city_compare1_initial), {
  
  # print(ordered_colnames1())
  
  old_vals <- previous_selection()
  new_vals <- if (is.null(input$city_compare1_initial)) {
    character(0)
  } else {
    input$city_compare1_initial
  }
  
  
  # Identify removed items
  removed <- setdiff(old_vals, new_vals)
  
  if (length(removed) > 0) {
    # Run code only when something was unselected
    message("Removed: ", paste(removed, collapse = ", "))
    # put your logic here
  }
  
  previous_selection(new_vals)
  
  if (length(removed) > 0) {
    
    print(removed)
    
    highchartProxy("comparison_chart") %>%
      hcpxy_remove_series(id = removed)
    
  }
  
  
  
}, ignoreNULL = FALSE)

# modal


output$comparison_max <- renderHighchart({
  
  
  
  if (city$city_code == "") {
    
    
    ui <- input$map_shape_click$id
    value_city <- subset(atlas_country(), region_type == input$world_view1 & name == ui)
    value_city <- st_sf(value_city)
    value_city <- st_set_geometry(value_city, NULL)
    value_city <- tidyr::pivot_longer(value_city,
                                      cols = 4:last_col(),
                                      names_sep = "_",
                                      names_to = c("ind_type", "ind", "year"),
                                      values_to = "value")
    
    # make sure we are filtering the right indicator
    value_city <- subset(value_city, ind == indicator$mode)
    value_city <- value_city[order(value_city$year),,drop=FALSE]
    
    # print("value_city")
    # print(value_city)
    
    # pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok)
    # filter
    # print("bububububu")
    # print(indicator$mode)
    # print(year$ok)
    
    if (indicator$mode %in% c("not")) {
      
      ano <- year$ok
      value_city <- subset(value_city, ind == indicator$mode & year == ano)
      
      
    }
    
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    # print(format_indicator_unit_value)
    # print("format_indicator_unit_value")
    
    # value_city$value <- if(format_indicator_unit_value == "percent") {
    #   round(value_city$value * 100) 
    #   
    # } else round(value_city$value)
    
    if (indicator$mode != "popdensity") {
      
      value_city$value <- format_indicator_values(value_city$value, transformation = indicator_info$transformation)
      
      
    }
    
    if (indicator$mode %in% c("not")) {
      
      hchart(value_city, type = "column", hcaes(x = name, y = value, group = name),
             name = unique(value_city$name),
             tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                            valueDecimals = 0)) %>%
        hc_chart(
          # marginBottom = 100,
          events = list(
            click = "function() alert();"
            # load = "function () { if(this.options.chart.forExport) { this.renderer.image('https://go.itdp.org/download/attachments/46271199/ITDP_BugPMS355C.png', 80, 40, 143, 57) .add()) } }"
          )
        ) %>%
        hc_plotOptions(series = list(
          pointWidth = 30,
          # dataLabels = list(enabled = TRUE,
          #                   align = "center",
          #                   y = -20,
          #                   # format = "City: {point.y}",
          #                   format = "{series.name}",
          #                   style = list(fontSize = 12,
          #                                color = "white",
          #                                textOutline = "0.3px black",
          #                                fontWeight = "bold")),
          grouping = FALSE,
          events = list(
            click = "function() alert();")
        )
        ) %>%
        hc_legend(verticalAlign = "bottom") %>%
        # hc_subtitle(text = input$year, align = "center", y = 220) %>%
        hc_title(text = sprintf("%s - %s", format_indicator_name, input$year)
                 # align = "left", x = 10, y = 250
        ) %>%
        hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                 labels = list(style = list(fontSize = 15)),
                 maxPadding = 0.001) %>%
        hc_xAxis(title = list(text = "", style = list(fontSize = 16)),
                 labels = list(enabled = FALSE)
        ) %>%
        hc_add_theme(hc_theme_darkunica(
          chart = list(backgroundColor = "#1C1C1C",
                       style = list(fontFamily = "Franklin Gothic Book"))
          ,
          title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                    textTransform = "none"))
          
        )) %>%
        hc_exporting(
          enabled = TRUE, # always enabled
          filename = sprintf("compare_%s", indicator$mode),
          buttons = list(contextButton = list( enabled = FALSE),
                         exportButton = list(text = "DOWNLOAD",
                                             menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'downloadCSV'),
                                             x = -20)),
          plotOptions = list(
            events = list(
              load = 'function () {
          this.renderer
                .image("https://images.adsttc.com/adbr001cdn.archdaily.net/wp-content/uploads/2012/06/1339769975_270468_186978104690637_2440712_n.jpg", 80, 40, 143, 57)
                .add();
          }
          '
            ))
          
        )
      
    } else {
      
      
      hchart(value_city, type = "line", hcaes(x = year, y = value, group = name),
             name = unique(value_city$name),
             tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                            valueDecimals = 0)) %>%
        hc_plotOptions(column = list(pointWidth = 10)) %>%
        hc_legend(verticalAlign = "bottom") %>%
        hc_title(text = format_indicator_name
                 # align = "left", x = 10, y = 340,
        ) %>%
        hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                 labels = list(style = list(fontSize = 15)),
                 maxPadding = 0.001) %>%
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
          filename = sprintf("compare_%s", indicator$mode),
          buttons = list(contextButton = list( enabled = FALSE),
                         exportButton = list(text = "DOWNLOAD",
                                             menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'downloadCSV'),
                                             x = -20))
        )
      
      
      
    }
    
    
    
  } else {
    
    ui <- if(is.null(input$map_shape_click)) city$city_code else input$map_shape_click$id
    ui <- if(rank$admin_level == 1) city$city_code else ui
    
    # print(ordered_colnames())
    value_city <- subset(ind_city(), osmid %in% c(ui, ordered_colnames()))
    value_city <- value_city[order(value_city$year),,drop=FALSE]
    
    # make sure we are filtering the right indicator
    # value_city <- subset(value_city, ind == indicator$mode)
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    
    # value_city$value <- if(format_indicator_unit_value == "percent") {
    #   round(value_city$value * 100)
    #   
    # } else round(value_city$value)
    if (indicator$mode != "popdensity") {
      value_city$value <- format_indicator_values(value_city$value, transformation = indicator_info$transformation)
    }
    
    if (indicator$mode %in% c("not")) {
      
      hchart(value_city, type = "column", hcaes(x = name, y = value, group = name),
             name = unique(value_city$name),
             tooltip = list(pointFormat = sprintf("{point.y} %s", format_indicator_unit),
                            valueDecimals = 0)) %>%
        hc_chart(
          # marginBottom = 100,
          events = list(
            click = "function() alert();"
            # load = "function () { if(this.options.chart.forExport) { this.renderer.image('https://go.itdp.org/download/attachments/46271199/ITDP_BugPMS355C.png', 80, 40, 143, 57) .add()) } }"
          )
        ) %>%
        hc_plotOptions(series = list(
          pointWidth = 30,
          # dataLabels = list(enabled = TRUE,
          #                   align = "center",
          #                   y = -20,
          #                   # format = "City: {point.y}",
          #                   format = "{series.name}",
          #                   style = list(fontSize = 12,
          #                                color = "white",
          #                                textOutline = "0.3px black",
          #                                fontWeight = "bold")),
          grouping = FALSE,
          events = list(
            click = "function() alert();")
        )
        ) %>%
        hc_legend(verticalAlign = "bottom") %>%
        # hc_subtitle(text = input$year, align = "center", y = 220) %>%
        hc_title(text = sprintf("%s - %s", format_indicator_name, input$year)
                 # align = "left", x = 10, y = 250
        ) %>%
        hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                 labels = list(style = list(fontSize = 15)),
                 maxPadding = 0.001) %>%
        hc_xAxis(title = list(text = "", style = list(fontSize = 16)),
                 labels = list(enabled = FALSE)
        ) %>%
        hc_add_theme(hc_theme_darkunica(
          chart = list(backgroundColor = "#1C1C1C",
                       style = list(fontFamily = "Franklin Gothic Book"))
          ,
          title = list(style = list(fontFamily = "Franklin Gothic Demi",
                                    textTransform = "none"))
          
        )) %>%
        hc_exporting(
          enabled = TRUE, # always enabled
          filename = sprintf("compare_%s", indicator$mode),
          buttons = list(contextButton = list( enabled = FALSE),
                         exportButton = list(text = "DOWNLOAD",
                                             menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'downloadCSV'),
                                             x = -20)),
          plotOptions = list(
            events = list(
              load = 'function () {
          this.renderer
                .image("https://images.adsttc.com/adbr001cdn.archdaily.net/wp-content/uploads/2012/06/1339769975_270468_186978104690637_2440712_n.jpg", 80, 40, 143, 57)
                .add();
          }
          '
            ))
          
        )
      
    } else {
      
      
      hchart(value_city, type = "line", hcaes(x = year, y = value, group = name),
             name = unique(value_city$name),
             tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                            valueDecimals = 0)) %>%
        hc_plotOptions(column = list(pointWidth = 10)) %>%
        hc_legend(verticalAlign = "bottom") %>%
        hc_title(text = format_indicator_name
                 # align = "left", x = 10, y = 340,
        ) %>%
        hc_yAxis(title = list(text = format_indicator_unit, style = list(fontSize = 16)),
                 labels = list(style = list(fontSize = 15)),
                 maxPadding = 0.001) %>%
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
          filename = sprintf("compare_%s", indicator$mode),
          buttons = list(contextButton = list( enabled = FALSE),
                         exportButton = list(text = "DOWNLOAD",
                                             menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'downloadCSV'),
                                             x = -20))
        )
      
    }
    
  }
  
  
  
})

observeEvent(c(input$maximize_comparison), {
  
  
  
  req(input$maximize_comparison >= 1)
  
  if (city$city_code == "") {
    
    
    # 1) select only the ones that are available for the indicator in question
    # hdc_available1 <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))
    # countries_available <- unique(hdc_available1$country)
    countries_available <- sort(subset(atlas_country(), region_type == input$world_view1)$name)
    
    
    showModal(modalDialog1(
      title = div(style = "display: flex; justify-content: space-between;", "COMPARE", modalButton(icon("close"))),
      id1 = "compare_max_modal",
      size = c("l"),
      easyClose = TRUE,
      footer = NULL,
      absolutePanel(
        class = "about_modal",
        # id = "compare_max_modal",
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_analysis_area",
                                      label = NULL,
                                      choices = countries_available,
                                      selected = NULL,
                                      width = "200px",
                                      multiple = TRUE,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Regions......",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        # div(style="display:inline-block",
        #     actionButton("reset_graph", "Clear Selection")),
        highchartOutput("comparison_max", height = "100%")
      )))
    
    
    
  } else {
    
    
    # get the levels
    # go <- length(unique(data_ind()$admin_level))
    # go1 <- unique(data_ind()$admin_level_name)
    # list_levels <- as.list(seq(1,go))
    # names(list_levels) <- go1
    
    country_current <- unique(data_ind3_spatial()$country)
    
    # identify the current admin level
    al <- unique(data_ind3_spatial()$admin_level)
    
    # 1) select only the ones that are available for the indicator in question
    hdc_available1 <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))
    countries_available <- unique(hdc_available1$country)
    
    
    # # 2) filter the hdc available for the selected country
    urban_area_available <- subset(list_osmid_name, country == country_current & admin_level == 0)
    urban_area_available_id <- unique(urban_area_available$hdc)
    urban_area_available_name <- unique(urban_area_available$name)
    names(urban_area_available_id) <- urban_area_available_name
    
    
    # 3) filter the level of analysis availables
    level_analysis_available <- subset(list_osmid_name, hdc == city$city_code)
    level_analysis_available <- level_analysis_available[!duplicated(level_analysis_available[,c('admin_level_name','admin_level')]),]
    level_analysis_available <- level_analysis_available[order(level_analysis_available$admin_level_ordered),]
    level_analysis_available_id <- level_analysis_available$admin_level
    level_analysis_available_name <- level_analysis_available$admin_level_name 
    names(level_analysis_available_id) <- level_analysis_available_name
    
    # 4) filter the level of analysis for that urban area
    analysis_area_available <- subset(list_osmid_name, admin_level == al & hdc == city$city_code)
    analysis_area_available_id <- analysis_area_available$osmid
    analysis_area_available_name <- analysis_area_available$name
    names(analysis_area_available_id) <- analysis_area_available_name
    
    
    # # fisr, filter the level
    # al <- unique(data_ind3_spatial()$admin_level)
    # choices_comparison <- subset(list_osmid_name, admin_level == al)
    # # get current country
    # country_current <- unique(data_ind3_spatial()$country)
    # 
    # 
    # # filter hdc with the indicators available
    # choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
    # # get countries
    # countries <- unique(choices_comparison$country)
    # # get hdc from that country
    # hdc_comparison <- subset(list_osmid_name, country == country_current & hdc %in% hdc_available)
    # 
    # go <- hdc_comparison[!duplicated(hdc_comparison[,c('admin_level_name','admin_level')]),]
    # list_levels <- as.list(go$admin_level)
    # names(list_levels) <- go$admin_level_name
    # 
    # hdc_comparison <- subset(list_osmid_name, country == country_current & admin_level == al & hdc %in% hdc_available)
    # # get options to show in the comparison - final
    # choices_comparison <- subset(choices_comparison, hdc == city$city_code)
    # # print(choices_comparison)
    # # remove the osmid that is already being shown
    # # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
    # 
    # 
    # # extract values
    # 
    # # for the hdc available
    # hdc_comparison_values <- hdc_comparison$osmid
    # hdc_comparison_names <- hdc_comparison$name
    # names(hdc_comparison_values) <- hdc_comparison_names
    # # for the final selection
    # choices_comparison_values <- choices_comparison$osmid
    # choices_comparison_names <- choices_comparison$name
    # names(choices_comparison_values) <- choices_comparison_names
    
    
    # print(countries)
    # print(country_current)
    
    
    showModal(modalDialog1(
      title = div(style = "display: flex; justify-content: space-between;", "COMPARE", modalButton(icon("close"))),
      id1 = "compare_max_modal",
      size = c("l"),
      easyClose = TRUE,
      footer = NULL,
      absolutePanel(
        class = "about_modal",
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_country",
                                      label = NULL,
                                      choices = countries_available,
                                      selected = country_current,
                                      multiple = FALSE,
                                      width = "200px",
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Country",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_urban_area",
                                      choices = urban_area_available_id,
                                      label = NULL,
                                      selected = al,
                                      multiple = FALSE,
                                      width = "150px",
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Urban area",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_level_analysis",
                                      label = NULL,
                                      choices = level_analysis_available_id,
                                      selected = city$city_code,
                                      width = "200px",
                                      multiple = FALSE,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Level of analysis",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        div(style="display:inline-block",
            shinyWidgets::pickerInput(inputId = "city_compare_analysis_area",
                                      label = NULL,
                                      choices = analysis_area_available_id,
                                      selected = NULL,
                                      width = "200px",
                                      multiple = TRUE,
                                      options = shinyWidgets::pickerOptions(size = 15,
                                                                            title = "Analysis area",
                                                                            liveSearch = TRUE,
                                                                            liveSearchPlaceholder = "Search...")
            )),
        div(style="display:inline-block",
            actionButton("reset_graph", "Clear Selection")),
        highchartOutput("comparison_max", height = "100%")
        
      )
    )
    
    )
    
  }
  
})

# create values to store the choices for comparison
comparison <- reactiveValues(choices = NULL)


# update the other values everytime a country is selected
observeEvent(c(input$city_compare_country_initial), {
  
  
  # get the admin level original
  al <- unique(data_ind3_spatial()$admin_level)
  
  
  if (rank$admin_level > 1) {
    
    # first, select only the ones that are available for the indicator in question
    hdc_available <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))$hdc
    # get hdc from that country
    hdc_comparison <- subset(list_osmid_name, country == input$city_compare_country_initial & admin_level == 0 & hdc %in% hdc_available)
    
    # for the hdc available
    hdc_comparison_values <- hdc_comparison$osmid
    hdc_comparison_names <- hdc_comparison$name
    names(hdc_comparison_values) <- hdc_comparison_names
    
    # comparison$choices <- choices_comparison_values
    
    
    updatePickerInput(
      session = session,
      inputId = "city_compare_hdc_initial",
      choices = hdc_comparison_values)
    
    
  } else {
    
    # first, select only the ones that are available for the indicator in question
    hdc_available <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))$hdc
    # get hdc from that country
    hdc_comparison <- subset(list_osmid_name, country == input$city_compare_country_initial & admin_level == 0 & hdc %in% hdc_available)
    
    
    # extract values
    choices_comparison_values <- hdc_comparison$osmid
    choices_comparison_names <- hdc_comparison$name
    names(choices_comparison_values) <- choices_comparison_names
    
    comparison$choices <- choices_comparison_values
    
    updatePickerInput(
      session = session,
      inputId = "city_compare1_initial",
      choices = comparison$choices)
    
  }
  
  
  # updatePickerInput(
  #   session = session,
  #   inputId = "city_compare1",
  #   choices = hdc_comparison_values)
  
  
})




# for the small compare graph
observeEvent(c(input$city_compare_hdc_initial), {
  
  req(rank$admin_level > 1)
  
  
  # get the admin level original
  al <- unique(data_ind3_spatial()$admin_level)
  
  
  # first, select only the ones that are available for the indicator in question
  hdc_available <-  subset(list_availability, grepl(pattern = indicator$mode, x = ind))$hdc
  
  # fisr, filter the level
  choices_comparison <- subset(list_osmid_name, admin_level == input$city_compare_level)
  
  # get options to show in the comparison
  choices_comparison <- subset(choices_comparison, country == input$city_compare_country_initial)
  # filter hdc with the indicators available
  choices_comparison <- subset(choices_comparison, hdc == input$city_compare_hdc_initial)
  # print(choices_comparison)
  # remove the osmid that is already being shown
  # choices_comparison <- subset(choices_comparison, osmid %nin% data_ind3_spatial()$osmid)
  
  # extract values
  choices_comparison_values <- choices_comparison$osmid
  choices_comparison_names <- choices_comparison$name
  names(choices_comparison_values) <- choices_comparison_names
  
  comparison$choices <- choices_comparison_values
  
  updatePickerInput(
    session = session,
    inputId = "city_compare1_initial",
    choices = comparison$choices)
  
  
})


# update the urban areas list when the country is selected - max version
observeEvent(c(input$city_compare_country), {
  
  req(input$maximize_comparison >= 1)
  
  # # 2) filter the hdc available for the selected country
  urban_area_available <- subset(list_osmid_name, country == input$city_compare_country & admin_level == 0)
  urban_area_available_id <- unique(urban_area_available$hdc)
  urban_area_available_name <- unique(urban_area_available$name)
  names(urban_area_available_id) <- urban_area_available_name
  
  updatePickerInput(
    selected = NULL,
    session = session,
    inputId = "city_compare_urban_area",
    choices = urban_area_available_id)
  
  shinyjs::enable("city_compare_analysis_area")
  
  
})

# update the admin level list when the country is selected - max version
observeEvent(c(input$city_compare_urban_area), {
  
  req(input$maximize_comparison >= 1)
  
  # # 2) filter the hdc available for the selected country
  level_analysis_available <- subset(list_osmid_name, hdc == input$city_compare_urban_area)
  level_analysis_available <- level_analysis_available[!duplicated(level_analysis_available[,c('admin_level_name','admin_level')]),]
  level_analysis_available <- level_analysis_available[order(level_analysis_available$admin_level_ordered),]
  level_analysis_available_id <- level_analysis_available$admin_level
  level_analysis_available_name <- level_analysis_available$admin_level_name 
  names(level_analysis_available_id) <- level_analysis_available_name
  
  updatePickerInput(
    selected = NULL,
    session = session,
    inputId = "city_compare_level_analysis",
    choices = level_analysis_available_id)
  
  shinyjs::enable("city_compare_analysis_area")
  
})

# update the analysis area list when the country is selected - max version
observeEvent(c(input$city_compare_level_analysis), {
  
  req(input$maximize_comparison >= 1, input$city_compare_level_analysis != "")
  
  analysis_area_available <- subset(list_osmid_name, admin_level == input$city_compare_level_analysis & hdc == input$city_compare_urban_area)
  analysis_area_available_id <- analysis_area_available$osmid
  analysis_area_available_name <- analysis_area_available$name
  names(analysis_area_available_id) <- analysis_area_available_name
  
  if (input$city_compare_level_analysis == "0") {
    
    
    
    updatePickerInput(
      selected = analysis_area_available_id,
      session = session,
      inputId = "city_compare_analysis_area",
      choices = analysis_area_available_id
    )
    
    shinyjs::disable("city_compare_analysis_area")
    
    
    
  } else {
    
    
    
    updatePickerInput(
      selected = NULL,
      session = session,
      inputId = "city_compare_analysis_area",
      choices = analysis_area_available_id
      
    )
    
    shinyjs::enable("city_compare_analysis_area")
    
  }
  
  
})


# use reactive to get and sort the selected terms in the order of selection
# ordered_colnames1 <- reactive({
#   
#   req(input$maximize_comparison >= 1)
#   
#   if (length(reV_order$values_max) > length(input$city_compare_analysis_area)) {
#     reV_order$values_max <- reV_order$values_max[reV_order$values_max %in% input$city_compare_analysis_area]
#   }else {
#     reV_order$values_max <- c(reV_order$values_max, input$city_compare_analysis_area[!input$city_compare_analysis_area %in% reV_order$values_max])
#   }
#   reV_order$values_max
# })

ordered_colnames1 <- reactive({
  req(input$maximize_comparison >= 1)
  
  # Start from previous values
  old_vals <- reV_order$values_max
  new_vals <- input$city_compare_analysis_area
  
  # Remove values that are no longer selected
  old_vals <- old_vals[old_vals %in% new_vals]
  
  # Find values that are new and append them
  added_vals <- setdiff(new_vals, old_vals)
  reV_order$values_max <- c(old_vals, added_vals)
  
  reV_order$values_max
})

observe({ ordered_colnames1() })



# to add cities to the comparison
observeEvent(c(input$city_compare_analysis_area), {
  
  
  req(input$maximize_comparison >= 1, input$city_compare_analysis_area != "")
  
  # print("ordered_colnames1()")
  # print(ordered_colnames1())
  
  if (city$city_code == "") {
    
    
    
    value_compare <- subset(atlas_country(), region_type == input$world_view1 & name == tail(ordered_colnames1(), 1))
    value_compare <- st_sf(value_compare)
    value_compare <- st_set_geometry(value_compare, NULL)
    value_compare <- tidyr::pivot_longer(value_compare,
                                         cols = 4:last_col(),
                                         names_sep = "_",
                                         names_to = c("ind_type", "ind", "year"),
                                         values_to = "value")
    value_compare <- subset(value_compare, ind == indicator$mode)
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    
    # 
    # value_compare$value <- if(format_indicator_unit_value == "percent") {
    #   round(value_compare$value * 100) 
    #   
    # } else round(value_compare$value)
    
    if (indicator$mode != "popdensity") {
      value_compare$value <- format_indicator_values(value_compare$value, transformation = indicator_info$transformation)
    }
    
    
    if (indicator$mode %in% c("not")) {
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      
      # add total
      highchartProxy("comparison_max") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = name, y = value),
                         id = tail(ordered_colnames1(), 1),
                         type = "column",
                         # color = "white",
                         name = unique(value_compare$name),
                         # size = 5,
                         tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                                        valueDecimals = 0),
                         pointWidth = 30
                         # pointPadding = "on"
                         
        )
      
    } else {
      
      print("Adding city")
      
      value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
      
      
      # add total
      highchartProxy("comparison_max") %>%
        # hcpxy_remove_series(id = "que") %>%
        hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                         id = tail(ordered_colnames1(), 1),
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
      
    }
    
    
  } else {
  
  value_compare <- subset(ind_compare$ind_compare, osmid == tail(ordered_colnames1(), 1))
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
  
  # print(format_indicator_unit_value)
  # print("format_indicator_unit_value")
  
  # value_compare$value <- if(format_indicator_unit_value == "percent") {
  #   round(value_compare$value * 100) 
  #   
  # } else round(value_compare$value)
  
  if (indicator$mode != "popdensity") {
    value_compare$value <- format_indicator_values(value_compare$value, transformation = indicator_info$transformation)
  }
  
  
  # print("ind_compare()")
  # print(value_compare)
  
  # print(input$city_compare)
  # print(ordered_colnames())
  
  if (indicator$mode %in% c("not")) {
    
    
    value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
    
    # add total
    highchartProxy("comparison_max") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = value_compare, hcaes(x = name, y = value),
                       id = tail(ordered_colnames1(), 1),
                       type = "column",
                       # color = "white",
                       name = unique(value_compare$name),
                       # size = 5,
                       tooltip = list(pointFormat = sprintf("{series.name}: {point.y} %s", format_indicator_unit),
                                      valueDecimals = 0),
                       pointWidth = 30
                       # pointPadding = "on"
                       
      )
    
  } else {
    
    message("Adding city ", tail(ordered_colnames1(), 1))
    print(value_compare)
    
    value_compare <- value_compare[order(value_compare$year),,drop=FALSE]
    
    # add total
    highchartProxy("comparison_max") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = value_compare, hcaes(x = year, y = value),
                       id = tail(ordered_colnames1(), 1),
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
    
  }
  
    }
  
}, ignoreNULL = FALSE)

previous_selection_max <- reactiveVal(character(0))

# to remove series from the comparision
observeEvent(c(input$city_compare_analysis_area), {
  
  req(input$maximize_comparison >= 1)
  
  # print("ordered_colnames1()")
  # print(input$city_compare_analysis_area)
  # print(ordered_colnames1())
  
  old_vals <- previous_selection_max()
  new_vals <- if (is.null(input$city_compare_analysis_area)) {
    character(0)
  } else {
    input$city_compare_analysis_area
  }
  
  
  # Identify removed items
  removed <- setdiff(old_vals, new_vals)
  
  if (length(removed) > 0) {
    # Run code only when something was unselected
    message("Removed: ", paste(removed, collapse = ", "))
    # put your logic here
  }
  
  previous_selection_max(new_vals)
  
  if (length(removed) > 0) {
    
    print(removed)
  
  highchartProxy("comparison_max") %>%
    hcpxy_remove_series(id = removed)
    
  }
  
  
  
}, ignoreNULL = FALSE)


# observeEvent(c(input$reset_graph), {
#   
#   
#   req(input$reset_graph >= 1)
#   
#   highchartProxy("comparison_max") %>%
#     hcpxy_remove_series(all = TRUE)
#   
#   updatePickerInput(
#     session = session,
#     inputId = "city_compare1",
#     choices = comparison$choices)
#   
#   
#   
# })


# observeEvent(c(input$city_compare1), {
#   
#   print("ordered_colnames1()")
#   print(ordered_colnames1())
#   
# }, ignoreNULL = FALSE)


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

# minimize the comprison when maximizing
observeEvent(c(input$maximize_comparison), {
  
  req(input$maximize_comparison >= 1)
  
  toggle("lalala", anim = TRUE, animType = "slide")
  
})