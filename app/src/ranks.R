# show ranks on the right panel -------------------------------------------

get_rank <- reactive({
  
  req(city$city_code)
  # filter rank from rank files
  a <- readRDS(sprintf("../data/sample3/ranks/rank_%s.rds", city$city_code))
  
  
})

filter_rank <- reactive({
  
  
  req(indicator_mode())
  # print(paste0("pattern: ", indicator_mode()))
  # print(head(get_rank()))
  pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
  # print("pattern")
  # print(pattern)
  cols <- c('osmid', 'admin_level_ordered', 'name', colnames(data_ind1())[startsWith(colnames(data_ind1()), pattern)], 'rank_type', 'n')
  a <- get_rank()[cols]
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
  a <- atlas_country_ranks[cols]
  colnames(a) <- c('name_long', 'a2', 'rank', 'n')
  # only top five
  a <- a[order(a$rank),]
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
observeEvent(c(indicator_mode()), {
  
  
  
  # req(indicator_mode())
  # req(indicator$type)
  
  # print(rank$admin_level)
  
  if(is.null(rank$admin_level)) {
    
    print("agora vai!")
    # print("queeeeeeeeeee")
    
    # value
    print(paste0("type: ", indicator$type))
    pattern <- sprintf("%s_%s", indicator$type, indicator_mode())
    # print(pattern)
    cols <- c('name_long', colnames(atlas_country)[startsWith(colnames(atlas_country), pattern)], "geom")
    # print(cols)
    a <- atlas_country[cols]
    colnames(a) <- c('name_long', 'valor', 'geom')
    # print(a)
    # only top five
    a <- a[order(-a$valor),]
    a <- a[1:3,]
    # mean for the world
    rank_indicator <- mean(a$valor)
    
    # print("oooia")
    # print(rank_indicator)
    
    # print(head(filter_rank()))
    # print(spatial_level_value$last)
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
    
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
    # print(rank$rank_value)
    
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
                 format_indicator_name <- subset(list_indicators, indicator_code == indicator_mode())$indicador_name
                 
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

