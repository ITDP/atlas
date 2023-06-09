# show ranks on the right panel -------------------------------------------

rank_country <- reactive({
  
  req(indicator$type, is.null(rank$admin_level))    
  
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  # open data
  a <- readRDS(sprintf("../data/data_alpha/countries/ranks/atlas_country_rank_%s.rds", pattern))
  
  return(a)
  
})  


# observer to watch the click on the polygons to update the right panel ----

rank <- reactiveValues(rank_value = NULL, rank_text = NULL,
                       rank_value_initial = NULL, rank_text_initial = NULL,
                       rank_value_world = NULL, rank_text_world = NULL,
                       admin_level = NULL,
                       rank_text_level0 = NULL,
                       country = NULL)

# this reactive value will store the indicator name, 
indicator_info <- reactiveValues(name = NULL,
                                 unit = NULL,
                                 transformation = NULL)


# display initial rank with indicators - in the world view
observeEvent(c(indicator$mode, year$ok, input$back_to_world), {
  
  
  
  req(indicator$mode)
  req(year$ok)
  # req(indicator$type)
  
  
  
  # print(rank$admin_level)
  
  if(is.null(rank$admin_level)) {
    
    # set the indicator in question
    pattern <- sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok)
    
    # filter both the indicators value and ranks for the requeired year
    country_values <- sf::st_set_geometry(atlas_country(), NULL)
    
    # country_ranks <- rank_country()
    # country_values <- atlas_country()
    
    # define the cols to subset 
    cols <- c('a3', 'name', colnames(atlas_country())[startsWith(colnames(atlas_country()), pattern)])
    
    # subset
    country_values <- country_values[cols]
    country_ranks <- rank_country()[cols]
    
    # rename the columns
    colnames(country_values) <- c('a3', 'name', 'value')
    colnames(country_ranks) <- c('a3', 'name', 'rank')
    
    # order
    country_values <- country_values[order(-country_values$value),]
    country_ranks <- country_ranks[order(-country_ranks$rank),]
    
    
    
    # mean for the world
    rank_indicator <- mean(country_values$value)
    
    # print("oooia")
    # print(a)
    
    # print(head(filter_rank()))
    # print(spatial_level_value$last)
    
    # format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    
    format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
    format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
    format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
    
    indicator_info$name <- format_indicator_name
    indicator_info$unit <- format_indicator_unit
    indicator_info$transformation <- format_indicator_unit_value
    
    # print("format_indicator_unit_value")
    # print(format_indicator_unit)
    
    format_indicator_world_mean <- if(format_indicator_unit_value == "percent") {
      round(rank_indicator * 100)
      
    } else if(format_indicator_unit_value %in% "thousands") {
      
      if (rank_indicator >= 1000000) scales::comma(rank_indicator, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(rank_indicator, accuracy = 1, scale = 0.001, suffix = "k")
      
      
    } else round(rank_indicator)
    
    
    format_indicator_value <- format_indicator_values(country_values$value, transformation = format_indicator_unit_value)
    
    
    
    rank$rank_value <- paste0(
      # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
      '<div class="title_indicator" style="font-size: 20px;">', 
      "THE WORLD", '</div>',
      div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_world_mean), " ", 
      p(style = "color: #B1B5B9; display: inline; font-size: 22px;", format_indicator_unit)
    )
    
    rank$rank_value_world <- rank$rank_value
    
    
    # ranking
    text_title <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "RANKING")
    
    
    scroll_world <- sprintf("<div class = \"text_compare\" style = \"padding-bottom: 0px; padding-top: 0px; font-size: 14px\"><span style=\"font-size: 17px;\">%s </span>&nbsp;%s <span  style=\"float:right; font-size: 12px; color: #B1B5B9 \">&nbsp;%s</span><span style=\"float:right; font-size: 17px;\">&nbsp;%s</span></div>",
                            1:length(country_values$name), country_values$name, indicator_info$unit, format_indicator_value)  
    
    scroll_world <- c("<div id=\"accordion_world1\" style=\"overflow-y:scroll; height:170px; margin-right: 15px;\">",
                      scroll_world,
                      "</div>"
    )
    scroll_world <- HTML(paste0(scroll_world, collapse = "\n"))
    
    rank$rank_text <- paste0(text_title, "<br>", scroll_world)
    
    
    rank$rank_text_world <- rank$rank_text
    
  }
  
  
  
})


# store the admin level in this reactivevalue, so it behaves as it should
observeEvent(c(input$admin_level), {
  
  # print(rank$admin_level)
  
  rank$admin_level <- input$admin_level
  # print("ARROCHA 1")
  print(rank$admin_level)
  
})

observeEvent(c(city$city_code), {
  
  req(city$city_code != "")
  # print("ARROCHA 2")
  
  rank$admin_level <- 1
  # print(rank$admin_level)
  
})

observeEvent(c(city$city_code), {
  
  if (city$city_code != "") {
    
    rank$admin_level <- 1
    
  }
  
  
})


# identify the name, unit and transformation for the indicators -----------
observeEvent(c(indicator$mode), {
  
  
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
  
  
  indicator_info$name <- format_indicator_name
  indicator_info$unit <- format_indicator_unit
  indicator_info$transformation <- format_indicator_unit_value
  
})



# display rank when a country is clicked
observeEvent(c(input$map_shape_click, indicator$indicator_mode, year$ok), {
  
  req(is.null(rank$admin_level), !is.null(input$map_shape_click$id))
  
  # get the click country
  ui <- input$map_shape_click$id
  
  value_indicator <- subset(st_set_geometry(atlas_country(), NULL), name == ui)
  rank_indicator <- subset(rank_country(), name == ui)
  
  
  # print("rank_indicator")
  # print(rank_indicator)
  
  # rank$country <- 
  
  # rename the columns
  colnames(value_indicator) <- c('a3', 'name', 'value')
  colnames(rank_indicator) <- c('a3', 'name', 'rank')
  
  rank$country <- rank_indicator$rank
  
  
  format_indicator_name <- subset(list_indicators, indicator_code == indicator$mode)$indicator_name
  format_indicator_unit <- subset(list_indicators, indicator_code == indicator$mode)$indicator_unit
  format_indicator_unit_value <- subset(list_indicators, indicator_code == indicator$mode)$indicator_transformation
  
  
  indicator_info$name <- format_indicator_name
  indicator_info$unit <- format_indicator_unit
  indicator_info$transformation <- format_indicator_unit_value
  
  # print(format_indicator_unit_value)
  # print("format_indicator_unit_value")
  
  format_indicator_value <- if(format_indicator_unit_value == "percent") {
    round(value_indicator$value * 100) 
    
  } else if(format_indicator_unit_value %in% "thousands") {
    
    if (value_indicator$value >= 1000000) scales::comma(value_indicator$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(value_indicator$value, accuracy = 1, scale = 0.001, suffix = "k")
    
    
  } else round(value_indicator$value)
  
  
  # print("value_indicator$value")
  # print(value_indicator)
  
  rank$rank_value <- paste0(
    # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
    '<div class="title_indicator" style="font-size: 20px;">', 
    rank_indicator$name, '</div>',
    div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_value), " ", 
    p(style = "color: #B1B5B9; display: inline; font-size: 22px", format_indicator_unit)
  )
  
  
}
)

# display rank when region or map is clicked
observeEvent(c(input$map_shape_click, city$city_code,
               year$ok,
               indicator$mode,
               # input$indicator_bike, input$indicator_walk, input$indicator_transit,
               input$regions_grid), label = "rank", {
                 
                 
                 req(data_ind3(), data_ind3_spatial())
                 
                 w$show()
                 
                 # print("rank$admin_level")
                 # print(rank$admin_level)
                 
                 # get the region that was clicked
                 ui <- if(is.null(input$map_shape_click) | rank$admin_level == 1) city$city_code else input$map_shape_click$id
                 
                 
                 # extract the admin_level
                 admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
                 
                 rank_indicator <- subset(data_ind3(), osmid == ui)[1,]
                 
                 que <- year$ok
                 
                 # print(format_indicator_unit_value)
                 # print("format_indicator_unit_value")
                 
                 format_indicator_value <- format_indicator_values(rank_indicator$value, transformation = indicator_info$transformation)
                 
                 
                 #   if(indicator_info$transformation == "percent") {
                 #   round(rank_indicator$value * 100) 
                 #   
                 # } else if(indicator_info$transformation %in% "thousands") {
                 #   
                 #   if (rank_indicator$value >= 1000000) scales::comma(rank_indicator$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(rank_indicator$value, accuracy = 1, scale = 0.001, suffix = "k")
                 #   
                 #   
                 # } else round(rank_indicator$value)
                 
                 
                 
                 
                 # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
                 if (input$regions_grid == "Grid") {
                   
                   rank$rank_value <- ""  
                   
                 } else { 
                   
                   rank$rank_value <- paste0(
                     # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
                     '<div class="title_indicator" style="font-size: 20px;">', 
                     rank_indicator$name, '</div>',
                     div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_value), " ", 
                     p(style = "color: #B1B5B9; display: inline; font-size: 22px", indicator_info$unit)
                   )
                 }
                 
                 # print("rank$rank_value")
                 # print(rank$rank_value)
                 
                 
                 # the number of ranks will depend on the admin level
                 
                 # this first condition will show the indicator ranks as soon as the city marker is clicked
                 if (input$regions_grid == "Grid") {
                   
                   base_text <- ""  
                   
                 } else { 
                   base_text <- div(class = "title_indicator_label", style ="padding-bottom: 0px", "COMPARED TO OTHER REGIONS",
                                    tags$button(
                                      id = "tooltip_compare_right",
                                      class="btn btn-light btn-xs",
                                      style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                                      icon("circle-info")
                                      
                                    ))
                 }
                 
                 # when is this applying?
                 if (!is.null(city$city_code) & isTRUE(is.null(rank$admin_level))) {
                   
                   a <- subset(filter_rank(), osmid == ui & type_rank == "world")
                   rank$rank_text <- sprintf('%s <div class="text_compare"> Ranks <strong style="font-size: 35px;">3</strong>%s</strong> out offff %s</strong> in the world</div>', 
                                             base_text, a$rank, a$n)
                   
                   
                   rank$rank_text_initial <- rank$rank_text
                   rank$rank_value_initial <- rank$rank_value
                   
                   
                   # for the region case
                 } else if (rank$admin_level == 1) {
                   
                   print("puhhh")
                   
                   # open the ranks text
                   indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                   ranks_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, 0, indicator_pattern))
                   ranks_text1 <- subset(ranks_text, type_rank == "world" & year == que)
                   rank_text_world <- ranks_text1$text
                   rank$value <- ranks_text1$rank
                   
                   ranks_text2 <- subset(ranks_text, type_rank == "country" & year == que)
                   rank_text_country <- ranks_text2$text
                   rank$value <- c(rank$value, ranks_text2$rank)
                   
                   # open the scroll text
                   scroll_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", city$city_code, city$city_code, 0, indicator_pattern))
                   scroll_world <- HTML(subset(scroll_text, type_rank == "world" & year == que)$text)
                   scroll_country <- HTML(subset(scroll_text, type_rank == "country" & year == que)$text)
                   
                   
                   
                   text_world <- accordion_ranks("accordion_world", rank_text_world, scroll_world)
                   
                   text_country <- accordion_ranks("accordion_country", rank_text_country, scroll_country)
                   
                   # print(pera)
                   
                   rank$rank_text <- paste0(base_text, text_world, text_country)
                   rank$rank_text_level0 <- paste0(base_text, text_world, text_country)
                   # print("rank$rank_text")
                   # print(rank$rank_text)
                   
                   
                   if (input$regions_grid == "Grid") {
                     
                     rank$rank_value <- ""  
                     
                   } else { 
                     rank$rank_value <- paste0(
                       # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
                       '<div class="title_indicator" style="font-size: 20px;">', 
                       rank_indicator$name, '</div>',
                       div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_value), " ", 
                       p(style = "color: #B1B5B9; display: inline; font-size: 22px;", indicator_info$unit)
                       
                       
                       
                       
                       
                       
                       
                     )
                   }
                   rank$rank_text_initial <- rank$rank_text
                   rank$rank_value_initial <- rank$rank_value
                   # print(paste0("teste: ", rank$rank_text_initial))
                   
                   
                   
                   # print("olha")
                   # print(rank$rank_value)
                   # print(a$rank)
                   # print(a$n)
                   
                   
                   # will run when we are above or equal to the neighborhood level
                 } else if (admin_level_osm >= 10) {
                   
                   # open the ranks text
                   indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                   ranks_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                   ranks_text <- subset(ranks_text, type_rank == "metro" & osmid == ui & year == que)
                   rank$value <- ranks_text$rank
                   rank_text_country <- ranks_text$text
                   
                   # open the scroll text
                   scroll_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                   scroll_country <- HTML(subset(scroll_text, type_rank == "metro" & year == que)$text)
                   
                   
                   text_country <- accordion_ranks("accordion_country", rank_text_country, scroll_country)
                   
                   rank$rank_text <- paste0(base_text, text_country)
                   
                   # will run for all situation from city to neighborhood
                 } else {
                   
                   # open the ranks text
                   indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                   ranks_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                   ranks_text1 <- subset(ranks_text, type_rank == "country" & osmid == ui & year == que)
                   rank$value <- ranks_text1$rank
                   rank_text_world <- ranks_text1$text
                   
                   ranks_text2 <- subset(ranks_text, type_rank == "metro" & osmid == ui & year == que)
                   rank$value <- c(rank$value, ranks_text2$rank)
                   rank_text_country <- ranks_text2$text
                   
                   # open the scroll text
                   scroll_text <- readRDS(sprintf("../data/data_alpha/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                   scroll_world <- HTML(subset(scroll_text, type_rank == "country" & year == que)$text)
                   scroll_country <- HTML(subset(scroll_text, type_rank == "metro"  & year == que)$text)
                   
                   # print("scroll_world")
                   # print(scroll_world)
                   
                   
                   
                   text_world <- accordion_ranks("accordion_world", rank_text_world, scroll_world)
                   
                   text_country <- accordion_ranks("accordion_country", rank_text_country, scroll_country)
                   
                   rank$rank_text <- paste0(base_text, text_world, text_country)
                   
                 }
                 
                 
                 
               })



# change color of selected region in carrousel ----------------------------

# observeEvent(c(input$map_shape_click), {
observeEvent(c(input$map_shape_click), {
  
  
  req(is.null(rank$admin_level))  
  
  delay(1000, runjs(sprintf("$('#accordion_world1 > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$country)))
  
  
  
})


observeEvent(c(input$map_shape_click, city$city_code, data_ind3_spatial()), {
  
  req(length(data_ind3_spatial()$admin_level) > 0)
  
  admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
  print("AQUIIIIIIIII")
  # print(admin_level_osm)
  
  if(admin_level_osm >= 10) {
    
    delay(2000, runjs(sprintf("$('#accordion_country > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[1])))
    
  } else {
    
    print("bumpppp")
    delay(2000, runjs(sprintf("$('#accordion_world > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[1])))
    delay(2000, runjs(sprintf("$('#accordion_country > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[2])))
    
  }
  
})

# if I change the spatial_level, the right panel should inform the user
# that they should click on a region to see more things
observeEvent(c(indicator$mode), {
  
  req(!is.null(rank$admin_level))
  
  # print("QUAQUA")
  rank$rank_value <- '<div class="text_compare"><i> Click on the map to see more info</i> </div>'
  rank$rank_text <- ""
  
})


observeEvent(c(rank$admin_level, input$map_marker_click, city$city_code, input$regions_grid), {
  
  # waiter_show()
  # print(paste0("rank admin level"))
  # print(rank$admin_level)
  
  
  req(data_ind3())
  
  # print("req")
  
  rank_indicator <- subset(data_ind3(), osmid == city$city_code)[1,]
  # it will run only when we are at the city level
  
  
  if (isTRUE(rank$admin_level == 1)) {
    
    
    # print(format_indicator_unit_value)
    # print(rank_indicator$value)
    
    format_indicator_value <- if(indicator_info$transformation == "percent") {
      round(rank_indicator$value * 100) 
      
    } else if(indicator_info$transformation %in% "thousands") {
      
      if (rank_indicator$value >= 1000000) scales::comma(rank_indicator$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(rank_indicator$value, accuracy = 1, scale = 0.001, suffix = "k")
      
    } else round(rank_indicator$value)
    
    # print("no idea")
    # print(rank_indicator$name)
    # print(format_indicator_value)
    # print(format_indicator_unit)
    
    
    
    # rank$rank_value <- sprintf("<h1>%s</h1><h2>%s</h2>", rank_indicator$name, rank_indicator$value)
    if (isTRUE(input$regions_grid == "Grid")) {
      
      rank$rank_value <- ""  
      
    } else { 
      rank$rank_value <- paste0(
        # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
        '<div class="title_indicator" style="font-size: 20px;">', 
        rank_indicator$name, '</div>',
        div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_value), " ", 
        p(style = "color: #B1B5B9; display: inline; font-size: 22px", indicator_info$unit)
      )
    }
    
    
    
    
    rank$rank_text <- rank$rank_text_level0
    
  } else
    
    if (isTRUE(rank$admin_level != 1)) {
      
      rank$rank_value <- '<div class="text_compare"><i> Click on the map to see more info</i> </div>'
      rank$rank_text <- ""
      
    }
  
  
})


# output$rank_value <- renderUI({
#   req(indicator$mode, rank$rank_value)
#   
#   tagList(
#     HTML(rank$rank_value),
#     div(
#       bsPopover(id = "tooltip_compare_right",
#                 # title = sprintf("<strong>%s</strong>", "LEVEL OF DETAIL"),
#                 title = "",
#                 content = HTML("Compared with the same Administrative Level"),
#                 placement = "top",
#                 trigger = "hover"
#                 # options = list(container = "body")
#       )
#     )
#   )
#   
# })
# 
# # create chart with indicator value
# 
# 
# 
# output$rank_text <- renderUI({
#   
#   req(indicator$mode, rank$rank_text)
#   
#   HTML(rank$rank_text)
#   # rank$rank_text
#   
# })


output$rank_final <- renderUI({
  
  # req(input$indicator)
  print(input$indicator)
  
  if (input$indicator == "") {
    
    tagList(
      HTML('<div class="title_indicator" style="font-size: 22px;">Welcome to the Atlas of Urban Transportation</div>'),
      p(style = "color: #B1B5B9; display: inline; font-size: 16px;", "Start by selecting an indicator")
    )
    
  } else {
    
    
    tagList(
      HTML(rank$rank_value),
      div(
        bsPopover(id = "tooltip_compare_right",
                  # title = sprintf("<strong>%s</strong>", "LEVEL OF DETAIL"),
                  title = "",
                  content = HTML("Compared with the same Administrative Level"),
                  placement = "top",
                  trigger = "hover"
                  # options = list(container = "body")
        )
      ),
      HTML(rank$rank_text)
    )
    
    
  }
  
})

# create chart with indicator value



output$rank_text <- renderUI({
  
  req(indicator$mode, rank$rank_text)
  
  HTML(rank$rank_text)
  # rank$rank_text
  
})