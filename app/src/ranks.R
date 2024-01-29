# show ranks on the right panel -------------------------------------------

rank_country <- reactive({
  
  req(indicator$type, is.null(rank$admin_level))    
  
  pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  
  # open data
  a <- readRDS(sprintf("../data/data_beta/countries/ranks/atlas_country_rank_%s.rds", pattern))
  
  
  return(a)
  
})  


# observer to watch the click on the polygons to update the right panel ----

rank <- reactiveValues(rank_value = NULL, rank_text = NULL,
                       rank_value_initial = NULL, rank_text_initial = NULL,
                       rank_value_world = NULL, rank_text_world = NULL,
                       admin_level = NULL,
                       vel0 = NULL,
                       country = NULL,
                       indicator = NULL,
                       data = NULL)

# this reactive value will store the indicator name, 
indicator_info <- reactiveValues(name = NULL,
                                 unit = NULL,
                                 transformation = NULL)


# display initial rank with indicators - in the world view
observeEvent(c(indicator$mode, year$ok, input$back_to_world), {
  
  
  
  req(indicator$mode)
  req(year$ok)
  # req(indicator$type)
  
  message("Rank: initial ranks")
  
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
    
    # format_indicator_world_mean <- if(format_indicator_unit_value == "percent") {
    #   round(rank_indicator * 100)
    #   
    # } else if(format_indicator_unit_value %in% "thousands") {
    #   
    #   if (rank_indicator >= 1000000) scales::comma(rank_indicator, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(rank_indicator, accuracy = 1, scale = 0.001, suffix = "k")
    #   
    #   
    # } else round(rank_indicator)
    
    
    
    format_indicator_world_mean <- format_indicator_values(rank_indicator, transformation = format_indicator_unit_value)
    
    format_indicator_value <- format_indicator_values(country_values$value, transformation = format_indicator_unit_value)
    
    
    
    rank$rank_value <- paste0(
      # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
      '<div class="title_indicator" style="font-size: 20px;">', 
      "THE WORLD", '</div>',
      div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_world_mean), 
      ifelse(format_indicator_unit == "%", "", " "), 
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
  # print(rank$admin_level)
  
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
  
}, priority = 10)



# display rank when a country is clicked
observeEvent(c(input$map_shape_click, indicator$indicator_mode, year$ok), {
  
  req(is.null(rank$admin_level), !is.null(input$map_shape_click$id))
  
  
  message("Rank: countries rank")
  
  # get the click country
  ui <- input$map_shape_click$id
  
  value_indicator <- subset(st_set_geometry(atlas_country(), NULL), name == ui)
  rank_indicator <- subset(rank_country(), name == ui)
  
  # print("jskfjakfmaks")
  # print(value_indicator)
  
  pattern <- sprintf("_%s", year$ok)
  cols <- c('a3', 'name', colnames(value_indicator)[endsWith(colnames(value_indicator), pattern)])
  value_indicator <- value_indicator[cols]
  colnames_new <- sub(pattern = sprintf("%s_%s_%s", indicator$type, indicator$mode, year$ok), replacement = "value",  colnames(value_indicator))
  # remove the year
  colnames_new <- sub(pattern = sprintf("_%s$", year$ok), replacement = "",  colnames_new)
  colnames(value_indicator) <- colnames_new
  
  
  rank$indicator <- value_indicator
  
  
  print("rank$indicator")
  print(rank$indicator)
  
  # rank$country <- 
  
  # rename the columns
  # colnames(value_indicator) <- c('a3', 'name', 'value')
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
  
  # format_indicator_value <- if(format_indicator_unit_value == "percent") {
  #   round(value_indicator$value * 100) 
  #   
  # } else if(format_indicator_unit_value %in% "thousands") {
  #   
  #   if (value_indicator$value >= 1000000) scales::comma(value_indicator$value, accuracy = 0.1, scale = 0.000001, suffix = "M") else scales::comma(value_indicator$value, accuracy = 1, scale = 0.001, suffix = "k")
  #   
  #   
  # } else round(value_indicator$value)
  
  format_indicator_value <- format_indicator_values(value_indicator$value, transformation = indicator_info$transformation)
  
  
  
  
  # print("value_indicator$value")
  # print(value_indicator)
  
  rank$rank_value <- paste0(
    # '<div class="title_indicator_label" style="padding-bottom: 0px; padding-top: 10px">THIS INDICATOR IN </div>', 
    '<div class="title_indicator" style="font-size: 20px;">', 
    rank_indicator$name, '</div>',
    div(class = "value_indicator_rightpanel", style = "display: inline", format_indicator_value), 
    ifelse(format_indicator_unit == "%", "", " "),
    p(style = "color: #B1B5B9; display: inline; font-size: 22px", format_indicator_unit)
  )
  
  
}
)


# if I change the indicator, I need to reset the element selected
indicator <- reactiveValues(changed = NULL)

observeEvent(c(indicator$mode), {
  
  
  element$selected1 <- NULL
  
  indicator$changed <- "yes"
  
})

observeEventTrigger <-  reactiveVal()

# como saber se mudou de cidade?
city <- reactiveValues(change = NULL)

observeEvent(c(city$city_code), {
  
  city$change <- "yes"
  
})
                       

# display rank when region or map is clicked
observeEvent(c(input$map_shape_click, city$city_code,
               year$ok,
               indicator$mode,
               input$regions_grid), label = "rank", {
                 
                 
                 # observeEventTrigger(req(input$changed))
                 # cat("My execution was triggered by input:", observeEventTrigger(), "\n")
                 
                 req(data_ind3_spatial())
                 
                 
                 ui <- if(is.null(input$map_shape_click) | rank$admin_level == 1) city$city_code else input$map_shape_click$id
                 
                 # we have a problem that the overlay is being clickable, regardless of what we set
                 # so we need to make this overlay un-reactable from here
                 ui_ok <- grepl("^\\d{3,}", ui)
                 
                 
                 if (ui_ok) {
                   
                   
                   
                   message("Rank: rank when select cities")
                   
                   
                   
                   element$selected1 <- c(element$selected1, ui)
                   
                   
                   variables$indicator <- variables$indicator[variables$indicator != ""]
                   
                   # conditions that this should run:
                   # when 
                   # print("when")
                   # print(element$selected1)
                   
                   run <- if (isTRUE(length(element$selected1) >= 1)) {
                     
                     TRUE
                     
                     # if the selected element is different to the previous one
                   }  else if (isTRUE(element$selected1[length(element$selected1)] != element$selected1[length(element$selected1)-1])) {
                     
                     TRUE
                     
                   } else {FALSE}
                   
                   # print(run)
                   
                   
                   run2 <- if (rank$admin_level == 1 & is.null(city$change) & is.null(indicator$changed)) {
                     
                     FALSE
                     
                   }  else {TRUE}
                   
                   
                   indicator$changed <- NULL
                   
                   
                   if(run & run2) {
                     
                     
                     
                     
                     
                     w$show()
                     
                     
                     # extract the admin_level
                     admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
                     
                     rank_indicator <- subset(data_ind3(), osmid == ui)[1,]
                     rank$indicator <- rank_indicator
                     
                     
                     que <- year$ok
                     
                     # print(format_indicator_unit_value)
                     # print("format_indicator_unit_value")
                     
                     format_indicator_value <- format_indicator_values(rank_indicator$value, transformation = indicator_info$transformation)
                     
                     # set style for the text
                     style_number <- function(x) {
                       
                       paste0('<strong style="font-size: 25px; color: #00AE42;">', x, '</strong>') 
                       
                     }
                     
                     style_text <- function(x) {
                       
                       paste0('<strong style="font-size: 20px; color: #00AE42;">', x, '</strong>') 
                       
                     }
                     
                     # create the indicator label for each indicator
                     # print(rank_indicator$name)
                     indicator_label <- switch(indicator$mode,
                                               "popdensity" = sprintf("%s has a weighted population density of %s people per km2", 
                                                                      style_text(rank_indicator$name), 
                                                                      style_number(format_indicator_value)),
                                               "blockdensity" = sprintf("%s has an average of %s blocks per km2", 
                                                                        style_text(rank_indicator$name), 
                                                                        style_number(format_indicator_value)),
                                               "journeygap" = sprintf("In %s, the average trip takes %s times as long by walking, bicycling, or public transport as by driving.", 
                                                                      style_text(rank_indicator$name), 
                                                                      style_number(format_indicator_value)),
                                               "pns" = sprintf("%s%s of people in %s live within 1km of both healthcare and education services.", 
                                                               style_number(format_indicator_value), 
                                                               style_number("%"), 
                                                               style_text(rank_indicator$name)),
                                               "pncf" = sprintf("%s%s of people in %s live within 100m of a car-free space.", 
                                                                style_number(format_indicator_value), 
                                                                style_number("%"), 
                                                                style_text(rank_indicator$name)),
                                               "pnh" = sprintf("%s%s of people in %s live farther than 500m from a grade-separated highway. ", 
                                                               style_number(format_indicator_value), 
                                                               style_number("%"), 
                                                               style_text(rank_indicator$name)),
                                               "pnpb" = sprintf('%s%s of people in %s live within a 300m walk of a protected bikeway. ', 
                                                                style_number(format_indicator_value), 
                                                                style_number("%"), 
                                                                style_text(rank_indicator$name)),
                                               "pnrt" = sprintf("%s%s of people in %s live within 1km of high-capacity public transport running on a dedicated right-of-way.", 
                                                                   style_number(format_indicator_value), 
                                                                   style_number("%"), 
                                                                   style_text(rank_indicator$name)),
                                               "pnft" = sprintf("%s%s of people in %s live within 500m of a transport stop where a bus or train comes every 10 minutes or sooner.", 
                                                                style_number(format_indicator_value), 
                                                                style_number("%"), 
                                                                style_text(rank_indicator$name))
                                               
                     )
                     
                     
                     
                     rank$rank_value <- paste0(
                       '<div class="title_indicator" style="font-size: 19px; font-weight: normal;">', 
                       indicator_label,
                       '</div>'
                     )
                     
                     # the number of ranks will depend on the admin level
                     
                     # this first condition will show the indicator ranks as soon as the city marker is clicked
                     base_text <- div(class = "title_indicator_label", style ="padding-bottom: 0px", 
                                      "COMPARED TO OTHER REGIONS",
                                      tags$button(
                                        id = "tooltip_compare_right",
                                        class="btn btn-light btn-xs",
                                        style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                                        icon("circle-info")
                                        
                                      ))
                     
                     # when is this applying?
                     if (!is.null(city$city_code) & isTRUE(is.null(rank$admin_level))) {
                       
                       a <- subset(filter_rank(), osmid == ui & type_rank == "world")
                       rank$rank_text <- sprintf('%s <div class="text_compare"> Ranks <strong style="font-size: 35px;">3</strong>%s</strong> out offff %s</strong> in the world</div>', 
                                                 base_text, a$rank, a$n)
                       
                       
                       rank$rank_text_initial <- rank$rank_text
                       rank$rank_value_initial <- rank$rank_value
                       
                       
                       # for the region case
                     } else if (rank$admin_level == 1) {
                       
                       # print("puhhh")
                       
                       # open the ranks text
                       indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                       ranks_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, 0, indicator_pattern))
                       ranks_text1 <- subset(ranks_text, type_rank == "world" & year == que)
                       # costumize
                       # # create text
                       ranks_text1_display <- sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                                                      ranks_text1$rank, ranks_text1$n, ranks_text1$type_rank)
                       
                       rank_text_world <- ranks_text1_display
                       rank$value <- ranks_text1$rank
                       
                       
                       
                       ranks_text2 <- subset(ranks_text, type_rank == "country" & year == que)
                       ranks_text2_display <- sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                                                      ranks_text2$rank, ranks_text2$n, ranks_text2$type_rank)
                       
                       # print("ANO AQUI")
                       # print(ranks_text2_display)
                       
                       rank_text_country <- ranks_text2_display
                       rank$value <- c(rank$value, ranks_text2$rank)
                       
                       rank$rank_text <- paste0(div(style = "padding-bottom: 10px;", HTML(rank_text_world), actionButton(inputId = "rank_more_1_world", label = "+", style = "display: inline; float: right")),
                                                div(style = "padding-bottom: 10px;", HTML(rank_text_country), actionButton(inputId = "rank_more_1_country", label = "+", style = "display: inline; float: right"))
                       )
                       
                       rank$vel0 <- rank$rank_text
                       
                       
                       # rank$rank_value <- paste0(
                       #   '<div class="title_indicator" style="font-size: 19px; font-weight: normal;">', 
                       #   indicator_label,
                       #   '</div>'
                       # )
                       
                       
                       rank$rank_text_initial <- rank$rank_text
                       rank$rank_value_initial <- rank$value
                       
                       
                       # print("initial:")
                       # print(rank$rank_value_initial)
                       
                       # will run when we are above or equal to the neighborhood level
                     } else if (admin_level_osm >= 10) {
                       
                       # open the ranks text
                       indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                       ranks_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                       ranks_text <- subset(ranks_text, type_rank == "metro" & osmid == ui & year == que)
                       ranks_text_display <- sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                                                     ranks_text$rank, ranks_text$n, ranks_text$type_rank)
                       
                       
                       rank$value <- ranks_text$rank
                       rank_text_metro <- ranks_text_display
                       
                       rank$rank_text <- paste0(div(style = "padding-bottom: 10px;", HTML(rank_text_metro), actionButton(inputId = "rank_more_1_metro", label = "+", style = "display: inline; float: right"))
                       )
                       
                       
                       # will run for all situation from city to neighborhood
                     } else if(rank$admin_level > 1) {
                       
                       # open the ranks text
                       indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
                       ranks_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
                       
                       
                       ranks_text1 <- subset(ranks_text, type_rank == "country" & osmid == ui & year == que)
                       ranks_text1_display <- sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                                                      ranks_text1$rank, ranks_text1$n, ranks_text1$type_rank)
                       rank$value <- ranks_text1$rank
                       rank_text_country <- ranks_text1_display
                       
                       
                       ranks_text2 <- subset(ranks_text, type_rank == "metro" & osmid == ui & year == que)
                       ranks_text2_display <- sprintf('<div class="text_compare" style="font-size: 14px; display: inline";> Ranks <strong style="font-size: 35px;">%s</strong> out of <strong>%s</strong> in the %s</div>',
                                                      ranks_text2$rank, ranks_text2$n, ranks_text2$type_rank)
                       rank$value <- c(rank$value, ranks_text2$rank)
                       rank_text_metro <- ranks_text2_display
                       
                       
                       rank$rank_text <- paste0(div(style = "padding-bottom: 10px;", HTML(rank_text_country), actionButton(inputId = "rank_more_1_country", label = "+", style = "display: inline; float: right")),
                                                div(style = "padding-bottom: 10px;", HTML(rank_text_metro), actionButton(inputId = "rank_more_1_metro", label = "+", style = "display: inline; float: right"))
                       )
                       
                       
                     }
                     
                   }
                   
                 }
                 
                 city$change <- NULL
                 
                 
               })



# change color of selected region in carrousel ----------------------------

# observeEvent(c(input$map_shape_click), {
observeEvent(c(input$map_shape_click), {
  
  
  req(is.null(rank$admin_level))  
  
  delay(1000, runjs(sprintf("$('#accordion_world1 > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$country)))
  
  
  
})



# open table as user clicks on the plus button on the rankings --------------------------------


observeEvent(c(input$rank_more_1_country), {
  
  req(input$rank_more_1_country >= 1)
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  que <- year$ok
  admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
  # admin_level_osm <- if (admin_level_osm == 1) 0 else admin_level_osm
  
  # print("admin_level_osm")
  # print(admin_level_osm)
  
  # open data
  scroll_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", city$city_code, city$city_code, admin_level_osm, indicator_pattern))
  
  # filter
  scroll_world <- subset(scroll_text, type_rank == "country" & year == que)
  
  output <- sprintf("<div class = \"text_compare\" style = \"padding-bottom: 0px; padding-top: 0px; font-size: 14px\"><span style=\"font-size: 17px;\">%s </span>&nbsp;%s <span  style=\"float:right; font-size: 12px; color: #B1B5B9 \">&nbsp;%s</span><span style=\"float:right; font-size: 17px;\">&nbsp;%s</span></div>", 
                    scroll_world$n, scroll_world$name, scroll_world$format_indicator_unit, scroll_world$value)
  output <- paste(output,
                  collapse = "\n"
  )
  output <- paste("<div id=\"\" style=\"overflow-y:scroll; height:380px;\">",
                  output,
                  "</div>",
                  sep = "\n"
  )
  
  # print("position")
  # print(rank$value[1])
  
  position <- if (admin_level_osm == 0) rank$value[2] else rank$value[1]
  
  # highlight the selected location
  delay(1000, runjs(sprintf("$('#modal_ranking_world1 > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", position)))
  
  showModal(modalDialog1(
    title = "Ranking",
    easyClose = TRUE,
    size = "l",
    # footer = NULL,
    id1 = "modal_ranking_world",
    id = "modal_ranking_world1",
    footer = NULL,
    # p(sprintf("Indicator: %s", indicator$mode)), 
    # p(sprintf("Year: %s", year$ok)),
    # p(sprintf("Level: %s", rank$admin_level)),
    HTML(output)
    
    
  ))
  
  
  waiter_hide()  
  
})

observeEvent(c(input$rank_more_1_world), {
  
  req(input$rank_more_1_world >= 1)
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  que <- year$ok
  # open data
  scroll_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", city$city_code, city$city_code, 0, indicator_pattern))
  
  # filter
  scroll_world <- subset(scroll_text, type_rank == "world" & year == que)
  
  output <- sprintf("<div class = \"text_compare\" style = \"padding-bottom: 0px; padding-top: 0px; font-size: 14px\"><span style=\"font-size: 17px;\">%s </span>&nbsp;%s <span  style=\"float:right; font-size: 12px; color: #B1B5B9 \">&nbsp;%s</span><span style=\"float:right; font-size: 17px;\">&nbsp;%s</span></div>", 
                    scroll_world$n, scroll_world$name, scroll_world$format_indicator_unit, scroll_world$value)
  output <- paste(output,
                  collapse = "\n"
  )
  output <- paste("<div id=\"\" style=\"overflow-y:scroll; height:380px;\">",
                  output,
                  "</div>",
                  sep = "\n"
  )
  
  
  
  # print("vaaaah")
  # print(rank$value)
  # highlight the selected location
  delay(1000, runjs(sprintf("$('#modal_ranking_world1 > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[1])))
  
  
  
  showModal(modalDialog1(
    title = "Ranking",
    easyClose = TRUE,
    size = "l",
    # footer = NULL,
    id1 = "modal_ranking_world",
    id = "modal_ranking_world1",
    footer = NULL,
    # p(sprintf("Indicator: %s", indicator$mode)), 
    # p(sprintf("Year: %s", year$ok)),
    # p(sprintf("Level: %s", rank$admin_level)),
    HTML(output)
    
    
  ))
  
  
  waiter_hide()
  
})


observeEvent(c(input$rank_more_1_metro), {
  
  req(input$rank_more_1_metro >= 1)
  
  waiter_show(
    html = tagList(spin_loaders(id = 3, color = "black")),
    color = "rgba(233, 235, 240, .2)")
  
  
  indicator_pattern <- sprintf("%s_%s", indicator$type, indicator$mode)
  que <- year$ok
  admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
  
  # open data
  scroll_text <- readRDS(sprintf("../data/data_beta/ghsl_%s/ranks/ranks_full_%s_%s_%s.rds", 
                                 city$city_code, city$city_code, admin_level_osm, indicator_pattern))
  
  # filter
  scroll_world <- subset(scroll_text, type_rank == "metro" & year == que)
  
  output <- sprintf("<div class = \"text_compare\" style = \"padding-bottom: 0px; padding-top: 0px; font-size: 14px\"><span style=\"font-size: 17px;\">%s </span>&nbsp;%s <span  style=\"float:right; font-size: 12px; color: #B1B5B9 \">&nbsp;%s</span><span style=\"float:right; font-size: 17px;\">&nbsp;%s</span></div>", 
                    scroll_world$n, scroll_world$name, scroll_world$format_indicator_unit, scroll_world$value)
  output <- paste(output,
                  collapse = "\n"
  )
  output <- paste("<div id=\"\" style=\"overflow-y:scroll; height:380px;\">",
                  output,
                  "</div>",
                  sep = "\n"
  )
  
  position <- if (admin_level_osm >= 10) rank$value[1] else rank$value[2]
  
  # highlight the selected location
  delay(1000, runjs(sprintf("$('#modal_beta_checkpoint1 > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", position)))
  
  showModal(modalDialog1(
    title = "Ranking",
    easyClose = TRUE,
    size = "l",
    # footer = NULL,
    id1 = "modal_ranking_world",
    id = "modal_beta_checkpoint1",
    footer = NULL,
    # p(sprintf("Indicator: %s", indicator$mode)), 
    # p(sprintf("Year: %s", year$ok)),
    # p(sprintf("Level: %s", rank$admin_level)),
    HTML(output)
    
    
  ))
  
  
  waiter_hide()
  
})


observeEvent(c(input$map_shape_click, city$city_code, data_ind3_spatial()), {
  
  req(length(data_ind3_spatial()$admin_level) > 0)
  
  admin_level_osm <- as.numeric(unique(data_ind3_spatial()$admin_level))
  # print("AQUIIIIIIIII")
  # print(admin_level_osm)
  
  if(admin_level_osm >= 10) {
    
    delay(2000, runjs(sprintf("$('#accordion_country > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[1])))
    
  } else {
    
    # print("bumpppp")
    delay(2000, runjs(sprintf("$('#accordion_world > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[1])))
    delay(2000, runjs(sprintf("$('#accordion_country > div > div > div:nth-child(%s)').css({'color': '#00AE42', 'font-weight': '600', 'font-size': '16px'})", rank$value[2])))
    
  }
  
})

# if I change the mode, the right panel should inform the user
# that they should click on a region to see more things
observeEvent(c(indicator$mode), {
  
  req(!is.null(rank$admin_level))
  
  
  if (rank$admin_level != 1) {
    
    # print("QUAQUA")
    
    delay(50, 
          rank$rank_value <- '<div class="text_compare"><i> Click on the map to see more info</i> </div>')
    
    delay(50, 
          rank$rank_text <- "")
    
  }
  
  
})



# THIS IR RUNNING DUPLICATE WHEN ADMIN LEVEL == 1 ----------------------------------------------
# THIS OBSERVER WOULD WORK TO COME BACK TO ADMIN LEVEL 1

observeEvent(c(rank$admin_level, input$map_marker_click, city$city_code, input$regions_grid, year$ok), {
  
  # waiter_show()
  # print(paste0("rank admin level"))
  # print(rank$admin_level)
  
  previous_city <- rv$prev_city[length(rv$prev_city)-1]
  
  req(data_ind3(), 
      city$city_code == previous_city,
      rank$admin_level >= 1)
  
  # print("gooooo")
  
  rank_indicator <- subset(data_ind3(), osmid == city$city_code)[1,]
  # it will run only when we are at the city level
  
  
  
  if (isTRUE(rank$admin_level == 1)) {
    
    
    # print("RUNNNNN")
    
    format_indicator_value <- format_indicator_values(rank_indicator$value, transformation = indicator_info$transformation)
    
    
    # set style for the text
    style_number <- function(x) {
      
      paste0('<strong style="font-size: 25px; color: #00AE42;">', x, '</strong>') 
      
    }
    style_text <- function(x) {
      
      paste0('<strong style="font-size: 20px; color: #00AE42;">', x, '</strong>') 
      
    }
    
    # create the indicator label for each indicator
    indicator_label <- switch(indicator$mode,
                              "popdensity" = sprintf("%s has a weighted population density of %s people per km2", 
                                                     style_text(rank_indicator$name), 
                                                     style_number(format_indicator_value)),
                              "blockdensity" = sprintf("%s has an average of %s blocks per km2", 
                                                       style_text(rank_indicator$name), 
                                                       style_number(format_indicator_value)),
                              "journeygap" = sprintf("In %s, the average trip takes %s times as long by walking, bicycling, or public transport as by driving.", 
                                                     style_text(rank_indicator$name), 
                                                     style_number(format_indicator_value)),
                              "pns" = sprintf("%s%s of people in %s live within 1km of both healthcare and education services.", 
                                              style_number(format_indicator_value), 
                                              style_number("%"), 
                                              style_text(rank_indicator$name)),
                              "pncf" = sprintf("%s%s of people in %s live within 100m of a car-free space.", 
                                               style_number(format_indicator_value), 
                                               style_number("%"), 
                                               style_text(rank_indicator$name)),
                              "pnh" = sprintf("%s%s of people in %s live farther than 500m from a grade-separated highway. ", 
                                              style_number(format_indicator_value), 
                                              style_number("%"), 
                                              style_text(rank_indicator$name)),
                              "pnpb" = sprintf('%s%s of people in %s live within a 300m walk of a protected bikeway. ', 
                                               style_number(format_indicator_value), 
                                               style_number("%"), 
                                               style_text(rank_indicator$name)),
                              "pnrt" = sprintf("%s%s of people in %s live within 1km of high-capacity public transport running on a dedicated right-of-way.", 
                                                  style_number(format_indicator_value), 
                                                  style_number("%"), 
                                                  style_text(rank_indicator$name)),
                              "pnft" = sprintf("%s%s of people in %s live within 500m of a transport stop where a bus or train comes every 10 minutes or sooner.", 
                                               style_number(format_indicator_value), 
                                               style_number("%"), 
                                               style_text(rank_indicator$name))
                              
    )
    
    
    rank$rank_value <- paste0(
      '<div class="title_indicator" style="font-size: 19px; font-weight: normal;">',
      indicator_label,
      '</div>'
    )
    
    # rank$rank_value <- rank$rank_value_initial
    rank$value <- rank$rank_value_initial
    rank$rank_text <- rank$rank_text_initial
    
    
    
  } else {
    
    # if (isTRUE(rank$admin_level != 1)) {
    
    rank$rank_value <- '<div class="text_compare"><i> Click on the map to see more info</i> </div>'
    rank$rank_text <- ""
    
  }
  
  
})




output$rank_final <- renderUI({
  
  # req(input$indicator)
  # print(input$indicator)
  
  if (input$indicator == "") {
    
    tagList(
      HTML('<div class="title_indicator" style="font-size: 22px;">Welcome to the Atlas of Urban Transportation</div>'),
      p(style = "color: #B1B5B9; display: inline; font-size: 16px;", "Start by selecting an indicator")
    )
    
  } else {
    
    base_text <- div(class = "title_indicator_label", style ="padding-bottom: 5px", 
                     "COMPARED TO OTHER REGIONS",
                     tags$button(
                       id = "tooltip_compare_right",
                       class="btn btn-light btn-xs",
                       style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                       icon("circle-info")
                       
                     ))
    
    
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
      base_text,
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