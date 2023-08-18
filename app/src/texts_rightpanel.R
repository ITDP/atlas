
output$text_indicator <- renderUI({
  
  req(indicator$mode, rank$indicator)
  
      format_indicator_value <- format_indicator_values(rank$indicator$value, transformation = indicator_info$transformation)
      source(sprintf("www/text/text_indicator_%s.R", indicator$mode), local = TRUE)
  
  tagList(
      
      
      
      HTML(c(p1, p2, p3))
      
      
    
    # # create the indicator label for each indicator
    # indicator_text <- switch(indicator$mode,
    #                           "popdensity" = sprintf("%s [has/had] a total population of %s in %s, and the average resident [lives/lived] in a neighborhood with a density of %s people per square kilometer.", 
    #                                                  rank_indicator$name, format_indicator_value, input$year, format_indicator_value),
    #                           "blockdensity" = "",
    #                           "pns" = sprintf("In %s, we identified %s healthcare services and %s education services. %s %% of people live within a 1km walk of healthcare; %s %% of people live within a 1km walk of education, and %s %% of people live within a 1km walk of both.", 
    #                                           rank_indicator$name, format_indicator_value, format_indicator_value, format_indicator_value, format_indicator_value, format_indicator_value),
    #                           "pncf" = "",
    #                           "pnh" = "",
    #                           "pnpb" = sprintf("In %s, %s %% of residents live within a 300m walk of any bikeway, protected or unprotected. Of those, %s %% live within a 300m walk of a physically-protected bikeway. In %s, there are %s kilometers of bikeways, of which %s are physically-protected.", 
    #                                            rank_indicator$name, format_indicator_value, format_indicator_value, rank_indicator$name, format_indicator_value, format_indicator_value),
    #                           "pnrtall" = sprintf("%s %% of people in %s live within 1km of high-capacity public transport running on a dedicated right-of-way.", format_indicator_value, rank_indicator$name),
    #                           "pnft" = sprintf("%s %% of people in %s live within 500m of a transport stop where a bus or train comes every 10 minutes or sooner.", format_indicator_value, rank_indicator$name)
    #                           
    # ),
    
    
    
    # tags$button(
    #   id = "link_see_more",
    #   class = "btn btn-default action-button shiny-bound-input",
    #   div(class = "link_button", "Read more")
    # )
  )
  
})

output$text_indicator2 <- renderUI({
  
  # str1 <- paste("You have selected", input$var)
  # str2 <- paste("You have chosen a range that goes from",
  #               input$range[1], "to", input$range[2])
  # HTML(paste(str1, str2, sep = '<br/>'))
  # 
  #   h3("People Near Protected Bikelanes")
  #   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
  
  req(indicator$mode)
  # print(indicator_mode())
  
  tagList(
    
    tags$button(
      id = "teste_id_uh",
      class = "btn btn-default action-button shiny-bound-input",
      icon("backward")
    ),
    includeHTML(sprintf("www/text/text_indicator_more_%s.html", indicator$mode))
  )
  
})