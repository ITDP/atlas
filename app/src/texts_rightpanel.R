
output$text_indicator <- renderUI({
  
  req(indicator$mode)
  
  # print("GOGAOGI")
  # print(rank$indicator)
  
  format_indicator_value <- format_indicator_values(rank$indicator$value, transformation = indicator_info$transformation)
  # print("AIAIAIAIA")
  # print(format_indicator_value)
  source(sprintf("www/text/text_indicator_%s.R", indicator$mode), local = TRUE)
  
  tagList(
    
    
    
    HTML(c(p1, p2, p3))
    
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