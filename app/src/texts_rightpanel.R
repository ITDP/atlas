
output$text_indicator <- renderUI({
  
  req(indicator_mode())
  
  includeHTML(sprintf("www/text/text_indicator_%s.html", indicator_mode()))
  
})

output$text_indicator2 <- renderUI({
  
  # str1 <- paste("You have selected", input$var)
  # str2 <- paste("You have chosen a range that goes from",
  #               input$range[1], "to", input$range[2])
  # HTML(paste(str1, str2, sep = '<br/>'))
  # 
  #   h3("People Near Protected Bikelanes")
  #   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
  
  req(indicator_mode())
  # print(indicator_mode())
  
  includeHTML(sprintf("www/text/text_indicator_more_%s.html", indicator_mode()))
  
})