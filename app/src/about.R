

observeEvent(c(input$about), {
  
  req(input$about >= 1)
  
  showModal(modalDialog(
    title = "ABOUT",
    size = c("l"),
    easyClose = TRUE,
    footer = NULL,
    tabsetPanel(type = "tabs", id = "about_tabs",
                tabPanel("THE ATLAS PROJECT",  value = "about_atlas",                                
                         absolutePanel(
                           class = "about_modal",
                           includeHTML("www/about/about_atlas.html"),
                           height = 300,
                           # top = 80, right = 0, width = 280,
                           # htmlOutput("text_indicator2")
                           
                         )
                ),
                tabPanel("DATA & ALGORITHMS",  value = "about_data",                                
                         absolutePanel(
                           class = "about_modal",
                           includeHTML("www/about/about_data.html"),
                           height = 300
                           # top = 80, right = 0, width = 280,
                           # htmlOutput("text_indicator2")
                           
                         )
                ),
                tabPanel("FAQ",  value = "about_data",                                
                         absolutePanel(
                           class = "about_modal",
                           includeHTML("www/about/about_faq.html"),
                           height = 300
                           # top = 80, right = 0, width = 280,
                           # htmlOutput("text_indicator2")
                           
                         )
                ),
                tabPanel("ABOUT ITDP", value = "about_itdp",         
                         absolutePanel(
                           class = "about_modal",
                           includeHTML("www/about/about_itdp.html"),
                           height = 300
                           
                         )
                         
                         
                ),
                tabPanel("THE AUTHORS", value = "about_itdp",         
                         absolutePanel(
                           class = "about_modal",
                           includeHTML("www/about/about_authors.html"),
                           height = 300
                           
                         )
                         
                         
                )
                
                
    )
  ))
  
})