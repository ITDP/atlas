about <- reactiveValues(input = NULL)

# identify from which input you came
observeEvent(c(input$about), {
  
  about$input <- "buttom"
  
}, priority = 10)

observeEvent(c(input$link1), {
  
  about$input <- "link"
  
}, priority = 10)


observeEvent(c(input$about, input$link1), {
  
  req(input$about >= 1 | input$link1 >= 1)
  
  print(about$input)
  
  # identify which tab to show by default
  tab_default <- ifelse(about$input == "buttom", "about_atlas", "about_data")
  
  showModal(modalDialog1(
    title = "ABOUT",
    # size = c("l"),
    easyClose = TRUE,
    footer = NULL,
    id1 = "modal_about",
    tabsetPanel(type = "tabs", id = "about_tabs", selected = tab_default,
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
  
}, priority = 2)