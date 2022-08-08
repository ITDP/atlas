

observeEvent(c(input$about), {
  
  req(input$about >= 1)
  
  showModal(modalDialog(
    title = "ABOUT",
    size = c("l"),
    easyClose = TRUE,
    footer = NULL,
    tabsetPanel(type = "tabs", id = "about_tabs",
                tabPanel("ITDP", value = "about_itdp",         
                         absolutePanel(
                           class = "about_modal",
                           "Since our founding in 1985, ITDP has grown from a small band of cycling activists to a leading organization in the fight to curb climate change, and transform our streets for a better quality of life in our cities. Through high quality public transport, safe and pleasant spaces for walking & cycling, a focus on transit-oriented development, and progressive, people-centered policies, cities are able to provide more equitable access to economic opportunity, community, and culture for everyone, including women, children, and the elderly. ITDP is a global organization at the forefront of innovation, using technical expertise, direct advocacy, and policy guidance to mitigate the impacts of climate change, improve air quality, and support prosperous, sustainable, and equitable cities. We have worked with over 100 cities in more than 40 nations to design and implement transport and urban development systems and policy solutions that make cities more viable, fair, and livable."
                           # top = 80, right = 0, width = 280,
                           # htmlOutput("text_indicator"),
                           # tags$button(
                           #   id = "link_see_more",
                           #   class = "btn btn-default action-button shiny-bound-input",
                           #   div(class = "link_button", "Read more")
                           # ),
                           # htmlOutput("rank_value"),
                           # htmlOutput("rank_text"),
                           # actionButton(inputId = "hide",
                           #              label = "Hide"
                           #              # selected = character(0)
                           # )
                           
                         )
                         
                         
                ),
                tabPanel("THE ATLAS PROJECT",  value = "about_atlas",                                
                         absolutePanel(
                           class = "right_panel_textbox",
                           # top = 80, right = 0, width = 280,
                           # htmlOutput("text_indicator2")
                           
                         )
                )
                
                
    )
  ))
  
})