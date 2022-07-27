fluidPage(
  # this script will add elements to the right side of the topbar (links etc) ok
  # Load Css
  tags$head(includeCSS("www/styles.css")),
  tags$head(includeCSS("www/right_panel.css")),
  tags$head(includeCSS("www/left_panel.css")),
  tags$head(includeCSS("www/spatial_level.css")),
  tags$head(includeCSS("www/accordion.css")),
  tags$head(includeCSS("www/scrollbar.css")),
  # tags$head(includeScript("www/bootstrap.js")),
  # tags$head(includeScript("www/jquery.js")),
  tags$head(
    tags$script(
      htmlwidgets::JS("$( document ).on('click', '.btn-default', function() {
                        Shiny.onInputChange('last_input', this.id);
                      });")
    )
  ),
  shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  # use_waiter(),
  # waiter_preloader(html = tagList(spin_loaders(id = 2, color = "black")), 
  #                  color = "rgba(233, 235, 240, .5)"),
  # Start navbar page
  fluidPage(
    # title = "Atlas of Urban Transportation", id = "tabs", collapsible = TRUE,
    # Map page
    # tabPanel(
      # Output map
      # title = "Atlas", 
      # value = "tab_general",
      # Output the map
      leafletOutput("map"),
      # create the napel to select city
      
      absolutePanel(id = "city_selection", 
                    # class = "panel panel-default", 
                    fixed = TRUE, draggable = FALSE,
                    top = 10, left = 30, width = 400, height = 50,
                    # Output the 'UI' that was generated in the server
                    uiOutput('city_selection')
                    
                    
      ),
      # Create the left side panel  
      absolutePanel(class = "left_panel", 
                    bottom = 30, left = 32, width = 250, height = 415,
                    # Output the 'UI' that was generated in the server
                    uiOutput('left_panel')
                 
                    
      ),
      # create the panel with the spatial scale
      uiOutput('spatial_level'),
      # right panel
      uiOutput('right_panel')
      
    # )
    
  )
)