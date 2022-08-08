list_bike <- structure(c("pnpb", "pnab", "abikeways", "pbikeways"), 
                       .Names = c("People Near Protected Bikelanes", "People Near All Bikelanes",
                                  "Bikeways", "Protected Bikeways"))

list_walk <- structure(c("pnh", "pne", "pns"), 
                       .Names = c("People Near Healthcare", "People Near Education", "People Near Services"))

list_transit <- structure(c("pntt", "etct"), 
                          .Names = c("People Near Transit&nbsp;&nbsp;&nbsp;", "ETC"))

list_performance <- structure(c("bikep", "walkp"), 
                              .Names = c("Bicycle&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Walk"))

list_city <- structure(c("poptotal", "density"), 
                       .Names = c("Population&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;","Population Density"))


fluidPage(
  # this script will add elements to the right side of the topbar (links etc) ok
  # Load Css
  tags$head(includeCSS("www/styles.css")),
  tags$head(includeCSS("www/right_panel.css")),
  tags$head(includeCSS("www/left_panel.css")),
  tags$head(includeCSS("www/spatial_level.css")),
  tags$head(includeCSS("www/accordion.css")),
  tags$head(includeCSS("www/scrollbar.css")),
  tags$head(includeCSS("www/map_legend.css")),
  tags$head(includeCSS("www/city_selector.css")),
  tags$head(includeCSS("www/about.css")),
  tags$head(includeCSS("www/minimize.css")),
  tags$head(includeCSS("www/popover.css")),
  tags$head(includeCSS("www/map_details.css")),
  tags$head(includeCSS("www/modal.css")),
  
  # tags$head(includeScript("www/jquery.js")),
  tags$head(
    tags$script(
      htmlwidgets::JS("$( document ).on('click', '.btn-default', function() {
                        Shiny.onInputChange('last_input', this.id);
                      });")
    )
  ),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  tags$head(tags$script("var ind_cum = ['bikep', 'walkp'];")),
  # tags$head(includeScript("www/popovers_indicators.js")),
  shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  use_waiter(),
  waiter_preloader(html = tagList(
    tags$img(src = "img/itdp_logo.png", style ="padding-bottom: 30px", width="150"), br(),
    spin_loaders(id = 2, color = "black")),
                   color = "rgba(233, 235, 240, 1)"),
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
                    top = 10, left = 30, width = 400, height = 50,
                    # Output the 'UI' that was generated in the server
                    uiOutput('city_selection')
                    
                    
      ),
      # Create the left side panel  
      absolutePanel(class = "left_panel", 
                    bottom = 30, left = 32, width = 250, height = 'auto',
                    # Output the 'UI' that was generated in the server
                    # uiOutput('left_panel')
                    tags$div(class = "title_left_panel", "INDICATORS", 
                             actionButton("teste1", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                                          class = "minimize")
                             
                    ),
                    
                    # Create the left side panel  
                    absolutePanel(class = "left_panel_indicators", 
                                  bottom = 5, left = -2, width = 240, height = 'auto',
                                  
                                  accordion_input(inputId = "indicator_city",
                                                  label = "City",
                                                  choices = c(list_city),
                                                  selected = character(0)),
                                  accordion_input(inputId = "indicator_bike",
                                                  label = "Bike",
                                                  choices = c(list_bike),
                                                  selected = "pnpb"),
                                  
                                  accordion_input(inputId = "indicator_walk",
                                                  label = "Walk",
                                                  choices = c(list_walk),
                                                  selected = character(0)),
                                  accordion_input(inputId = "indicator_transit",
                                                  label = "Transit",
                                                  choices = c(list_transit),
                                                  selected = character(0)),
                                  accordion_input(inputId = "indicator_performance",
                                                  label = "Performance",
                                                  choices = c(list_performance),
                                                  selected = character(0))
                                  
                    )
                    
                 
                    
      ),
      # create the panel with the filters
      uiOutput('left_panel_filter'),
      # create the panel with the spatial scale
      uiOutput('spatial_level'),
      # right panel
      uiOutput('right_panel'),
      # back to world button
      uiOutput('back_to_world_panel'),
      # about
      absolutePanel(
        class = "about_panel",
        # class = "w3-container w3-animate-opacity", 
        # class = "panel panel-default",
        # fixed = TRUE, draggable = FALSE,
        top = 30, width = 60, height = 30,
        actionButton(inputId = "about",
                     label = "About",
                     class = "about_button"
                     # selected = character(0)
        )
      )
      
      
      
      
    # )
    
  )
)