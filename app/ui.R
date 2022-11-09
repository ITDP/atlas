list_bike <- structure(c("pnpb", "pnab", "abikeways", "pbikeways"), 
                       .Names = c("People Near Protected Bikelanes", "People Near All Bikelanes",
                                  "Bikeways", "Protected Bikeways"))

list_walk <- structure(c("pnh", "pne", "pns"), 
                       .Names = c("People Near Healthcare", "People Near Education", "People Near Services"))

list_transit <- structure(c("pnrtall", "pnrtlrt", "pnrtmrt", "pnrtbrt"), 
                          .Names = c("People Near Rapid Transit&nbsp;&nbsp;&nbsp;", 
                                     "People Near Rapid Transit - LRT&nbsp;&nbsp;&nbsp;",
                                     "People Near Rapid Transit - MRT&nbsp;&nbsp;&nbsp;",
                                     "People Near Rapid Transit - BRT&nbsp;&nbsp;&nbsp;"))

list_performance <- structure(c("bikep45", "walkp45"), 
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
  tags$head(includeCSS("www/compare.css")),
  tags$head(includeCSS("www/download.css")),
  
  # tags$head(includeScript("www/jquery.js")),
  # tags$head(
  #   tags$script(
  #     htmlwidgets::JS("$( document ).on('click', '.btn-default', function() {
  #                       Shiny.onInputChange('last_input', this.id);
  #                     });")
  #   )
  # ),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  # tags$script("
  #   $( document ).on('click', '#comparison_panel', function() {
  #   $('#comparison_panel').toggle('show');
  #   });
  # "),
  tags$head(tags$script("var ind_cum = ['bikep', 'walkp'];")),
  # tags$head(includeScript("www/popovers_indicators.js")),
  shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  use_waiter(),
  # waiter_on_busy(),
  # autoWaiter(    html = tagList(spin_loaders(id = 2, color = "black")),
  #                color = "rgba(233, 235, 240, .0)"),
  waiter_preloader(html = tagList(
    tags$img(src = "img/itdp_logo.png", style ="padding-bottom: 30px", width="150"), br(),
    spin_loaders(id = 2, color = "black")),
    color = "rgba(233, 235, 240, 1)"),
  # waiter_on_busy(html = tagList(spin_loaders(id = 2, color = "black")),
  #                color = "rgba(233, 235, 240, .05)"),
  # waiter_on_busy(spin_fading_circles()),
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
    # leafletOutput("map"),
    # create the napel to select city
    
    absolutePanel(id = "city_selection", 
                  # class = "panel panel-default", 
                  top = 20, left = 30, width = 400, height = 80,
                  # Output the 'UI' that was generated in the server
                  uiOutput('city_selection')
                  
                  
    ),
    # Create the left side panel  
    absolutePanel(class = "left_panel", 
                  bottom = 45, left = 15, width = 270, height = 'auto',
                  # Output the 'UI' that was generated in the server
                  # uiOutput('left_panel')
                  tags$div(class = "title_left_panel", "INDICATORS", 
                           actionButton("teste1", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                                        class = "minimize")
                           
                  ),
                  
                  # Create the left side panel  
                  absolutePanel(class = "left_panel_indicators", 
                                bottom = 5, left = -2, width = 260, height = 'auto',
                                
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
    # create the panel with the comparison
    uiOutput('comparison_button'),
    uiOutput('comparison_panel'),
    # right panel
    uiOutput('right_panel'),
    # back to world button
    # uiOutput('download_button'),
    uiOutput('download_button_maps'),
    # uiOutput('back_to_world_panel'),
    absolutePanel(
      class = "about_button",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, right = 730, width = 130, height = 40,
      actionButton(inputId = "back_to_world",
                   icon = icon("rotate-left"),
                   label = HTML("&nbsp;&nbsp;Reset map")
                   # selected = character(0)
      )
      
      
    ),
    # absolutePanel(
    #   class = "about_button",
    #   # class = "w3-container w3-animate-opacity", 
    #   # class = "panel panel-default",
    #   # fixed = TRUE, draggable = FALSE,
    #   top = 40, right = 870, width = 120, height = 40,
    #   actionButton(inputId = "print",
    #                icon = icon("download"),
    #                label = HTML("&nbsp;&nbsp;Export map")
    #                # selected = character(0)
    #   )
    #   
    #   
    # ),
    # about
    absolutePanel(
      class = "about_button",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, width = 70, height = 40, left = 370,
      style = "border: 0px; background: transparent",
      actionButton(inputId = "about",
                   label = "About"
                   # class = "about_button"
                   # selected = character(0)
      )
    ),
    absolutePanel(
      class = "about_button",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, width = 130, height = 40, right = 590,
      bookmarkButton(
        label = HTML("&nbsp;&nbsp;Share"),
        icon = icon("share"),
        id = "bookmark"
      )
    )
    
    
    
    
    
    
    # )
    
  )
)