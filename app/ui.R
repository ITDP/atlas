list_bike <- structure(c("pnpb" 
                         # "pnab", 
                         # "abikeways", 
                         # "pbikeways"
), 
.Names = c("People Near Protected Bikelanes"
           # "People Near All Bikelanes",
           # "Bikeways", 
           # "Protected Bikeways"
))

list_walk <- structure(c(
  # "pnh", 
  # "pne", 
  "pns",
  "pncf",
  "pnnhighways"
), 
.Names = c(
  # "People Near Healthcare", 
  # "People Near Education", 
  "People Near Services",
  "People Near Car-Free Places",
  "People Not Near Highways"
  ))

list_transit <- structure(c("pnft", "pnrtall"
                            # "pnrtlrt", "pnrtmrt", "pnrtbrt"
                            ), 
                          .Names = c(
                            "People Near Frequent Transit", 
                            "People Near Rapid Transport"
                            # "People Near Rapid Transport - LRT",
                            # "People Near Rapid Transport - MRT",
                            # "People Near Rapid Transport - BRT"
                            ))

list_performance <- structure(c("bikep45", "walkp45"), 
                              .Names = c("Bicycle", "Walk"))

list_city <- structure(c(
  # "poptotal", 
  "popdensity",
  "blockdensity",
  "journeygap"
), 
.Names = c(
  # "Block Density", 
  "Population Density",
  "Block Density",
  "Journey Gap"
))


ui <- fluidPage(
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
  tags$script(src="https://kit.fontawesome.com/57bce24588.js", crossorigin="anonymous"),
  # tags$head(includeScript("www/popovers_indicators.js")),
  shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  use_waiter(),
  autoWaiter(c("comparison_chart"),
             html = tagList(spin_loaders(id = 2, color = "black")),
             color = "rgba(233, 235, 240, .1)"),
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
    # leafglOutput("map"),
    leafletOutput("map"),

    # create the panel with the comparison
    uiOutput('comparison_panel'),
    
    # right panel
    tagList(
      
      # conditionalPanel(
      # condition = "typeof input.indicator_bike !== 'undefined'",
      absolutePanel(
        class = "right_panel",
        id = "right_panel_id",
        # class = "w3-container w3-animate-opacity", 
        # class = "panel panel-default",
        # fixed = TRUE, draggable = FALSE,
        top = 0, left = 0, width = 310, height = "calc(100vh - 15px)",
        tabsetPanel(id = "right_tabs", type = "hidden",
                    tabPanelBody(value = "tab_overview",         
                             absolutePanel(
                               class = "right_panel_textbox",
                               id = "right_panel_textbox_id",
                               top = 25, right = 5, width = 285,
                               # indicator selection
                               shinyWidgets::pickerInput(inputId = "indicator",
                                                         label = "INDICATOR",
                                                         width = "250px",
                                                         choices = list("City" = list_city, "Bike" = list_bike, "Walk" = list_walk, "Transit" = list_transit),
                                                         options = shinyWidgets::pickerOptions(size = 15,
                                                                                               iconBase = "fa",
                                                                                               tickIcon = "fa-check",
                                                                                               title = "Search for a indicator",
                                                                                               liveSearch = TRUE)
                               ),
                               uiOutput('year'),
                               uiOutput('city_selection'),
                               # create the panel with the spatial scale
                               uiOutput('spatial_level'),
                               uiOutput("rank_final"),
                               absolutePanel(
                                 id = "compare_panel",
                                 class = "spatial_level",
                                 style = "background: #00AE42; display: none; text-align: center;",
                                 # class = "w3-container w3-animate-opacity", 
                                 # class = "panel panel-default",
                                 # fixed = TRUE, draggable = FALSE,
                                 # bottom = 45, left = 10, 
                                 height = 50, width = 270,
                                 actionButton(inputId = "comparison_button", 
                                              label = "COMPARE",
                                              icon = icon("chart-simple"),
                                              style = "display: inline; padding-right: 2px; padding-left: 2px;"),
                                 tags$button(
                                   id = "tooltip_compare",
                                   class="btn btn-light btn-xs",
                                   style = "display: inline; width: 5px; background: transparent; padding-left: 0; color: #1C1C1C; font-size: 14px",
                                   icon("circle-info")
                                   
                                 ),
                                 div(
                                   bsPopover(id = "tooltip_compare",
                                             title = "",
                                             content = HTML(includeHTML('www/tooltips/tooltip_comparison.html')),
                                             placement = "top",
                                             trigger = "hover",
                                             options = list(container = "body"))
                                 )
                               ),
                               htmlOutput("text_indicator")
                               # uiOutput("rank_value"),
                               # uiOutput("rank_text")
                               
                             )
                             
                             
                             
                    ),
                    tabPanelBody(value = "tab_viewmore",                                
                             absolutePanel(
                               class = "right_panel_textbox",
                               id = "right_panel_textbox_id_more",
                               top = 25, right = 5, width = 285,
                               
                               htmlOutput("text_indicator2")
                               
                             )
                    )
                    
                    
                    # )
                    
        )
        
      )
    ),
    
    
    
    # back to world button
    # uiOutput('download_button'),
    # uiOutput('download_button_maps'), # da pra trazer
    absolutePanel(class = "about_button", 
                  id = "download_button_id",
                  style = "background: #00AE42",
                  top = 40, left = 750, height = 40, width = 130,
                  dropdown(
                    tagList(
                      downloadButton("downloadData1", "Download indicator for this region", icon = NULL),
                      downloadButton("downloadData2", "Download all indicators for this region", icon = NULL)
                    ),
                    hr(),
                    tagList(
                      downloadButton("download_overlay", "Download overlay for this indicator", icon = NULL)
                    ),
                    # hr(),
                    # actionButton("downloadDic", "Download Data Dictionary", 
                    #              onclick = "window.open('https://www.ipea.gov.br/acessooportunidades/dados');"),
                    circle = FALSE, 
                    # status = "danger",
                    label = HTML("&nbsp;&nbsp;Download"),
                    icon = icon("download"),
                    right = FALSE,
                    up = FALSE,
                    # icon = icon("download"), 
                    width = "350px",
                    # tooltip = tooltipOptions(title = "Click to see inputs !"),
                    inputId = "download_dropdown_maps"
                    
                  )
    ),
    
    # uiOutput('back_to_world_panel'),
    absolutePanel(
      class = "about_button",
      id = "back_to_world_button_id",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, left = 470, width = 130, height = 40,
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
      id = "about_button_id",
      style = "background: #00AE42",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, width = 130, height = 40, left = 330,
      # style = "border: 0px; background: transparent",
      actionButton(inputId = "about",
                   label = "About"
                   # class = "about_button"
                   # selected = character(0)
      )
    ),
    absolutePanel(
      class = "about_button",
      id = "share_button_id",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      top = 40, width = 130, height = 40, left = 610,
      bookmarkButton(
        label = HTML("&nbsp;&nbsp;Share"),
        icon = icon("share"),
        id = "bookmark"
      )
    ),
    verbatimTextOutput("auth_output")
    
    
    
    
    
    
    # )
    
  )
)


# ui <- secure_app(ui)