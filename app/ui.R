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
  "pncf"
), 
.Names = c(
  # "People Near Healthcare", 
  # "People Near Education", 
  "People Near Services",
  "People New Car Free Places"
  ))

list_transit <- structure(c("pnft", "pnrtall", "pnrtlrt", "pnrtmrt", "pnrtbrt"), 
                          .Names = c(
                            "People Near Frequent Transit&nbsp;&nbsp;&nbsp;", 
                            "People Near Rapid Transit&nbsp;&nbsp;&nbsp;", 
                            "People Near Rapid Transit - LRT&nbsp;&nbsp;&nbsp;",
                            "People Near Rapid Transit - MRT&nbsp;&nbsp;&nbsp;",
                            "People Near Rapid Transit - BRT&nbsp;&nbsp;&nbsp;"))

list_performance <- structure(c("bikep45", "walkp45"), 
                              .Names = c("Bicycle&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", "Walk"))

list_city <- structure(c(
  # "poptotal", 
  "popdensity",
  "blockdensity",
  "pnnhighways"
), 
.Names = c(
  # "Block Density", 
  "Population Density&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
  "Block Density&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
  "People Not Near Highways"
))


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
  tags$script(src="https://kit.fontawesome.com/57bce24588.js", crossorigin="anonymous"),
  # tags$head(includeScript("www/popovers_indicators.js")),
  shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  use_waiter(),
  autoWaiter(c("comparison_chart"),
             html = tagList(spin_loaders(id = 2, color = "black")),
             color = "rgba(233, 235, 240, .2)"),
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
                                                selected = character(0))
                                # for now, this indicators will not be available
                                # accordion_input(inputId = "indicator_performance",
                                #                 label = "Performance",
                                #                 choices = c(list_performance),
                                #                 selected = character(0))
                                
                  )
                  
                  
                  
    ),
    # create the panel with the filters
    # uiOutput('left_panel_filter'), # da pra trazer
    tagList(
      # conditionalPanel(
      # condition = "ind_cum.indexOf(input.indicator_performance) > -1",
      # condition = "typeof input.indicator_performance != ''",
      absolutePanel(
        id = "year_panel",
        class = "spatial_level",
        # fixed = TRUE, draggable = FALSE,
        bottom = 45, left = 300, height = 'auto', width = 120,
        # 'typeof undefined' identifies when is null 
        tags$div(class = "title_left_panel", style = "padding: 10px 0", "YEAR" ,
                 # actionButton("teste5", label = "", icon = icon("minus"), style= "float: right; padding: 0",
                 # class = "minimize")
                 tags$button(
                   id = "tooltip_year",
                   class="btn btn-light btn-xs",
                   style = "display: inline; width: 5px; background: transparent; padding: 0 1px; color: #00AE42; font-size: 14px",
                   icon("circle-info")
                   
                 ),
        ),
        div(
          bsPopover(id = "tooltip_year",
                    # title = sprintf("<strong>%s</strong>", "LEVEL OF DETAIL"),
                    title = "",
                    content = HTML(includeHTML('www/tooltips/tooltip_year.html')),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
          )
        ),
        shinyWidgets::pickerInput(inputId = "year",
                                  label = NULL,
                                  choices = 2022,
                                  selected = 2022,
                                  options = shinyWidgets::pickerOptions(
                                    size = 5
                                  )
                                  # selected = character(0)
        )
        
      )
      
    ),
    
    
    # create the panel with the spatial scale
    uiOutput('spatial_level'),
    # create the panel with the comparison
    # uiOutput('comparison_button'),
    absolutePanel(
      id = "compare_panel",
      class = "spatial_level",
      style = "background: #00AE42; display: none",
      # class = "w3-container w3-animate-opacity", 
      # class = "panel panel-default",
      # fixed = TRUE, draggable = FALSE,
      bottom = 45, left = 440, height = 'auto', width = 130,
      # tags$div(class = "title_left_panel", 
      #          # "COMPARE", 
      #          actionButton("maximize_comparison", label = "", icon = icon("plus"), style= "float: right; padding: 0",
      #                       class = "minimize"),
      #          actionButton("teste4", label = "", icon = icon("minus"), style= "float: right; padding: 0; padding-right: 10px;",
      #                       class = "minimize")
      # ),
      actionButton(inputId = "comparison_button", 
                   label = "COMPARE",
                   style = "display: inline; padding-right: 2px; padding-left: 2px;"),
      tags$button(
        id = "tooltip_compare",
        class="btn btn-light btn-xs",
        style = "display: inline; width: 5px; background: transparent; padding-left: 0; color: #1C1C1C; font-size: 14px",
        icon("circle-info")
        
      ),
      # label = label_with_info("COMPARE", tooltip_id = "tooltip_compare")
      # , onclick = '$("#comparison_panel").toggle("show");'
      
      div(
        bsPopover(id = "tooltip_compare",
                  title = "",
                  content = HTML(includeHTML('www/tooltips/tooltip_comparison.html')),
                  placement = "top",
                  trigger = "hover",
                  options = list(container = "body"))
      )
    ),
    
    
    
    uiOutput('comparison_panel'),
    # right panel
    
    # uiOutput('right_panel'), # da pra trazer
    tagList(
      
      # conditionalPanel(
      # condition = "typeof input.indicator_bike !== 'undefined'",
      absolutePanel(
        class = "right_panel",
        id = "right_panel_id",
        # class = "w3-container w3-animate-opacity", 
        # class = "panel panel-default",
        # fixed = TRUE, draggable = FALSE,
        top = 0, right = 0, width = 310, height = "calc(100vh - 15px)",
        tabsetPanel(type = "tabs", id = "right_tabs",
                    tabPanel("OVERVIEW", value = "tab_overview",         
                             absolutePanel(
                               class = "right_panel_textbox",
                               id = "right_panel_textbox_id",
                               top = 65, right = 5, width = 285,
                               htmlOutput("text_indicator"),
                               tags$button(
                                 id = "link_see_more",
                                 class = "btn btn-default action-button shiny-bound-input",
                                 div(class = "link_button", "Read more")
                               ),
                               uiOutput("rank_value"),
                               uiOutput("rank_text")
                               
                             )
                             
                             
                             
                    ),
                    tabPanel("MORE INFO",  value = "tab_viewmore",                                
                             absolutePanel(
                               class = "right_panel_textbox",
                               id = "right_panel_textbox_id_more",
                               top = 65, right = 5, width = 285,
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
                  top = 40, right = 452, height = 40, width = 130,
                  dropdown(
                    tagList(
                      downloadButton("downloadData1", "Download indicator for this region", icon = NULL),
                      downloadButton("downloadData2", "Download all indicators for this region", icon = NULL)
                    ),
                    hr(),
                    tagList(
                      downloadButton("download_overlay", "Download overlay for this indicator", icon = NULL)
                    ),
                    hr(),
                    actionButton("downloadDic", "Download Data Dictionary", 
                                 onclick = "location.href='https://www.ipea.gov.br/acessooportunidades/dados';"),
                    circle = FALSE, 
                    # status = "danger",
                    label = HTML("&nbsp;&nbsp;Download"),
                    icon = icon("download"),
                    right = TRUE,
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
      id = "about_button_id",
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
      id = "share_button_id",
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