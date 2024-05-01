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

list_transit <- structure(c("pnft", "pnrt"
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
  "blockdensity"
  # "journeygap"
), 
.Names = c(
  # "Block Density", 
  "Population Density",
  "Block Density"
  # "Journey Gap"
))

js <- "
var mytips = ['HELLO I AM TOOLTIP 1', 'HI I AM TOOLTIP 2'];
$('#city').on('shown.bs.select', function() {
  var $lis = $($(this).data('selectpicker').selectpicker.current.elements);
  $lis.each(function(i) {
    $(this).attr('title', mytips[i]);
  });
});"

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
  tags$head(includeCSS("src/beta_checkpoint/beta_checkpoint.css")),
  tags$head(includeCSS("src/modal_brazilian_cities/modal_brazil.css")),
  tags$script(HTML(js)),
  
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
  # tags$head(
  # tags$script(
  #   "$(document).on('shiny:inputchanged', function(event) {
  #         if (event.name != 'changed') {
  #           Shiny.setInputValue('changed', event.name);
  #         }
  #       });"
  # )),
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
    tags$img(src = "img/atlas-logos-04.png", style ="padding-bottom: 30px; padding-right: 50px", width="200"),
    tags$img(src = "img/itdp_logo.png", style ="padding-bottom: 50px", width="130"), br(),
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
        top = 0, left = 0, width = 400, height = "calc(100vh - 15px)",
        tabsetPanel(id = "right_tabs", type = "hidden",
                    tabPanelBody(value = "tab_overview",         
                                 absolutePanel(
                                   class = "right_panel_textbox",
                                   id = "right_panel_textbox_id",
                                   top = 25, right = 5, width = 375,
                                   # indicator selection
                                   shinyWidgets::pickerInput(inputId = "indicator",
                                                             label = "INDICATOR",
                                                             width = "330px",
                                                             choices = list("City" = list_city, "Bike" = list_bike, "Walk" = list_walk, "Transit" = list_transit),
                                                             options = shinyWidgets::pickerOptions(size = 15,
                                                                                                   iconBase = "fa",
                                                                                                   tickIcon = "fa-check",
                                                                                                   title = "Search for a indicator",
                                                                                                   liveSearch = TRUE,
                                                                                                   showContent = FALSE)
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
                                     height = 50, width = 360,
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
    
    
    
    absolutePanel(
      # class = "about_button",
      # id = "about_button_id",
      top = 40, height = 40, left = 420,
      tags$img(src = "img/atlas-logos-04_short.png", width="100", id = "logo_map"),
      style = "text-align: center; background: transparent",
    ),
    
    
    absolutePanel(
      class = "about_button",
      id = "about_button_id",
      style = "background: #00AE42",
      top = 40, width = 130, height = 40, left = 540,
      # style = "border: 0px; background: transparent",
      actionButton(inputId = "about",
                   label = "About"
                   # class = "about_button"
                   # selected = character(0)
      )
    ),

    
    
    # uiOutput('back_to_world_panel'),
    absolutePanel(
      class = "about_button",
      id = "back_to_world_button_id",
      top = 40, left = 680, width = 130, height = 40,
      actionButton(inputId = "back_to_world",
                   icon = icon("rotate-left"),
                   label = HTML("&nbsp;&nbsp;Reset map")
                   # selected = character(0)
      )
      
      
    ),

    absolutePanel(
      class = "about_button",
      id = "share_button_id",
      top = 40, width = 130, height = 40, left = 820,
      bookmarkButton(
        label = HTML("&nbsp;&nbsp;Share"),
        icon = icon("share"),
        id = "bookmark"
      )
    ),
    
    # back to world button
    absolutePanel(class = "about_button", 
                  id = "download_button_id",
                  style = "background: #00AE42; ",
                  top = 40, left = 960, height = 40, width = 130,
                  uiOutput('download_button')
    ),
    verbatimTextOutput("auth_output")
    
    
    
    
    
    
    # )
    
  )
)


# ui <- secure_app(ui)