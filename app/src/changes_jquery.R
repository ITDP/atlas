# disable the reset map and download button when in the world view
observeEvent(c(city$city_code), {
  
  # print("city$city_code")
  # print(city$city_code)
  # show("download_button_id")
  
  if (city$city_code == "") {
    
    disable("back_to_world")
    hide("download_city")
    # hide("download_overlay_panel")
    
  } else {
    
    show("download_city")
    # show("download_button_id")
    enable("back_to_world")
  }
  
  disable("bookmark")
  
})

observeEvent(c(indicator$mode), {
  
  enable("download_dropdown_maps")
  enable("downloadData_countries_csv")
  enable("downloadData_countries_gpkg")
  enable("downloadData_cities_csv")
  enable("downloadData_cities_gpkg")
  enable("downloadData1_csv")
  enable("downloadData1_gpkg")
  enable("downloadData2_csv")
  enable("downloadData2_gpkg")
  enable("download_overlay")
  
})

# 
observeEvent(c(input$back_to_world), {
  
  
  
  # hide("download_button_id")
  
  
})




# # changes to be made to UI afterwards
# observeEvent(c(input$admin_level, city$city_code), {
#   
#   
#   # remove the iris tick markes on the slider
#   delay(1, shinyjs::runjs('$(".irs-single").remove();'))
#   
#   # a <- tags$div(class = "title_left_panel", "MAP DETAILS",
#   #          actionButton("teste3", label = "", icon = icon("minus"), style= "float: right; padding: 0",
#   #                       class = "minimize")
#   # 
#   # )
#   
#   
# }, ignoreInit = TRUE, label = "TESTE")

# collpase the the indicators inside each indicator header - and keep the header
onclick("teste1", runjs("if(!$('#indicator_bike').hasClass('in'))
    {
    // alert('Collapsed');
    $('#indicator_bike, #indicator_walk, #indicator_transit, #indicator_city, #indicator_performance').collapse('show');
    }
    else {
    // alert('Collapsed');
    $('#indicator_bike, #indicator_walk, #indicator_transit, #indicator_city, #indicator_performance').not('active').collapse('hide');
    }
    "))
onclick("teste2", runjs("$('#spatial_level > div > div > div.form-group.shiny-input-container > span').slideToggle('')"))
onclick("teste3", runjs("$('.leaflet-control-layers > .leaflet-control-layers-list').slideToggle('')"))
onclick("teste4", runjs("$('.leaflet-control-layers > .leaflet-control-layers-list').slideToggle('')"))

onclick("hide", "$('#right_panel').hide(\"slide\", {direction: \"right\")}, 1000);")


# minimize the level of detail when the comparison tab is open

observeEvent(c(input$comparison_button), {
  
  
  req(input$comparison_button >= 1)
  
  runjs("$('#spatial_level > div > div > div.form-group.shiny-input-container > span').slideToggle('')")
  
  
})

# create and change the 'map details' tab
# it's gonna react only when people change cities / select new indicator
observeEvent(c(indicator$mode, city$city_code), {
  
  # adicionar o titulo 'map details'
  # a <- "<div class='title_left_panel'>  MAP DETAILS  <button class='btn btn-default action-button minimize' id='teste3' style='float: right; padding: 0' type='button'><i class='fa fa-minus' role='presentation' aria-label='minus icon'></i> </button></div>"
  
  # remove o titulo que por acaso veio da interecao anterior (para evitar sobreposicao)
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers > .title_left_panel" ).remove();'))
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers-base > #title_base" ).remove();'))
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers-overlays > #title_overlay" ).remove();'))
  # delay(1, shinyjs::runjs('$(".leaflet-control-layers-overlays > #teste12").remove();'))
  # adicionar o titul com o botao de minimizar
  
  delay(3, shinyjs::runjs('$( ".leaflet-control-layers-base" ).prepend( "<h3 id = \'title_base\' class = \'control-label\'>BASEMAP</h3>" );'))
  delay(2, shinyjs::runjs('$( ".leaflet-control-layers-overlays" ).prepend( "<h3 id = \'title_overlay\' class = \'control-label\'>ELEMENTS</h3>" );'))
  # delay(4, shinyjs::runjs('$(".leaflet-control-layers-overlays").append("<button class=\'btn btn-default action-button shiny-bound-input\' id=\'teste12\' type=\'button\'>Add population</button>")'))
  # delay(1, shinyjs::runjs(sprintf('$( ".leaflet-control-layers" ).prepend( "%s");', a)))
  
  
  
  # inserir icone de cada um dos basemaps (em troco do texto)
  # a primeira imagem vai ser do basemap dark
  
  # delay(1, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(2) input[type=radio] + img").remove()'))
  # delay(2, shinyjs::runjs('$("<img src=\'img/background_dark.png\' width=\'60\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(2) input[type=radio]")'))
  # # a segunda imagem vai ser do basemap light
  # delay(3, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(3) input[type=radio] + img").remove()'))
  # delay(4, shinyjs::runjs('$("<img src=\'img/background_light.png\' width=\'60\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(3) input[type=radio]")'))
  # # a terceira imagem vai ser do basemap satellite
  # delay(5, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(4) input[type=radio] + img").remove()'))
  # delay(6, shinyjs::runjs('$("<img src=\'img/background_sattelite.png\' width=\'60\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(4) input[type=radio]")'))
  # remover o texto
  # delay(7, shinyjs::runjs('$( ".leaflet-control-layers-base span" ).remove();'))
  
})

