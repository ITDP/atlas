# disable indicators
observeEvent(c(indicator_mode()), {
  
  delay(1, runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", true);'))
  delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", true);'))
  delay(1, runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", true);'))
  delay(1, runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", true);'))
  delay(1, runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", true);'))
  delay(1, runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", true);'))
  
  
})

observeEvent(c(city$city_code), {
  
  
  # disable indicators that are not available
  # shinyjs::disable()
  
  
  # if (input$city == "") {
  
  
  runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", true);')
  runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", true);')
  runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", true);')
  runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", true);')
  runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", true);')
  runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", true);')
  
  # }
  
  # } else if (city$city_code == "1406") {
  #   
  #   
  #   runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", false);')
  #   runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  #   runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", false);')
  #   runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", false);')
  #   
  #   
  #   runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", false);')
  #   runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  #   runjs('$("#indicator_walk > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  # }  else if(city$city_code == "1445") {
  #   
  #   runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", false);')
  #   runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  #   runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", false);')
  #   runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", false);')
  #   
  #   
  #   runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", false);')
  #   runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  #   runjs('$("#indicator_walk > div > div:nth-child(2) > button").attr("disabled", false);')
  #   
  #   
  #   
  # }
  
  
})



# create and change the 'map details' tab
# it's gonna react only when people change cities / select new indicator
observeEvent(c(indicator_mode(), city$city_code), {
  
  # adicionar o titulo 'map details'
  a <- "<div class='title_left_panel'>  MAP DETAILS  <button class='btn btn-default action-button minimize' id='teste3' style='float: right; padding: 0' type='button'><i class='fa fa-minus' role='presentation' aria-label='minus icon'></i> </button></div>"
  
  # remove o titulo que por acaso veio da interecao anterior (para evitar sobreposicao)
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers > .title_left_panel" ).remove();'))
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers-base > #title_base" ).remove();'))
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers-overlays > #title_overlay" ).remove();'))
  # adicionar o titul com o botao de minimizar
  
  delay(3, shinyjs::runjs('$( ".leaflet-control-layers-base" ).prepend( "<h3 id = \'title_base\' class = \'control-label\'>BASEMAP</h3>" );'))
  delay(2, shinyjs::runjs('$( ".leaflet-control-layers-overlays" ).prepend( "<h3 id = \'title_overlay\' class = \'control-label\'>OVERLAYS</h3>" );'))
  delay(1, shinyjs::runjs(sprintf('$( ".leaflet-control-layers" ).prepend( "%s");', a)))
  
  
  
  # inserir icone de cada um dos basemaps (em troco do texto)
  # a primeira imagem vai ser do basemap dark
  
  delay(1, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(2) input[type=radio] + img").remove()'))
  delay(1, shinyjs::runjs('$("<img src=\'https://via.placeholder.com/40x60/0bf/fff&text=A\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(2) input[type=radio]")'))
  # a segunda imagem vai ser do basemap light
  delay(1, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(3) input[type=radio] + img").remove()'))
  delay(1, shinyjs::runjs('$("<img src=\'https://via.placeholder.com/40x60/0bf/fff&text=A\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(3) input[type=radio]")'))
  # a terceira imagem vai ser do basemap satellite
  delay(1, shinyjs::runjs('$(".leaflet-control-layers-base > label:nth-child(4) input[type=radio] + img").remove()'))
  delay(1, shinyjs::runjs('$("<img src=\'https://via.placeholder.com/40x60/0bf/fff&text=A\' alt=\'Option 1\'>").insertAfter(".leaflet-control-layers-base > label:nth-child(4) input[type=radio]")'))
  # remover o texto
  delay(1, shinyjs::runjs('$( ".leaflet-control-layers-base span" ).remove();'))
  
})



# changes to be made to UI afterwards
observeEvent(c(input$admin_level, city$city_code), {
  
  
  # remove the iris tick markes on the slider
  delay(1, shinyjs::runjs('$(".irs-single").remove();'))
  
  # a <- tags$div(class = "title_left_panel", "MAP DETAILS",
  #          actionButton("teste3", label = "", icon = icon("minus"), style= "float: right; padding: 0",
  #                       class = "minimize")
  # 
  # )
  
  
}, ignoreInit = TRUE)

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

onclick("hide", "$('#right_panel').hide(\"slide\", {direction: \"right\")}, 1000);")


  


