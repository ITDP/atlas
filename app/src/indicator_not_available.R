# this script is disabled for now - all cities have all the indicators


# function to disable indicators on the left panel
disable_indicators <- function(indicators) {

  # indicators <- c("poptotal", "density")
  poptotal <-    if("popdensity"      %in% indicators)  c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  density <-     if("blockdensity"  %in% indicators)    c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  journeygap <-  if("journeygap"     %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pnpb <-        if("pnpb"           %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pns <-         if("pns"            %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pncf <-        if("pncf"           %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pnnhighways <- if("pnnhighways"    %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pnft <-        if("pnft"           %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  pnrtall <-     if("pnrtall"        %in% indicators)   c("true", "default", "#B1B5B9") else  c("false", "not-allowed", "#36454F")
  

  #   popup <-  '$("#bs-select-1-2").append("<div id="pop-up"><p>Not available</p></div>")'
  #   popup_css <- '$("#bs-select-1-2").css{"display": "none", "position": "absolute", "width": "280px", "padding": "5px", "background": "#eeeeee", "color": "#000000", "border": "1px solid #1a1a1a", "font-size": "90%"}'
  # popup_jquery <-  '$(function() {
  #   var moveLeft = 20;
  #   var moveDown = 10;
  #   
  #   $("#bs-select-1-2").hover(function(e) {
  #     $("div#pop-up").show();
  #     //.css("top", e.pageY + moveDown)
  #     //.css("left", e.pageX + moveLeft)
  #     //.appendTo("body");
  #   }, function() {
  #     $("div#pop-up").hide();
  #   });
  #   
  #   $("#bs-select-1-2").mousemove(function(e) {
  #     $("div#pop-up").css("top", e.pageY + moveDown).css("left", e.pageX + moveLeft);
  #   });
  #   
  # });'
  

  # run code
  delay(1, runjs(sprintf('$("#bs-select-1-1").attr("disabled", "%s");' , poptotal[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-1").css("cursor","%s");', poptotal[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-1 > span").css("color", "%s");', poptotal[3])))
  
  delay(1, runjs(sprintf('$("#bs-select-1-2").attr("disabled", "%s");' , density[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-2").css("cursor","%s");', density[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-2 > span").css("color", "%s");', density[3])))
  
  delay(1, runjs(sprintf('$("#bs-select-1-3").attr("disabled", "%s");' , journeygap[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-3").css("cursor","%s");', journeygap[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-3").css("color","%s");', journeygap[3])))
  delay(1, runjs('$("#bs-select-1-3").attr({"title":"Indicator not available for this city",  "data-toggle":"tooltip"})'))
  delay(1, runjs('$("#bs-select-1-3").tooltip({"trigger": "hover", "animation": true, delay: {show: 1,hide: 10}, placement: "auto"})'))
  # delay(1, runjs('$("[data-toggle=\"popover\"]").popover({trigger: "hover", html: true})'))
  # delay(1, runjs(popup))
  # delay(1, runjs(popup_css))
  # delay(1, runjs(popup_jquery))
  
  
  
  
  delay(1, runjs(sprintf('$("#bs-select-1-6").attr("disabled", "%s");' , pnpb[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-6").css("cursor","%s");', pnpb[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-6 > span").css("color", "%s");', pnpb[3])))
  delay(1, runjs(sprintf('$("#bs-select-1-9").attr("disabled", "%s");' , pns[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-9").css("cursor","%s");', pns[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-9 > span").css("color", "%s");', pns[3])))
  delay(1, runjs(sprintf('$("#bs-select-1-10").attr("disabled", "%s");' , pncf[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-10").css("cursor","%s");', pncf[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-10 > span").css("color", "%s");', pncf[3])))
  delay(1, runjs(sprintf('$("#bs-select-1-11").attr("disabled", "%s");', pnnhighways[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-11").css("cursor","%s");', pnnhighways[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-11 > span").css("color", "%s");', pnnhighways[3])))
  delay(1, runjs(sprintf('$("#bs-select-1-14").attr("disabled", "%s");', pnft[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-14").css("cursor","%s");', pnft[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-14 > span").css("color", "%s");', pnft[3])))
  delay(1, runjs(sprintf('$("#bs-select-1-15").attr("disabled", "%s");', pnrtall[1])))
  delay(1, runjs(sprintf('$("#bs-select-1-15").css("cursor","%s");', pnrtall[2])))
  delay(1, runjs(sprintf('$("#bs-select-1-15 > span").css("color", "%s");', pnrtall[3])))
  
  
  
  
  
  # delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", %s);',poptotal )))
  # delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", %s);',density )))
  # delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", %s);',pnpb )))
  # delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", %s);',pnab )))
  # delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(3) > button").prop("disabled", %s);',abikeways )))
  # delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(4) > button").prop("disabled", %s);',pbikeways )))
  # delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", %s);',pnrtall )))
  # delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", %s);',pnrtbrt )))
  # delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", %s);',pnrtlrt )))
  # delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", %s);',pnrtmrt )))
  # delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", %s);',bikep45 )))
  # delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", %s);',walkp45 )))

}
# 
# 
observeEvent(c(city$city_code, indicator$mode), {


  req(city$city_code != "")

  # disable indicators that are not available
  # shinyjs::disable()


  disable_indicator_list <- function(code) {

    # get indicators available
    ind_available <-  subset(list_availability, hdc == code)$ind
    
    
    # print("ind_available")
    # print(ind_available)

    # apply the function
    disable_indicators(ind_available)


  }
  
  
  disable_indicator_list(city$city_code)
#   
#   
#   # print("OOOOOOOOOOOOIII")
#   
#   # get options to show in the comparison
#   # choices_comparison <- subset(list_osmid_name, admin_level == al)
#   # filter hdc with the indicators available
#   # choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
#   
#   if (city$city_code == c("1406"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("1445"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0621"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0561"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0154"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0088"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0088"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0200"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0634"))  disable_indicator_list(city$city_code)
#   if (city$city_code == c("0014"))  disable_indicator_list(city$city_code)
#   
#   
#   
#   
#   # # fortaleza and recife
#   # if (city$city_code %in% c("1406", "1445")) {
#   #   
#   #   
#   #   delay(1, runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", true);'))
#   #   
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(3) > button").prop("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(4) > button").prop("disabled", true);'))
#   #   
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", true);'))
#   #   
#   #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", false);'))
#   #   
#   #   
#   #   # chicago and LA
#   # } else if (city$city_code %in% c("0634", "0014")) {
#   #   
#   #   delay(1, runjs('$("#indicator_city > div > div:nth-child(1) > button").prop("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").prop("disabled", false);'))
#   #   
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", true);'))
#   #   
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", false);'))
#   #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", false);'))
#   #   
#   #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", true);'))
#   #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", true);'))
#   #   
#   
#   # }
#   
#   # } else if (city$city_code == "1406") {
#   #   
#   #   
#   #   runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   #   runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", false);')
#   #   
#   #   
#   #   runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   #   runjs('$("#indicator_walk > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   # }  else if(city$city_code == "1445") {
#   #   
#   #   runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   #   runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", false);')
#   #   
#   #   
#   #   runjs('$("#indicator_transit > div > div:nth-child(1) > button").attr("disabled", false);')
#   #   runjs('$("#indicator_transit > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   #   runjs('$("#indicator_walk > div > div:nth-child(2) > button").attr("disabled", false);')
#   #   
#   #   
#   #   
#   # }
#   
#   
})
# 
# 
# 
