# this script is disabled for now - all cities have all the indicators


# function to disable indicators on the left panel
disable_indicators <- function(indicators) {
  
  # indicators <- c("poptotal", "density")
  poptotal <- ifelse("poptotal" %in% indicators,   "false", "true")
  density <- ifelse("density" %in% indicators,     "false", "true")
  pnpb <- ifelse("pnpb" %in% indicators,           "false", "true")
  pnab <- ifelse("pnab" %in% indicators,           "false", "true")
  abikeways <- ifelse("abikeways" %in% indicators, "false", "true")
  pbikeways <- ifelse("pbikeways" %in% indicators, "false", "true")
  pnrtall <- ifelse("pnrtall" %in% indicators,     "false", "true")
  pnrtbrt <- ifelse("pnrtbrt" %in% indicators,     "false", "true")
  pnrtlrt <- ifelse("pnrtlrt" %in% indicators,     "false", "true")
  pnrtmrt <- ifelse("pnrtmrt" %in% indicators,     "false", "true")
  bikep45 <- ifelse("bikep45" %in% indicators,     "false", "true")
  walkp45 <- ifelse("walkp45" %in% indicators,     "false", "true")
  
  # run code
  delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", %s);',poptotal )))
  delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", %s);',density )))
  delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", %s);',pnpb )))
  delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", %s);',pnab )))
  delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(3) > button").prop("disabled", %s);',abikeways )))
  delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(4) > button").prop("disabled", %s);',pbikeways )))
  delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", %s);',pnrtall )))
  delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", %s);',pnrtbrt )))
  delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", %s);',pnrtlrt )))
  delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", %s);',pnrtmrt )))
  delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", %s);',bikep45 )))
  delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", %s);',walkp45 )))
  
}


observeEvent(c(city$city_code, indicator$mode), {
  
  
  req(city$city_code != "")
  
  # disable indicators that are not available
  # shinyjs::disable()
  
  
  disable_indicator_list <- function(code) {
    
    # get indicators available
    ind_available <-  subset(list_availability, hdc == code)$ind
    
    # apply the function
    disable_indicators(ind_available)
    
    
  }
  
  
  # print("OOOOOOOOOOOOIII")
  
  # get options to show in the comparison
  # choices_comparison <- subset(list_osmid_name, admin_level == al)
  # filter hdc with the indicators available
  # choices_comparison <- subset(choices_comparison, hdc %in% hdc_available)
  
  if (city$city_code == c("1406"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("1445"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0621"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0561"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0154"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0088"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0088"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0200"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0634"))  disable_indicator_list(city$city_code)
  if (city$city_code == c("0014"))  disable_indicator_list(city$city_code)
  
  
  
  
  # # fortaleza and recife
  # if (city$city_code %in% c("1406", "1445")) {
  #   
  #   
  #   delay(1, runjs('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", true);'))
  #   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", true);'))
  #   
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", false);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", false);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(3) > button").prop("disabled", true);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(4) > button").prop("disabled", true);'))
  #   
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", true);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", true);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", true);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", true);'))
  #   
  #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", false);'))
  #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", false);'))
  #   
  #   
  #   # chicago and LA
  # } else if (city$city_code %in% c("0634", "0014")) {
  #   
  #   delay(1, runjs('$("#indicator_city > div > div:nth-child(1) > button").prop("disabled", false);'))
  #   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").prop("disabled", false);'))
  #   
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", false);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", false);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(3) > button").attr("disabled", true);'))
  #   delay(1, runjs('$("#indicator_bike > div > div:nth-child(4) > button").attr("disabled", true);'))
  #   
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", false);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", false);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", false);'))
  #   delay(1, runjs('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", false);'))
  #   
  #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", true);'))
  #   delay(1, runjs('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", true);'))
  #   
  
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



