# # few indicators are not available at the world view - make sure they are disabled
# 
# 
# 
# observeEvent(c(city$city_code, indicator$mode), {
#   
#   print("a1")
#   
#   req(city$city_code == "")
#   
#   print("a2")
#   
#   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").prop("disabled", true);' ))
#   delay(1, runjs('$("#indicator_city > div > div:nth-child(3) > button").prop("disabled", true);' ))
#   
#   
#   
# })
# 
# observeEvent(c(city$city_code), {
#   
#   # print("a1")
#   
#   req(city$city_code != "")
#   
#   # print("a2")
#   
#   delay(1, runjs('$("#indicator_city > div > div:nth-child(2) > button").prop("disabled", false);'))
#   delay(1, runjs('$("#indicator_city > div > div:nth-child(3) > button").prop("disabled", false);'))
#   
#   
#   
# })
# 
# 
# 
# 
# # # function to disable indicators on the left panel
# # disable_indicators <- function(indicators) {
# #   
# #   # indicators <- c("poptotal", "density")
# #   poptotal <- ifelse("poptotal" %in% indicators,   "false", "true")
# #   density <- ifelse("density" %in% indicators,     "false", "true")
# #   pnpb <- ifelse("pnpb" %in% indicators,           "false", "true")
# #   pnab <- ifelse("pnab" %in% indicators,           "false", "true")
# #   abikeways <- ifelse("abikeways" %in% indicators, "false", "true")
# #   pbikeways <- ifelse("pbikeways" %in% indicators, "false", "true")
# #   pnrtall <- ifelse("pnrtall" %in% indicators,     "false", "true")
# #   pnrtbrt <- ifelse("pnrtbrt" %in% indicators,     "false", "true")
# #   pnrtlrt <- ifelse("pnrtlrt" %in% indicators,     "false", "true")
# #   pnrtmrt <- ifelse("pnrtmrt" %in% indicators,     "false", "true")
# #   bikep45 <- ifelse("bikep45" %in% indicators,     "false", "true")
# #   walkp45 <- ifelse("walkp45" %in% indicators,     "false", "true")
# #   
# #   # run code
# #   delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(1) > button").attr("disabled", %s);',poptotal )))
# #   delay(1, runjs(sprintf('$("#indicator_city > div > div:nth-child(2) > button").attr("disabled", %s);',density )))
# #   delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(1) > button").attr("disabled", %s);',pnpb )))
# #   delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(2) > button").attr("disabled", %s);',pnab )))
# #   delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(3) > button").prop("disabled", %s);',abikeways )))
# #   delay(1, runjs(sprintf('$("#indicator_bike > div > div:nth-child(4) > button").prop("disabled", %s);',pbikeways )))
# #   delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(1) > button").prop("disabled", %s);',pnrtall )))
# #   delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(2) > button").prop("disabled", %s);',pnrtbrt )))
# #   delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(3) > button").prop("disabled", %s);',pnrtlrt )))
# #   delay(1, runjs(sprintf('$("#indicator_transit > div > div:nth-child(4) > button").prop("disabled", %s);',pnrtmrt )))
# #   delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(1) > button").prop("disabled", %s);',bikep45 )))
# #   delay(1, runjs(sprintf('$("#indicator_performance > div > div:nth-child(2) > button").prop("disabled", %s);',walkp45 )))
# #   
# # }
# 
# 
# 
