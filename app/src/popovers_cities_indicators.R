
add_popover_city <- function(position) {
  
  a1 <- sprintf('$("#bs-select-7-%i > span").after(\'<i style="float: right; color: red" class="fa-solid fa-triangle-exclamation"></i>\');', position)
  # print("a1")
  # print(a1)
  # add icon
  shinyjs::logjs(a1)
  delay(10000, runjs(a1))
  # runjs("var today = new Date(); alert(today);")
  # add popover property
  delay(11000, runjs(sprintf("$('#bs-select-7-%s').attr({'data-container':'body', 'data-toggle':'popover'});", position)))
  text2 <- 'Indicator not available for this city'
  
  # create the titles and text for each popover
  delay(12000, runjs(sprintf("$('#bs-select-7-%s').attr({'data-content':'%s'});", position, text2)))
  
  # format popover
  delay(14000, runjs(sprintf("    $('[data-toggle=\"popover\"]').popover(
          {trigger: 'hover', html: true, 
          viewport : {selector: '#bs-select-7-%s', padding: 0}, container: 'body'
          }
    );", position)))
  
  
  
  
}


# popovers!
observeEvent(c(indicator$mode, city$city_code), {
  
  # add necessary atrributes to create popover
  
  # print("bs-select-1-1")
  
  # if(indicator$mode == "pnpb") {
  # req(city$city_code != "")
  
  # print("rolou")
  # print(indicator$mode)
  
  # remove previous
  remove_previous <- function(position) {
    # runjs(sprintf("$('#bs-select-1-%s').removeAttr('data-container');", position))
    # runjs(sprintf("$('#bs-select-1-%s').removeAttr('data-toggle');", position))
    runjs(sprintf("$('#bs-select-7-%s > i').remove();", position))
    # delay(1, runjs(sprintf("$('#bs-select-1-%s').removeAttr('data-content');", position)))
    delay(2, runjs(sprintf("$('#bs-select-7-%s').popover('destroy');", position)))
    
    
  }
  
  # lapply(1:20, remove_previous)
  
  # add_popover_city_filter <- function(ind1) {
  #   
  #   # ind1 <- c("poptotal", "density")
  #   
  #   # get list of cities and get an id
  #   
  #   ind_teste <- subset(list_availability, ind %nin% ind1)
  #   
  # }
  
  
  if (indicator$mode %in% c("popdensity"))  {
    
    
    # lapply(c(1, 2, 5, 6, 10, 11), add_popover_city)
    
    add_popover_city(1)
    
  }
    
  
    
    
    
    
})