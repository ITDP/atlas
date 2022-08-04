
# popovers!
observe({
  
  # add necessary atrributes to create popover
  
  runjs("$('#indicator_bike :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_city :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_walk :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_transit :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  
  
  text1 <- 'People Near Protected Bikelanes'
  text2 <- 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat'
  
  # create the titles and text for each popover
  runjs(sprintf("$('#indicator_bike > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_bike > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_bike > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_bike > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_city > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_city > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_city > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_city > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_walk > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_walk > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_walk > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_walk > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  
  
  
  
  
  runjs("    $('[data-toggle=\"popover\"]').popover(
          {trigger: 'hover', html: true, 
          viewport : {selector: 'indicator_bike .btn', padding: 0}, container: 'body'
          }
    );")
  
  
  # runjs("    $('#indicator_bike > div > div:nth-child(1) > button').popover(
  #   {trigger: 'hover', html: true,  title:'Popover title', content:'<p>Popover content.</p>', boundary: 'viewport', container: 'body'}
  # );")
  
  
  
})