
# popovers!
observe({
  
  # add necessary atrributes to create popover
  
  runjs("$('#indicator_bike :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_city :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_walk :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  runjs("$('#indicator_transit :button').attr({'data-container':'body', 'data-toggle':'popover'});")
  
  
  popdensity_title <- 'Population Density'
  popdensity_text <- 'Weighted population density – not the density of the average neighborhood, but the density of the neighborhood where the average person lives'
  
  blockdensity_title <- 'Block Density'
  blockdensity_text <- 'Average number of blocks per square kilometer'
  
  peoplennhighways_title <- 'Safety from Highways'
  peoplennhighways_text <- 'Percentage of people living at least 500m from a grade-separated highway'
  
  pnpb_title <- 'People Near Protected Bikeways'
  pnpb_text <- 'Percentage of people living within a 300-meter walk of a protected bicycle lane'
  
  pns_title <- 'People Near Services'
  pns_text <- 'Percentage of people living within 1km of healthcare and education'
  
  pnft_title <- 'People Near Frequent Transit'
  pnft_text <- 'Number of people living within 500m of a stop where public transport comes at least every ten minutes'
  
  pnrtall_title <- 'People Near Rapid Transit'
  pnrtall_text <- 'Number of people living within 1km of a rapid transit station'
  
  pnrtlrt_title <- 'People Near Rapid Transit - Light Rail'
  pnrtlrt_text <- 'Number of people living within 1km of a rapid transit station'
  
  pnrtmrt_title <- 'People Near Rapid Transit - Medium Rail'
  pnrtmrt_text <- 'Number of people living within 1km of a rapid transit station'
  
  pnrtbrt_title <- 'People Near Rapid Transit - Bus Rapid'
  pnrtbrt_text <- 'Number of people living within 1km of a rapid transit station'
  
  
  # create the titles and text for each popover
  
  runjs(sprintf("$('#indicator_city > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", popdensity_title, popdensity_text))
  runjs(sprintf("$('#indicator_city > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", blockdensity_title, blockdensity_text))
  runjs(sprintf("$('#indicator_city > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", peoplennhighways_title, peoplennhighways_text))
  # runjs(sprintf("$('#indicator_city > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  # runjs(sprintf("$('#indicator_city > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_bike > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", pnpb_title, pnpb_text))
  # runjs(sprintf("$('#indicator_bike > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  # runjs(sprintf("$('#indicator_bike > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  # runjs(sprintf("$('#indicator_bike > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_walk > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", pns_title, pns_text))
  # runjs(sprintf("$('#indicator_walk > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  # runjs(sprintf("$('#indicator_walk > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  # runjs(sprintf("$('#indicator_walk > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", text1, text2))
  
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(1) > button').attr({'title':'%s', 'data-content':'%s'});", pnft_title, pnft_text))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(2) > button').attr({'title':'%s', 'data-content':'%s'});", pnrtall_title, pnrtall_text))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(3) > button').attr({'title':'%s', 'data-content':'%s'});", pnrtlrt_title, pnrtlrt_text))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(4) > button').attr({'title':'%s', 'data-content':'%s'});", pnrtmrt_title, pnrtmrt_text))
  runjs(sprintf("$('#indicator_transit > div > div:nth-child(5) > button').attr({'title':'%s', 'data-content':'%s'});", pnrtbrt_title, pnrtbrt_text))
  
  
  
  
  
  
  runjs("    $('[data-toggle=\"popover\"]').popover(
          {trigger: 'hover', html: true, 
          viewport : {selector: 'indicator_bike .btn', padding: 0}, container: 'body'
          }
    );")
  
  
  # runjs("    $('#indicator_bike > div > div:nth-child(1) > button').popover(
  #   {trigger: 'hover', html: true,  title:'Popover title', content:'<p>Popover content.</p>', boundary: 'viewport', container: 'body'}
  # );")
  
  
  
})