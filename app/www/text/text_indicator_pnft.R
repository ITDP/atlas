style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Frequent Transport</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Frequent Transport measures the percentage of an area’s population living within walking distance (500m) of a transport station with service in both directions with headways of 10 minutes or less from 5 a.m. to 9 p.m. on a weekday. Such frequencies are necessary for public transport to be consistently reliable for people to make trips. </p>',
sprintf('<p>In %s in %s, there were %s transit stops where a bus, train, or other vehicle stops at least every 10 minutes throughout the day. %s%% of people live within 500m of one.</p>', 
        style(rank$indicator$name), 
        style(input$year), 
        style(scales::comma(rank$indicator$transit_pnftpoints)), 
        style(format_indicator_value)),
'<p>Frequent transport connects different city neighborhoods, allowing all residents, including those with mobility impairments or young children, to reach the wide variety of destinations necessary to live a fulfilling life.</p>',
'<p>This indicator can help us understand progress on the United Nations’ Sustainable Development Goal 11.2: <em>By 2030, provide access to safe, affordable, accessible, and sustainable transport systems for all, improving road safety, notably by expanding public transport, with special attention to the needs of those in vulnerable situations, women, children, persons with disabilities, and older persons.</em> </p>'
)


        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR FREQUENT TRANSPORT IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>GTFS, or <a target="_blank" href="https://mobilitydata.org/data-standards/#what-is-gtfs">General Transit Feed Specification</a>, is the digital data format that represents transit maps and timetables. GTFS is usually published by transit agencies, although in some cases it is collected by advocates, researchers, or private firms.</p>',
'<p>The organization Mobility Data maintains a <a target="_blank" href="http://database.mobilitydata.org/">database</a> of over 2,000 GTFS files from around the world. We reference that database, downloading GTFS files from it that we use to identify transit stops where vehicles come every 10 minutes or better, 5 a.m. to 9 p.m. </p>',
'<p>We use the street network from <a href="http://openstreetmap.org/">OpenStreetMap</a> to draw a walkshed of 500m around those transport stops, and we use <a href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> data to count the percentage of the city’s population within that walkshed. </p>',
sprintf('<p>If (part of) your city’s transport is not included here, you can help us add it by following the instructions in the %s section</p>', as.character(actionLink(inputId = "link1", label = "data & algorithms"))),
'</div>')       
        


p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Plan for an integrated network of frequent service</strong>.',
'<li><strong>Contract services effectively</strong> using<strong> </strong>gross-cost contracting, performance-based awards and penalties, independent fare collection, data-sharing provisions, a competitive tendering process, and multiple operators.',
'<li><strong>Prioritize service expansion </strong>in densely populated areas without reliable, frequent public transport.',
'<li><strong>Plan for population growth</strong> in areas already served by frequent transport.',
'<li><strong>Increase funding for public transport</strong>, providing sufficient subsidies for operation, combined with effective government oversight, via shifting funding away from constructing new roads.',
'</div>'
)
