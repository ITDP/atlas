style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Frequent Transport</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Frequent Transport measures the percentage of an area’s population living within walking distance, 500 meters, of a transport station with service in both directions with headways of 10 minutes or less from 5 a.m. to 9 p.m. on a weekday. Such reliable public transit is necessary for it to be a consistent form of transportation for individuals. </p>',
sprintf('<p>In %s, there are %s transit stops where a bus, train, or other vehicle stops at least every 10 minutes throughout the day. %s of people live within 500m of one.</p>', 
        style(rank$indicator$name), 
        style(rank$indicator$transit_pnftpoints), 
        style(format_indicator_value)),
'<p>Frequent transport connects parts of the city which rapid transport cannot reach. This allows residents, including those with mobility impairments or young children, to reach far-off destinations without a car. Accessible transit is especially important for low-income people, who are less likely to own cars and who might not be able to access necessary services without transit.</p>',
'<p>This indicator corresponds to the United Nations’ Sustainable Development Goal 11.2: <em>By 2030, provide access to safe, affordable, accessible and sustainable transport systems for all, improving road safety, notably by expanding public transport, with special attention to the needs of those in vulnerable situations, women, children, persons with disabilities and older persons.</em> </p>'
)


        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>GTFS, or <a href="https://mobilitydata.org/data-standards/#what-is-gtfs">General Transit Feed Specification</a>, is the digital data format that represents transit maps and timetables. GTFS is usually published by transit agencies, although in some cases it is collected by advocates, researchers, or private firms.</p>',
'<p>The organization Mobility Data maintains a <a href="http://database.mobilitydata.org/">database</a> of over 1800 GTFS files from around the world. We reference that database, downloading GTFS files from it, and use them to identify transit stops where vehicles come every 10 minutes or better, 5 a.m. to 9 p.m. </p>',
'<p>If (part of) your city’s transport is not included here, you can help us add it by following the instructions in the <a href="#data-&-algorithms">data & algorithms section</a>.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Prioritize service expansion </strong>in densely-populated areas without reliable, frequent public transport.',
'<li><strong>Plan for population growth</strong> in areas already served by frequent transport.',
'<li><strong>Increase funding for public transport</strong>, providing sufficient subsidies for operation. Shift funding away from constructing new roads.',
'</div>'
)
