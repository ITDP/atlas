style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Rapid Transport</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Rapid Transport measures the percentage of an area’s population living within 1km of rapid transport. We include all forms of high-capacity public transport that run at high speeds in their own separate paths, such as metro trains (aka subways or MRT/HRT), light rail, and <a href="http://brtstandard.org/">bus rapid transit</a>. As with most other indicators, we calculate the 1km in terms of walking distance along walkable streets or paths rather than a simple circle.</p>',
sprintf('<p>At the end of <strong>%s</strong>, %s%% of urban residents of <strong>%s</strong> lived within 1km of a rapid transport station. <strong>%s</strong> had the following rapid transport infrastructure:', 
        style(input$year), 
        unique(format_indicator_value),
        style(unique(rank$indicator$name)),
        style(unique(rank$indicator$name))
        ), 
"<p>Metro</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",        scales::comma(rank$indicator$transit_pnrtkmmrt)),
sprintf("<li>%s %% people within 1km of a station",   round(rank$indicator$transit_pnrtmrt * 100)),
"</li>",
"</ul>",
"<p>Light Rail:</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",     scales::comma(rank$indicator$transit_pnrtkmlrt)),
sprintf("<li>%s %% people within 1km of a station",   round(rank$indicator$transit_pnrtlrt * 100)),
"</li>",
"</ul>",
"<p>Bus Rapid Transport:</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",     scales::comma(rank$indicator$transit_pnrtkmbrt)),
sprintf("<li>%s %% people within 1km of a station",   round(rank$indicator$transit_pnrtbrt * 100)),
"</li>",
"</ul>",

ifelse(is.null(rank$admin_level), "<p>This measurement includes all urban agglomerations with a population of more than 500,000.</p>", ""),

"<p>Because rapid transport bypasses traffic congestion, it can be the fastest way to move around a city. Rapid transport can carry more people, farther and faster, than any other mode.</p>",
"<p>However, rapid transport can never serve every neighborhood in a city, and so other modes — including frequent transport, walking, and bicycling — are necessary for shorter trips, connections to rapid transport, or trips in less dense areas. This indicator is designed to help cities make better decisions by showing their existing systems and enabling comparisons to peer cities.</p>"
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR RAPID TRANSPORT IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use data from <a href="https://www.thetransportpolitic.com/transitexplorer/">Transit Explorer</a> to identify the locations of rapid transport stations. We align the Transit Explorer data with the definitions of rapid transport used in ITDP’s <a href="https://docs.google.com/spreadsheets/d/1uMuNG9rTGO52Vuuq6skyqmkH9U5yv1iSJDJYjH64MJM/edit#gid=1523036623">Rapid Transport Database</a>. In summary, this definition includes public transport that is separated from car traffic, is designed for high capacities, and has frequencies of at least every 20 minutes.</p>',
'<p>We use the street network from <a href="http://openstreetmap.org/">OpenStreetMap</a> to draw a walkshed of 1km around those transport stations, and we use <a href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> data to count the percentage of the city’s population within that walkshed.</p>',
'<p>If you notice an error in the data representing your city, or if your city is not included in Transit Explorer, please use the interface on <a href="https://www.thetransportpolitic.com/transitexplorer/#6/38.625/-78.673">that page</a> to submit a correction, or contact <a href="mailto:data@itdp.org">data@itdp.org</a>.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Extend existing rapid transit lines</strong> or build new lines or stations in areas where the most people live.',
'<li><strong>Build more <a href="https://tod.itdp.org/what-is-tod.html">housing near rapid transport</a> stations</strong>, to make more efficient use of existing infrastructure.',
'<li><strong>Develop long-term plans </strong>for rapid transport.',
'<li><strong>Follow the <a href="https://www.itdp.org/library/standards-and-guides/the-bus-rapid-transit-standard/what-is-brt/">Bus Rapid Transit Standard</a> </strong>to build effective high-quality rapid transit at a much lower cost than metro rail.',
'</div>'
)
