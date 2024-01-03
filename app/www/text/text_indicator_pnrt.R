style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Rapid Transport</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Rapid Transport measures the percentage of an area’s population living within walking distance, 1km, of all forms of high-capacity public transport that run at high speeds in their own separate paths, such as metro trains, heavy and light rail, and bus rapid transit. Rapid transport is the backbone of a sustainable transportation system. </p>',
sprintf('<p>In <strong>%s</strong>, <strong>%s</strong> [has/had] a population of <strong>%s</strong> the following rapid transport infrastructure:', 
        style(input$year), 
        style(unique(rank$indicator$name)),
        ifelse(is.null(rank$indicator$city_popdensitytotal), "X", scales::number(rank$indicator$city_popdensitytotal, big.mark = ","))
        ), 
"<p>Metro / Heavy Rail:</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",     ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtkmmrt))),
sprintf("<li>%s kilometers per million residents", ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtrtrmrt))),
sprintf("<li>%s %% people within 1km of a station",   ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtmrt * 100))),
"</li>",
"</ul>",
"<p>Light Rail:</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",     ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtkmlrt))),
sprintf("<li>%s kilometers per million residents", ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtrtrlrt))),
sprintf("<li>%s %% people within 1km of a station",   ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtlrt * 100))),
"</li>",
"</ul>",
"<p>Bus Rapid Transport:</p>",
"<ul>",
sprintf("<li>%s kilometers of infrastructure",     ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtkmbrt))),
sprintf("<li>%s kilometers per million residents", ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtrtrbrt))),
sprintf("<li>%s %% people within 1km of a station",   ifelse(is.null(rank$indicator$transit_pnrtkmmrt), "X", round(rank$indicator$transit_pnrtbrt * 100))),
"</li>",
"</ul>",

"<p>Because rapid transport bypasses traffic congestion, it can be the fastest way to move around a city. Rapid transport can carry more people, farther and faster, than any other mode.</p>",
"<p>However, rapid transport can never serve every neighborhood in a city, and so other modes - including frequent transport, walking, and bicycling - are necessary for shorter trips, connections to rapid transport, or trips in less dense areas.</p>"
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use data from the <a href="https://www.thetransportpolitic.com/transitexplorer/">Transit Explorer</a> to identify the locations of rapid transport stations, and use the street network from <a href="http://openstreetmap.org/">OpenStreetMap</a> to count the number of people living within a 1km walk of those stations. </p>',
'<p>If you notice an error in the data representing your city, or if your city is not included in the Transit Explorer, please use the interface on <a href="https://www.thetransportpolitic.com/transitexplorer/#6/38.625/-78.673">that page</a> to submit a correction, or contact <a href="mailto:data@itdp.org">data@itdp.org</a>.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Extend existing rapid transit lines</strong> or build new ones in areas where the most people live.',
'<li><strong>Build more <a href="https://tod.itdp.org/what-is-tod.html">housing near rapid transport</a> stations</strong>, to make more efficient use of existing infrastructure.',
'<li><strong>Develop long-term plans </strong>for rapid transport.',
'<li><strong>Follow the <a href="https://www.itdp.org/library/standards-and-guides/the-bus-rapid-transit-standard/what-is-brt/">Bus Rapid Transit Standard</a> </strong>to build effective high-quality rapid transit at a much lower cost than metro rail.',
'</div>'
)
