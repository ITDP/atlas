style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">Block Density</div>',
'<div class = "text_indicator"><p>',
'<p>Block Density measures the total number of walkable blocks in the city divided by total size of those walkable blocks in km2. This excludes rivers, lakes, parks, and industrial zones that are not part of the urban fabric. A “walkable block” is an area surrounded on all sides by walkable streets or paths and is not bisected by any street or path.</p>',
sprintf('<p>In %s, %s had an average of %s blocks per square kilometer. %s</p>', 
        style(input$year), 
        style(rank$indicator$name), 
        style(format_indicator_value),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ), 

'<p><a href="https://staging.unhabitat.org/downloads/docs/StreetPatterns.pdf">Small blocks</a> make a neighborhood walkable by letting people take the most direct route to their destination, combine trips more easily, or choose the most pleasant route. In cities with smaller blocks, cars must move more slowly, increasing pedestrian safety. </p>',
'<p>Small blocks are <a href="https://www.researchgate.net/publication/318029894_Does_block_size_matter_The_impact_of_urban_design_on_economic_vitality_for_Chinese_cities">economically beneficial</a> because they create more locations, and therefore opportunities, for exchange and business. </p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW BLOCK DENSITY IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use data from <a href="http://openstreetmap.org/">OpenStreetMap</a> (OSM) to identify the spaces between streets as city blocks and count the number of city blocks per square kilometer. OSM data may be incomplete, and our algorithm cannot always distinguish between actual blocks and some road medians or traffic circles that are shaped like blocks.</p>',
'<p>We aggregate these blocks into larger tiles so we can render them in an online dashboard. Note that although some of these tiles might include uninhabited areas like large parks, lakes, or the ocean, the measurement of Block Density in a tile only includes the part of that tile that’s actually built up with city blocks.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong><strong>Set a citywide goal of having all blocks below a certain size</strong>. The size may vary between cities, but 12,000m<sup>2 </sup>(i.e., 110m x 110m) is an adequate target.',
'<li><strong><a target="_blank" href="https://ovacen.com/wp-content/uploads/2015/04/espacios-urbanos-y-paisajes.pdf">Plan for a dense street network</a></strong> by laying out a citywide network of streets and walkways with small blocks both beyond existing city limits and within the built-up area, and implement this network alongside any subdivision of property for development.',
'<li><strong>As neighborhoods redevelop, use a local planning process</strong> to allow small streets or pedestrian/bicycle paths to cut through existing blocks.',
'<li><strong>Enhance existing shortcuts</strong> used by the community, such as small streets, alleys, and stairs. Ensure that they can be effective parts of the pedestrian network by making them attractive, well lit, and safe.'
)
