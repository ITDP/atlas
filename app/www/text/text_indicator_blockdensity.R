p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">Block Density</div>',
'<div class = "text_indicator"><p>',
'<p>Block Density measures the total number of walkable blocks in the city divided by total size of those walkable blocks, in km<sup>2</sup>. This excludes rivers, lakes, parks, and industrial zones that are not part of the urban fabric. A “walkable block” is an area surrounded on all sides by walkable streets or paths and is not bisected by any street or path.</p>',
'<p><a target="_blank" href="https://staging.unhabitat.org/downloads/docs/StreetPatterns.pdf">Small blocks</a> make a neighborhood walkable by letting people take the most direct route to their destination, combine trips more easily, or choose the most pleasant route. On smaller blocks, cars move more slowly, increasing pedestrian safety. </p>',
'<p>Small blocks are economically beneficial because they create more locations, and therefore opportunities, for exchange and business. </p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use data from OpenStreetMap, identifying the spaces between streets as city blocks, and counting the number of city blocks per square kilometer. OSM data may be incomplete, and our algorithm cannot always distinguish between actual blocks and some road medians or traffic circles that are shaped like blocks.</p>',
'<p>We aggregate these blocks into larger tiles, to make it possible to render them in an online dashboard. Note that although some of these tiles might include uninhabited areas like large parks, lakes, or ocean, the measurement of block density in a tile only includes the part of that tile actually built-up with city blocks. </p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Set a citywide goal of having all blocks below a certain size</strong>. The size may vary between cities, but 12,000m<sup>2 </sup>(i.e., 110m x 110m) is an adequate target. ',
'<li><strong><a target="_blank" href="https://ovacen.com/wp-content/uploads/2015/04/espacios-urbanos-y-paisajes.pdf">Plan for urban expansion</a></strong> by laying out a network of streets and walkways with small blocks ahead of time, allowing for the densification necessary for walkability. ',
'<li><strong>As neighborhoods redevelop, use a local planning process</strong> to allow small streets or pedestrian/bicycle paths to cut through existing blocks.',
'<li><strong>Enhance existing shortcuts</strong> used by the community, such as small streets, alleys, and stairs. Ensure they can be effective parts of the pedestrian network by making them attractive, well lit, and safe.'
)
