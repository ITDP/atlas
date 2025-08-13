style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Services</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Services measures the percentage of an area’s population living within walking distance (1km) of some form of both healthcare and education services. Proximity is the first requirement for walkability. In a city where people live within a 15-minute walk of their daily needs, they will be able to live without a car. </p>',
'<p>Walking is environmentally sustainable, cost-effective, <a href="https://www.vtpi.org/walkability.pdf">economically productive</a>, and beneficial for both <a href="https://www.emerald.com/insight/content/doi/10.1108/S2044-994120170000009004/full/html">physical</a> and <a href="https://pubmed.ncbi.nlm.nih.gov/29858467/">mental health</a>.</p>',
sprintf('<p>In %s at the end of %s, we identified %s healthcare services and %s education services. We found that %s %% of urban residents live within a 1km walk of healthcare; %s %% of urban residents live within a 1km walk of education, and %s %% of urban residents live within a 1km walk of both. %s</p>', 
        style(rank$indicator$name), 
        style(input$year), 
        style(scales::comma(rank$indicator$walk_pnshealthpoints * 1)), 
        style(scales::comma(rank$indicator$walk_pnsschoolspoints * 1)), 
        style(round(rank$indicator$walk_pnspnh * 100)), 
        style(round(rank$indicator$walk_pnspne * 100)),
        style(format_indicator_value),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ),
'<p>A high score on this indicator does not guarantee walkability. Safe infrastructure, like wide sidewalks and raised crosswalks, is also necessary, as is urban design, including street trees, lighting, and shade. ITDP’s <a target="_blank" href="http://pedestriansfirst.itdp.org/">Pedestrians First</a> Neighborhood- and Street-level tools provide an in-depth look at tools for a walkable city. </p>',
'<p>Walking is environmentally sustainable, cost-effective, <a target="_blank" href="https://www.vtpi.org/walkability.pdf">economically productive</a>, and beneficial for both <a target="_blank" href="https://www.emerald.com/insight/content/doi/10.1108/S2044-994120170000009004/full/html">physical</a> and <a target="_blank" href="https://pubmed.ncbi.nlm.nih.gov/29858467/">mental health</a>. </p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR SERVICES IS CALCULATED</div>',
'<div class = "text_indicator2">',
'<p>We use data from <a href="http://openstreetmap.org/">OpenStreetMap</a> (OSM) to identify the spaces between streets as city blocks and count the number of city blocks per square kilometer. OSM data may be incomplete, and our algorithm cannot always distinguish between actual blocks and some road medians or traffic circles that are shaped like blocks.</p>',
'<p>We aggregate these blocks into larger tiles so we can render them in an online dashboard. Note that although some of these tiles might include uninhabited areas like large parks, lakes, or the ocean, the measurement of Block Density in a tile only includes the part of that tile that’s actually built up with city blocks. </p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Use zoning to permit or promote mixed land uses</strong>.',
'<li><strong>Adopt citywide goals and policies</strong> to provide daily amenities within walking distance of all residents.',
'<li><strong>Encourage or support <a href="https://itdpdotorg.wpengine.com/wp-content/uploads/2015/09/Densify_ITDP.pdf">high-density</a></strong> urban development so more people can live within 1km of a service. ',
'<li><strong>Co-locate social services and amenities of different kinds </strong>so that people can access daily needs on the same trip, as described in the <a href="https://bernardvanleer.org/app/uploads/2019/06/BvLF-StarterKit-Update-Digital-Single-Pages.pdf">Urban95 Starter Kit</a>. Develop plans that address the specific problems faced by caregivers that limit their <a href="https://www.brookings.edu/wp-content/uploads/2021/12/City-playbook_Bogota.pdf">political and economic participation</a>, such as <a href="https://oecd-opsi.org/innovations/bogota-care-blocks/#:~:text=The%20Care%20Blocks%20aim%20to,can%20pursue%20personal%20development%20activities.">Bogotá, Colombia’s Blocks of Care</a>.',
'<li><strong>Encourage new uses of existing public spaces, </strong>such as municipal healthcare pavilions.',
'<li><strong>Promote smaller but more dispersed service options,</strong> like vegetable stands or mobile healthcare stations.',
'</div>'
)
