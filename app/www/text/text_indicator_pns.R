style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Services</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Services measures the percentage of an area’s population living within walking distance (1km) of some form of both healthcare and education. Proximity is the first requirement for walkability. In a city where people live within a 15-minute walk of their daily needs, they will be able to live without a car.  </p>',
sprintf('<p>In %s, we identified %s healthcare services and %s education services. %s %% of people live within a 1km walk of healthcare; %s %% of people live within a 1km walk of education, and %s %% of people live within a 1km walk of both.</p>', 
        style(rank$indicator$name), 
        style(rank$indicator$walk_pnshealthpoints), 
        style(rank$indicator$walk_pnsschoolspoints), 
        style(round(rank$indicator$walk_pnspnh * 100)), 
        style(round(rank$indicator$walk_pnspne * 100)),
        style(format_indicator_value)),
'<p>A high score on this indicator does not guarantee walkability. Infrastructure, like sidewalks and crosswalks, is also necessary. So is urban design, including street trees, lighting, and shade. ITDP’s <a target="_blank" href="http://pedestriansfirst.itdp.org/">Pedestrians First</a> Neighborhood- and Street-level tools provide an in-depth look at tools for a walkable city. </p>',
'<p>Walking is environmentally sustainable, cost-effective, <a target="_blank" href="https://www.vtpi.org/walkability.pdf">economically productive</a>, and beneficial for both <a target="_blank" href="https://www.emerald.com/insight/content/doi/10.1108/S2044-994120170000009004/full/html">physical</a> and <a target="_blank" href="https://pubmed.ncbi.nlm.nih.gov/29858467/">mental health</a>. </p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use <a target="_blank" href="http://openstreetmap.org/">OpenStreetMap</a> data to identify the locations of healthcare and educational facilities. For healthcare, we include pharmacies, doctors’ offices, hospitals, and other non-specialized healthcare. For schools, we include primary and secondary schools but not daycare or universities. We then count the fraction of people living within a 1km distance, in network distance along walkable paths, of at least one of each type of service. </p>',
'<p>Ideally, we would measure access to fresh food, banking, post offices, childcare, and other services. Worldwide data quality, unfortunately, is yet insufficient to make useful measurements.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Use zoning to permit or promote mixed land uses</strong>.',
'<li><strong>Adopt citywide goals</strong> that aim to provide daily amenities within walking distance of all residents.',
'<li><strong>Encourage or require <a target="_blank" href="https://itdpdotorg.wpengine.com/wp-content/uploads/2015/09/Densify_ITDP.pdf">high-density</a></strong> urban development so more people can live within 1km of a service. ',
'<li><strong>Co-locate social services and amenities of different kinds </strong>so that people can access daily needs on the same trip, as described in the <a target="_blank" href="https://bernardvanleer.org/app/uploads/2019/06/BvLF-StarterKit-Update-Digital-Single-Pages.pdf">Urban95 Starter Kit</a>.',
'<li><strong>Develop inclusive urban solutions </strong>which address the specific problems faced by caregivers that limit their <a target="_blank" href="https://www.brookings.edu/wp-content/uploads/2021/12/City-playbook_Bogota.pdf">political and economic participation</a>, such as <a target="_blank" href="https://oecd-opsi.org/innovations/bogota-care-blocks/#:~:text=The%20Care%20Blocks%20aim%20to,can%20pursue%20personal%20development%20activities.">Bogotá’s Blocks of Care</a>.',
'<li><strong>Encourage new uses of existing public spaces, </strong>such as municipal healthcare pavilions.',
'<li><strong>Promote smaller but more frequent service options,</strong> like vegetable stands or mobile healthcare stations.',
'</div>'
)
