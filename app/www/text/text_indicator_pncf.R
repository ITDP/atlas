style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Car-Free Places</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Car-Free Places measures the percentage of an area’s population living immediately near (within 100m of) a car-free place. This includes parks, squares, car-free streets, recreation grounds, sports fields, and forests. </p>',
sprintf('<p>In %s at the end of %s, %s %% of people lived within a 100m buffer of a car-free park, plaza, street, or other space. %s</p>', 
        style(rank$indicator$name), 
        style(input$year), 
        style(format_indicator_value),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ),
'<p>Car-free places provide a safe space for adults to stop and relax, which is especially important for those who are not able-bodied. They provide children with a comfortable and safe place to play as well as an outdoor refuge in <a target="_blank" href="https://pedestriansfirst.itdp.org/about">times of crisis</a>, like during the COVID-19 pandemic. </p>',
'<p>Car-free public places can be both an amenity and a piece of pedestrian transportation infrastructure, because they provide walking routes to destinations that are safe from vehicle traffic and are often calmer and more direct.  </p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR CAR-FREE PLACES IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use data from <a href="http://openstreetmap.org/">OpenStreetMap</a>, identifying all places marked as pathways, parks, plazas, or otherwise car-free areas. We exclude sidewalks, crosswalks, and privately owned or access-limited areas. We then use the <a href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> to count the number of people living within a 100m circle of these places.</p>',
'<p>BecauseBecause OpenStreetMap data quality varies so dramatically and correlates with income, it may appear that higher-income cities have a much higher score than low-income ones, even if the actual infrastructure on the ground is comparable. If you know a place where our data is inaccurate, please help to make our information better. See the <em>Data and Algorithms</em> section of the About page for details on how to contribute to OpenStreetMap.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Make space for people</strong> to relax and play by creating <a target="_blank" href="https://wriciudades.org/sites/default/files/pocket_parks.pdf">pocket parks</a> on vacant lots and converting street parking to ‘<a target="_blank" href="https://www.itdp.org/2014/05/09/reclaiming-the-city-one-space-at-a-time/">parklets</a>.” ',
'<li><strong>Open streets</strong> by converting them to <a target="_blank" href="https://www.weforum.org/agenda/2019/10/car-free-streets-benefits-around-the-world/">car-free areas</a>.',
'<li><strong>Encourage new uses of existing public spaces.</strong> For example, schoolyards can be opened to the public after hours.',
'<li><strong>Build spaces <a target="_blank" href="https://90ffb89e-d990-4f1a-90f3-7974ef8ea8c5.filesusr.com/ugd/534edb_67a4d41d9f9e4e5f80ef259bd60fd7d8.pdf">designed for play</a> into neighborhoods.</strong>',
'<li><strong>Embrace <a target="_blank" href="https://www.pps.org/article/lighter-quicker-cheaper">lighter, quicker, cheaper</a><span style="text-decoration:underline;">,</span> or <a href="https://www.itdp.org/event/power-of-tactical-urbanism/">tactical urbanism</a></strong> approaches to creating public spaces for people.',
'<li><strong>Convert places where people already walk</strong> into pedestrian malls.',
'<li><strong>Follow <a target="_blank" href="https://publications.wri.org/citiessafer/#c5">Cities Safer By Design</a> guidelines</strong> for pedestrian streets and plazas.',
'<li><strong>Adopt a citywide plan</strong>, like Barcelona’s <a target="_blank" href="https://www.vox.com/energy-and-environment/2019/4/9/18300797/barcelona-spain-superblocks-urban-plan">superblocks</a>, that creates space in neighborhoods for people.',
'</div>'
)
