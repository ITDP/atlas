style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}
  

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Protected Bikeways</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Protected Bikeways measures the percentage of the population which lives within 300 meters, walking distance, of a physically-protected bikeway. Citywide networks of physically-protected bicycle lanes are the most important factor in encouraging people to use cycling as their preferred mode of transportation. </p>',
sprintf('<p>In %s, in %s, %s%% of residents live within a 300m walk of a physically protected bikeway. More generally, %s%% of residents lived within a 300m walk of any bikeway, protected or unprotected. In %s, in %s, there were %s kilometers of bikeways, of which %s were physically protected.</p>', 
        style(unique(rank$indicator$name)), 
        style(input$year), 
        style(unique(format_indicator_value)), 
        style(ifelse(is.null(rank$indicator$bike_pnpbpnab), "X", round(rank$indicator$bike_pnpbpnab * 100))), 
        style(unique(rank$indicator$name)), 
        style(input$year), 
        style(ifelse(is.null(rank$indicator$bike_pnpbabikeways), "X",round(rank$indicator$bike_pnpbabikeways))),
        style(ifelse(is.null(rank$indicator$bike_pnpbpbikeways), "X",round(rank$indicator$bike_pnpbpbikeways)))), 
'<p>Studies have shown that <a href="https://jenniferdill.net/types-of-cyclists/">most</a> people would ride a bicycle for transport if it felt safe, but they choose not to because the existing streets feel too dangerous. In cities that provide extensive, well-connected networks of physically protected bikeways, large numbers of people cycle to get around. Increased cycling saves individuals time and money and <a href="https://ehp.niehs.nih.gov/doi/full/10.1289/ehp.0901747">improves their health</a>. It also reduces air and noise pollution, carbon emissions, and <a href="https://www.heatwalkingcycling.org/">healthcare costs</a>. Protected bikeways <a href="https://www.rff.org/publications/journal-articles/bicycle-infrastructure-and-traffic-congestion-evidence-from-dcs-capital-bikeshare/">reduce congestion</a>, promote <a href="https://www.fastcompany.com/3021074/making-the-economic-case-for-cycling-friendly-cities-with-bikeonomics">local economic development</a>, and make streets safer and more pleasant, not only for cyclists but also for <a href="https://www.sciencedaily.com/releases/2019/05/190529113036.htm">motorists and pedestrians</a>.</p>',
'<p>ITDP <a href="https://www.itdp.org/publication/protected-bicycle-lanes-protect-the-climate/">research</a> in middle-income countries shows that People Near Bikeways is very strongly correlated with the number of people who ride bicycles. In technical terms, the variation in People Near Bikeways explains 88% of the variation in total bicycle travel at the city level.</p>'
)


        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR BIKEWAYS IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use <a href="http://openstreetmap.org/">OpenStreetMap</a> (OSM) data to identify physically protected bicycle facilities. We use the street network from OSM to draw a walkshed of 1km around these facilities, and we use <a href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> data to count the percentage of the city’s population within that walkshed.</p>',
'<p>The People Near Protected Bikeways indicator relies on accurate OSM data. Higher-income countries often have better OSM data, but anyone can help improve OSM for a city they are familiar with. If you know a place where our data is inaccurate, please help make our information better. See the <em>Data and Algorithms</em> section of the About page for details on how to contribute to OpenStreetMap.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Develop a plan for a citywide network</strong> of connected protected bikeways, greenways, low-speed and low-volume streets, and cycle highways.',
'<li><strong>Set up “<a href="https://www.itdp.org/publication/from-pilot-to-permanent/">quick build</a>” or temporary protected bikeway cycle lanes</strong> with the ability to transition them to permanent design. Explore additional recommendations in ITDP\'s <a href="https://growcycling.itdp.org/">Grow Cycling Toolkit</a>, which offers actionable steps for cities to rapidly enhance cycling infrastructure.',
'<li><strong>Design and install networks of protected bikeways</strong> and/or add protection to existing bicycle lanes. Follow international <a href="https://www.itdp.org/publication/africa-streets-walking-cycling/">best practice guidance</a> and <a href="https://globaldesigningcities.org/publication/global-street-design-guide/">quality standards</a>. Refer to ITDP’s <a href="https://www.itdp.org/publication/protected-bicycle-lanes-protect-the-climate/">Protected Bicycle Lanes Protect the Climate</a> for evidence on the positive impact of protected bicycle lane networks, including reduced greenhouse gas emissions and enhanced safety.',
'<li><strong>Develop the institutions to support and sustain cycling. </strong>Ensure lanes are well-designed, well-maintained, and integrated into ongoing city service provisions. Refer to specific modules in the ITDP’s <a href="https://cyclingcities.itdp.org/education">Mastering the Cycling City</a> course, such as “Design it” and “Institutionalize it,” for detailed insights.',
'</div>'
)
