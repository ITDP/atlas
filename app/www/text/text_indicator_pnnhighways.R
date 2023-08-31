p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Protected Bikeways</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Protected Bikeways measures the percentage of the population which lives within 300 meters, walking distance, of a physically-protected bikeway. Citywide networks of physically-protected bicycle lanes are the most important factor in encouraging people to use cycling as their preferred mode of transportation. </p>',
sprintf('<p>In %s, %s %% of residents live within a 300m walk of any bikeway, protected or unprotected. Of those, %s %% live within a 300m walk of a physically-protected bikeway. In %s, there are %s kilometers of bikeways, of which %s are physically-protected.</p>', 
        rank$indicator$name, format_indicator_value, format_indicator_value, rank$indicator$name, format_indicator_value, format_indicator_value),
'<p>The majority of people are interested in bicycling more but choose not to because they lack the safe infrastructure to do so. In cities that provide extensive, well-connected networks of physically-connected bikeways, more people cycle to get around. Increased cycling saves individuals time and money and <a href="https://ehp.niehs.nih.gov/doi/full/10.1289/ehp.0901747">improves their health</a>. It also reduces air and noise pollution, carbon emissions, and <a href="https://www.heatwalkingcycling.org/">healthcare costs</a>. Protected Bikeways <a href="https://www.rff.org/publications/journal-articles/bicycle-infrastructure-and-traffic-congestion-evidence-from-dcs-capital-bikeshare/">reduce congestion</a>, promote <a href="https://www.fastcompany.com/3021074/making-the-economic-case-for-cycling-friendly-cities-with-bikeonomics">local economic development</a>, and make streets safer and more pleasant, not only for cyclists, but also for <a href="https://www.sciencedaily.com/releases/2019/05/190529113036.htm">motorists and pedestrians</a>.</p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We use <a href="http://openstreetmap.org/">OpenStreetMap</a> data to identify physically-protected bicycle facilities (those tagged as <em>highway=cycleway</em> or <em>cycleway=track</em>) and count the people living within a 300m walk of these facilities.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Install protected bicycle lanes</strong> and/or add protection to existing lanes. Follow international <a href="https://globaldesigningcities.org/publication/global-street-design-guide/">quality standards</a>.',
'<li><strong>Set up “<a href="https://s3-us-west-2.amazonaws.com/static.peopleforbikes.org/images/glp/PeopleForBikes-Quick-Builds-for-Better-Streets-Report.pdf">quick build</a>” or temporary cycle lanes</strong>, with the ability to transition them to permanent design.',
'<li><strong>Form a city network</strong> by connecting protected bikeways to greenways, low-speed low-volume streets, and cycle highways.',
'<li><strong>Ensure lanes are well-lit, well-maintained</strong>, and reflect city conditions.',
'<li><strong>Redesign streets and intersections </strong>to be safe, <a href="https://www.itdp.in/resource/complete-streets-framework-toolkit/">complete streets</a> that prioritize <a href="https://www.itdp.org/publication/africa-streets-walking-cycling/">safe cycling and walking</a>. ',
'</div>'
)
