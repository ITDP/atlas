style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Near Bikeways + Public Transport</div>',
'<div class = "text_indicator"><p>',
'<p>People Near Bikeways + Public Transport measures the percentage of an area’s population living within walking distance of BOTH protected bicycle infrastructure AND public transport (either frequent or rapid). </p>',
sprintf('<p>In %s in %s, %s%% of people live within 300m of protected bicycle infrastructure and also within either 500m of frequent public transport or 1km of mass rapid transport. %s</p>', 
        style(rank$indicator$name), 
        style(input$year), 
        style(format_indicator_value),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ),
'<p>Bicycling and public transport are not substitutes for each other; rather, they are complements, often used for different kinds of trip. Public transport might be more useful for a long trip to a commercial hub or in rainy weather, while bicycling might be more useful for a trip to a nearby neighborhood that would require a difficult public transport connection. To build a city where all people can conveniently access what they need, both bicycling and public transport are necessary.</p>',
'<p>This indicator measures the percentage of people who live in a neighborhood covered by important sustainable transport options.</p>'
)


        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE NEAR BIKEWAYS + PUBLIC TRANSPORT IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>We combine the areas measured for <em>People Near Bikeways, People Near Rapid Transport</em>, and <em>People Near Frequent Transport</em>. We then use <a href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> data to count the percentage of the city’s population within walking distance of both Protected Bikeways and either Rapid or Frequent Transport, and we apply the same walkshed distance that we do for each of those indicators calculated independently. </p>',
'</div>')       
        


p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Follow policy recommendations</strong> for each of the three indicators above.',
'<li><strong>Plan integrated connections</strong> between bicycling and public transport, including protected bicycle lanes leading directly to transport stops and stations, integrated safe bicycle parking, and integrated bikeshare stations.',
'</div>'
)
