style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">Population Density</div>',
'<div class = "text_indicator"><p>',
'<p>Weighted population density measures the density of the neighborhood where a city’s average resident lives. </p>',
sprintf('<p>%s has a total population of %s in %s, and the average resident lives in a neighborhood with a density of %s people per square kilometer.</p>', 
        style(rank$indicator$name), 
        style(scales::number(rank$indicator$city_popdensitytotal, big.mark = ",")), 
        style(input$year), 
        style(format_indicator_value)), 
        # style(rank$indicator$name), 
        # style(rank$indicator$city_poptotal), 
        # style(format_indicator_value)),
'<p>Compact land use is a fundamental requirement for sustainability.  Shorter distances between places make it easier to walk or bicycle, make transit more efficient because each station can serve more people, and make car trips shorter and therefore less environmentally destructive. Compact land reduces sprawl, preserving farmland and natural surroundings.</p>',
'<p>Population density is necessary but not sufficient for sustainable transport. Compact cities also need supportive infrastructure, such as bicycle lanes and public transit, which are measured in other indicators.</p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>Traditional population density statistics measure the total population divided by the total area of a city. To give a <a target="_blank" href="https://www.bloomberg.com/news/articles/2016-10-26/the-deception-of-density">more meaningful measurement</a>, this indicator calculates weighted population density, the average of the densities of subareas of the city weighted by the populations of those subareas. We use the grid cells of the <a target="_blank" href="http://ghsl.jrc.ec.europa.eu/">Global Human Settlement Layer</a> as both our input data and our analysis resolution.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Update land-use regulations, zoning codes,</strong> and other rules to allow denser construction, prevent low-density sprawl, and improve the pedestrian environment. Reference the <a target="_blank" href="http://todstandard.org/">Transit-Oriented Development Standard</a>.',
'<li><strong>Incentivize development within the city</strong> instead of on the fringes. Apply development impact fees to low-density developments.',
'<li><strong>Change parking restrictions</strong> to encourage building space for people instead of cars. Remove parking minimums and create parking maximums.',
'<li><strong>Accommodate people of all income levels</strong> by setting citywide policies to maintain and encourage housing affordability. Plan for densification in a way that avoids gentrification and displacement.',
'<li><strong>Prevent overcrowding.</strong> Ensure that all residents have necessary living space, natural lighting, and ventilation.',
'<li><strong>Retract policies and projects </strong>that require public investment in infrastructure on the urban fringes.',
'<li><strong>Design with children in mind,</strong> as is described in <a target="_blank" href="https://www.citiesforplay.com/child-friendly-neighbourhoods">Designing Child-Friendly High-Density Neighborhoods</a><span style="text-decoration:underline;">.</span>',
'</div>'
)
