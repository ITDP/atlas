style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">Population Density</div>',
'<div class = "text_indicator"><p>',
'<p>Weighted Population Density measures the average density experienced by residents of the city.</p>',
sprintf('<p>%s had a total urban population of %s at the end of %s, and the average density experienced by urban residents was %s people per square kilometer. %s</p>', 
        style(rank$indicator$name), 
        style(scales::number(rank$indicator$city_popdensitytotal, big.mark = ",")), 
        style(input$year), 
        style(format_indicator_value),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ), 
        # style(rank$indicator$name), 
        # style(rank$indicator$city_poptotal), 
        # style(format_indicator_value)),
'<p>Compact urban form is fundamental to sustainable cities. Shorter distances between places make all trips shorter. This makes it easier to walk or bicycle. Shorter trip distances make transit more efficient because each station can serve more people and destinations. Compact cities also make car trips shorter and therefore less environmentally destructive. Compact development preserves farmland and natural surroundings by reducing urban sprawl. </p>',
'<p>Population density is necessary but not sufficient for sustainable transport. Compact cities also need supportive infrastructure, such as bicycle lanes and public transit, which are measured in other indicators.</p>',
'<p>Density is not the same as crowding. Density is a function of the number of people per area of land, while crowding is a function of the number of people per dwelling or per room. High density supports sustainability, but high crowding — overcrowding — can be detrimental to public health and living conditions. Twenty people living in a single house is overcrowding, but the same number living in an apartment building is dense without being crowded.</p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW POPULATION DENSITY IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>Traditional population density statistics measure the total population divided by the total area of a city. To give a
<a target="_blank" href="https://www.bloomberg.com/news/articles/2016-10-26/the-deception-of-density">
  <span style="color:rgb(17, 85, 204);">more meaningful measurement</span>
</a>, this indicator calculates weighted Population Density, the average of the densities of subareas of the city weighted by the populations of those subareas. We use the grid cells of the
<a target="_blank" href="http://ghsl.jrc.ec.europa.eu/">
  <span style="color:rgb(17, 85, 204);">Global Human Settlement Layer</span>
</a>
as both our input data and our analysis resolution.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'  <li><strong>Update land-use regulations, zoning codes,</strong> and other rules to allow denser construction, prevent low-density sprawl, and improve the pedestrian environment. Reference ITDP’s <a href="http://todstandard.org/">Transit-Oriented Development Standard</a>.',
'  <li><strong>Incentivize development within the city</strong> and discourage development on the urban fringes. Apply development impact fees to low-density developments. ',
'  <li><strong>Remove parking requirements</strong> to encourage building space for people instead of cars. Consider parking maximums.',
'  <li><strong>Accommodate people of all income levels</strong> by setting citywide policies to maintain and expand housing affordability. Plan for densification in a way that avoids displacement.',
'  <li><strong>Prevent overcrowding </strong>by ensuring that all residents have necessary living space, natural lighting, and ventilation. ',
'  <li><strong>Limit policies and projects </strong>that require public investment in infrastructure on the urban fringes. ',
'  <li><strong>Design with children in mind,</strong> as described in Cities for Play’s <a href="https://www.citiesforplay.com/child-friendly-neighbourhoods">Designing Child-Friendly High-Density Neighborhoods</a><span style="text-decoration:underline;">.</span>',

'</div>'
)
