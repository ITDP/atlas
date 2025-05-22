style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">People Safe From Highways</div>',
'<div class = "text_indicator"><p>',
'<p>People Safe From Highways measures the percentage of an area’s population not living near (within 500m of) a grade-separated highway. Living near highways exposes people to many forms of harm. It makes walking more dangerous, increases air pollution, and incentivizes driving. </p>',
sprintf('<p>In %s at the end of %s, %s%% of people lived at least 500m from a grade-separated highway. There were about %s km of highways in this area. %s</p>', 
        # 
        style(rank$indicator$name), 
        style(input$year), 
        style(format_indicator_value),
        style(scales::comma(rank$indicator$walk_pnnhighwayskm)),
        ifelse(is.null(rank$admin_level), "This measurement includes all urban agglomerations with a population of more than 500,000.", "")
        ), 
# style(rank$indicator$name), 
# style(rank$indicator$city_poptotal), 
# style(format_indicator_value)),
'<p>Grade-separated highways, also known as freeways or expressways, are especially bad because they move the highest volumes of cars at the greatest speeds, with the fewest opportunities for pedestrians to cross. They also attract cars from other areas, increasing the danger from traffic. Finally, when approaching or exiting highways, drivers often drive faster, increasing traffic danger for people living in those areas.</p>',
'<p>High volumes of vehicle traffic on highways also generate high levels of air pollution, and areas within <a href="https://www.lung.org/clean-air/outdoors/who-is-at-risk/highways">500m</a> of a highway suffer the most harm. Highway traffic also exposes people to high levels of <a href="https://hms.harvard.edu/magazine/viral-world/effects-noise-health">noise pollution</a>, the second-most-harmful type of environmental pollution for human health. Finally, highways form barriers between neighborhoods, increasing risk when moving between them.</p>',
'<p>Although a shift to electric cars will lessen some of these problems and EVs are an important tool in combating climate change, highways will still contribute to noise and <a href="https://www.greencarcongress.com/2020/03/20200308-emissionsanalytics.html">air pollution</a> –– and high-speed traffic is no less dangerous to humans.</p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW PEOPLE SAFE FROM HIGHWAYS IS CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>To identify grade-separated highways, we first identify all roads labeled in <a href="http://openstreetmap.org/">OpenStreetMap</a> as major roadways. Then we find all parts of those highways with at least 1km between four-way intersections, where traffic can flow freely without being stopped by signals or crossings. After identifying these grade-separated roads, we count the number of people living at least 500m from the nearest one. </p>',
'<p>We exclude stretches of highway that are in a tunnel for at least 300m, because tunneling prevents the worst effects of highways –– air and noise pollution, and traffic danger –– from being felt. However, to be excluded, a highway must be entirely in a full tunnel, not merely under a bridge.</p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Immediately stop allocating money to grade-separated highway construction and widening</strong>. Once a highway is built, it is immediately harmful and difficult to remove.',
'<li><strong>Replace highways with public transport, human-scale streets, and parks</strong>, or, at the very least, with non-grade-separated surface streets.<strong> </strong>This process has been successful in <a href="https://www.cnu.org/publicsquare/2022/05/31/eight-completed-highway-removals-tell-story-movement">cities around the world</a> and has recently become a <a href="https://www.route-fifty.com/infrastructure/2021/11/highway-removal-funding-infrastructure-bill/186714/">national policy</a> priority in the United States. In the short term, implement demand-based tolls on existing highways to manage usage and pollution, and direct money to investments in walking, bicycling, and public transport.',
'<li><strong>Establish or revise city- or national-level transportation plans </strong>to remove, and no longer build, urban highways.',
'</div>'
)
