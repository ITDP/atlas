style <- function(x) {
  
  paste0('<strong style="font-size: 16px; color: #00AE42;">', x, '</strong>') 
  
}

p1 <- c('<div class = "title_indicator_label">INDICATOR</div>',
'<div class = "title_indicator">Safety From Highways</div>',
'<div class = "text_indicator"><p>',
'<p>Safety From Highways measures the percentage of an area’s population not living nearby (within 500m of) a grade-separated highway. Living near highways exposes people to many forms of harm. It makes walking more dangerous and incentivizes driving. Grade-separated highways, also known as freeways or expressways, are especially bad because they hold the highest volumes of cars, moving at the greatest speeds, with the fewest opportunities for pedestrians to cross. </p>',
'<p>High volumes of vehicle traffic on highways contribute to large pollution emissions, and areas within <a target="_blank" href="https://www.lung.org/clean-air/outdoors/who-is-at-risk/highways">500m</a> of a highway suffer the most harm. Highway traffic also exposes people to high levels of <a href="https://hms.harvard.edu/magazine/viral-world/effects-noise-health">noise pollution</a>, the second-most harmful type of environmental pollution for human health. Highways also form barriers between neighborhoods, increasing risk when moving between them.</p>',
'<p>Although electric cars will lessen some of these problems, they still contribute to noise and <a target="_blank" href="https://www.greencarcongress.com/2020/03/20200308-emissionsanalytics.html">air pollution</a> ––and are no less dangerous on the road.</p>'
)

        

        
p2 <- c('<div class = "title_indicator_label2">HOW IT\'S CALCULATED</div>',
'<div class = "text_indicator2"><p>',
'<p>To identify grade-separated highways, we first identify all roads labeled in <a target="_blank" href="http://openstreetmap.org/">OpenStreetMap</a> as <em>primary</em>,<em> motorway</em>, or <em>trunk</em>. Then we find all parts of those highways with at least 1500m between four-way intersections, where traffic can flow freely without being stopped by signals or crossings. After identifying these grade-separated roads, we count the number of people living at least 500m from the nearest one. </p>',
'</div>')       
        

p3 <- c(
'<div class = "title_indicator_label2">Policy Recommendations</div>',
'<div class = "text_indicator2">',
'<ul>',
'<li><strong>Immediately stop allocating money to highway construction</strong>. Once a highway is built, it is immediately harmful and difficult to remove.',
'<li><strong>Replace highways with public transit, boulevards, and parks. </strong>This process has been successful in <a target="_blank" href="https://www.cnu.org/publicsquare/2022/05/31/eight-completed-highway-removals-tell-story-movement">cities around the world</a> and has recently become a <a target="_blank" href="https://www.route-fifty.com/infrastructure/2021/11/highway-removal-funding-infrastructure-bill/186714/">national policy</a> priority in the United States.',
'<li><strong>Establish city-level or national-level transportation plans </strong>to remove, and no longer build, urban highways.',
'</div>'
)
