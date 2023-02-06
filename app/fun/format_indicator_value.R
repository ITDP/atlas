

# data <- c(1, 1200, 10000, 140000000)

format_indicator_values <- function(data, transformation) {
  
  if(transformation == "percent") {
    round(data * 100) 
    
  } else if(transformation %in% "thousands") {
    
    
    ifelse(data >= 1000000, 
           scales::comma(data, accuracy = 0.1, scale = 0.000001, suffix = "M"),
           ifelse(data >= 1000, scales::comma(data, accuracy = 0.1, scale = 0.001, suffix = "k"), round(data)))
    
    
  } else round(data)
  
  
}