
# round(NULL) returns an error
# round(NULL * 1) returns a numeric(0)

# data <- c(1, 1200, 10000, 140000000)

format_indicator_values <- function(data, transformation) {
  
  if(transformation == "percent") {
    round(data * 100) 
    
  } else if(transformation %in% "thousands") {
    
    
    ifelse(data >= 1000000, 
           scales::comma(data * 1, accuracy = 0.1, scale = 0.000001, suffix = "M"),
           ifelse(data * 1 >= 1000, scales::comma(data * 1, accuracy = 0.1, scale = 0.001, suffix = "k"), round(data * 1)))
    
    
  } else if (transformation == "round1") {
    
    round(data * 1, 1)
    
    
    } else round(data * 1)
  
  
}