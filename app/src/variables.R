


# store the indicator everytime the user changes it

variables <- reactiveValues(indicator = NULL)


observeEvent(c(indicator$mode), {
  
  variables$indicator <- c(variables$indicator, indicator$mode)
  
})