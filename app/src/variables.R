


# store the indicator everytime the user changes it

variables <- reactiveValues(indicator = NULL)


observeEvent(c(indicator$mode), {

  # only the last two values are ever used (see src/ranks.R), so cap growth
  variables$indicator <- tail(c(variables$indicator, indicator$mode), 2)

})