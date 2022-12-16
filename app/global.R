library(shiny)
library(leaflet)
library(shinyWidgets)
library(sf)
# library(data.table)
library(shinyjs)
library(waiter)
# library(leafgl)
library(highcharter)
# library(htmltools)
# library(htmlwidgets)
library(leaflet.extras2)
library(shinyBS)
library(shinycssloaders)
library(mapboxapi)


source("fun/accordion_input.R")
source("fun/label_with_info.R")

`%nin%` = Negate(`%in%`)


enableBookmarking(store = "server")