library(shiny)
library(leaflet)
library(stars)
library(shinyWidgets)
library(sf)
library(sfheaders)
library(leafem)
library(shinyjs)
library(waiter)
library(highcharter)
library(leaflet.extras2)
library(shinyBS)
library(shinycssloaders)
library(shinymanager)
# install.packages('mapboxapi')


source("fun/accordion_input.R")
source("fun/label_with_info.R")
source("fun/accordion_ranks.R")
source("fun/format_indicator_value.R")
source("fun/modalDialog.R")

`%nin%` = Negate(`%in%`)


# enableBookmarking(store = "url")