

library(shiny)

div(class="modal fade", id="shiny-modal", tabindex="-1",
    div(class="modal-dialog modal-lg",
        div(class="modal-content",
            div(class="modal-header",
                h4(class="modal-title", "ABOUT")
            ),
            div(class = "modal-body"),
        )
    ),
    tags$script(HTML("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {\n         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));\n         modal.show();\n      } else {\n         $('#shiny-modal').modal().focus();\n      }")
    )
)




# ---------------------------------------------------------------------------------------------
city <- list()
indicator <- list()
input <- list()
city$city_code <- "02095"
indicator$mode <- "pnpb"
input$year <- 2022

# extract overlay files
files <- dir(sprintf("data/data_july2023/ghsl_%s/overlays/%s", city$city_code, indicator$mode), pattern = ".rds$", full.names = TRUE)
file_year <- files[grepl(pattern = paste0(input$year, ".rds"), x= files)]
# extract overlay geom types
files_geometry <- sub("(.*)(lines|points|polygons)(.*)", replacement = "\\2",  x=files)

teste_overlays <- lapply(
  files,
  readRDS
)

names(teste_overlays) <- files_geometry

map <- leaflet() %>%
  addTiles()


if ("polygons" %in% names(teste_overlays)) {
  
  for(i in 1:nrow(teste_overlays[["polygons"]])){
    data1 <- teste_overlays[["polygons"]][i, ]
    map = map %>% addPolygons(data = data1,
                              group = data1$ind)
  }
  
}

if ("lines" %in% names(teste_overlays)) {
  
  for(i in 1:nrow(teste_overlays[["lines"]])){
    data1 <- teste_overlays[["lines"]][i, ]
    map = map %>% addPolylines(data = data1,
                              group = data1$ind)
  }
  
}

if ("points" %in% names(teste_overlays)) {
  
  for(i in 1:nrow(teste_overlays[["points"]])){
    data1 <- teste_overlays[["points"]][i, ]
    map = map %>% addPolylines(data = data1,
                              group = data1$ind)
  }
  
}

map <- map %>%
  addLayersControl(
    overlayGroups = c(teste_overlays[["polygons"]]$ind, teste_overlays[["lines"]]$ind),
    options = layersControlOptions(collapsed = FALSE))
map







# ---------------------------------------------------------------------------------------------

a1 <- dir()



# ---------------------------------------------------------------------------------------------


overlays_lines_01406_pnpb_2022 <- readRDS("data/data_july2023/ghsl_01406/overlays/pnpb/overlays_lines_01406_pnpb_2022.rds")

ai <- sfheaders::sf_cast(overlays_lines_01406_pnpb_2022, "LINESTRING")
ai <- sfheaders::sf_cast(overlays_polygons_01406_pnpb_2022, "POLYGON")

ai1 <- rmapshaper::ms_simplify(ai, keep = 0.05)
ai1 <- sf::st_simplify(overlays_lines_01406_pnpb_2022, dTolerance = 100)

library(leafem)

tictoc::tic()
leaflet() %>%
  addPolylines(data = overlays_lines_01406_pnpb_2022)
tictoc::toc()

tictoc::tic()
leaflet() %>%
  addPolylines(data = ai1)
tictoc::toc()

tictoc::tic()
leaflet() %>% addProviderTiles("OpenStreetMap") %>% addPolylines(data = ai, gl = TRUE)
tictoc::toc()

tictoc::tic()
leaflet() %>% addProviderTiles("OpenStreetMap") %>% addFeatures(data = ai, gl = TRUE)
tictoc::toc()


leaflet() %>% addProviderTiles("OpenStreetMap") %>% addFeatures(data = ai, gl = TRUE,
                                                                weigth = 2,
                                                                color = "black",
                                                                fillColor = "blue",
                                                                fill = FALSE)
