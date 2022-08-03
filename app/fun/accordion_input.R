

accordion_input <- function(inputId,label,choices, selected = character(0), ...) {
  
  # inputId <- "indicator_bike"
  # label <- "Bike"
  # choices <- structure(c("pnpb", "pnab", "abikeways", "pbikeways"),
  #                      .Names = c("PN Protected Bikelane", "PN All Bikelanes",
  #                                 "Bikeways", "Protected Bikeways"))
  
  # generate options
  options <- mapply(choices, names(choices), FUN = function(value, name) {
    inputTag <-
      tags$button(class = "btn radiobtn btn-default  action-button", name = inputId, value = value,
                  # tags$input(type="radio", name = inputId, value = value),
                  name
      )
    
    
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  
  a <- div(class = "shiny-options-group", options)
  
  go <- div(class = "accordion",  id="accordionExample",
            div(class = "accordion_item",
                tags$button(class="accordion-button", type="button", 'data-toggle'="collapse", 'data-target'=paste0("#", inputId), 'aria-expanded'="true", 'aria-controls'="collapseOne",
                            tags$label(class = "control_label", 'for' = inputId, label)),
                div(id=inputId, class="panel-collapse collapse in", 'aria-labelledby'=paste0(inputId, "-label"), 'data-bs-parent'="#accordionExample",
                    div(class = "accordion-body",
                        shinyWidgets::radioGroupButtons(
                          inputId = inputId,
                          # label = "",
                          choices = choices,
                          direction = "vertical",
                          selected = selected,
                          ...
                        )
                        # a
                    ))))
  
  return(go)
  
  
}