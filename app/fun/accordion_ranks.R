accordion_ranks <- function(id_accordion, id1, id2) {
  
  
  div(class = "accordion collapsed in",  id="accordionExample",
      div(class = "accordion_item",
          tags$button(class="accordion-button", style = "padding: 0", type="button", 'data-toggle'="collapse", 'data-target'=paste0("#", id_accordion), 'aria-expanded'="false", 'aria-controls'="collapseOne",
                      tags$label(class = "control_label", 'for' = id_accordion, 
                                 HTML(id1))),
          div(id=id_accordion, class="panel-collapse collapse", 'aria-labelledby'=paste0(id_accordion, "-label"), 'data-bs-parent'="#accordionExample",
              div(class = "accordion-body",
                  # p("parara")
                  id2
                  
              )))) %>% as.character()
  
}