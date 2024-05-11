
# the modal at startup
modal_brazil <- modalDialog1(
  title = "Brazil-specific data is available in MobiliDADOS",
  p("The Atlas of City Transport provides data for international comparisons, but another site, ", a(href="https://mobilidados.org.br/", "mobilidados.org.br", target = 	"_blank"), " offers specific data for Brazil. The Atlas uses global data sources, allowing worldwide city comparisons with consistent methods. However, it may not fully consider local contexts. MobiliDADOS has Brazil-specific data with strict quality control. Atlas and MobiliDADOS measurements for the same city may be different. For international city comparisons, use the Atlas. ", strong("For comparisons, city planning, or policymaking within Brazil, use MobiliDADOS.")),
  easyClose = FALSE,
  size = "l",
  # footer = NULL,
  id1 = "modal_brazil",
  id = "modal_brazil1",
  footer = tagList(
    # modalButton("I understand that this is an invite-only beta and can only be used for testing and preview purposes."),
    tagList(actionButton(inputId  = "modal_brazil_close", 
                         label = "I understand."))
  )
)

observeEvent(c(city$city_code, indicator$mode), {

  req(
    city$city_code,
    city$city_code %in% brazil_cities$hdc)
 
  
  # Show the model on start up ...
  delay(10, showModal(modal_brazil))
  
})



observeEvent(input$modal_brazil_close, {
  removeModal()
})