# the modal at startup
query_modal_beta <- modalDialog1(
  title = "INVITE-ONLY BETA",
  p("This version of the Atlas is an invite-only beta, meant for testing and preview purposes only. The data has not yet been fully validated. Please refrain from this data for actual city planning or policymaking purposes. If you encounter any issues regarding the data quality or the display or function of the website, please either list them at", a(href = "https://github.com/ITDP/atlas/issues", target = 	"_blank", "github.com/ITDP/atlas/issues"),  "or report them by email to taylor.reich@itdp.org."),
  p("Please do not share your username and password with others, even within your own organization. If you wish to grant other individuals access to the beta, kindly request an invitation from taylor.reich@itdp.org. We anticipate a public release of the Atlas in early 2024."),
  easyClose = FALSE,
  size = "l",
  # footer = NULL,
  id1 = "modal_beta_checkpoint",
  id = "modal_beta_checkpoint1",
  footer = tagList(
    # modalButton("I understand that this is an invite-only beta and can only be used for testing and preview purposes."),
    tagList(actionButton(inputId  = "modal_beta_close", 
                          label = "I understand that this is an invite-only beta and can only be used for testing and preview purposes."))
    )
  # footer = tagList(
  #   actionButton("run", "Run query")
  # )
)

# Show the model on start up ...
# delay(50, showModal(query_modal_beta))


observeEvent(input$modal_beta_close, {
  removeModal()
})