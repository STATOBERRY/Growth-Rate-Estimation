##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##         ui of RAISINS Growth Rate Estimation and Trend Analysis  (Single Universe)           ----
##  Page head, loading screen, logout, navbar title and the Welcome tab come
##  from greyskull::raisins_page(); only this app's own tabs are here.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- raisins_app_ui(
  head = tags$head(
    ## Navbar title colour, as in Regression (greyskull hard-codes #e38f12 inline).
    tags$style(HTML('.navbar span[style*="#e38f12"] { color: #FFFFFF !important; }')),
    ## Calculator loader for results tables (same rules as RAISINS RBD's ui.R).
    tags$style(HTML(r"---(
.rcbd-calculator-loader > .shiny-loader-output-container > .load-container {
  position: relative;
  top: auto;
  min-height: 144px;
  transform: none;
  padding: 16px;
  box-sizing: border-box;
  text-align: center;
}

.rcbd-calculator-loader .loader-img {
  display: block;
  width: 450px;
  max-width: 100%;
  height: 450px;
  object-fit: contain;
  margin: 0 auto;
}

/* "Calculating, please wait..." dialog opened by greyskull's usage tracker
   (raisins_invoice_server) on Run: wider and centred, as in RBD. */
#shiny-modal:has(.crdp-calculator-modal) {
  padding: 16px !important;
}

#shiny-modal .modal-dialog:has(.crdp-calculator-modal) {
  width: 600px;
  max-width: 100%;
  min-height: calc(100vh - 32px);
  min-height: calc(100dvh - 32px);
  margin: 0 auto;
  display: flex;
  align-items: center;
}

#shiny-modal .modal-content:has(.crdp-calculator-modal) {
  width: 100%;
}

#shiny-modal .modal-body:has(.crdp-calculator-modal) {
  padding: 20px;
}

.crdp-calculator-modal {
  padding: 0;
  text-align: center;
}

.crdp-calculator-modal img {
  display: block;
  width: 480px;
  max-width: 100%;
  height: auto;
  max-height: calc(100vh - 132px);
  max-height: calc(100dvh - 132px);
  object-fit: contain;
  margin: 0 auto;
}

.crdp-calculator-modal p {
  margin: 12px 0 0;
  color: #59636a;
  font-size: 14px;
  font-weight: 500;
}
)---"))
  ),
  analysis    = gre_ui(),
  create_data = gre_arrange_ui(),
  datasets    = rcbd_dataset_ui(),
  ra_one      = chat_ui("gre_chat"),
  user        = invoice_rcbd_ui(),
  view_data   = fluidPage(
    uiOutput("rcbd_instr_filecheck"),
    div(style = "text-align: center;", DTOutput("rcbd_ViewData")),
    tags$br(), tags$br()
  ),
  faqs        = fluidPage(uiOutput("instr_bttns_FAQs"))
)
