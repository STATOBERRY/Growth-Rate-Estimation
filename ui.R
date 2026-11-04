##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##         ui of RAISINS Growth Rate Estimation  (Single Universe)           ----
##  Page head, loading screen, logout, navbar title and the Welcome tab come
##  from greyskull::raisins_page(); only this app's own tabs are here.
##  The RA-One (AI) tab is intentionally dropped in v1 (ra_one = NULL).
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- raisins_app_ui(
  analysis    = gre_ui(),
  create_data = rcbd_gen_ui(),
  datasets    = rcbd_dataset_ui(),
  user        = invoice_rcbd_ui(),
  view_data   = tagList(
    uiOutput("rcbd_instr_filecheck"),
    div(style = "text-align: center;", DTOutput("rcbd_ViewData"))
  ),
  faqs        = uiOutput("instr_bttns_FAQs")
)
