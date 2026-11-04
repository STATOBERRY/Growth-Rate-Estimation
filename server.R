##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##         server of RAISINS Growth Rate Estimation  (Single Universe)       ----
##  The shared login layer (demo / individual / institutional OTP login,
##  domain + website SSO, stay-signed-in, forgot password, license /
##  subscription / version dialogs) lives in greyskull. This app supplies the
##  growth-rate analysis engine, the data tools and the report download.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {

  core   <- raisins_login_server(input, output, session)
  shared <- core$shared

  ## Growth Rate Estimation analysis engine
  gre_support <- gre_server(input, output, session, shared,
                            support_send = core$support_send)

  ## Usage / invoice tab (shared) -- uses this app's upload + submit input IDs
  invoice_rcbd_server(input, output, session, shared, error_support = gre_support,
                      file_id = "rcbd_file1", submit_id = "rcbd_submit",
                      demo_select_id = "demo_data_select")

  ## Data tools reused from RM-ANOVA
  rcbd_gen_server(input, output, session, error_support = gre_support)
  rcbd_dataset_server(input, output, session, error_support = gre_support)

  ## Shared feedback tab
  cfg <- raisins_app()
  raisins_feedback_server("app_feedback", app_name = paste("RAISINS", cfg$app_name),
                          module_code = cfg$module_code, send_mail = core$support_send)

  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")
}
