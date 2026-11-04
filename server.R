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
  gre_server(input, output, session, shared)

  ## Usage / invoice tab (shared) -- uses this app's upload + submit input IDs
  invoice_rcbd_server(input, output, session, shared,
                      file_id = "rcbd_file1", submit_id = "rcbd_submit",
                      demo_select_id = "demo_data_select")

  ## Data tools reused from RM-ANOVA
  rcbd_gen_server(input, output, session)
  rcbd_dataset_server(input, output, session)

  ## Shared feedback tab
  raisins_feedback_server("app_feedback", app_name = "RAISINS Growth Rate Estimation",
                          module_code = "RMAOW", send_mail = core$support_send)

  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")
}
