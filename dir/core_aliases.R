## Old RM-ANOVA names for helpers that now live in greyskull.
## They keep the analysis code, reports and tests working unchanged.
## New code should call the raisins_* names from greyskull directly.
##
## Table and font styles are called directly through greyskull in the analysis and report.

rcbd_static_ui            <- greyskull::raisins_static_ui
rcbd_login_email          <- greyskull::raisins_login_email
rcbd_prefill_login_email  <- greyskull::raisins_prefill_login_email
rcbd_existing_otp_login   <- greyskull::raisins_existing_otp_login

# User details & Invoice tab (now shared; this app's rcbd_* input ids are passed in server.R)
invoice_rcbd_ui            <- greyskull::raisins_invoice_ui
invoice_rcbd_server        <- greyskull::raisins_invoice_server
