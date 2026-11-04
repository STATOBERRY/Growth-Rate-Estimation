########### global of RAISINS Growth Rate Estimation  (Single Universe: shared code in greyskull)
options(shiny.maxRequestSize = 100 * 1024^2) # bounded large-file uploads: 100 MiB

##Packages----
library(shiny)
library(dplyr)
library(tidyr)
library(agricolae)
library(gtools)
library(shinyWidgets)
library(shinyalert)
library(shinycustomloader)
library(shinycssloaders)
library(kableExtra)
library(rmarkdown)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
library(colourpicker)
library(ggthemes)
library(ggpattern)
library(DT)
library(lubridate)
library(formattable)
library(shinybusy)
library(shinyjs)
library(readxl)
library(GGally)
library(lsr)
library(broom)
library(effectsize)
library(FactoMineR)
library(factoextra)
library(scales)
library(shinyBS)
library(magick)
library(car)
library(phia)
library(plotly)
library(httr)
library(jsonlite)
library(data.table)
library(reactable)
library(emmeans)
library(multcompView)
library(multcomp)
library(blastula)
library(gtExtras)
library(gt)
library(ggdist)
library(ggbeeswarm)
library(viridis)
library(patchwork)
library(svglite)
library(nortest)
library(rintrojs)
library(later)
library(httr2)
library(digest)
library(shinymanager)

### Shared RAISINS layer: login, session kit, SSO, support, feedback, page shell----
library(greyskull)

# Everything that makes this app "Growth Rate Estimation" rather than another RAISINS module.
# NOTE (temporary): Firebase / login / SSO / invoice / branding values are REUSED FROM
# RM-ANOVA One-Way for now, so sign-in works immediately against the existing nodes.
# Swap these (individual_db, logo_url, navbar_color, subscribe_url, website_page, demo_video_url)
# for Growth-Rate-specific values before public deployment -- see BOOT-TEST-TODO.md.
gre_config <- raisins_app_config(
  app_id         = "rmanova_oneway",              # TODO: dedicated id before deploy
  app_name       = "Growth Rate Estimation",
  module_code    = "RMAOW",                       # TODO: dedicated feedback/SSO code
  version        = "1.0.0",
  title_letters  = c("G", "R", "E"),
  page_title     = "RAISINS: GROWTH RATE & TREND",
  welcome_title  = "RAISINS Growth Rate Estimation Module",
  welcome_text   = "<p style='text-align: justify;'>
    Discover <strong>R A I S I N S</strong> (R & AI Solutions in INferential Statistics) - a tool built for
    <strong>time-series production and area data</strong>. Upload your yearly figures and instantly obtain compound growth
    rates (CGR, CAGR), trend fitting, the non-parametric Mann-Kendall test with Sen's slope, instability indices
    (CV and the Cuddy-Della Valle Index) and an area-yield decomposition of production. Download a polished Word, PDF
    or HTML report with every table, the fitted-trend figure and a written interpretation.
</p>",
  individual_db      = "RMAOW_Zap/Sheet2",            # reused from RM-ANOVA (see note)
  demo_db            = "demo/Sheet1",
  demo_video_url     = "https://www.raisins.live/video.html",
  website_page       = "RMAOW.html",                  # reused from RM-ANOVA (see note)
  logo_url           = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/ORMANOVA%2FLOGORM.png?alt=media",
  navbar_color       = "#2e7d32",                     # green, fitting an agri/growth module
  logout_id          = "rcbd_logout",
  subscribe_url      = "https://www.raisins.live/subscription/RMAOW_sub.html",
  version_record_url = "version.html",
  protected_tabs     = c("Analysis", "Arrange Data", "Datasets",
                         "Feedback", "User", "retake_tour", "View Data", "FAQs")
)
raisins_use_app(gre_config)

# This module's looser %||% (empty string counts as missing); kept from the old global.R.
`%||%` <- function(a,b) if (is.null(a) || identical(a, "")) b else a

#########################PANEL PATH (Growth-Rate-specific modules only)
source("dir/core_aliases.R", local = TRUE)

# RBD layout: plot workspace plus main UI/server (including tour, support and interpretation).
source("dir/gre_plot_workspace.R", local = TRUE)
source("dir/gre_ui.R", local = TRUE)
source("dir/gre_server.R", local = TRUE)

# invoice / usage tab now comes from greyskull (aliased in core_aliases.R)
# Reused unchanged from RM-ANOVA: "Arrange Data" CSV creator + "Datasets" tab.
source("dir/rcbd_gen_server.R", local = TRUE)
source("dir/rcbd_gen_ui.R", local = TRUE)
source("dir/rcbd_dataset_server.R", local = TRUE)
source("dir/rcbd_dataset_ui.R", local = TRUE)

# Table/font styles come directly from greyskull, as in RBD.

# NOTE: the RA-One AI assistant (dir/AI/*) is NOT wired in v1. Its chat server is
# invoked from inside the old ANOVA engine and needs a Growth-Rate chat context +
# an OpenAI key. See BOOT-TEST-TODO.md to enable it.
