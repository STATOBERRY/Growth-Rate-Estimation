# Keep source files, uploaded labels and report rendering in a UTF-8 locale.
if (!l10n_info()[["UTF-8"]]) {
  for (locale in c("C.UTF-8", "en_US.UTF-8", ".UTF-8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (l10n_info()[["UTF-8"]]) break
  }
}

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
gre_config <- raisins_app_config(
  app_id         = "gre",
  app_name       = "Growth Rate Estimation",
  module_code    = "GRE",
  version        = "1.0.0",
  title_letters  = c("G", "R", "E"),
  page_title     = "RAISINS: GROWTH RATE & TREND",
  welcome_title  = "RAISINS-Growth Rate Estimation Module",
  welcome_text   = "<p style='text-align: justify;'>
    Discover <strong>R A I S I N S</strong> (R & AI Solutions in INferential Statistics) - a tool built for
    <strong>time-series production and area data</strong>. Upload your yearly figures and instantly obtain compound growth
    rates (CGR, CAGR), trend fitting, the non-parametric Mann-Kendall test with Sen's slope, instability indices
    (CV and the Cuddy-Della Valle Index) and an area-yield decomposition of production. Download a polished Word, PDF
    or HTML report with every table, the fitted-trend figure and a written interpretation.
</p>",
  individual_db      = "GRE_Zap/Sheet2",              # usage counters: GRE_Zap/user, Instiuser, demouser
  demo_db            = "demo/Sheet1",
  demo_video_url     = NULL,                          # no video button, as in fsQCA
  website_page       = "GRE.html",
  # logo and navbar colour: the RAISINS defaults from greyskull, as in fsQCA
  logout_id          = "rcbd_logout",
  subscribe_url      = "https://www.raisins.live/subscription/GRE_sub.html",
  version_record_url = "version.html",
  protected_tabs     = c("Analysis", "Arrange Data", "Datasets", "RA-One",
                         "Feedback", "User", "retake_tour", "View Data", "FAQs")
)
raisins_use_app(gre_config)

# This module's looser %||% (empty string counts as missing); kept from the old global.R.
`%||%` <- function(a,b) if (is.null(a) || identical(a, "")) b else a

#########################PANEL PATH (Growth-Rate-specific modules only)
source("dir/core_aliases.R", local = TRUE)

# RBD layout: plot workspace plus main UI/server (including tour, support and interpretation).
source("dir/gre_plot_workspace.R", local = TRUE)
source("dir/gre_help.R", local = TRUE)
source("dir/gre_ui.R", local = TRUE)
source("dir/gre_server.R", local = TRUE)

# invoice / usage tab now comes from greyskull (aliased in core_aliases.R)
# Reused unchanged from RM-ANOVA: "Arrange Data" CSV creator + "Datasets" tab.
source("dir/rcbd_gen_server.R", local = TRUE)
source("dir/rcbd_gen_ui.R", local = TRUE)
source("dir/rcbd_dataset_server.R", local = TRUE)
source("dir/rcbd_dataset_ui.R", local = TRUE)

### RA-One (RAISINS AI assistant, as in fsQCA)
source("dir/AI/chat_ui.R", local = TRUE)
source("dir/AI/chat_server.R", local = TRUE)
source("dir/AI/gre_chat_server.R", local = TRUE)
source("dir/AI/gre_chat_context.R", local = TRUE)
source("dir/AI/gre_prompt.R", local = TRUE)

# Table/font styles come directly from greyskull, as in RBD.

