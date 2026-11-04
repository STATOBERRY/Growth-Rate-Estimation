## ============================================================================
##  RAISINS Growth Rate Estimation — Analysis tab UI  (gre_ui)
##  Self-contained module that plugs into the shared greyskull shell.
##  Shared input IDs reused so the greyskull invoice / View Data / login wiring
##  keeps working unchanged:
##     rcbd_file1  (upload)   rcbd_submit (run)   demo_data_select (demo picker)
##     rcbd_ViewData / rcbd_instr_filecheck (View Data)   instr_bttns_FAQs (FAQs)
##     rcbd_logout (logout fab rendered by greyskull::raisins_page)
## ============================================================================

gre_ui <- function() {

  sidebar <- sidebarPanel(
    width = 3,
    id = "sidebar",
    style = "background-color:#fff7db;",

    tags$b("1. Upload your data (CSV / Excel)", style = "color:#0c1687;"),
    fileInput("rcbd_file1", NULL,
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Browse...", placeholder = "No file selected"),

    tags$b("…or try a demo dataset", style = "color:#8B0000;"),
    selectInput("demo_data_select", NULL,
                choices = c("None" = "",
                            "Coconut – Production & Area" = "dataset1.csv",
                            "Coconut – District panel"    = "dataset2.csv",
                            "Coconut – Yield series"       = "dataset3.csv"),
                selected = ""),
    tags$hr(),

    tags$b("2. Columns", style = "color:#345915;"),
    uiOutput("gre_year_ui"),
    uiOutput("gre_series_ui"),
    uiOutput("gre_group_ui"),
    tags$hr(),

    tags$b("3. Options", style = "color:#87560c;"),
    radioButtons("gre_growth_basis", "Growth rate basis",
                 choices = c("Compound (log-linear CGR)" = "cgr",
                             "Endpoint CAGR"              = "cagr"),
                 selected = "cgr"),
    checkboxGroupInput(
      "gre_modules", "Trend analyses to run",
      choices = c("Growth rates"                 = "growth",
                  "Trend fitting (lin/quad/cubic/exp)" = "trend",
                  "Mann-Kendall + Sen's slope"   = "mk",
                  "Instability (CV, CDVI)"       = "instab",
                  "Area–Yield decomposition"     = "decomp"),
      selected = c("growth", "trend", "mk", "instab")),
    numericInput("gre_alpha", "Significance level (α)",
                 value = 0.05, min = 0.001, max = 0.2, step = 0.01),
    tags$hr(),

    div(style = "text-align:center;",
        shinyWidgets::actionBttn("rcbd_submit", "Run Analysis",
                                 color = "success", style = "unite",
                                 icon = icon("play"))),
    tags$br(),
    uiOutput("gre_sidebar_msg")
  )

  main <- mainPanel(
    width = 9,
    id = "rcbd_content",
    tabsetPanel(
      id = "subtabs", type = "tabs",

      tabPanel("Growth Rates", value = "Analysis Results",
               tags$br(), uiOutput("gre_run_hint"),
               shinycssloaders::withSpinner(DT::DTOutput("gre_growth_table")),
               tags$br(), uiOutput("gre_growth_note")),

      tabPanel("Trend Fitting", value = "Trend",
               tags$br(),
               shinycssloaders::withSpinner(DT::DTOutput("gre_trend_table")),
               tags$br(),
               shinycssloaders::withSpinner(plotOutput("gre_trend_plot", height = "460px")),
               div(style = "text-align:center;",
                   downloadButton("gre_trend_plot_dl", "Download plot (PNG)"))),

      tabPanel("Trend Test", value = "MannKendall",
               tags$br(), uiOutput("gre_mk_intro"),
               shinycssloaders::withSpinner(DT::DTOutput("gre_mk_table"))),

      tabPanel("Instability", value = "Instability",
               tags$br(), uiOutput("gre_instab_intro"),
               shinycssloaders::withSpinner(DT::DTOutput("gre_instab_table"))),

      tabPanel("Decomposition", value = "Decomposition",
               tags$br(), uiOutput("gre_decomp_intro"),
               shinycssloaders::withSpinner(DT::DTOutput("gre_decomp_table")),
               tags$br(), uiOutput("gre_decomp_note")),

      tabPanel("Interpretation", value = "Interpretation",
               tags$br(), shinycssloaders::withSpinner(uiOutput("gre_interpretation"))),

      tabPanel("Download Report", value = "Report",
               tags$br(),
               div(style = "max-width:620px;margin:0 auto;",
                   tags$h4("Download the full Growth & Trend report"),
                   tags$p("Choose a format. The report includes every table you ran, ",
                          "the fitted-trend figure and the written interpretation, ",
                          "styled to match other RAISINS reports."),
                   radioButtons("gre_report_format", "Format",
                                choices = c("Word (.docx)" = "Word",
                                            "PDF"          = "PDF",
                                            "HTML"         = "html"),
                                selected = "Word", inline = TRUE),
                   selectInput("gre_table_style", "Table style",
                               choices = raisins_tstyle_choices(), selected = "classic"),
                   selectInput("gre_font", "Font",
                               choices = raisins_font_choices(), selected = "cambria"),
                   tags$br(),
                   div(style = "text-align:center;",
                       downloadButton("gre_download_report", "Download report",
                                      class = "btn-success"))))
    )
  )

  fluidPage(
    titlePanel(tags$div(tags$b("Growth Rate Estimation & Trend Analysis",
                               style = "color:#000000;"))),
    sidebarLayout(sidebar, main)
  )
}
