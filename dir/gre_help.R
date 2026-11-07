## ============================================================================
##  RAISINS Growth Rate Estimation and Trend Analysis - help behind every "?" (as in RAISINS fsQCA)
##  Each topic: title, what (one or two sentences), read (short bullets).
##  Plain text only, so it can be edited without touching the app code.
## ============================================================================

GRE_HELP <- list(

  analyses = list(
    title = "Analyses to run",
    what  = "Choose which analyses Run Analysis calculates. Each one fills its own tab.",
    read  = c("Growth rates also gives the group comparison when you select a group column.",
              "Area-Yield decomposition needs columns for area and production.",
              "Changing a tick, a column or the file asks for a new Run.")),

  growth = list(
    title = "Growth rates",
    what  = "How fast each series grew per year, measured in three ways.",
    read  = c("CGR is the compound growth rate from a straight line fitted to the logarithm of the series on year. It uses every year and comes with a confidence interval and p-value.",
              "CAGR uses only the first and last observed values, so an unusual end year can move it.",
              "The annual average growth rate (AAGR) is the mean of the year-to-year percentage changes in adjacent calendar years.",
              "Lag-1 r describes how strongly adjacent residuals are related. A large value means the CGR interval and p-value may be too optimistic.",
              "A blank rate means it cannot be calculated, usually because of zero or negative values.")),

  trend = list(
    title = "Trend model comparison",
    what  = "Linear, quadratic, cubic and exponential trends fitted to each series and compared by AICc on the original scale.",
    read  = c("Lower AICc is better. Best marks the preferred model.",
              "R-squared is the share of variation the curve explains, on the original scale. It can only rise as polynomial terms are added, so choose the model by AICc.",
              "Models within about 2 AICc units have similar support; prefer the simpler one.",
              "A good fit describes the past. It does not show why the series changed, and higher-order curves can mislead when extended beyond the data.")),

  mk = list(
    title = "Mann-Kendall test and Sen's slope",
    what  = "A non-parametric test of whether a series tends to move in one direction, with a robust estimate of the change per year.",
    read  = c("Kendall's tau runs from -1 to 1 and gives the direction and strength of the trend; S and Z are the test statistics.",
              "A p-value below the chosen level of significance indicates a monotonic trend, not necessarily a straight line.",
              "Sen's slope is the median slope over all pairs of years, in series units per year, with its confidence interval.",
              "The test assumes independent observations, and p-values are not adjusted across series.")),

  instab = list(
    title = "Instability",
    what  = "How much a series varies from year to year.",
    read  = c("CV is the coefficient of variation, in percent. A steady trend alone raises it.",
              "CDVI, the Cuddy-Della Valle Index, adjusts CV for the fitted linear trend, so it is the better measure of instability around the trend.",
              "Lower values mean a more stable series.",
              "Both need nonnegative observations and a positive mean.")),

  decomp = list(
    title = "Area-yield decomposition",
    what  = "Splits the change in production between the first and last periods into an area effect, a yield effect and their interaction.",
    read  = c("Shares are percentages of the total change in production.",
              "Effects in opposite directions can give negative shares or shares above 100%.",
              "Shares are undefined when production did not change between the end periods.",
              "It needs columns for area and production, and is ticked under Analyses to run.")),

  groups = list(
    title = "Group and combined growth",
    what  = "Growth estimates for each group, such as a district, and for all the groups combined.",
    read  = c("Appears when you select a group column and tick Growth rates.",
              "Sum only additive series such as area and production across groups.",
              "Combined yield is total production divided by total area.",
              "These are descriptive comparisons, not formal tests of differences between groups.")),

  quality = list(
    title = "Coverage and exclusions",
    what  = "What data the results are based on: the period covered, years with no record, missing values and any series left out by the data checks.",
    read  = c("Check this table before reporting the results.",
              "Gaps in the years or many missing values make growth estimates less reliable.")),

  interpretation = list(
    title = "Interpretation",
    what  = "A plain-language summary of every analysis that was run, written from your results.",
    read  = c("It does not yet cover the Group Comparison tab.",
              "Have it checked by someone who knows the data before you use it in a publication.")),

  report = list(
    title = "Statistical report",
    what  = "Downloads the tables and sections you select as an HTML, PDF, Word or Excel file.",
    read  = c("Only the analyses that were run can be selected.",
              "Fill in Data provenance so readers know the source, units and any definition changes.",
              "Font, table style and digits come from the settings bar on the results tabs."))
)

## "?" link next to a heading.
gre_help_link <- function(topic) {
  actionLink(paste0("gre_help_", topic), icon("question-circle"),
             class = "gre-help-link", title = "What is this?")
}

## Table heading with its "?".
gre_heading <- function(text, topic, level = tags$h4) level(text, gre_help_link(topic))

## One observer per help topic.
gre_help_server <- function(input) {
  lapply(names(GRE_HELP), function(topic) {
    observeEvent(input[[paste0("gre_help_", topic)]], {
      h <- GRE_HELP[[topic]]
      showModal(modalDialog(
        title = h$title,
        tags$p(h$what),
        tags$ul(lapply(h$read, tags$li)),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })
  invisible(NULL)
}

## Compact data-check list (same look as fsQCA), from gre_data_checks().
gre_data_check_panel <- function(checks) {
  if (!length(checks$issues)) return(NULL)
  blocked <- isTRUE(checks$blocked)
  n <- length(checks$issues)
  head_txt <- if (blocked) "Data check: please fix the problems below before running"
              else sprintf("Data check: passed, with %d note%s", n, if (n == 1L) "" else "s")
  div(class = paste("gre-datacheck", if (blocked) "has-error"), role = "status",
      tags$div(class = "gre-datacheck-head", head_txt),
      tags$ul(lapply(checks$issues, function(issue) {
        tags$li(
          tags$span(class = "gre-chk-mark", "!"),
          tags$b(paste0(issue$title, ":")),
          paste(utils::head(issue$entries, 10L), collapse = "; "),
          if (length(issue$entries) > 10L)
            tags$details(tags$summary(paste("Show all", length(issue$entries), "affected entries")),
                         lapply(issue$entries, tags$div)),
          tags$div(issue$action))
      })),
      if (!blocked && length(checks$skipped))
        tags$div("Run continues with the other selected columns. The columns that cannot be analysed are skipped."))
}

## Shown in the main panel before a file is uploaded (same look as fsQCA).
gre_start_content <- function() {
  circle <- function(id, icon_name, class, label) {
    div(class = "circle-wrapper",
        div(class = paste("circle-icon", class),
            actionLink(id, label = NULL, icon = icon(icon_name),
                       style = "font-size:45px; text-decoration:none;")),
        div(class = "circle-label", label))
  }
  tab_link <- function(tab) {
    paste0("<a href='#' onclick=\"document.querySelector('a[data-value=&quot;", tab, "&quot;]').click(); return false;\" ",
           "style='font-weight: bold; color: #2E8B57; text-decoration: underline; cursor: pointer;'>",
           "<span style='display: inline-block; margin-left: 4px;'>&#128073;</span>click here</a>")
  }
  tagList(
    tags$style(HTML("
.circle-wrapper { text-align: center; width: 200px; }
.circle-icon {
  width: 80px; height: 75px; border-radius: 50%; background: white;
  display: flex; align-items: center; justify-content: center; margin: 0 auto;
  cursor: pointer; transition: all 0.35s ease; box-shadow: 0 8px 20px rgba(0,0,0,0.08);
}
.circle-icon i { font-size: 45px; transition: all 0.35s ease; }
.circle-upload { border: 3px solid #2E8B57; color: #2E8B57; }
.circle-upload:hover { background: #2E8B57; color: white; transform: translateY(-6px); box-shadow: 0 15px 30px rgba(0,0,0,0.2); }
.circle-csv { border: 3px solid #1f618d; color: #1f618d; }
.circle-csv:hover { background: #1f618d; color: white; transform: translateY(-6px); box-shadow: 0 15px 30px rgba(0,0,0,0.2); }
.circle-trans { border: 3px solid #e67e22; color: #e67e22; }
.circle-trans:hover { background: #e67e22; color: white; transform: translateY(-6px); box-shadow: 0 15px 30px rgba(0,0,0,0.2); }
.circle-analysis { border: 3px solid #8e44ad; color: #8e44ad; }
.circle-analysis:hover { background: #8e44ad; color: white; transform: translateY(-6px); box-shadow: 0 15px 30px rgba(0,0,0,0.2); }
.circle-label { margin-top: 12px; font-weight: 600; font-size: 14px; color: #496f9e; }
.circle-wrapper a { color: black !important; text-decoration: none !important; }
.circle-wrapper a:visited { color: black !important; }
.circle-wrapper a:hover { color: inherit !important; }
")),
    div(
      style = "display:flex; justify-content:center; gap:40px; flex-wrap:wrap; margin-top:30px;",
      circle("gre_btn_upload",   "file-upload", "circle-upload",   "Data Upload Instructions"),
      circle("gre_btn_csv",      "file-csv",    "circle-csv",      "How to Save as CSV"),
      circle("gre_btn_analysis", "chart-line",  "circle-analysis", "How to Perform Analysis"),
      circle("gre_btn_growth",   "seedling",    "circle-trans",    "CGR, CAGR and Trend")
    ),
    div(
      style = "margin-top:40px; padding:30px; background:#ffffff; border-radius:16px; box-shadow:0 10px 25px rgba(0,0,0,0.08);",
      p(style = "font-size: 16px; line-height: 1.6; margin-bottom: 15px; text-align: left;",
        "Before uploading your file for analysis, please read these instructions carefully.
         In RAISINS you can upload file in xls, xlsx or csv format.
         But we recommend a csv format as it will be much lighter."),
      p(style = "font-size: 16px; line-height: 1.7; margin-bottom: 20px; text-align: left;",
        HTML(paste0(
          "Arrange your data with <b>one row per year</b> (or per year and group): a year column and ",
          "one column for each series. ",
          "RAISINS provides a built-in feature to Arrange Data ",
          tab_link("Arrange Data"),
          " or select the <b>Arrange Data</b> tab above",
          "<span style='display: inline-block; margin-left: 4px;'>&#9757;&#65039;</span>",
          " to generate your dataset. To explore sample datasets instead, ",
          tab_link("Datasets"),
          " or select the <b>Datasets</b> tab above",
          "<span style='display: inline-block; margin-left: 4px;'>&#9757;&#65039;</span>."))),
      p(style = "font-size: 16px; line-height: 1.6; margin-bottom: 15px; text-align: left; border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9;",
        span(style = "font-weight: bold; color: green;", "Column Naming Guidelines: "),
        "Always start the column names with a letter. Use only letters, numbers, dots (.) or underscores (_),
         kindly avoid spaces and special characters."),
      p(style = "font-size: 14px; font-weight: bold; color: #27ae60; margin-top: 20px;font-family: 'Times New Roman', Times, serif;",
        "Once your data is ready, you're all set to enjoy growth rate analysis in RAISINS!")
    )
  )
}
