gre_arrange_ui <- function() {
  year <- as.integer(format(Sys.Date(), "%Y"))
  fluidPage(
    titlePanel("Annual data file creator"),
    tabsetPanel(type = "pills",
      tabPanel("Create template", sidebarLayout(
      sidebarPanel(
        tags$p("Set the year range and the columns you want to analyse."),
        numericInput("gre_arrange_start", "Starting year", year - 9L, min = 1, max = 9999, step = 1),
        numericInput("gre_arrange_end", "Ending year", year, min = 1, max = 9999, step = 1),
        checkboxInput("gre_arrange_grouped", "Include groups", FALSE),
        conditionalPanel(
          "input.gre_arrange_grouped",
          textInput("gre_arrange_group_name", "Group column name", "Group"),
          textAreaInput("gre_arrange_groups", "Group names, one per line",
                        placeholder = "North\nCentral\nSouth", rows = 4)
        ),
        textAreaInput("gre_arrange_series", "Series names, one per line",
                      value = "Value", placeholder = "Count\nAmount", rows = 4),
        actionButton("gre_arrange_create", "Create template", icon = icon("table"), class = "btn-success")
      ),
      mainPanel(
        h3("Annual data entry"),
        p("The year range includes both endpoints. With groups, the template has one row for every group in every year."),
        p("Type or paste values from Excel into the series columns. Leave missing observations blank. Growth and trend analysis require at least three usable years."),
        uiOutput("gre_arrange_status"),
        rhandsontable::rHandsontableOutput("gre_arrange_table"),
        uiOutput("gre_arrange_download_ui")
      )
    )),
      tabPanel("Import and reshape", gre_import_ui())
    )
  )
}
gre_import_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      fileInput("gre_import_file", "Upload workbook or CSV", accept = c(".xlsx", ".xls", ".csv")),
      uiOutput("gre_import_preset_ui"),
      conditionalPanel("!input.gre_import_preset || input.gre_import_preset === 'manual'",
        uiOutput("gre_import_sheet_ui"),
        textInput("gre_import_range", "Table range", placeholder = "e.g. B8:K26"),
        tags$small("Select one rectangular block including its headers. Leave out notes and overlapping totals."),
        selectInput("gre_import_layout", "Table layout", c(
          "Periods across columns, two header rows" = "wide",
          "Years across columns, one measure" = "matrix",
          "Periods down rows, one header row" = "long")),
        uiOutput("gre_import_mapping")
      )
    ),
    mainPanel(
      h3("Import and reshape"),
      p("Upload your file. If it matches the CDB sample layout, choose a table and review the preview before downloading. Use manual mapping for other layouts."),
      p("Reporting periods such as 2022-23 automatically become 2022 for analysis. Daily and monthly observations need an annual summary first."),
      h4("Source preview"), DT::DTOutput("gre_import_preview"),
      h4("Analysis-ready preview"), uiOutput("gre_import_status"),
      DT::DTOutput("gre_import_result"), uiOutput("gre_import_download_ui")
    )
  )
}
