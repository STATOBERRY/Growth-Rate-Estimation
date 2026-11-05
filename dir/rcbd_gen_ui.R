gre_arrange_ui <- function() {
  year <- as.integer(format(Sys.Date(), "%Y"))
  fluidPage(
    titlePanel("Annual data file creator"),
    sidebarLayout(
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
    )
  )
}
