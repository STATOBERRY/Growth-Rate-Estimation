gre_arrange_template <- function(start, end, series, grouped = FALSE, group_name = "Group", groups = "") {
  is_year <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x == trunc(x) && x >= 1 && x <= 9999
  if (!is_year(start) || !is_year(end) || start > end)
    stop("Enter whole starting and ending years from 1 to 9999, with the starting year first.", call. = FALSE)

  lines <- function(x) {
    items <- trimws(strsplit(x, "\r?\n")[[1]])
    items[nzchar(items)]
  }
  series <- lines(series)
  group_name <- trimws(group_name)
  if (grouped && (!nzchar(group_name) || tolower(group_name) == "year"))
    stop("Enter a group column name different from Year.", call. = FALSE)
  if (!length(series) || anyDuplicated(tolower(c("Year", if (grouped) group_name, series))))
    stop("Enter at least one series name. Year, group and series column names must be distinct.", call. = FALSE)

  groups <- if (grouped) lines(groups) else ""
  if (!length(groups) || anyDuplicated(groups))
    stop("Enter unique group names, one per line.", call. = FALSE)

  years <- seq.int(start, end)
  rows <- length(years) * length(groups)
  if (rows > 10000 || rows * (length(series) + 1L + grouped) > 100000)
    stop("Use a smaller year range or fewer groups or series. The editor supports up to 10,000 rows and 100,000 cells.", call. = FALSE)
  df <- data.frame(Year = rep(years, each = length(groups)))
  if (grouped) df[[group_name]] <- rep(groups, times = length(years))
  for (name in series) df[[name]] <- rep(NA_real_, rows)
  list(data = df, series = series, grouped = grouped)
}

gre_arrange_server <- function(input, output, session, error_support = NULL) {
  gre_import_server(input, output)
  template <- reactiveVal(NULL)
  values <- reactiveVal(NULL)
  problem <- reactiveVal(NULL)
  if (!is.null(error_support)) downloadHandler <- error_support$downloadHandler

  observeEvent(input$gre_arrange_create, {
    next_template <- tryCatch(
      gre_arrange_template(input$gre_arrange_start, input$gre_arrange_end,
        input$gre_arrange_series, isTRUE(input$gre_arrange_grouped),
        input$gre_arrange_group_name, input$gre_arrange_groups),
      error = function(e) e
    )
    if (inherits(next_template, "error")) {
      problem(conditionMessage(next_template))
      return()
    }
    problem(NULL)
    values(next_template$data)
    template(next_template)
  })

  output$gre_arrange_status <- renderUI({
    if (!is.null(problem())) return(tags$p(class = "text-danger", role = "alert", problem()))
    spec <- template()
    if (is.null(spec)) return(tags$p("Create a template to begin."))
    tags$p(paste(nrow(spec$data), "rows created."),
           if (spec$grouped) "In Analysis, select Year and your group column before running.")
  })

  output$gre_arrange_table <- rhandsontable::renderRHandsontable({
    spec <- template()
    req(spec)
    grid <- rhandsontable::rhandsontable(spec$data, rowHeaders = NULL, height = 420, contextMenu = FALSE)
    for (name in setdiff(names(spec$data), spec$series))
      grid <- rhandsontable::hot_col(grid, name, readOnly = TRUE)
    grid
  })

  observeEvent(input$gre_arrange_table, {
    spec <- template()
    req(spec)
    edited <- rhandsontable::hot_to_r(input$gre_arrange_table)
    if (nrow(edited) != nrow(spec$data) || !identical(names(edited), names(spec$data))) return()
    keys <- setdiff(names(spec$data), spec$series)
    same_keys <- vapply(keys, function(key) identical(as.character(edited[[key]]),
                                                    as.character(spec$data[[key]])), logical(1))
    if (!all(same_keys)) return()
    data <- spec$data
    data[spec$series] <- edited[spec$series]
    values(data)
  })

  output$gre_arrange_download_ui <- renderUI({
    req(template())
    downloadButton("gre_arrange_csv", "Download CSV", class = "btn-success")
  })
  output$gre_arrange_csv <- downloadHandler(
    filename = function() paste0("growth_data_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(req(values()), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  )
}
