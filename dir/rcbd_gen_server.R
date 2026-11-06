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

gre_import_year <- function(x) {
  x <- trimws(x)
  x <- gsub("[\u2013\u2014/]", "-", x)
  start <- suppressWarnings(as.integer(substr(x, 1L, 4L)))
  suffix <- sub("^[0-9]{4}-", "", x)
  valid <- grepl("^[0-9]{4}$", x) |
    (grepl("^[0-9]{4}-([0-9]{2}|[0-9]{4})$", x) &
       (suffix == as.character(start + 1L) |
         suffix == sprintf("%02d", (start + 1L) %% 100L)))
  start[is.na(valid) | !valid | is.na(start) | start < 1L] <- NA_integer_
  start
}

gre_import_reshape <- function(block, layout, year_col, group_col, series_cols, period_row, measure_name = "Value") {
  clean <- function(x) trimws(gsub("[[:space:]]+", " ", ifelse(is.na(x), "", x)))
  block[] <- lapply(block, clean)
  wide <- layout %in% c("wide", "matrix")
  headers <- if (layout == "wide") 2L else 1L
  if (nrow(block) <= headers) stop("The selected range needs headers and at least one data row.")
  data <- block[-seq_len(headers), , drop = FALSE]
  data <- data[rowSums(data != "") > 0L, , drop = FALSE]
  if (!nrow(data)) stop("The selected range has no data rows.")
  if (!length(series_cols)) stop("Select at least one measure column.")
  if (group_col %in% series_cols || (!wide && (year_col %in% c(group_col, series_cols))))
    stop("Use different columns for year, group and measures.")
  fill_header <- function(row) {
    values <- unlist(block[row, ], use.names = FALSE)
    for (i in seq_along(values)[-1L]) if (!nzchar(values[i])) values[i] <- values[i - 1L]
    values
  }
  measures <- switch(layout,
    matrix = rep(trimws(measure_name), length(series_cols)),
    wide = fill_header(3L - period_row)[series_cols],
    unlist(block[1L, series_cols], use.names = FALSE))
  if (any(!nzchar(measures)) || any(tolower(measures) %in% c("year", "group")))
    stop("Measure headers must be present and different from Year and Group.")
  convert <- function(x) {
    missing <- tolower(x) %in% c("", "na", "n/a", "-", "\u2014")
    x[missing] <- NA_character_
    values <- suppressWarnings(as.numeric(x))
    invalid <- !missing & !is.finite(values)
    if (any(invalid)) stop(paste0("A selected measure contains nonnumeric text: ", x[which(invalid)[1]], ". Review the range or column mapping."))
    values
  }
  group <- if (group_col > 0L) data[[group_col]] else NULL
  if (!is.null(group) && any(!nzchar(group))) stop("Every selected data row needs a group name or ID.")
  if (wide) {
    periods <- if (layout == "matrix") unlist(block[1L, ], use.names = FALSE) else fill_header(period_row)
    years <- gre_import_year(periods[series_cols])
    if (anyNA(years)) stop("Each selected measure needs a year or reporting year in the period header.")
    if (anyDuplicated(data.frame(years, measures))) stop("Each period must have at most one column per measure.")
    if (is.null(group) && nrow(data) != 1L) stop("Choose a group column, or select only one data row.")
    result <- do.call(rbind, lapply(sort(unique(years)), function(year) {
      part <- data.frame(Year = rep(year, nrow(data)))
      if (!is.null(group)) part$Group <- group
      for (name in unique(measures)) {
        column <- series_cols[years == year & measures == name]
        part[[name]] <- if (length(column)) convert(data[[column]]) else NA_real_
      }
      part
    }))
  } else {
    if (anyDuplicated(measures)) stop("Measure headers must be unique.")
    year <- gre_import_year(data[[year_col]])
    if (anyNA(year)) stop("The period column must contain annual years or reporting years such as 2022-23.")
    result <- data.frame(Year = year)
    if (!is.null(group)) result$Group <- group
    for (i in seq_along(measures)) result[[measures[i]]] <- convert(data[[series_cols[i]]])
  }
  keys <- intersect(c("Year", "Group"), names(result))
  if (anyDuplicated(result[keys])) stop("Duplicate year/group records found. Select one block and remove overlapping totals or duplicate rows.")
  result[order(result$Year), , drop = FALSE]
}

gre_import_server <- function(input, output) {
  excel <- reactive({
    req(input$gre_import_file)
    tolower(tools::file_ext(input$gre_import_file$name)) %in% c("xlsx", "xls")
  })
  sheets <- reactive(readxl::excel_sheets(req(input$gre_import_file)$datapath))
  presets <- list(
    states = list(label = "Area and production: states", sheet = "area pdn", range = "B8:K26",
                  group = "State/Districts"),
    kerala = list(label = "Area and production: Kerala districts", sheet = "area pdn", range = "N8:W23",
                  group = "State : Kerala. State/Districts"),
    exports = list(label = "Trade: exports", sheet = "import and export", range = "B10:L19", group = "Item"),
    imports = list(label = "Trade: imports", sheet = "import and export", range = "B26:L35", group = "Item")
  )
  preset <- reactive({
    key <- input$gre_import_preset %||% "manual"
    if (key %in% names(presets) && excel() && presets[[key]]$sheet %in% sheets())
      presets[[key]]
  })
  output$gre_import_preset_ui <- renderUI({
    available <- if (excel()) Filter(function(x) x$sheet %in% sheets(), presets) else list()
    options <- c(setNames(names(available), vapply(available, function(x) x$label, "")),
                 "Other layout (manual mapping)" = "manual")
    selectInput("gre_import_preset", "Choose a table", options, selected = unname(options[1L]))
  })
  output$gre_import_sheet_ui <- renderUI({
    if (excel()) selectInput("gre_import_sheet", "Workbook sheet", sheets())
  })
  block <- reactive({
    file <- req(input$gre_import_file)
    selected <- preset()
    range <- if (!is.null(selected)) selected$range else toupper(trimws(input$gre_import_range %||% ""))
    shiny::validate(need(!nzchar(range) || grepl("^[A-Z]{1,3}[1-9][0-9]{0,5}:[A-Z]{1,3}[1-9][0-9]{0,5}$", range),
                         "Enter a rectangular range such as B8:K26."))
    tryCatch({
      limits <- if (nzchar(range)) cellranger::as.cell_limits(range) else NULL
      if (!is.null(limits) && (any(limits$lr < limits$ul) || prod(limits$lr - limits$ul + 1) > 100000))
        stop("Select a range of up to 100,000 cells.")
      if (excel()) {
        sheet <- if (!is.null(selected)) selected$sheet else input$gre_import_sheet
        if (is.null(sheet) || !sheet %in% sheets()) sheet <- sheets()[1L]
        if (is.null(limits)) limits <- cellranger::cell_limits(c(1L, 1L), c(100L, 40L))
        d <- as.data.frame(readxl::read_excel(file$datapath, sheet = sheet, range = limits,
          col_names = FALSE, col_types = "text", .name_repair = "minimal"))
        keep_rows <- which(rowSums(!is.na(d)) > 0L)
        keep_cols <- which(colSums(!is.na(d)) > 0L)
        if (!nzchar(range) && length(keep_rows) && length(keep_cols))
          d <- d[seq_len(max(keep_rows)), seq_len(max(keep_cols)), drop = FALSE]
      } else {
        fields <- utils::count.fields(file$datapath, sep = ",", quote = "\"",
                                      blank.lines.skip = FALSE, comment.char = "")
        width <- max(c(0L, fields), na.rm = TRUE)
        if (!width || length(fields) * width > 100000L)
          stop("Use a CSV block of up to 100,000 cells, or select a range in an Excel workbook.")
        d <- utils::read.csv(file$datapath, header = FALSE, fill = TRUE,
          col.names = paste0("V", seq_len(width)), colClasses = "character",
          encoding = "UTF-8", blank.lines.skip = FALSE)
        if (!is.null(limits)) {
          if (any(limits$lr > c(nrow(d), ncol(d)))) stop("The range extends beyond the CSV.")
          d <- d[seq.int(limits$ul[1], limits$lr[1]), seq.int(limits$ul[2], limits$lr[2]), drop = FALSE]
        }
      }
      names(d) <- paste("Column", seq_len(ncol(d)))
      d
    }, error = function(e) shiny::validate(need(FALSE, conditionMessage(e))))
  })
  output$gre_import_preview <- DT::renderDT(DT::datatable(block(), rownames = TRUE,
    options = list(scrollX = TRUE, pageLength = 6)))
  output$gre_import_mapping <- renderUI({
    d <- block()
    req(ncol(d) >= 2L, nrow(d) >= 2L)
    layout <- input$gre_import_layout %||% "wide"
    headers <- vapply(d, function(x) paste(na.omit(head(x, if (layout == "wide") 2L else 1L)), collapse = " / "), "")
    columns <- setNames(seq_along(d), paste(names(d), headers, sep = " - "))
    tagList(
      if (layout == "wide") selectInput("gre_import_period_row", "Period header row", c("First" = 1, "Second" = 2)),
      if (layout == "long") selectInput("gre_import_year", "Year column", columns, selected = 1),
      if (layout == "matrix") textInput("gre_import_measure", "Name of the measure", "Value"),
      selectInput("gre_import_group", "Group column", c("No group" = 0, columns), selected = if (layout == "long") 0 else 1),
      selectInput("gre_import_series", "Measure columns", columns, multiple = TRUE, selected = seq.int(2L, ncol(d)))
    )
  })
  result <- reactive({
    d <- block()
    selected <- preset()
    if (!is.null(selected)) {
      header <- trimws(d[[1L]][1:2])
      shiny::validate(need(selected$group %in% header,
        "This block differs from the CDB sample. Choose Other layout to select the correct range."))
      return(tryCatch(gre_import_reshape(d, "wide", 0L, 1L, seq.int(2L, ncol(d)), 1L),
        error = function(e) shiny::validate(need(FALSE, conditionMessage(e)))))
    }
    req(input$gre_import_group, input$gre_import_series)
    layout <- input$gre_import_layout %||% "wide"
    period <- if (layout == "wide") as.integer(req(input$gre_import_period_row)) else 1L
    year <- if (layout == "long") as.integer(req(input$gre_import_year)) else 0L
    group <- as.integer(input$gre_import_group)
    series <- as.integer(input$gre_import_series)
    req(all(c(year, group, series) <= ncol(d)))
    tryCatch(gre_import_reshape(d, layout, year, group, series, period, input$gre_import_measure %||% "Value"),
             error = function(e) shiny::validate(need(FALSE, conditionMessage(e))))
  })
  output$gre_import_status <- renderUI({
    d <- result()
    tags$p(paste(nrow(d), "rows ready."),
           if ("Group" %in% names(d)) "In Analysis select Group if combining groups. Review the preview for subtotals before downloading.")
  })
  output$gre_import_result <- DT::renderDT(DT::datatable(result(), rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 8)))
  output$gre_import_download_ui <- renderUI({
    req(tryCatch(result(), shiny.silent.error = function(e) NULL))
    downloadButton("gre_import_download", "Download reshaped CSV", class = "btn-success")
  })
  output$gre_import_download <- downloadHandler(
    filename = function() "reshaped_annual_data.csv",
    content = function(file) utils::write.csv(result(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8"))
}
