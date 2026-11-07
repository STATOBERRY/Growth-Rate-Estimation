## ============================================================================
##  RAISINS Growth Rate Estimation and Trend Analysis - Arrange Data > Import and reshape
##  Turns a table in almost any annual layout into the analysis format:
##  Year, optional Group, then one numeric column per series.
##    gre_import_read()    file -> table of text cells (CSV, TSV, XLS, XLSX)
##    gre_import_detect()  table -> which layout it is and which cells mean what
##    gre_import_build()   table + layout -> analysis-ready data and notes
##    gre_import_ui()/gre_import_server()  the Shiny tab
##  Values are never guessed: a cell that cannot be read as a number stops the
##  reshape with its position, and every row left out is reported in the notes.
## ============================================================================

## ---- Text and number helpers ----------------------------------------------
gre_import_clean <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  for (space in intToUtf8(c(160L, 8201L, 8239L), multiple = TRUE)) x <- gsub(space, " ", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(gsub(" +", " ", x))
}

gre_import_dash <- function(x) {
  for (dash in intToUtf8(c(8208L, 8209L, 8210L, 8211L, 8212L, 8722L), multiple = TRUE)) x <- gsub(dash, "-", x, fixed = TRUE)
  x
}

## First year of an annual period, or NA: 2001, 2001.0, 2022-23, 2022/23, 2022-2023,
## FY 2022-23, 2022-23 (P), 2021*. A range must run into the following year.
gre_import_year <- function(x) {
  x <- gre_import_dash(gre_import_clean(x))
  x <- sub("\\.0+$", "", x)
  x <- sub("^(fy|f\\.y\\.|ay|a\\.y\\.|cy|c\\.y\\.|year|yr|season)[ .:-]*", "", x, ignore.case = TRUE)
  x <- sub("[ *#^+]*(\\(?(p|e|r|f|a|re|be|prov|provisional|est|estimate|estimated|revised|final|actual)\\.?\\)?)?[ *#^+]*$",
           "", x, ignore.case = TRUE)
  x <- gsub(" *[-/] *'?", "-", x)
  start <- suppressWarnings(as.integer(substr(x, 1L, 4L)))
  end <- sub("^[0-9]{4}-", "", x)
  plain <- grepl("^[12][0-9]{3}$", x)
  range <- grepl("^[12][0-9]{3}-([0-9]{2}|[0-9]{4})$", x) &
    (end == as.character(start + 1L) | end == sprintf("%02d", (start + 1L) %% 100L))
  start[!(plain | range)] <- NA_integer_
  start
}

## TRUE for dates, months and quarters, which must be summarised to years first.
gre_import_subannual <- function(x) {
  x <- gre_import_dash(gre_import_clean(x))
  months <- "(^|[^a-z])(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*([^a-z]|$)"
  (grepl(months, tolower(x)) & grepl("[0-9]", x)) |
    grepl("^[0-9]{1,4}[-/.][0-9]{1,2}[-/.][0-9]{1,4}$", x) |
    (grepl("^[12][0-9]{3}-[0-9]{2}$", x) & is.na(gre_import_year(x))) |
    grepl("^(q[1-4][ -]?[12][0-9]{3}|[12][0-9]{3}[ -]?q[1-4])$", tolower(x))
}

## Numbers as people write them: 1,200 and 1,23,456 (grouping), 12%, 123*, (45) as
## negative, 1 200, and 1.234,5 when the file uses decimal commas.
gre_import_number <- function(x, dec = ".") {
  x <- gre_import_dash(gre_import_clean(x))
  low <- tolower(x)
  nil <- low %in% c("nil", "neg", "negl", "negl.")
  blank <- nil | low %in% c("", "na", "n/a", "n.a", "n.a.", "nan", "null", "none", "-", "--", "---",
                            ".", "..", "...", "not available")
  y <- x
  y[blank] <- NA_character_
  y <- gsub("^[*#^+ ]+|[*#^ ]+$", "", y)
  y <- sub(" *%$", "", y)
  y <- sub("^\\(([^()]*)\\)$", "-\\1", y)
  y <- gsub("(?<=[0-9]) (?=[0-9]{3}([ .,]|$))", "", y, perl = TRUE)
  if (identical(dec, ",")) {
    decimal <- !is.na(y) & grepl("^-?[0-9]{1,3}(\\.[0-9]{3})*,[0-9]+$|^-?[0-9]+,[0-9]+$", y)
    y[decimal] <- sub(",", ".", gsub(".", "", y[decimal], fixed = TRUE), fixed = TRUE)
  }
  grouped <- !is.na(y) & grepl("^-?[0-9]{1,3}(,[0-9]{2,3})+(\\.[0-9]+)?$", y)
  y[grouped] <- gsub(",", "", y[grouped], fixed = TRUE)
  values <- suppressWarnings(as.numeric(y))
  list(values = values, invalid = !blank & !is.finite(values), nil = nil)
}

## Share of the filled cells in x that read as numbers (0 when nothing is filled).
gre_import_numeric_share <- function(x, dec = ".") {
  x <- x[nzchar(x)]
  if (!length(x)) return(0)
  mean(!gre_import_number(x, dec)$invalid)
}

## ---- Reading a file --------------------------------------------------------
gre_import_read_text <- function(path, range = NULL) {
  raw_bytes <- readBin(path, "raw", file.info(path)$size)
  if (length(raw_bytes) >= 3L && identical(raw_bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) raw_bytes <- raw_bytes[-(1:3)]
  if (!length(raw_bytes)) stop("This file is empty.")
  text <- rawToChar(raw_bytes[raw_bytes != as.raw(0)])
  note <- NULL
  if (!validUTF8(text)) {
    text <- iconv(text, from = "windows-1252", to = "UTF-8", sub = "?")
    note <- "The file was not saved as UTF-8, so accented text was converted. Check the labels in the preview."
  }
  Encoding(text) <- "UTF-8"
  lines <- strsplit(text, "\r\n|\r|\n", perl = TRUE)[[1]]
  filled <- lines[nzchar(trimws(lines))]
  if (!length(filled)) stop("This file is empty.")
  sample <- utils::head(filled, 200L)
  ## The separator that splits most lines into the same number of cells.
  fields <- function(sep) {
    counts <- vapply(sample, function(l) length(scan(text = l, what = "", sep = sep, quote = "\"",
      quiet = TRUE, comment.char = "", na.strings = NULL)), integer(1))
    list(common = as.integer(names(which.max(table(counts)))), share = max(table(counts)) / length(counts))
  }
  separators <- c(",", ";", "\t", "|")
  stats <- lapply(separators, fields)
  score <- vapply(stats, function(s) if (s$common > 1L) s$common * s$share else 0, numeric(1))
  sep <- if (any(score > 0)) separators[which.max(score)] else ","
  cells <- lapply(lines, function(l) if (nzchar(trimws(l))) scan(text = l, what = "", sep = sep, quote = "\"",
    quiet = TRUE, comment.char = "", na.strings = NULL, strip.white = TRUE) else character())
  width <- max(lengths(cells))
  if (!width || length(cells) * width > 100000L)
    stop("Use a file of up to 100,000 cells, or select a range in an Excel workbook.")
  d <- as.data.frame(do.call(rbind, lapply(cells, function(v) c(v, rep("", width - length(v))))),
                     stringsAsFactors = FALSE)
  if (!is.null(range)) {
    if (any(range$lr > c(nrow(d), ncol(d)))) stop("The range extends beyond the file.")
    d <- d[seq.int(range$ul[1], range$lr[1]), seq.int(range$ul[2], range$lr[2]), drop = FALSE]
  }
  list(data = d, dec = if (sep == ";") "," else ".", note = note)
}

## Returns list(data, dec, note). Cells are all text; row numbers match the file
## (or the selected range), so a message can point at the cell.
gre_import_read <- function(path, filename, sheet = NULL, range = "") {
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("xlsx", "xls", "csv", "tsv", "txt")) stop("Choose an XLSX, XLS, CSV, TSV or TXT file.")
  range <- toupper(trimws(range %||% ""))
  if (nzchar(range) && !grepl("^[A-Z]{1,3}[1-9][0-9]{0,5}:[A-Z]{1,3}[1-9][0-9]{0,5}$", range))
    stop("Enter a rectangular range such as B8:K26, or leave it empty to use the whole sheet.")
  limits <- if (nzchar(range)) cellranger::as.cell_limits(range) else NULL
  if (!is.null(limits) && (any(limits$lr < limits$ul) || prod(limits$lr - limits$ul + 1) > 100000))
    stop("Select a range of up to 100,000 cells, with its first cell at the top left.")
  if (ext %in% c("xlsx", "xls")) {
    d <- tryCatch({
      sheets <- readxl::excel_sheets(path)
      if (is.null(sheet) || !sheet %in% sheets) sheet <- sheets[1L]
      as.data.frame(readxl::read_excel(path, sheet = sheet, range = if (is.null(limits)) NULL else limits,
        col_names = FALSE, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
    }, error = function(e) stop("The workbook could not be read. Open it in Excel, remove any password, save a new XLSX copy and upload that.", call. = FALSE))
    out <- list(data = d, dec = ".", note = NULL)
  } else {
    out <- gre_import_read_text(path, limits)
  }
  d <- out$data
  if (!nrow(d) || !ncol(d)) stop("The selected sheet or range is empty.")
  d[] <- lapply(d, gre_import_clean)
  if (is.null(limits)) {
    rows <- which(rowSums(d != "") > 0L)
    cols <- which(colSums(d != "") > 0L)
    if (!length(rows)) stop("The selected sheet or range is empty.")
    d <- d[seq_len(max(rows)), seq_len(max(cols)), drop = FALSE]
  }
  names(d) <- paste("Column", seq_len(ncol(d)))
  rownames(d) <- NULL
  out$data <- d
  out
}

## ---- Recognising the layout ------------------------------------------------
gre_import_labels <- list(
  auto = "Detect automatically",
  long = "Years down the rows, a column for each series",
  stacked = "Years down the rows, one column names the series, one holds the value",
  across = "Years across the top, one series (rows are groups)",
  across_series = "Years across the top, each row is a series",
  wide = "Years across the top, two header rows (year and series)"
)

gre_import_kw <- list(
  year = "(^|[^a-z])(year|yr|period|fy|season|time)([^a-z]|$)",
  measure = "(^|[^a-z])(series|variable|indicator|measure|parameter|particulars?|item|commodity|metric)([^a-z]|$)",
  value = "(^|[^a-z])(values?|amount|quantity|figure|data)([^a-z]|$)",
  series_label = "^(area|acreage|production|prod|yield|productivity|price|exports?|imports?|consumption|quantity|output|sales|revenue|income|population|rainfall|cost)([^a-z]|$)",
  skip = "^(s\\.? ?no\\.?|sl\\.? ?no\\.?|sr\\.? ?no\\.?|serial( no\\.?| number)?|#|no\\.?|units?|uom)$"
)

## The contiguous run of filled columns around a column (a blank column ends a table).
gre_import_table_cols <- function(m, col) {
  empty <- colSums(m != "") == 0L
  left <- col
  while (left > 1L && !empty[left - 1L]) left <- left - 1L
  right <- col
  while (right < ncol(m) && !empty[right + 1L]) right <- right + 1L
  seq.int(left, right)
}

gre_import_find_long <- function(m, is_year) {
  best <- NULL
  for (col in seq_len(ncol(m))) {
    hits <- which(is_year[, col])
    if (length(hits) < 2L) next
    first <- hits[1L]
    if (first < 2L) next
    ## Judge only down to the last year, so notes or totals underneath do not count against it.
    span <- first:hits[length(hits)]
    below <- m[span, col]
    filled <- below[nzchar(below)]
    if (mean(is_year[span, col][nzchar(below)]) < 0.7) next
    if (length(unique(gre_import_year(filled[!is.na(gre_import_year(filled))]))) < 2L) next
    head_text <- tolower(m[first - 1L, col])
    if (!nzchar(head_text) || !is.na(gre_import_year(head_text))) next
    keyword <- grepl(gre_import_kw$year, head_text)
    candidate <- list(col = col, first = first, header = first - 1L, keyword = keyword, count = length(hits))
    if (is.null(best) || (keyword && !best$keyword)) best <- candidate
  }
  best
}

gre_import_find_across <- function(m, is_year, dec) {
  for (row in seq_len(min(nrow(m), 30L))) {
    cols <- which(is_year[row, ])
    if (length(cols) < 1L || row >= nrow(m)) next
    if (length(unique(gre_import_year(m[row, cols]))) < 2L) next
    below <- m[(row + 1L):nrow(m), cols, drop = FALSE]
    if (!any(nzchar(below))) next
    return(list(row = row, cols = cols))
  }
  NULL
}

## Which rows hold data for an across layout: stops at a blank row followed by a
## second table's header.
gre_import_data_end <- function(m, is_year, start, cols) {
  if (start > nrow(m)) return(start - 1L)
  for (row in seq.int(start, nrow(m))) {
    if (row > start && !any(nzchar(m[row - 1L, ])) && sum(is_year[row, cols]) >= 2L) return(row - 2L)
    if (row > start + 1L && sum(is_year[row, cols]) >= max(2L, 0.8 * length(cols))) return(row - 1L)
  }
  nrow(m)
}

## Returns the mapping: layout, header (row), second (row), year_col, group_col,
## measure_col, value_cols, first/last data row. Stops when nothing fits.
gre_import_detect <- function(block, layout = "auto", dec = ".") {
  m <- as.matrix(block)
  dimnames(m) <- NULL
  is_year <- matrix(!is.na(gre_import_year(m)), nrow(m), ncol(m))
  long <- gre_import_find_long(m, is_year)
  across <- gre_import_find_across(m, is_year, dec)
  want_long <- layout %in% c("long", "stacked")
  want_across <- layout %in% c("across", "across_series", "wide")
  use_long <- if (want_long) TRUE else if (want_across) FALSE else if (is.null(across)) TRUE
              else if (is.null(long)) FALSE else if (across$row > long$first) TRUE
              else if (across$row == long$first) long$keyword else FALSE
  if (use_long && is.null(long) || (!use_long && is.null(across))) {
    if (sum(gre_import_subannual(m)) >= 2L)
      stop("This table looks monthly, quarterly or daily. Growth analysis needs one value per year, so make an annual summary first.")
    stop("No column or row of years was found. Choose the table layout yourself, or select the range of one table.")
  }
  if (use_long) gre_import_map_long(m, long, layout, dec)
  else gre_import_map_across(m, is_year, across, layout, dec)
}

gre_import_map_long <- function(m, long, layout, dec) {
  table_cols <- gre_import_table_cols(m, long$col)
  rows <- seq.int(long$first, nrow(m))
  second_head <- which(rows > long$first & (m[rows, long$col] == m[long$header, long$col]))
  last <- if (length(second_head)) rows[second_head[1L]] - 1L else nrow(m)
  rows <- seq.int(long$first, last)
  usable <- rows[is_year_cell(m, rows, long$col)]
  headers <- m[long$header, ]
  others <- setdiff(table_cols, long$col)
  others <- others[!grepl(gre_import_kw$skip, tolower(headers[others]))]
  others <- others[vapply(others, function(j) any(nzchar(m[usable, j])), logical(1))]
  shares <- vapply(others, function(j) gre_import_numeric_share(m[usable, j], dec), numeric(1))
  numeric_cols <- others[shares >= 0.5 & nzchar(headers[others])]
  text_cols <- setdiff(others, numeric_cols)
  text_cols <- text_cols[nzchar(headers[text_cols]) | vapply(text_cols, function(j) length(unique(m[usable, j])) > 1L, logical(1))]
  measure <- text_cols[grepl(gre_import_kw$measure, tolower(headers[text_cols]))]
  if (identical(layout, "stacked") && !length(measure) && length(text_cols)) measure <- text_cols[length(text_cols)]
  value_named <- numeric_cols[grepl(gre_import_kw$value, tolower(headers[numeric_cols]))]
  stacked <- identical(layout, "stacked") ||
    (layout == "auto" && length(measure) >= 1L && length(numeric_cols) >= 1L)
  if (stacked && length(measure)) {
    group <- setdiff(text_cols, measure[1L])
    return(list(layout = "stacked", header = long$header, second = 0L, year_col = long$col,
                group_col = if (length(group)) group[1L] else 0L, measure_col = measure[1L],
                value_cols = if (length(value_named)) value_named[1L] else numeric_cols[1L],
                first = long$first, last = last))
  }
  ## Without a keyword, a text column with one numeric column could be either; treat it as a group.
  list(layout = "long", header = long$header, second = 0L, year_col = long$col,
       group_col = if (length(text_cols)) text_cols[1L] else 0L, measure_col = 0L,
       value_cols = numeric_cols, first = long$first, last = last)
}

is_year_cell <- function(m, rows, col) !is.na(gre_import_year(m[rows, col]))

gre_import_map_across <- function(m, is_year, across, layout, dec) {
  r <- across$row
  k <- across$cols[1L]
  table_cols <- gre_import_table_cols(m, k)
  right <- table_cols[table_cols >= k]
  text_cell <- function(row, cols) nzchar(m[row, cols]) & is.na(gre_import_year(m[row, cols])) &
    gre_import_number(m[row, cols], dec)$invalid
  below <- r < nrow(m) && sum(text_cell(r + 1L, right)) >= 2L
  above <- r > 1L && sum(text_cell(r - 1L, right)) >= 2L
  wide <- if (layout == "wide") TRUE else if (layout %in% c("across", "across_series")) FALSE else below || above
  second <- 0L
  if (wide) second <- if (below) r + 1L else if (above) r - 1L else
    stop("A second header row with the series names was not found. Choose another layout.")
  periods <- m[r, ]
  if (wide && second > r) {
    for (j in right[-1L]) if (!nzchar(periods[j])) periods[j] <- periods[j - 1L]
    value_cols <- right[!is.na(gre_import_year(periods[right])) & nzchar(m[second, right])]
  } else {
    value_cols <- intersect(across$cols, right)
  }
  first <- max(r, second) + 1L
  last <- gre_import_data_end(m, is_year, first, value_cols)
  rows <- seq.int(first, max(first, last))
  ## A text column inside the table, such as remarks, is not a series.
  if (wide) value_cols <- value_cols[vapply(value_cols, function(j) {
    v <- m[rows, j]
    !any(nzchar(v)) || gre_import_numeric_share(v, dec) >= 0.5
  }, logical(1))]
  id <- table_cols[table_cols < k]
  ## Label columns to the left of the years, named by their header cells.
  head_text <- tolower(vapply(id, function(j) {
    v <- m[seq.int(min(r, if (second > 0L) second else r), first - 1L), j]
    paste(v[nzchar(v)], collapse = " ")
  }, ""))
  id <- id[!grepl(gre_import_kw$skip, head_text) & vapply(id, function(j) any(nzchar(m[rows, j])), logical(1))]
  head_text <- head_text[match(id, table_cols[table_cols < k])]
  measure_id <- id[grepl(gre_import_kw$measure, head_text)]
  ## Rows called Area, Production and so on are series, whatever the column header says.
  if (!length(measure_id)) measure_id <- id[vapply(id, function(j) {
    v <- tolower(m[rows, j])
    v <- v[nzchar(v) & !grepl("^(source|note)", v)]
    length(v) > 0L && all(grepl(gre_import_kw$series_label, v))
  }, logical(1))]
  by_series <- identical(layout, "across_series") || (layout == "auto" && !wide && length(measure_id) >= 1L)
  if (identical(layout, "across_series") && !length(measure_id) && length(id)) measure_id <- id[length(id)]
  group <- setdiff(id, if (by_series) measure_id[1L] else integer())
  list(layout = if (wide) "wide" else if (by_series && length(measure_id)) "across_series" else "across",
       header = r, second = second, year_col = 0L,
       group_col = if (length(group)) group[1L] else 0L,
       measure_col = if (by_series && length(measure_id)) measure_id[1L] else 0L,
       value_cols = value_cols, first = first, last = last)
}

## ---- Building the analysis table ------------------------------------------
gre_import_total_pattern <- "^(grand |sub[- ]?|state )?totals?([^a-z]|$)|^all[- ]india([^a-z]|$)|^overall$|^combined$|^all (states|districts|regions|groups|items)$"

## Long-form records -> Year, optional Group, then one column per series.
gre_import_pivot <- function(rec, dec) {
  parsed <- gre_import_number(rec$raw, dec)
  if (any(parsed$invalid)) {
    i <- which(parsed$invalid)[1L]
    hint <- if (grepl("^-?[0-9]+,[0-9]+$", rec$raw[i])) " It may use a decimal comma; save the file with a decimal point or with semicolons between columns." else ""
    stop(sprintf("Row %d, under \"%s\", contains \"%s\", which is not a number.%s Correct the cell, or select different columns or a smaller range.",
                 rec$row[i], rec$measure[i], rec$raw[i], hint), call. = FALSE)
  }
  rec$value <- parsed$values
  has_group <- !all(rec$group == "")
  key <- paste(rec$year, rec$group, rec$measure, sep = "\r")
  if (anyDuplicated(key)) {
    i <- which(duplicated(key))[1L]
    stop(sprintf("Year %d%s has two values for \"%s\". Select one table, and leave out overlapping totals or repeated rows.",
                 rec$year[i], if (nzchar(rec$group[i])) paste0(", ", rec$group[i]) else "", rec$measure[i]), call. = FALSE)
  }
  measures <- unique(rec$measure)
  ids <- unique(rec[c("year", "group")])
  ids <- ids[order(ids$year, match(ids$group, unique(rec$group))), , drop = FALSE]
  out <- data.frame(Year = ids$year)
  if (has_group) out$Group <- ids$group
  id_key <- paste(ids$year, ids$group, sep = "\r")
  rec_key <- paste(rec$year, rec$group, sep = "\r")
  for (name in measures) {
    keep <- rec$measure == name
    out[[name]] <- rec$value[keep][match(id_key, rec_key[keep])]
  }
  list(data = out, nil = sum(parsed$nil))
}

## spec comes from gre_import_detect() or the manual controls.
## Returns list(data, notes, summary).
gre_import_build <- function(block, spec, dec = ".", drop_totals = TRUE, measure_name = "Value") {
  m <- as.matrix(block)
  dimnames(m) <- NULL
  notes <- character()
  if (!length(spec$value_cols)) stop("Select at least one column of values.", call. = FALSE)
  if (spec$group_col > 0L && spec$group_col %in% spec$value_cols) stop("Use different columns for the group and the values.", call. = FALSE)
  if (spec$measure_col > 0L && spec$measure_col %in% c(spec$value_cols, spec$group_col)) stop("Use different columns for the series names, the group and the values.", call. = FALSE)
  if (spec$year_col > 0L && spec$year_col %in% c(spec$value_cols, spec$group_col, spec$measure_col)) stop("Use different columns for the year and the other selections.", call. = FALSE)
  if (spec$first > nrow(m) || spec$last < spec$first) stop("The selected range has headers but no data rows.", call. = FALSE)
  rows <- seq.int(spec$first, spec$last)

  fill_down <- function(col) {
    v <- m[rows, col]
    has_values <- rowSums(m[rows, spec$value_cols, drop = FALSE] != "") > 0L
    filled <- FALSE
    for (i in seq_along(v)[-1L]) if (!nzchar(v[i]) && has_values[i] && nzchar(v[i - 1L])) { v[i] <- v[i - 1L]; filled <- TRUE }
    if (filled) notes <<- c(notes, sprintf("Blank (merged) cells under \"%s\" were filled from the row above.", m[spec$header, col]))
    v
  }
  group <- if (spec$group_col > 0L) fill_down(spec$group_col) else rep("", length(rows))
  measure_ids <- if (spec$measure_col > 0L) fill_down(spec$measure_col) else NULL

  if (spec$layout %in% c("long", "stacked")) {
    period <- fill_down(spec$year_col)
    years <- gre_import_year(period)
    if (sum(gre_import_subannual(period)) > length(rows) / 2)
      stop("The year column holds dates, months or quarters. Growth analysis needs one value per year, so make an annual summary first.", call. = FALSE)
    bad <- is.na(years)
    if (all(bad)) stop("The selected year column does not contain years such as 2001 or 2022-23.", call. = FALSE)
    dropped <- bad & rowSums(m[rows, , drop = FALSE] != "") > 0L
    if (any(dropped)) notes <- c(notes, sprintf("Left out %d row%s with no year: %s.", sum(dropped), if (sum(dropped) == 1L) "" else "s",
      paste(sprintf("\"%s\"", substr(utils::head(ifelse(nzchar(period[dropped]), period[dropped], m[rows[dropped], max(1L, spec$group_col)]), 3L), 1L, 40L)), collapse = ", ")))
    reporting <- grepl("[-/]", gre_import_dash(period[!bad]))
    keep <- !bad
    each <- length(spec$value_cols)
    if (spec$layout == "stacked") {
      if (spec$measure_col < 1L) stop("Choose the column that names each series.", call. = FALSE)
      if (each != 1L) stop("Choose one column of values for this layout.", call. = FALSE)
      use <- keep & nzchar(measure_ids)
      rec <- data.frame(row = rows[use], year = years[use], group = group[use], measure = measure_ids[use],
                        raw = m[rows[use], spec$value_cols], stringsAsFactors = FALSE)
    } else {
      heads <- m[spec$header, spec$value_cols]
      if (any(!nzchar(heads))) stop("Every selected column needs a header name.", call. = FALSE)
      if (anyDuplicated(tolower(heads))) stop(sprintf("Two selected columns are both called \"%s\". Rename one, or select only one of them.", heads[duplicated(tolower(heads))][1L]), call. = FALSE)
      rec <- data.frame(row = rep(rows[keep], times = each), year = rep(years[keep], times = each),
                        group = rep(group[keep], times = each), measure = rep(heads, each = sum(keep)),
                        raw = as.vector(m[rows[keep], spec$value_cols, drop = FALSE]), stringsAsFactors = FALSE)
    }
    fiscal <- any(reporting)
  } else {
    periods <- m[spec$header, ]
    if (spec$second > spec$header) {
      for (j in seq.int(min(spec$value_cols) + 1L, max(spec$value_cols, min(spec$value_cols) + 1L))) if (j <= ncol(m) && !nzchar(periods[j])) periods[j] <- periods[j - 1L]
    }
    years <- gre_import_year(periods[spec$value_cols])
    if (anyNA(years)) stop(sprintf("\"%s\" in the header is not a year or reporting year.", periods[spec$value_cols][is.na(years)][1L]), call. = FALSE)
    fiscal <- any(grepl("[-/]", gre_import_dash(periods[spec$value_cols])))
    if (spec$layout == "wide") {
      series <- m[spec$second, ]
      if (spec$second < spec$header) for (j in seq.int(min(spec$value_cols) + 1L, max(spec$value_cols, min(spec$value_cols) + 1L))) if (j <= ncol(m) && !nzchar(series[j])) series[j] <- series[j - 1L]
      series <- series[spec$value_cols]
      if (any(!nzchar(series))) stop("Every selected column needs a series name in the second header row.", call. = FALSE)
    } else series <- rep(trimws(measure_name %||% "Value"), length(spec$value_cols))
    if (spec$layout == "across_series" && spec$measure_col < 1L) stop("Choose the column that names each series.", call. = FALSE)
    if (spec$layout != "across_series" && !nzchar(series[1L])) series[] <- "Value"
    each <- length(spec$value_cols)
    n <- length(rows)
    rec <- data.frame(row = rep(rows, times = each), year = rep(years, each = n), group = rep(group, times = each),
                      measure = if (spec$layout == "across_series") rep(measure_ids, times = each) else rep(series, each = n),
                      raw = as.vector(m[rows, spec$value_cols, drop = FALSE]), stringsAsFactors = FALSE)
    rec <- rec[nzchar(rec$measure), , drop = FALSE]
  }

  ## Rows with no values at all: titles, notes and section headings.
  rec_id <- paste(rec$row)
  no_value <- !gre_import_number(rec$raw, dec)$invalid & is.na(gre_import_number(rec$raw, dec)$values)
  empty_rows <- tapply(no_value, rec_id, all)
  empty_ids <- names(empty_rows)[empty_rows]
  if (length(empty_ids) && !spec$layout %in% c("long", "stacked")) {
    labels <- unique(rec$group[rec_id %in% empty_ids])
    if (spec$layout == "across_series") labels <- unique(rec$measure[rec_id %in% empty_ids])
    notes <- c(notes, sprintf("Left out %d row%s with no values: %s.", length(empty_ids), if (length(empty_ids) == 1L) "" else "s",
      paste(sprintf("\"%s\"", substr(utils::head(labels[nzchar(labels)], 3L), 1L, 40L)), collapse = ", ")))
    rec <- rec[!rec_id %in% empty_ids, , drop = FALSE]
  }
  if (!nrow(rec)) stop("The selected range has no values.", call. = FALSE)
  if (spec$group_col > 0L) {
    missing_group <- !nzchar(rec$group)
    if (any(missing_group)) stop(sprintf("Row %d has values but no group name. Add the name, or choose \"No group\".", rec$row[which(missing_group)[1L]]), call. = FALSE)
  } else if (spec$layout %in% c("across", "wide") && length(unique(rec$row)) > 1L) {
    stop("This table has several data rows, so choose a group column, or select a range with only one data row.", call. = FALSE)
  }

  ## Total rows would double-count the groups.
  if (spec$group_col > 0L) {
    labels <- unique(rec$group)
    is_total <- grepl(gre_import_total_pattern, tolower(labels)) | (length(labels) > 2L & tolower(labels) == "india")
    if (any(is_total) && !all(is_total)) {
      if (isTRUE(drop_totals)) {
        rec <- rec[!rec$group %in% labels[is_total], , drop = FALSE]
        notes <- c(notes, sprintf("Left out the total row%s so groups are not counted twice: %s. The analysis calculates its own combined series.",
          if (sum(is_total) == 1L) "" else "s", paste(sprintf("\"%s\"", labels[is_total]), collapse = ", ")))
      } else notes <- c(notes, sprintf("The table still includes total rows (%s). Do not sum these with the other groups.", paste(sprintf("\"%s\"", labels[is_total]), collapse = ", ")))
    }
  }

  result <- gre_import_pivot(rec, dec)
  d <- result$data
  has_group <- "Group" %in% names(d)
  if (has_group && length(unique(d$Group)) == 1L) { d$Group <- NULL; has_group <- FALSE }
  clash <- names(d)[-seq_len(1L + has_group)] %in% c("Year", "Group")
  if (any(clash)) names(d)[-seq_len(1L + has_group)][clash] <- paste(names(d)[-seq_len(1L + has_group)][clash], "value")

  if (fiscal) notes <- c(notes, "Reporting years such as 2022-23 were converted to their first year (2022).")
  if (result$nil > 0L) notes <- c(notes, sprintf("%d cell%s marked nil or negligible %s treated as missing. Enter 0 in your file if the true value is zero.", result$nil, if (result$nil == 1L) "" else "s", if (result$nil == 1L) "was" else "were"))
  all_years <- sort(unique(d$Year))
  gaps <- setdiff(seq.int(min(all_years), max(all_years)), all_years)
  if (length(gaps)) notes <- c(notes, sprintf("%d year%s missing between %d and %d: %s.", length(gaps), if (length(gaps) == 1L) " is" else "s are",
    min(all_years), max(all_years), paste(utils::head(gaps, 8L), collapse = ", ")))
  if (length(all_years) < 3L) notes <- c(notes, "Growth and trend analysis need at least three years.")
  if (has_group) {
    per_group <- table(d$Group)
    if (length(unique(per_group)) > 1L) notes <- c(notes, "Not every group has a row for every year. Add the missing rows, or the group analysis will ask you to.")
  }
  n_series <- length(names(d)) - 1L - has_group
  summary <- sprintf("%d rows ready: %d years (%d to %d)%s, %d series.", nrow(d), length(all_years), min(all_years), max(all_years),
                     if (has_group) sprintf(", %d groups", length(unique(d$Group))) else "", n_series)
  list(data = d, notes = notes, summary = summary)
}

## ---- UI --------------------------------------------------------------------
gre_import_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      fileInput("gre_import_file", "Upload a workbook or text file",
                accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt")),
      uiOutput("gre_import_sheet_ui"),
      textInput("gre_import_range", "Table range (optional)", placeholder = "e.g. B8:K26"),
      tags$small("Only needed when a sheet has more than one table: give the block, including its headers."),
      tags$br(), tags$br(),
      selectInput("gre_import_layout", "Table layout",
                  stats::setNames(names(gre_import_labels), unlist(gre_import_labels))),
      uiOutput("gre_import_mapping"),
      checkboxInput("gre_import_drop_totals", "Leave out total rows, such as Total or All India", TRUE)
    ),
    mainPanel(
      h3("Import and reshape"),
      p("Upload your table in any annual layout. RAISINS recognises where the years, groups and series are, and shows the result below for you to check."),
      p("It reads years across the top or down the rows, one or two header rows, and titles or notes around the table. Reporting years such as 2022-23 become 2022. Daily, monthly and quarterly data need an annual summary first."),
      h4("Source preview"), DT::DTOutput("gre_import_preview"),
      h4("Analysis-ready preview"), uiOutput("gre_import_status"),
      DT::DTOutput("gre_import_result"), uiOutput("gre_import_download_ui")
    )
  )
}

## ---- Server ----------------------------------------------------------------
gre_import_server <- function(input, output) {
  stop_message <- function(expr) tryCatch(expr, error = function(e) {
    if (inherits(e, c("shiny.silent.error", "validation"))) stop(e)
    shiny::validate(shiny::need(FALSE, conditionMessage(e)))
  })
  is_excel <- reactive(tolower(tools::file_ext(req(input$gre_import_file)$name)) %in% c("xlsx", "xls"))
  sheets <- reactive(tryCatch(readxl::excel_sheets(req(input$gre_import_file)$datapath), error = function(e) character()))
  output$gre_import_sheet_ui <- renderUI({
    if (is_excel() && length(sheets()) > 1L) selectInput("gre_import_sheet", "Workbook sheet", sheets())
  })
  source_file <- reactive({
    file <- req(input$gre_import_file)
    stop_message(gre_import_read(file$datapath, file$name,
                                 sheet = if (is_excel()) input$gre_import_sheet, range = input$gre_import_range))
  })
  block <- reactive(source_file()$data)
  output$gre_import_preview <- DT::renderDT(DT::datatable(block(), rownames = TRUE, selection = "none",
    options = list(scrollX = TRUE, pageLength = 6, ordering = FALSE)))
  layout <- reactive(input$gre_import_layout %||% "auto")
  guess <- reactive(tryCatch(gre_import_detect(block(), layout(), source_file()$dec),
                             error = function(e) if (inherits(e, "shiny.silent.error")) stop(e) else NULL))

  ## Manual controls start from the best guess for the chosen layout.
  output$gre_import_mapping <- renderUI({
    d <- block()
    req(layout() != "auto", ncol(d) >= 2L, nrow(d) >= 2L)
    g <- guess()
    first_cells <- function(j) {
      v <- d[[j]][nzchar(d[[j]])]
      paste(utils::head(v, 2L), collapse = " / ")
    }
    columns <- stats::setNames(seq_along(d), sprintf("%s: %s", seq_along(d), substr(vapply(seq_along(d), first_cells, ""), 1L, 28L)))
    rows <- seq_len(min(nrow(d), 30L))
    row_names <- stats::setNames(rows, sprintf("Row %d: %s", rows, substr(apply(d[rows, , drop = FALSE], 1L, function(v) paste(utils::head(v[nzchar(v)], 3L), collapse = " | ")), 1L, 32L)))
    one <- function(id, label, choices, selected) selectInput(id, label, choices, selected = selected, selectize = FALSE)
    tagList(
      one("gre_import_header", if (layout() %in% c("long", "stacked")) "Header row" else "Row with the years", row_names, g$header %||% 1L),
      if (layout() == "wide") one("gre_import_second", "Row with the series names", row_names, g$second %||% 2L),
      if (layout() %in% c("long", "stacked")) one("gre_import_year", "Year column", columns, g$year_col %||% 1L),
      if (layout() %in% c("stacked", "across_series")) one("gre_import_measure_col", "Column naming each series", columns, g$measure_col %||% 2L),
      if (layout() == "across") textInput("gre_import_measure", "Name of the series", "Value"),
      one("gre_import_group", "Group column", c("No group" = 0, columns), g$group_col %||% 0L),
      selectInput("gre_import_series", if (layout() == "stacked") "Value column" else "Columns of values", columns,
                  multiple = layout() != "stacked", selected = if (is.null(g)) NULL else g$value_cols)
    )
  })

  spec <- reactive({
    d <- block()
    if (layout() == "auto") return(stop_message(gre_import_detect(d, "auto", source_file()$dec)))
    req(input$gre_import_header, input$gre_import_group)
    shiny::validate(shiny::need(length(input$gre_import_series) > 0L,
      "Choose the columns of values on the left, or check that this layout fits your table."))
    header <- as.integer(input$gre_import_header)
    second <- if (layout() == "wide") as.integer(req(input$gre_import_second)) else 0L
    value_cols <- as.integer(input$gre_import_series)
    req(all(value_cols <= ncol(d)), header <= nrow(d))
    first <- max(header, second) + 1L
    if (first > nrow(d)) shiny::validate(shiny::need(FALSE, "The selected header row has no data rows below it."))
    list(layout = layout(), header = header, second = second,
         year_col = if (layout() %in% c("long", "stacked")) as.integer(req(input$gre_import_year)) else 0L,
         group_col = as.integer(input$gre_import_group),
         measure_col = if (layout() %in% c("stacked", "across_series")) as.integer(req(input$gre_import_measure_col)) else 0L,
         value_cols = value_cols, first = first, last = nrow(d))
  })
  built <- reactive({
    file <- source_file()
    stop_message(gre_import_build(file$data, spec(), file$dec, !isFALSE(input$gre_import_drop_totals),
                                  input$gre_import_measure %||% "Value"))
  })
  result <- reactive(built()$data)

  output$gre_import_status <- renderUI({
    if (is.null(input$gre_import_file)) return(tags$p(class = "text-muted", "Upload a file to begin."))
    b <- built()
    notes <- c(source_file()$note, b$notes)
    tagList(
      tags$p(tags$strong(b$summary), if (layout() == "auto") paste0(" Layout found: ", tolower(gre_import_labels[[spec()$layout]]), ".")),
      if ("Group" %in% names(b$data)) tags$p("In Analysis, select Year and Group, then only additive series such as area and production."),
      if (length(notes)) tags$ul(class = "text-muted", lapply(notes, tags$li))
    )
  })
  output$gre_import_result <- DT::renderDT(DT::datatable(result(), rownames = FALSE, selection = "none",
    options = list(scrollX = TRUE, pageLength = 8)))
  output$gre_import_download_ui <- renderUI({
    req(tryCatch(result(), error = function(e) NULL))
    tags$div(style = "margin-top:12px;", downloadButton("gre_import_download", "Download reshaped CSV", class = "btn-success"))
  })
  output$gre_import_download <- downloadHandler(
    filename = function() "reshaped_annual_data.csv",
    content = function(file) utils::write.csv(result(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8"))
}
