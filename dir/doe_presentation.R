## ##tablestyle(17-09-26) Table style option for every results table
## One sidebar choice (input$doe_table_style) restyles the tables on screen
## (Analysis Results + Individual ANOVA), in the HTML / PDF / Word reports and
## in the Excel download. "classic" is the original look and changes nothing.
##
##   classic   - original look (kable_classic on screen, booktabs + stripes in PDF)
##   threeline - three-line (journal / APA) table: thick top rule, thin rule
##               under the header, thick bottom rule, no vertical lines
##   grid      - full grid: every cell boxed
##   striped   - three-line table with shaded alternate rows
##   minimal   - only the rule under the header

# Table & font styles now come from greyskull (identical behaviour, single
# source of truth; the Word exporter is greyskull's newer shared version, so
# RM-ANOVA's .docx tables now match CRD/RBD). App-specific presentation /
# report functions stay below.
doe_tstyle_choices    <- greyskull::raisins_tstyle_choices
doe_tstyle_value      <- greyskull::raisins_tstyle_value
doe_tstyle_css        <- greyskull::raisins_tstyle_css
doe_tstyle_uncolor    <- greyskull::raisins_tstyle_uncolor
doe_tstyle_latex      <- greyskull::raisins_tstyle_latex
doe_tstyle_knitr_hook <- greyskull::raisins_tstyle_knitr_hook
doe_tstyle_docx       <- greyskull::raisins_tstyle_docx
doe_font_choices      <- greyskull::raisins_font_choices
doe_font_value        <- greyskull::raisins_font_value
doe_font_twin         <- greyskull::raisins_font_twin
doe_font_pdf_args     <- greyskull::raisins_font_pdf_args
doe_font_docx         <- greyskull::raisins_font_docx


# Shared presentation implementation copied into each app; model fits are untouched.
doe_standard_moment <- function(x, order) {
  x <- x[!is.na(x)]
  centered <- x - mean(x)
  mean(centered^order) / mean(centered^2)^(order / 2)
}

doe_exact_character <- function(x) {
  ifelse(is.finite(x), format(x, digits = 17, scientific = FALSE, trim = TRUE), "-")
}

doe_print_report_tables <- function(tables, font = "cambria") {
  for (i in seq_along(tables)) {
    title <- names(tables)[i]
    tb <- tables[[i]]
    if (knitr::is_latex_output()) {
      title <- gsub("([_%&#$])", "\\\\\\1", title)
      cat(as.character(kableExtra::kable_styling(kableExtra::kbl(tb, format = "latex",
        caption = title, booktabs = TRUE, row.names = FALSE), latex_options = "scale_down")), "\n\n")
    } else if (knitr::is_html_output()) {
      cat(as.character(kableExtra::kable_classic(kableExtra::kbl(tb, caption = title,
        row.names = FALSE), full_width = FALSE, html_font = font)), "\n\n")
    } else {
      print(knitr::kable(tb, format = "pipe", caption = title, row.names = FALSE))
      cat("\n\n")
    }
  }
}
doe_summary_tables <- function(df, variables, groups, digits = 2L) {
  stopifnot(is.data.frame(df))
  variables <- intersect(variables, names(df))
  variables <- variables[vapply(df[variables], is.numeric, logical(1))]
  if (!is.list(groups)) groups <- as.list(unique(intersect(groups, names(df))))
  tabs <- list()
  for (v in variables) for (g in groups) {
    g <- intersect(g, names(df))
    if (!length(g)) next
    d <- df[, c(g, v), drop = FALSE]
    names(d)[ncol(d)] <- ".value"
    z <- dplyr::summarise(dplyr::group_by(d, dplyr::across(dplyr::all_of(g))),
      Mean = mean(.data$.value, na.rm = TRUE), SD = sd(.data$.value, na.rm = TRUE),
      SE = SD / sqrt(dplyr::n()), Min = min(.data$.value, na.rm = TRUE),
      Max = max(.data$.value, na.rm = TRUE), CV = SD / Mean * 100,
      Skewness = doe_standard_moment(.data$.value, 3),
      Kurtosis = doe_standard_moment(.data$.value, 4), .groups = "drop")
    z <- as.data.frame(z)
    for (j in seq_along(g)) z[[j]] <- as.character(z[[j]])
    for (j in seq.int(length(g)+1L, ncol(z))) z[[j]] <- ifelse(is.finite(z[[j]]),
      formatC(z[[j]], format = "f", digits = digits), "-")
    names(z)[names(z) == "CV"] <- "CV(%)"
    tabs[[paste(v, paste(g, collapse = " x "), sep = " - ")]] <- z
  }
  tabs
}

doe_log_constant <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x < 0 || x > 1) 1 else x
}

doe_log_positive <- function(x, constant = 1) {
  # The caller retains its existing handling of zero and negative observations.
  if (any(x > 0 & x < 1, na.rm = TRUE)) log10(x + doe_log_constant(constant)) else log10(x)
}

doe_letter_cells <- function(cells, groups, position = "sd") {
  cells <- as.character(cells)
  sup <- paste0("<sup>", groups, "</sup>")
  if (!identical(position, "mean")) return(paste0(cells, sup))
  has_sd <- grepl("&plusmn;|\u00b1", cells)
  mean <- sub("\\s*(?:&plusmn;|\u00b1).*", "", cells, perl = TRUE)
  rest <- substring(cells, nchar(mean) + 1L)
  ifelse(has_sd, paste0(mean, sup, rest), paste0(cells, sup))
}

doe_parenthesis_cells <- function(A, B, SD, original_inside = FALSE, position = "sd") {
  result <- as.matrix(A)
  B <- as.matrix(B); SD <- as.matrix(SD)
  for (j in intersect(colnames(result), colnames(B))) {
    # Existing tables use the same sorted treatment order for both scales.
    cells <- result[, j]
    if (isTRUE(original_inside)) {
      original <- sub("<sup>.*?</sup>", "", cells, perl = TRUE)
      original <- sub("\\s*(?:&plusmn;|\u00b1).*", "", original, perl = TRUE)
      groups <- ifelse(grepl("<sup>", cells), sub(".*<sup>(.*?)</sup>.*", "\\1", cells), "")
      main <- as.character(B[, j])
      has_sd <- grepl("&plusmn;|\u00b1", cells)
      main[has_sd] <- paste0(main[has_sd], " &plusmn; ", SD[has_sd, j])
      grouped <- doe_letter_cells(main, groups, position)
      grouped[groups == ""] <- main[groups == ""]
      result[, j] <- paste0(grouped, "<br> (", original, ")")
    } else result[, j] <- paste0(cells, "<br> (", B[, j], ")")
  }
  result
}

doe_report_renderer <- function(session_inputs, prefix, app_dir, exact = function() list()) {
  force(session_inputs); force(prefix); force(app_dir)
  function(input, output_format = NULL, ...) {
    args <- list(...)
    style <- doe_tstyle_value(session_inputs[[paste0(prefix, "_table_style")]])
    font <- doe_font_value(session_inputs[[paste0(prefix, "_font")]])
    supported <- names(rmarkdown::yaml_front_matter(input)$params)
    if ("doe_table_style" %in% supported) args$params$doe_table_style <- style
    if ("doe_font" %in% supported) args$params$doe_font <- font
    if ("doe_exact_tables" %in% supported) args$params$doe_exact_tables <- exact()
    if ("doe_scale_note" %in% supported) args$params$doe_scale_note <-
      if (!isTRUE(session_inputs[[paste0(prefix, "_trans_switch")]])) "" else
      if (identical(session_inputs$doe_paren_value, "original"))
        "Original means are in parentheses; the main mean and SD are on the transformed scale." else
        "Transformed means are in parentheses; the main mean and SD are on the original scale."
    if (inherits(output_format, "rmarkdown_output_format") &&
        identical(output_format$pandoc$to, "latex")) {
      output_format$pandoc$latex_engine <- "xelatex"
      output_format$pandoc$args <- c(output_format$pandoc$args,
        doe_font_pdf_args(font, file.path(app_dir, "www", "fonts")))
    }
    result <- do.call(rmarkdown::render, c(list(input = input, output_format = output_format), args))
    if (grepl("\\.docx$", result, ignore.case = TRUE)) {
      doe_tstyle_docx(result, style)
      doe_font_docx(result, font)
    }
    result
  }
}

doe_presentation_server <- function(input, output, session, prefix, data, groups,
                                    variables, has_style = FALSE, has_log = FALSE, log_data = data,
                                    letters = FALSE, paren = FALSE, exact = function() list(),
                                    original_data = NULL,
                                    scale_active = function() isTRUE(input[[paste0(prefix, "_trans_switch")]])) {
  app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  output$doe_presentation_controls <- shiny::renderUI({
    shiny::req(data())
    htmltools::tagList(
      if (!has_style) shiny::selectInput(paste0(prefix, "_table_style"), "Table Style:",
        doe_tstyle_choices, selected = doe_tstyle_value(shiny::isolate(input[[paste0(prefix, "_table_style")]]))),
      if (letters) shiny::radioButtons("doe_letter_position", "Letters next to:",
        c("SD" = "sd", "Mean" = "mean"), selected = if (identical(shiny::isolate(input$doe_letter_position), "mean")) "mean" else "sd", inline = TRUE),
      if (paren && scale_active()) shiny::radioButtons(
        "doe_paren_value", if (is.null(original_data)) "Value in parentheses:" else "Value in parentheses (descriptive means):",
        c("Transformed mean" = "transformed", "Original mean" = "original"),
        selected = if (identical(shiny::isolate(input$doe_paren_value), "original")) "original" else "transformed", inline = TRUE),
      if (paren && scale_active()) htmltools::tags$p(
        if (is.null(original_data)) "The main mean and SD use the other scale. Letters always follow the fitted analysis."
        else "The Summary stats tab compares descriptive means on both scales. Adjusted means and BLUPs retain their model-defined scale.")
    )
  })
  output$doe_presentation_css <- shiny::renderUI({
    # Scope to result tables; controls, navigation and plots keep their own styles.
    css <- doe_tstyle_css(input[[paste0(prefix, "_table_style")]],
      scope = c(paste0("#", prefix, "_mainpanel"), "#crdp_mainpanel", "#rcbd_mainpanel", "#split_mainpanel", "#strip_mainpanel",
        '.tab-pane[data-value="Analysis.Results"]',
        '.tab-pane[data-value="Analysis Results"]', '.tab-pane[data-value="Individual ANOVA"]',
        '.tab-pane[data-value="Summary stats"]', "#doe_summary_tables"))
    htmltools::tags$style(htmltools::HTML(css))
  })
  shiny::outputOptions(output, "doe_presentation_css", suspendWhenHidden = FALSE)
  if (!has_log) output$doe_log_constant <- shiny::renderUI({
    shiny::req(isTRUE(input[[paste0(prefix, "_trans_switch")]]), log_data())
    cols <- intersect(input[[paste0(prefix, "_log_selvar")]], names(log_data()))
    needs <- any(vapply(cols, function(v) {
      x <- log_data()[[v]]
      is.numeric(x) && !any(x <= 0, na.rm = TRUE) && any(x > 0 & x < 1, na.rm = TRUE)
    }, logical(1)))
    shiny::req(needs)
    shiny::numericInput("doe_log_c", "Log constant: log10(x + c), c from 0 to 1",
      value = doe_log_constant(shiny::isolate(input$doe_log_c)), min = 0, max = 1, step = 0.1)
  })
  tables <- shiny::reactive({
    shiny::req(data(), variables(), groups())
    digits <- input[[paste0(prefix, "_digit")]]
    if (is.null(digits) || !is.finite(digits)) digits <- 2L
    tabs <- doe_summary_tables(data(), variables(), groups(), digits)
    if (!is.null(original_data) && scale_active()) {
      originals <- doe_summary_tables(original_data(), variables(), groups(), digits)
      for (n in intersect(names(tabs), names(originals))) {
        original <- originals[[n]]; transformed <- tabs[[n]]
        id <- setdiff(names(original), c("Mean","SD","SE","Min","Max","CV(%)","Skewness","Kurtosis"))
        key <- function(x) do.call(paste, c(x[id], sep = "\r"))
        original <- original[match(key(transformed), key(original)), , drop = FALSE]
        main <- original; inside <- transformed
        if (identical(input$doe_paren_value,"original")) {main <- transformed;inside <- original}
        comparison <- transformed[id]
        label <- if (identical(input$doe_paren_value,"original")) "Transformed mean +/- SD (original mean)" else "Original mean +/- SD (transformed mean)"
        comparison[[label]] <- paste0(main$Mean, " +/- ", main$SD, " (", inside$Mean, ")")
        tabs[[paste0(n, " - original/transformed descriptive means")]] <- comparison
      }
    }
    tabs
  })
  output$doe_summary_tables <- shiny::renderUI({
    tabs <- tables()
    shiny::req(length(tabs))
    htmltools::tagList(lapply(names(tabs), function(n) htmltools::tagList(
      htmltools::tags$h4(n, style = "text-align:center"),
      htmltools::div(style = "overflow-x:auto", htmltools::HTML(as.character(
        kableExtra::kable_classic(kableExtra::kbl(tabs[[n]], align = "c", row.names = FALSE),
          full_width = FALSE, position = "center",
          html_font = doe_font_value(input[[paste0(prefix, "_font")]]))))))),
      shiny::radioButtons("doe_summary_format", "Download summary statistics:",
        c("HTML", "PDF", "Word"), inline = TRUE), shiny::downloadButton("doe_summary_download", "Download"))
  })
  output$doe_exact_tables <- shiny::renderUI({
    tabs <- exact()
    shiny::req(length(tabs))
    htmltools::tagList(htmltools::tags$h3("Original unrounded CD values"),
      lapply(names(tabs), function(n) htmltools::tagList(htmltools::tags$h4(n),
        htmltools::HTML(as.character(kableExtra::kable_classic(
          kableExtra::kbl(tabs[[n]], align = "c", row.names = FALSE), full_width = FALSE,
          html_font = doe_font_value(input[[paste0(prefix, "_font")]])))))))
  })
  output$doe_summary_download <- shiny::downloadHandler(
    filename = function() paste0("Summary_statistics_", Sys.Date(), ".",
      switch(input$doe_summary_format, HTML = "html", PDF = "pdf", Word = "docx")),
    content = function(file) {
      tabs <- c(tables(), exact())
      shiny::req(length(tabs))
      shiny::validate(shiny::need(input$doe_summary_format != "PDF" || length(tabs) <= 80,
        "For more than 80 tables, choose HTML or Word."))
      style <- doe_tstyle_value(input[[paste0(prefix, "_table_style")]])
      font <- doe_font_value(input[[paste0(prefix, "_font")]])
      fmt <- switch(input$doe_summary_format, HTML = rmarkdown::html_document(),
        PDF = rmarkdown::pdf_document(latex_engine = "xelatex",
          pandoc_args = doe_font_pdf_args(font, file.path(app_dir, "www", "fonts"))),
        Word = rmarkdown::word_document())
      work <- tempfile("doe_summary_"); dir.create(work)
      on.exit(unlink(work, recursive = TRUE), add = TRUE)
      report <- file.path(work, "doe_summary_report.Rmd")
      file.copy(file.path(app_dir, "doe_summary_report.Rmd"), report)
      env <- new.env(parent = environment())
      rmarkdown::render(report, output_format = fmt, output_file = file,
        params = list(tables = tabs, table_style = style, font = font), envir = env, quiet = TRUE)
      if (input$doe_summary_format == "Word") {
        doe_tstyle_docx(file, style); doe_font_docx(file, font)
      }
    })
  invisible(tables)
}

doe_tstyle_xlsx <- greyskull::raisins_tstyle_xlsx

doe_save_workbook <- function(wb, file, overwrite = TRUE, style = "classic", font = "cambria") {
  openxlsx::modifyBaseFont(wb, fontName = doe_font_value(font))
  for (sheet in names(wb)) doe_tstyle_xlsx(wb, sheet, style = style)
  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
}
