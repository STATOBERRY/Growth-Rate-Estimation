## ============================================================================
##  RAISINS Growth Rate Estimation — Analysis engine  (gre_server)
##  Base-R statistics (no extra package installs). Plugs into greyskull shell.
##  Signature matches the shell:  gre_server(input, output, session, shared)
## ============================================================================

## ---- statistical helpers (base R) ------------------------------------------

# Compound growth rate from a log-linear fit:  ln(y) = a + b*t
gre_cgr <- function(year, y) {
  ok <- is.finite(y) & y > 0 & is.finite(year)
  year <- year[ok]; y <- y[ok]
  if (length(y) < 3) return(list(rate = NA, p = NA, r2 = NA))
  t <- year - min(year)
  fit <- stats::lm(log(y) ~ t)
  b   <- stats::coef(fit)[["t"]]
  p   <- tryCatch(summary(fit)$coefficients["t", "Pr(>|t|)"], error = function(e) NA)
  list(rate = (exp(b) - 1) * 100,
       p    = p,
       r2   = summary(fit)$r.squared)
}

# Endpoint CAGR using first and last available values
gre_cagr <- function(year, y) {
  ok <- is.finite(y) & y > 0 & is.finite(year)
  year <- year[ok]; y <- y[ok]
  if (length(y) < 2) return(NA)
  n <- length(y)
  ((y[n] / y[1])^(1 / (year[n] - year[1])) - 1) * 100
}

# Average of year-on-year percentage changes
gre_annual_growth <- function(y) {
  y <- y[is.finite(y)]
  if (length(y) < 2) return(NA)
  mean(diff(y) / head(y, -1) * 100, na.rm = TRUE)
}

# Fit linear / quadratic / cubic / exponential trends; rank by adjusted R^2
gre_trend_fit <- function(year, y) {
  ok <- is.finite(y) & is.finite(year)
  year <- year[ok]; y <- y[ok]
  t <- year - min(year)
  out <- list()
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  models <- list(
    Linear    = safe(stats::lm(y ~ t)),
    Quadratic = safe(stats::lm(y ~ t + I(t^2))),
    Cubic     = safe(stats::lm(y ~ t + I(t^2) + I(t^3))),
    Exponential = if (all(y > 0)) safe(stats::lm(log(y) ~ t)) else NULL
  )
  rows <- lapply(names(models), function(nm) {
    m <- models[[nm]]
    if (is.null(m)) return(NULL)
    s <- summary(m)
    data.frame(Model = nm,
               `Adj R2` = round(s$adj.r.squared, 4),
               AIC = round(stats::AIC(m), 2),
               check.names = FALSE)
  })
  tab <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(tab) || !nrow(tab)) return(NULL)
  best_name <- tab$Model[which.max(tab$`Adj R2`)]
  best <- models[[best_name]]
  # fitted values on original scale
  tt <- data.frame(t = t)
  fitted <- if (best_name == "Exponential") exp(stats::predict(best, tt)) else stats::predict(best, tt)
  list(table = tab, best = best_name,
       fitted = data.frame(year = year, y = y, fit = as.numeric(fitted)))
}

# Mann-Kendall trend test (with tie correction)
gre_mann_kendall <- function(y) {
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 3) return(NULL)
  S <- 0
  for (i in 1:(n - 1)) S <- S + sum(sign(y[(i + 1):n] - y[i]))
  ties <- as.numeric(table(y))
  varS <- (n * (n - 1) * (2 * n + 5) - sum(ties * (ties - 1) * (2 * ties + 5))) / 18
  Z <- if (S > 0) (S - 1) / sqrt(varS) else if (S < 0) (S + 1) / sqrt(varS) else 0
  p <- 2 * (1 - stats::pnorm(abs(Z)))
  tau <- S / (0.5 * n * (n - 1))
  list(S = S, Z = Z, p = p, tau = tau, varS = varS, n = n)
}

# Theil-Sen slope with (1-alpha) confidence interval
gre_sens_slope <- function(year, y, alpha = 0.05) {
  ok <- is.finite(y) & is.finite(year)
  year <- year[ok]; y <- y[ok]
  n <- length(y)
  if (n < 3) return(NULL)
  slopes <- c()
  for (i in 1:(n - 1)) for (j in (i + 1):n)
    slopes <- c(slopes, (y[j] - y[i]) / (year[j] - year[i]))
  slopes <- sort(slopes)
  mk <- gre_mann_kendall(y)
  Calpha <- stats::qnorm(1 - alpha / 2) * sqrt(mk$varS)
  N <- length(slopes)
  M1 <- floor((N - Calpha) / 2)
  M2 <- ceiling((N + Calpha) / 2)
  lo <- if (M1 >= 1 && M1 <= N) slopes[M1] else NA
  hi <- if (M2 >= 1 && M2 <= N) slopes[M2] else NA
  list(slope = stats::median(slopes), lower = lo, upper = hi)
}

# Coefficient of variation and Cuddy-Della Valle instability index (%)
gre_instability <- function(year, y) {
  ok <- is.finite(y) & is.finite(year)
  year <- year[ok]; y <- y[ok]
  if (length(y) < 3) return(list(cv = NA, cdvi = NA))
  cv <- stats::sd(y) / mean(y) * 100
  t  <- year - min(year)
  r2adj <- tryCatch(summary(stats::lm(y ~ t))$adj.r.squared, error = function(e) NA)
  r2adj <- max(min(r2adj, 1), 0)
  list(cv = cv, cdvi = cv * sqrt(1 - r2adj))
}

## ---- module ----------------------------------------------------------------

gre_server <- function(input, output, session, shared) {

  `%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

  observeEvent(session$input$rcbd_logout, { session$reload() })

  ## -- read uploaded / demo data --------------------------------------------
  gre_raw <- reactive({
    df <- NULL
    if (!is.null(input$rcbd_file1)) {
      path <- input$rcbd_file1$datapath
      ext  <- tolower(tools::file_ext(input$rcbd_file1$name))
      df <- tryCatch(
        if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(path))
        else read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
        error = function(e) NULL)
    } else if (nzchar(input$demo_data_select %||% "")) {
      fp <- file.path("data", input$demo_data_select)
      if (file.exists(fp)) df <- read.csv(fp, check.names = FALSE, stringsAsFactors = FALSE)
    }
    df
  })

  num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

  ## -- column pickers --------------------------------------------------------
  output$gre_year_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    guess <- grep("year|yr|time", names(df), ignore.case = TRUE, value = TRUE)
    selectInput("gre_year", "Year / time column",
                choices = names(df),
                selected = if (length(guess)) guess[1] else names(df)[1])
  })
  output$gre_series_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    nc <- setdiff(num_cols(df), input$gre_year %||% "")
    shinyWidgets::pickerInput("gre_series", "Series to analyse (numeric)",
                              choices = nc, selected = nc,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE))
  })
  output$gre_group_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    cat_cols <- setdiff(names(df), c(input$gre_year %||% "", num_cols(df)))
    selectInput("gre_group", "Optional region/group filter",
                choices = c("All data" = "", cat_cols), selected = "")
  })

  output$gre_sidebar_msg <- renderUI({
    if (is.null(gre_raw()))
      return(div(style = "color:#2E7D32;",
                 "Upload a CSV/Excel file, or pick a demo dataset, to begin."))
    if (length(input$gre_series %||% character(0)) < 1)
      return(div(style = "color:#2E7D32;", "Select at least one series to analyse."))
    NULL
  })

  ## -- apply optional group filter -------------------------------------------
  data_filt <- reactive({
    df <- gre_raw(); req(df)
    g <- input$gre_group %||% ""
    if (nzchar(g) && g %in% names(df)) {
      # when a group col is chosen but no single level, aggregate by year sum
      yc <- input$gre_year
      keep <- c(yc, input$gre_series)
      agg <- stats::aggregate(df[, input$gre_series, drop = FALSE],
                              by = list(df[[yc]]), FUN = sum, na.rm = TRUE)
      names(agg)[1] <- yc
      agg
    } else df
  })

  ## -- core computation on Run -----------------------------------------------
  res <- eventReactive(input$rcbd_submit, {
    df <- data_filt(); req(df)
    yc <- input$gre_year; series <- input$gre_series
    validate(need(length(series) >= 1, "Select at least one series."))
    year <- suppressWarnings(as.numeric(df[[yc]]))
    validate(need(sum(is.finite(year)) >= 3,
                  "The year column needs at least 3 numeric values."))
    mods  <- input$gre_modules %||% character(0)
    alpha <- input$gre_alpha %||% 0.05

    growth <- trend <- mk <- instab <- NULL
    trend_fits <- list()

    for (s in series) {
      y <- suppressWarnings(as.numeric(df[[s]]))

      if ("growth" %in% mods) {
        cg <- gre_cgr(year, y)
        growth <- rbind(growth, data.frame(
          Series = s,
          `Start year` = min(year[is.finite(y)]),
          `End year`   = max(year[is.finite(y)]),
          `Mean`       = round(mean(y, na.rm = TRUE), 2),
          `CGR (%/yr)`  = round(cg$rate, 2),
          `CGR p`       = signif(cg$p, 3),
          `CAGR (%/yr)` = round(gre_cagr(year, y), 2),
          `Annual avg (%/yr)` = round(gre_annual_growth(y), 2),
          check.names = FALSE))
      }
      if ("trend" %in% mods) {
        tf <- gre_trend_fit(year, y)
        if (!is.null(tf)) {
          trend_fits[[s]] <- tf
          trend <- rbind(trend, cbind(Series = s, tf$table,
                                      Best = ifelse(tf$table$Model == tf$best, "✔", "")))
        }
      }
      if ("mk" %in% mods) {
        m <- gre_mann_kendall(y[is.finite(y)])
        ss <- gre_sens_slope(year, y, alpha)
        if (!is.null(m))
          mk <- rbind(mk, data.frame(
            Series = s,
            `Kendall tau` = round(m$tau, 3),
            `S` = m$S,
            `Z` = round(m$Z, 3),
            `p-value` = signif(m$p, 3),
            `Trend` = ifelse(m$p < alpha,
                             ifelse(m$tau > 0, "Increasing", "Decreasing"),
                             "No significant trend"),
            `Sen slope/yr` = if (!is.null(ss)) round(ss$slope, 3) else NA,
            `Sen CI low`  = if (!is.null(ss)) round(ss$lower, 3) else NA,
            `Sen CI high` = if (!is.null(ss)) round(ss$upper, 3) else NA,
            check.names = FALSE))
      }
      if ("instab" %in% mods) {
        ii <- gre_instability(year, y)
        instab <- rbind(instab, data.frame(
          Series = s,
          `CV (%)`   = round(ii$cv, 2),
          `CDVI (%)` = round(ii$cdvi, 2),
          `Stability` = cut(ii$cdvi, c(-Inf, 15, 30, Inf),
                            labels = c("Low instability", "Moderate", "High instability")),
          check.names = FALSE))
      }
    }

    ## area-yield decomposition (auto-detect Area / Production [/ Yield])
    decomp <- NULL; decomp_msg <- NULL
    if ("decomp" %in% mods) {
      find <- function(pat) { h <- grep(pat, names(df), ignore.case = TRUE, value = TRUE); if (length(h)) h[1] else NA }
      Acol <- find("area"); Pcol <- find("prod")
      if (is.na(Acol) || is.na(Pcol)) {
        decomp_msg <- "Decomposition needs columns named like 'Area' and 'Production'. Rename your columns and re-run."
      } else {
        o  <- order(year)
        A  <- as.numeric(df[[Acol]])[o]; P <- as.numeric(df[[Pcol]])[o]
        # Derive yield as Production/Area so the identity P = A*Y holds exactly
        # (any supplied Yield column may be in different units and would break the
        # additive decomposition). The effects then sum to the total change.
        Y  <- P / A
        A0 <- A[1]; A1 <- A[length(A)]; Y0 <- Y[1]; Y1 <- Y[length(Y)]
        dP <- P[length(P)] - P[1]
        area_e <- Y0 * (A1 - A0); yield_e <- A0 * (Y1 - Y0); inter_e <- (A1 - A0) * (Y1 - Y0)
        decomp <- data.frame(
          Effect = c("Area effect", "Yield effect", "Interaction effect", "Total change"),
          `Contribution` = round(c(area_e, yield_e, inter_e, dP), 2),
          `Share (%)` = round(c(area_e, yield_e, inter_e, dP) / dP * 100, 1),
          check.names = FALSE)
      }
    }

    interp <- gre_build_interpretation(growth, mk, instab, decomp, alpha)

    list(raw = df, year_col = yc, growth = growth, trend = trend,
         trend_fits = trend_fits, mk = mk, instab = instab,
         decomp = decomp, decomp_msg = decomp_msg, interp = interp, alpha = alpha)
  })

  ## -- interpretation text builder -------------------------------------------
  gre_build_interpretation <- function(growth, mk, instab, decomp, alpha) {
    parts <- c()
    if (!is.null(growth)) {
      for (i in seq_len(nrow(growth))) {
        g <- growth[i, ]
        dir <- if (is.na(g$`CGR (%/yr)`)) "could not be estimated" else
          if (g$`CGR (%/yr)` > 0) "grew" else "declined"
        sig <- if (!is.na(g$`CGR p`) && g$`CGR p` < alpha) "statistically significant" else "not statistically significant"
        parts <- c(parts, sprintf(
          "<li><b>%s</b> %s at a compound rate of <b>%s%% per year</b> over %s–%s (%s at α = %s).</li>",
          g$Series, dir, g$`CGR (%/yr)`, g$`Start year`, g$`End year`, sig, alpha))
      }
    }
    if (!is.null(mk)) {
      for (i in seq_len(nrow(mk)))
        parts <- c(parts, sprintf(
          "<li>Mann-Kendall for <b>%s</b>: %s (τ = %s, p = %s); Sen's slope ≈ %s per year.</li>",
          mk$Series[i], tolower(mk$Trend[i]), mk$`Kendall tau`[i], mk$`p-value`[i], mk$`Sen slope/yr`[i]))
    }
    if (!is.null(instab)) {
      for (i in seq_len(nrow(instab)))
        parts <- c(parts, sprintf(
          "<li>Instability of <b>%s</b>: CDVI = %s%% (%s).</li>",
          instab$Series[i], instab$`CDVI (%)`[i], tolower(as.character(instab$Stability[i]))))
    }
    if (!is.null(decomp)) {
      ae <- decomp$`Share (%)`[decomp$Effect == "Area effect"]
      ye <- decomp$`Share (%)`[decomp$Effect == "Yield effect"]
      driver <- if (length(ae) && length(ye)) (if (ae > ye) "area expansion" else "yield improvement") else "mixed factors"
      parts <- c(parts, sprintf(
        "<li>Production change was driven mainly by <b>%s</b> (area %s%%, yield %s%%).</li>",
        driver, ae, ye))
    }
    if (!length(parts)) return("<p>No analyses selected.</p>")
    paste0("<ul style='line-height:1.7;'>", paste(parts, collapse = ""), "</ul>")
  }

  ## -- output renderers ------------------------------------------------------
  dt <- function(d) DT::datatable(d, rownames = FALSE,
                                  options = list(dom = "t", ordering = FALSE,
                                                 scrollX = TRUE, pageLength = 50))

  output$gre_run_hint <- renderUI({
    if (input$rcbd_submit %||% 0 == 0)
      div(style = "color:#2E7D32;", "Set your columns and options, then click Run Analysis.")
  })
  output$gre_growth_table <- DT::renderDT({ req(res()$growth); dt(res()$growth) })
  output$gre_growth_note  <- renderUI({
    req(res())
    HTML("<small>CGR from the log-linear model ln(y)=a+b·t; CAGR from first and last values. Rates need positive values.</small>")
  })
  output$gre_trend_table <- DT::renderDT({ req(res()$trend); dt(res()$trend) })
  output$gre_trend_plot <- renderPlot({
    r <- res(); req(length(r$trend_fits) > 0)
    dd <- do.call(rbind, lapply(names(r$trend_fits), function(s) {
      f <- r$trend_fits[[s]]$fitted; f$Series <- s; f }))
    ggplot2::ggplot(dd, ggplot2::aes(year)) +
      ggplot2::geom_point(ggplot2::aes(y = y, colour = Series)) +
      ggplot2::geom_line(ggplot2::aes(y = fit, colour = Series), linewidth = 1) +
      ggplot2::labs(x = r$year_col, y = "Value", title = "Observed values with best-fit trend") +
      ggplot2::theme_minimal(base_size = 13)
  })
  output$gre_trend_plot_dl <- downloadHandler(
    filename = function() paste0("GRE_trend_plot_", Sys.Date(), ".png"),
    content = function(file) {
      r <- res()
      dd <- do.call(rbind, lapply(names(r$trend_fits), function(s) {
        f <- r$trend_fits[[s]]$fitted; f$Series <- s; f }))
      p <- ggplot2::ggplot(dd, ggplot2::aes(year)) +
        ggplot2::geom_point(ggplot2::aes(y = y, colour = Series)) +
        ggplot2::geom_line(ggplot2::aes(y = fit, colour = Series), linewidth = 1) +
        ggplot2::labs(x = r$year_col, y = "Value") + ggplot2::theme_minimal(base_size = 13)
      ggplot2::ggsave(file, p, width = 8, height = 5, dpi = 150)
    })

  output$gre_mk_intro <- renderUI(HTML(
    "<p>The <b>Mann-Kendall</b> test checks for a monotonic trend without assuming a shape; <b>Sen's slope</b> gives a robust magnitude of change per year.</p>"))
  output$gre_mk_table <- DT::renderDT({ req(res()$mk); dt(res()$mk) })

  output$gre_instab_intro <- renderUI(HTML(
    "<p><b>CV</b> is raw variability; the <b>Cuddy-Della Valle Index (CDVI)</b> removes the trend component, so it reflects genuine year-to-year instability.</p>"))
  output$gre_instab_table <- DT::renderDT({ req(res()$instab); dt(res()$instab) })

  output$gre_decomp_intro <- renderUI(HTML(
    "<p>Splits the total change in <b>Production</b> into an <b>area effect</b>, a <b>yield effect</b> and their <b>interaction</b> (needs columns named like 'Area' and 'Production').</p>"))
  output$gre_decomp_table <- DT::renderDT({
    r <- res(); req(r)
    validate(need(!is.null(r$decomp), r$decomp_msg %||% "Enable decomposition and provide Area/Production columns."))
    dt(r$decomp)
  })
  output$gre_decomp_note <- renderUI({ r <- res(); if (!is.null(r$decomp_msg)) div(style = "color:#8B0000;", r$decomp_msg) })

  output$gre_interpretation <- renderUI({ req(res()); HTML(res()$interp) })

  ## -- View Data + file-check guidance (shared IDs) --------------------------
  output$rcbd_instr_filecheck <- renderUI({
    if (is.null(gre_raw()))
      HTML("<p style='text-align:center;color:#2E7D32;'>Upload a dataset or choose a demo to preview it here.</p>")
  })
  output$rcbd_ViewData <- DT::renderDT({
    df <- gre_raw(); req(df)
    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
  })

  ## -- FAQs ------------------------------------------------------------------
  output$instr_bttns_FAQs <- renderUI({
    HTML("
    <div style='max-width:820px;margin:0 auto;font-size:14px;line-height:1.7;'>
      <h3 style='color:#6f2c91;'>Frequently Asked Questions</h3>
      <p><b>What data do I need?</b> One column of years (or time points) and one or more
         numeric series &mdash; e.g. Area, Production and Yield.</p>
      <p><b>What is CGR vs CAGR?</b> CGR is the compound growth rate from a log-linear
         regression over all points; CAGR uses only the first and last values.</p>
      <p><b>Why is a rate blank?</b> Compound rates need strictly positive values; zeros or
         negatives are skipped.</p>
      <p><b>How does decomposition work?</b> It needs columns named like <i>Area</i> and
         <i>Production</i> (Yield optional); it splits production change into area, yield and
         interaction effects.</p>
      <p><b>Can I download everything?</b> Yes &mdash; the <b>Download Report</b> tab produces a
         Word, PDF or HTML report with all tables, the trend figure and the interpretation.</p>
    </div>")
  })

  ## -- report download (HTML / Word via styles.docx / PDF) -------------------
  output$gre_download_report <- downloadHandler(
    filename = function()
      paste0("RAISINS_GRE_report_", Sys.Date(), ".",
             switch(input$gre_report_format, PDF = "pdf", Word = "docx", "html")),
    content = function(file) {
      r <- res()
      fmt <- switch(input$gre_report_format,
                    html = "html_document",
                    Word = "word_document",
                    PDF  = "pdf_document")
      td <- tempdir()
      file.copy("growth_rate_report.Rmd", file.path(td, "growth_rate_report.Rmd"), overwrite = TRUE)
      if (file.exists("styles.docx"))
        file.copy("styles.docx", file.path(td, "styles.docx"), overwrite = TRUE)
      # render trend plot to a temp png for embedding
      plot_path <- NA
      if (length(r$trend_fits)) {
        plot_path <- file.path(td, "gre_trend.png")
        dd <- do.call(rbind, lapply(names(r$trend_fits), function(s) {
          f <- r$trend_fits[[s]]$fitted; f$Series <- s; f }))
        p <- ggplot2::ggplot(dd, ggplot2::aes(year)) +
          ggplot2::geom_point(ggplot2::aes(y = y, colour = Series)) +
          ggplot2::geom_line(ggplot2::aes(y = fit, colour = Series), linewidth = 1) +
          ggplot2::labs(x = r$year_col, y = "Value") + ggplot2::theme_minimal(base_size = 12)
        ggplot2::ggsave(plot_path, p, width = 8, height = 5, dpi = 150)
      }
      out <- rmarkdown::render(
        input = file.path(td, "growth_rate_report.Rmd"),
        output_format = fmt,
        params = list(
          table_style = input$gre_table_style %||% "classic",
          font        = input$gre_font %||% "cambria",
          growth = r$growth, trend = r$trend, mk = r$mk,
          instab = r$instab, decomp = r$decomp,
          interp = r$interp, plot_path = plot_path, alpha = r$alpha),
        envir = new.env(parent = globalenv()))
      file.copy(out, file, overwrite = TRUE)
    })
}
