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

gre_server <- function(input, output, session, shared, support_send = NULL) {

  `%||%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

  ## Dynamic auth (login-card wording per mode) + authentication module----
  raisins_auth_mode_server(input, session, shared)
  observeEvent(input$cancel_modal, { removeModal() })
  observeEvent(input$demo_cancel_modal, { removeModal() })

  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = raisins_check_credentials(shared)
  )

  ## RAISINS session kit: stay signed in + newest login wins + leave guard----
  raisins_after_login(auth, input, session, shared)
  raisins_website_sso_after_login(auth, session)

  observe({
    req(auth$user)
    shinyjs::show("logout_fab")
  })

  observeEvent(session$input$rcbd_logout, { session$reload() })

  ## current subscriber----
  current_subscriber <- reactive({
    authentication <- reactiveValuesToList(auth)
    authentication$user_info$user
  })

  ## Quick Tour: welcome box after login, tour steps, Quick Tour tab (helpers below)
  gre_tour_server(input, session, current_subscriber)

  ## Error support (greyskull): alert, "Ask RAISINS" ticket form, error capture----
  gre_cfg <- raisins_app()
  error_support <- raisins_support_server("gre_error_support",
    app_name = paste0(gre_cfg$app_name, " (", gre_cfg$module_code, ")"),
    app_version = gre_cfg$version, module_code = gre_cfg$module_code,
    contact_email = function() {
      user <- auth$user
      if (identical(user, "demo")) shared$verified_demo_email else user
    },
    user_details = function() gre_support_user_details(auth$user, auth$user_info,
      shared$latest_paid_users, shared$verified_demo_email),
    send_mail = if (is.function(support_send)) support_send else function(report) FALSE,
    on_error = function(record) {
      # A plot, download or other tab failure must not discard valid analysis.
      if (identical(record$context, "Growth rate analysis")) gre_submitted(FALSE)
      removeModal()
    })
  output$gre_support_panel <- renderUI({
    req(auth$user)
    record <- error_support$current()
    if (is.null(record)) return(NULL)
    gre_error_panel(record$public_message,
      if (identical(record$context, "Data upload")) "Check your data" else "This action could not be completed",
      assistance = raisins_support_ui("gre_error_support"))
  })
  gre_error_alert_server(error_support, session)
  # Scope recovery to data/analysis callbacks, after the login setup.
  observe <- error_support$observe
  observeEvent <- error_support$observeEvent
  downloadHandler <- error_support$downloadHandler

  ## -- upload box (preview mode gets the demo picker, like RBD) --------------
  gre_demo_labels <- c("dataset1.csv" = "Coconut – Production & Area",
                       "dataset2.csv" = "Coconut – District panel",
                       "dataset3.csv" = "Coconut – Yield series")

  output$uploadfile <- renderUI({
    req(current_subscriber())

    if (current_subscriber() == "demo") {
      demo_files <- list.files("data", pattern = "\\.csv$", full.names = FALSE)
      names(demo_files) <- ifelse(demo_files %in% names(gre_demo_labels),
                                  gre_demo_labels[demo_files], demo_files)

      tagList(
        # Fake fileInput with alert trigger
        div(
          class = "form-group shiny-input-container",
          tags$label("Upload data file Excel or CSV here \U0001F447"),
          div(
            class = "input-group",
            tags$label(
              class = "input-group-btn input-group-prepend",
              actionButton("demo_file_block", "Browse", class = "btn btn-default")
            ),
            tags$input(
              type = "text",
              class = "form-control",
              placeholder = "No file selected",
              disabled = "disabled"
            )
          )
        ),

        # Demo dataset dropdown
        selectInput("demo_data_select", "Choose a demo dataset:",
                    choices = demo_files,
                    selected = demo_files[1])
      )
    } else {
      fileInput("rcbd_file1",
                "Upload data file Excel or CSV here \U0001F447",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx",
                  "application/vnd.ms-excel",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                )
      )
    }
  })

  ##Shiny alert on Preview mode----
  observeEvent(input$demo_file_block, {
    shinyalert(
      title = "You are in Preview Mode!",
      text = "You cannot upload any data in preview mode.
      Please subscribe to enable this feature. In the meantime,
      feel free to explore using the built-in datasets below to learn
      how the app works. <b>Choose a demo dataset</b> in the side bar select proper options and
                  <b>click Run Analysis</b> see RAISINS in action.",
      type = "info",
      html = TRUE
    )
  })

  ## -- read uploaded / demo data --------------------------------------------
  gre_raw <- reactive({
    df <- NULL
    if (!is.null(input$rcbd_file1)) {
      path <- input$rcbd_file1$datapath
      ext  <- tolower(tools::file_ext(input$rcbd_file1$name))
      df <- tryCatch(
        if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(path))
        else read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
        error = function(e) {
          error_support$report(e, context = "Data upload",
            public_message = "The file could not be read. Save it as CSV or Excel (.xlsx / .xls) and upload it again.")
          NULL
        })
    } else if (nzchar(input$demo_data_select %||% "")) {
      fp <- file.path("data", input$demo_data_select)
      if (file.exists(fp)) df <- read.csv(fp, check.names = FALSE, stringsAsFactors = FALSE)
      else error_support$report("The selected model dataset is unavailable. Choose another dataset.",
                                context = "Data upload")
    }
    if (!is.null(df)) {
      record <- isolate(error_support$current())
      if (!is.null(record) && identical(record$context, "Data upload")) error_support$clear()
    }
    df
  })

  num_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]
  # Series candidates: numeric columns, plus text columns that are mostly numbers
  # (so the data checks can flag numbers saved as text instead of hiding them).
  series_cols <- function(df) names(df)[vapply(df, function(x) {
    if (is.numeric(x)) return(TRUE)
    v <- trimws(as.character(x)); v <- v[!is.na(v) & nzchar(v)]
    length(v) > 0 && mean(!is.na(suppressWarnings(as.numeric(v)))) >= 0.5
  }, logical(1))]

  ## -- column pickers --------------------------------------------------------
  # Pickers start empty, as in RBD: the user chooses every column. A re-render
  # (e.g. after choosing the year) keeps choices that are still valid.
  output$gre_year_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    tags$div(
      pickerInput(
        "gre_year",
        label = HTML("Year column: <br><small style='font-size: 10px; color: gray;'>(Select the year / time column in your file)</small>"),
        choices = names(df),
        selected = character(0),
        options = pickerOptions(container = "body", title = "Select year column",
                                liveSearch = TRUE, size = 6),
        width = "100%"
      ),
      class = "brand-picker1"
    )
  })
  output$gre_series_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    nc <- setdiff(series_cols(df), input$gre_year %||% "")
    tags$div(
      pickerInput(
        "gre_series",
        label = HTML("Series columns: <br><small style='font-size: 10px; color: gray;'>(You can select all the numeric series for analysis at once.)</small>"),
        choices = nc, selected = intersect(isolate(input$gre_series), nc),
        multiple = TRUE,
        options = pickerOptions(container = "body", actionsBox = TRUE,
                                title = "Select series columns", size = 6),
        width = "100%"
      ),
      class = "brand-picker2"
    )
  })
  output$gre_group_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    cat_cols <- setdiff(names(df), c(input$gre_year %||% "", series_cols(df)))
    prev <- isolate(input$gre_group) %||% ""
    tags$div(
      pickerInput(
        "gre_group",
        label = HTML("Group column (optional): <br><small style='font-size: 10px; color: gray;'>(Region / district column; series are summed by year)</small>"),
        choices = c("No grouping" = "", cat_cols),
        selected = if (prev %in% cat_cols) prev else character(0),
        options = pickerOptions(container = "body", title = "No grouping (optional)", size = 6),
        width = "100%"
      ),
      class = "brand-picker3"
    )
  })

  ## -- guided picker walk-through (as in RBD): Year -> Series, then stop ------
  # The violet glow starts on the Year picker once the sidebar has rendered,
  # moves to Series when a year is chosen, and clears when the Series
  # dropdown closes with something selected. Group is optional and not guided.
  observeEvent(gre_raw(), {
    session$onFlushed(function() {
      if (session$isClosed()) return()
      shiny::withReactiveDomain(session, shiny::isolate({
        if (!is.null(gre_raw()) && !nzchar(input$gre_year %||% "")) {
          session$sendCustomMessage("removeHighlight", list())
          session$sendCustomMessage("highlightControl", list(
            id = "gre_year", panel = NULL, persistent = FALSE))
        }
      }))
    }, once = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$gre_year, {
    req(nzchar(input$gre_year %||% ""))
    req(gre_raw())
    if (isTRUE(gre_submitted())) return()
    session$sendCustomMessage("removeHighlight", list())
    later::later(function() {
      session$sendCustomMessage("highlightControl", list(
        id = "gre_series", panel = NULL, persistent = FALSE))
    }, delay = 0.3)
  }, ignoreInit = TRUE)

  # Last step: series chosen and dropdown closed - clear the glow and stop.
  observeEvent(input$variables_dropdown_closed, {
    if (length(input$gre_series) > 0 && !isTRUE(gre_submitted()))
      session$sendCustomMessage("removeHighlight", list())
  }, ignoreInit = TRUE)

  # Safety net: Run clicked with the dropdown still open.
  observeEvent(input$rcbd_submit, {
    session$sendCustomMessage("removeHighlight", list())
  }, ignoreInit = TRUE)
  output$gre_modules_ui <- renderUI({
    req(gre_raw())
    checkboxGroupInput(
      "gre_modules", "Trend analyses to run",
      choices = c("Growth rates"                       = "growth",
                  "Trend fitting (lin/quad/cubic/exp)" = "trend",
                  "Mann-Kendall + Sen's slope"         = "mk",
                  "Instability (CV, CDVI)"             = "instab",
                  "Area–Yield decomposition"           = "decomp"),
      selected = isolate(input$gre_modules) %||% c("growth", "trend", "mk", "instab"))
  })

  output$gre_sidebar_msg <- renderUI({
    if (is.null(gre_raw())) return(NULL)
    if (!nzchar(input$gre_year %||% ""))
      return(div(style = "color:#2E7D32;", "Select the year column."))
    if (length(input$gre_series %||% character(0)) < 1)
      return(div(style = "color:#2E7D32;", "Select at least one series to analyse."))
    NULL
  })

  ## -- data checks (shown with greyskull::raisins_check_boxes) ---------------
  gre_checks <- reactive({
    df <- gre_raw()
    gre_data_checks(df, input$gre_year, input$gre_series %||% character(0),
                    grouped = nzchar(input$gre_group %||% ""))
  })
  gre_run_ok <- reactive(!is.null(gre_raw()) && nzchar(input$gre_year %||% "") &&
                           !isTRUE(gre_checks()$blocked) && length(gre_checks()$valid) >= 1)
  output$gre_checks_ui <- renderUI({
    # Upload checks belong only to Analysis Results, like RBD.
    if (!identical(input$subtabs, "Analysis Results")) return(NULL)
    raisins_check_boxes(gre_checks(), can_run = gre_run_ok())
  })

  ## -- run state (drives the Run button and the result panels) --------------
  output$gre_can_run <- reactive(gre_run_ok())
  outputOptions(output, "gre_can_run", suspendWhenHidden = FALSE)

  gre_submitted <- reactiveVal(FALSE)
  # Run in two steps, as RBD's results appear: first open the results panel
  # (spinner note + calculator loaders), then calculate on the next round.
  # Calculating in the same round as the click would keep the panel closed
  # until everything had finished, so no loader would ever be seen.
  gre_run_ready <- reactiveVal(FALSE)
  observeEvent(input$rcbd_submit, {
    if (!gre_run_ok()) return()
    gre_run_ready(FALSE)
    gre_submitted(TRUE)
    session$onFlushed(function() {
      if (!session$isClosed()) gre_run_ready(TRUE)
    }, once = TRUE)
  })
  observe({ if (!isTRUE(gre_submitted())) gre_run_ready(FALSE) })
  observe({ if (isTRUE(gre_checks()$blocked)) gre_submitted(FALSE) })
  # A new file or a new column choice needs a fresh Run.
  observeEvent(list(gre_raw(), input$gre_year, input$gre_series, input$gre_group, input$gre_modules),
               { gre_submitted(FALSE) }, ignoreInit = TRUE)
  output$submitClicked <- reactive(gre_submitted())
  outputOptions(output, "submitClicked", suspendWhenHidden = FALSE)

  ## -- apply optional group filter -------------------------------------------
  data_filt <- reactive({
    df <- gre_raw(); req(df)
    g <- input$gre_group %||% ""
    if (nzchar(g) && g %in% names(df)) {
      # when a group col is chosen but no single level, aggregate by year sum
      yc <- input$gre_year
      valid <- gre_checks()$valid
      agg <- stats::aggregate(df[, valid, drop = FALSE],
                              by = list(df[[yc]]), FUN = sum, na.rm = TRUE)
      names(agg)[1] <- yc
      agg
    } else df
  })

  ## -- core computation on Run -----------------------------------------------
  # Recomputes live when alpha changes after a Run, like RBD's options well.
  res <- reactive({
    req(gre_submitted(), gre_run_ready())
    df <- data_filt(); req(df)
    # Only the series that passed the data checks (skipped ones are listed there).
    yc <- input$gre_year; series <- gre_checks()$valid
    shiny::validate(need(length(series) >= 1, "Select at least one series."))
    year <- suppressWarnings(as.numeric(df[[yc]]))
    shiny::validate(need(sum(is.finite(year)) >= 3,
                  "The year column needs at least 3 numeric values."))
    mods  <- input$gre_modules %||% character(0)
    alpha <- as.numeric(input$gre_alpha %||% "0.05")

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

    # Plain-language write-up (gre_interpretation_text below), shown on the
    # Interpretation tab and used in the Statistical Report.
    interp <- gre_interpretation_text(list(year = year, series = series, growth = growth,
                                           trend = trend, trend_fits = trend_fits, mk = mk,
                                           instab = instab, decomp = decomp, alpha = alpha))

    list(raw = df, year_col = yc, growth = growth, trend = trend,
         trend_fits = trend_fits, mk = mk, instab = instab,
         decomp = decomp, decomp_msg = decomp_msg, interp = interp, alpha = alpha)
  })

  ## -- output renderers ------------------------------------------------------
  dt <- function(d) DT::datatable(d, rownames = FALSE,
                                  options = list(dom = "t", ordering = FALSE,
                                                 scrollX = TRUE, pageLength = 50))

  output$gre_run_hint <- renderUI({
    if (!gre_submitted() && !is.null(gre_raw()))
      div(style = "color:#2E7D32;", "Set your columns and analyses, then click Run Analysis.")
  })
  # Drives RBD's "results will appear shortly" note until the run has finished.
  output$gre_results_ready <- reactive({
    isTRUE(gre_run_ready()) && !is.null(tryCatch(res(), error = function(e) NULL))
  })
  outputOptions(output, "gre_results_ready", suspendWhenHidden = FALSE)
  output$gre_growth_table <- DT::renderDT({ req(res()$growth); dt(res()$growth) })
  # Calculate with the run (not after the browser reports the panel visible),
  # so the calculator loader shows on this table while results are worked out.
  # Higher priority so it starts first: its "recalculating" signal (which opens
  # the GIF) then reaches the browser before the calculation, not after it.
  outputOptions(output, "gre_growth_table", suspendWhenHidden = FALSE, priority = 10)
  output$gre_growth_note  <- renderUI({
    req(res())
    HTML("<small>CGR from the log-linear model ln(y)=a+b·t; CAGR from first and last values. Rates need positive values.</small>")
  })
  output$gre_trend_table <- DT::renderDT({ req(res()$trend); dt(res()$trend) })
  ## Trend-plot workspace (dir/gre_plot_workspace.R): one customised plot is
  ## shown on screen, downloaded, and embedded in the Statistical Report.
  gre_trend_labels <- gre_labels_server("gre_ws_trend_edit",
    originals   = reactive({ r <- res(); names(r$trend_fits) }),
    dataset_key = reactive(input$rcbd_file1$datapath %||% input$demo_data_select %||% ""))
  gre_trend_plot_obj <- reactive({
    r <- res(); req(length(r$trend_fits) > 0)
    gre_trend_plot_build(r, input, gre_trend_labels())
  })
  output$gre_trend_plot <- renderPlot(gre_trend_plot_obj())
  # Only offer the download when a trend was actually fitted.
  output$gre_trend_plot_dl_ui <- renderUI({
    r <- res(); req(length(r$trend_fits) > 0)
    gre_trend_export_ui(input)
  })
  output$gre_trend_plot_dl <- downloadHandler(
    filename = function() paste0("GRE_trend_plot_", Sys.Date(), ".", input$gre_trend_format %||% "png"),
    content = function(file) {
      d <- gre_trend_dimensions(input)
      p <- gre_trend_plot_obj()
      gre_open_device(file, d$format, d)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(p)
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
    shiny::validate(need(!is.null(r$decomp), r$decomp_msg %||% "Enable decomposition and provide Area/Production columns."))
    dt(r$decomp)
  })
  output$gre_decomp_note <- renderUI({ r <- res(); if (!is.null(r$decomp_msg)) div(style = "color:#8B0000;", r$decomp_msg) })

  ## -- Interpretation tab (RBD's RAISINS Assistant flow) ---------------------
  gre_interpretation_ready <- reactiveVal(FALSE)
  output$gre_interpretation_ready <- reactive(gre_interpretation_ready())
  outputOptions(output, "gre_interpretation_ready", suspendWhenHidden = FALSE)
  gre_clear_interpretation <- function() {
    gre_interpretation_ready(FALSE)
    updateCheckboxInput(session, "checkforAI_gre", value = FALSE)
    output$AI_typing_path_gre <- renderUI(NULL)
    output$var_AI_path_end_gre <- renderUI(NULL)
    shinyjs::runjs("window.raisinsGreStopped=true; clearTimeout(window.raisinsGreTimer); ['output_path_typing_gre','typed-gre-full','typed-gre-plain'].forEach(function(id){var out=document.getElementById(id); if(out){out.textContent=''; if('value' in out) out.value='';}});")
  }
  # The write-up is a snapshot: a new Run or a new alpha clears it.
  observeEvent(list(input$rcbd_submit, gre_submitted(), input$gre_alpha), {
    gre_clear_interpretation()
  }, ignoreInit = TRUE, priority = 100)

  output$var_AI_path_actionbttn_gre <- renderUI({
    if (!isTRUE(input$checkforAI_gre)) return(NULL)
    actionBttn(inputId = "aibttn_path_gre", label = "Click here for interpretation",
               color = "warning", size = "sm", style = "fill")
  })

  observeEvent(input$aibttn_path_gre, {
    req(isTRUE(input$checkforAI_gre))
    final_plain <- trimws(res()$interp)
    gre_interpretation_ready(TRUE)

    output$AI_typing_path_gre <- renderUI({
      tagList(
        # toolbar
        HTML(
          "<div style='display:flex; gap:10px; margin-bottom:10px;'>
         <button type='button' onclick='raisinsGreCopy(this)'
           style='background:#4682B4; color:#fff; border:none; border-radius:6px;
                  padding:6px 14px; font-size:13px; font-weight:600; cursor:pointer;'>
           <i class='fa fa-copy'></i> Copy
         </button>
         <button type='button' onclick='raisinsGreStop(this)'
           style='background:#c0392b; color:#fff; border:none; border-radius:6px;
                  padding:6px 14px; font-size:13px; font-weight:600; cursor:pointer;'>
           <i class='fa fa-stop'></i> Quick
         </button>
       </div>"
        ),
        # hidden holders for Quick and Copy
        div(id = "typed-gre-full", style = "display:none;", final_plain),
        tags$textarea(id = "typed-gre-plain", style = "display:none;", final_plain),
        tags$script(HTML(paste0(
          "window.raisinsGreLinkReferences = function(out){",
          "  var text=out.textContent, urls=['https://doi.org/10.5281/zenodo.15622128','https://www.R-project.org/'];",
          "  var fragment=document.createDocumentFragment(), start=0;",
          "  while(start<text.length){ var next=-1,url='';",
          "    urls.forEach(function(candidate){ var index=text.indexOf(candidate,start);",
          "      if(index!==-1&&(next===-1||index<next)){next=index;url=candidate;} });",
          "    if(next===-1)break;",
          "    fragment.appendChild(document.createTextNode(text.slice(start,next)));",
          "    var link=document.createElement('a'); link.href=url;link.textContent=url;",
          "    link.target='_blank';link.rel='noopener noreferrer';fragment.appendChild(link);",
          "    start=next+url.length; }",
          "  fragment.appendChild(document.createTextNode(text.slice(start)));out.textContent='';out.appendChild(fragment);",
          "};",
          "window.raisinsGreStop = function(btn){",
          "  window.raisinsGreStopped = true;",
          "  if(window.raisinsGreTimer){ clearTimeout(window.raisinsGreTimer); }",
          "  var full=document.getElementById('typed-gre-full');",
          "  var vis=document.getElementById('output_path_typing_gre');",
          "  if(full&&vis){ vis.textContent=full.textContent; window.raisinsGreLinkReferences(vis); }",
          "};",
          "window.raisinsGreCopy = function(btn){",
          "  var el=document.getElementById('typed-gre-plain');",
          "  var text=el?(el.value||''):'';",
          "  var mark=function(ok){ if(!btn)return;",
          "    if(btn.getAttribute('data-orig')===null){ btn.setAttribute('data-orig',btn.innerHTML); }",
          "    btn.innerHTML=ok?'&#10003; Copied!':'Press Ctrl+C';",
          "    setTimeout(function(){ btn.innerHTML=btn.getAttribute('data-orig'); },1600); };",
          "  var legacy=function(){ var ta=document.createElement('textarea'); ta.value=text;",
          "    ta.style.position='fixed'; ta.style.opacity='0'; document.body.appendChild(ta);",
          "    ta.focus(); ta.select(); var ok=false; try{ok=document.execCommand('copy');}catch(e){ok=false;}",
          "    document.body.removeChild(ta); return ok; };",
          "  if(navigator.clipboard&&navigator.clipboard.writeText){",
          "    navigator.clipboard.writeText(text).then(function(){mark(true);},function(){mark(legacy());});",
          "  } else { mark(legacy()); }",
          "};",
          "(function(){",
          "  window.raisinsGreStopped=false;",
          "  clearTimeout(window.raisinsGreTimer); var i=0; var txt=",
          gsub("<", "\\u003c", jsonlite::toJSON(final_plain, auto_unbox = TRUE), fixed = TRUE), "; var speed=10;",
          "  var target=document.getElementById('output_path_typing_gre');",
          "  if(target){ target.textContent=''; target.style.whiteSpace='pre-wrap';",
          "    function typeWriter(){",
          "      if(window.raisinsGreStopped) return;",
          "      if(i<txt.length){",
          "        target.textContent += txt.charAt(i);",
          "        i++; window.raisinsGreTimer=setTimeout(typeWriter,speed);",
          "      } else { window.raisinsGreLinkReferences(target); }",
          "    }",
          "    typeWriter();",
          "  }",
          "})();"
        )))
      )
    })

    output$var_AI_path_end_gre <- renderUI({
      HTML("<p style='color: red; font-weight: bold; font-size: 10px; text-align: justify;'>Warning: Please note that the statistical interpretations generated by RAISINS AI are intended for reference purposes only and should be thoroughly reviewed and verified by a human expert before being utilized in any research publications. It is important to exercise caution and ensure the accuracy and validity of the output through rigorous human analysis and review. Also please note that if there is any error in uploaded csv file it will affect Interpretation. So confirm there is no error in Analysis.Our expert statisticians are available for consultation based on generated report.</p>")
    })
  }, ignoreInit = TRUE)

  ## -- View Data + file-check guidance (shared IDs) --------------------------
  # Same structure as RBD: the file health-check instructions (www/filecheck.html)
  # above a colour-coded table - numbers green, text yellow, missing red.
  output$rcbd_instr_filecheck <- renderUI({
    if (is.null(gre_raw())) return(NULL)
    tags$iframe(
      src = "filecheck.html",
      style = "width:100%; height:300px; border:none;",
      seamless = TRUE
    )
  })
  output$rcbd_ViewData <- DT::renderDT({
    dt <- gre_raw()
    if (is.null(dt)) return()
    char_cols <- vapply(dt, is.character, logical(1))
    tbl <- DT::datatable(dt,
      selection = "none",
      options = list(
        scrollX = TRUE,
        lengthMenu = if (nrow(dt) > 5000) list(c(20, 50, 100, 500), c("20", "50", "100", "500"))
                     else list(c(20, 50, 100, -1), c("20", "50", "100", "All")),
        paging = TRUE,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        ordering = TRUE,
        dom = "lfrtipB"
      ),
      class = "cell-border stripe")
    if (any(!char_cols))
      tbl <- DT::formatStyle(tbl, names(dt)[!char_cols],
                             backgroundColor = DT::styleEqual(NA, "red", default = "lightgreen"))
    if (any(char_cols))
      tbl <- DT::formatStyle(tbl, names(dt)[char_cols],
                             backgroundColor = DT::styleEqual(c("", NA), c("red", "red"), default = "yellow"))
    tbl
  })

  ## -- FAQs ------------------------------------------------------------------
  ## FAQ buttons open www/*.html in a modal, like RBD.
  output$instr_bttns_FAQs <- renderUI({
    list(
      tags$h4("FAQs"),
      actionBttn(
        inputId = "instr_gre_fileprep_FAQ",
        label = HTML("&#129300; How to prepare and upload file?"),  # 🤔
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      actionBttn(
        inputId = "instr_gre_growth_FAQ",
        label = HTML("&#129488; What is CGR vs CAGR, and why is a rate blank?"),  # 🧐
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      actionBttn(
        inputId = "instr_gre_trend_FAQ",
        label = HTML("&#129327; Trend test, instability and decomposition"),  # 🤯
        style = "stretch",
        color = "warning",
        size = "sm"
      )
    )
  })

  gre_faq_modal <- function(title, src) {
    showModal(
      modalDialog(
        title = title,
        tags$iframe(
          src = src,  # file in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }
  observeEvent(input$instr_gre_fileprep_FAQ, {
    gre_faq_modal("Instructions", "dataprep_gre.html")
  })
  observeEvent(input$instr_gre_growth_FAQ, {
    gre_faq_modal("Growth rates", "growthrates.html")
  })
  observeEvent(input$instr_gre_trend_FAQ, {
    gre_faq_modal("Trend, instability and decomposition", "trend_decomp.html")
  })

  ## -- report download (HTML / Word via styles.docx / PDF) -------------------
  ## -- Statistical Report tab (RBD full-report layout; shown only after Run) --
  gre_report_tables <- c(growth = "Growth rates", trend = "Trend fitting",
                         mk = "Mann-Kendall trend test", instab = "Instability",
                         decomp = "Area-Yield decomposition")
  gre_report_notes  <- c(growth = "CGR, CAGR and average annual growth for each series",
                         trend  = "Linear, quadratic, cubic and exponential fits ranked by adjusted R²",
                         mk     = "Kendall's τ, p-value and Sen's slope with its confidence interval",
                         instab = "Coefficient of variation and Cuddy-Della Valle Index",
                         decomp = "Area, yield and interaction effects on production change")
  gre_report_option <- function(label, description)
    tagList(span(class = "stat-report-option-title", label),
            span(class = "stat-report-option-description", description))

  output$gre_report_body <- renderUI({
    req(gre_submitted())
    r <- res(); req(r)
    avail <- names(gre_report_tables)[vapply(names(gre_report_tables),
                                             function(k) !is.null(r[[k]]), logical(1))]
    sections <- c(interp = "Interpretation", plot = "Fitted-trend figure")
    section_notes <- c(interp = "Plain-language summary of every analysis that was run",
                       plot   = "Observed values with the best-fit trend for each series")
    if (!length(r$trend_fits)) sections <- sections["interp"]
    prev_tables   <- isolate(input$gre_report_tables)
    prev_sections <- isolate(input$gre_report_sections)
    tagList(
      h3(class = "stat-report-heading", "Statistical Analysis Report"),
      p(class = "stat-report-intro",
        "Prepare a consolidated account of the growth and trend analysis. Select the required tables and report sections below. Font and table style follow the options on the Analysis Results tab."),
      div(class = "stat-report-grid",
        tags$fieldset(class = "stat-report-panel stat-report-tables",
          tags$legend("Statistical tables"),
          p(class = "stat-report-note", "Select the results to include. Only the analyses that were run are listed."),
          checkboxGroupInput("gre_report_tables", NULL,
            choiceNames  = lapply(avail, function(k) gre_report_option(gre_report_tables[[k]], gre_report_notes[[k]])),
            choiceValues = avail,
            selected = if (is.null(prev_tables)) avail else intersect(prev_tables, avail),
            width = "100%")),
        tags$fieldset(class = "stat-report-panel stat-report-sections",
          tags$legend("Report sections"),
          p(class = "stat-report-note", "Select the supporting sections to include."),
          checkboxGroupInput("gre_report_sections", NULL,
            choiceNames  = lapply(names(sections), function(k) gre_report_option(sections[[k]], section_notes[[k]])),
            choiceValues = names(sections),
            selected = if (is.null(prev_sections)) names(sections) else intersect(prev_sections, names(sections)),
            width = "100%"))),
      div(class = "stat-report-export",
        radioButtons("gre_report_format", "Document format", c("HTML", "PDF", "Word"),
                     inline = TRUE, selected = isolate(input$gre_report_format) %||% "HTML"),
        uiOutput("gre_report_download_ui"))
    )
  })

  output$gre_report_download_ui <- renderUI({
    if (!length(input$gre_report_tables) && !length(input$gre_report_sections))
      return(p("Select at least one table or supporting section."))
    downloadButton("gre_download_report", "Download statistical report", class = "btn-success")
  })

  output$gre_download_report <- downloadHandler(
    filename = function()
      paste0("RAISINS_GRE_report_", Sys.Date(), ".",
             switch(input$gre_report_format, PDF = "pdf", Word = "docx", "html")),
    content = function(file) {
      showModal(modalDialog(
        title = "Downloading please wait...",
        tags$img(
          src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fdownload.gif?alt=media",
          style = "display: block; margin: 0 auto; width: 80%; height: auto;"
        ),
        footer = NULL
      ))
      on.exit(removeModal())
      r <- res()
      # Leave out what the user unticked; the Rmd skips NULL tables.
      keep <- input$gre_report_tables %||% character(0)
      for (k in setdiff(names(gre_report_tables), keep)) r[k] <- list(NULL)
      secs <- input$gre_report_sections %||% character(0)
      if (!"interp" %in% secs) r$interp <- NULL
      if (!"plot" %in% secs) r$trend_fits <- list()
      font  <- raisins_font_value(input$gre_font %||% "cambria")
      style <- raisins_tstyle_value(input$gre_table_style %||% "classic")
      # As in RBD: PDF via xelatex with the chosen font's twin (www/fonts);
      # Word gets font and table style applied to the .docx after rendering.
      fmt <- switch(input$gre_report_format,
                    HTML = rmarkdown::html_document(df_print = "kable"),
                    Word = rmarkdown::word_document(reference_docx = "styles.docx"),
                    PDF  = rmarkdown::pdf_document(
                      latex_engine = "xelatex",
                      pandoc_args  = raisins_font_pdf_args(font, normalizePath("www/fonts", winslash = "/"))))
      td <- tempdir()
      file.copy("growth_rate_report.Rmd", file.path(td, "growth_rate_report.Rmd"), overwrite = TRUE)
      if (file.exists("styles.docx"))
        file.copy("styles.docx", file.path(td, "styles.docx"), overwrite = TRUE)
      # The report uses the trend plot exactly as customised in its workspace.
      plot_path <- NA
      if (length(r$trend_fits)) {
        plot_path <- file.path(td, "gre_trend.png")
        ggplot2::ggsave(plot_path, gre_trend_plot_obj(), width = 8, height = 5, dpi = 150, bg = "white")
      }
      out <- rmarkdown::render(
        input = file.path(td, "growth_rate_report.Rmd"),
        output_format = fmt,
        params = list(
          table_style = style,
          font        = font,
          growth = r$growth, trend = r$trend, mk = r$mk,
          instab = r$instab, decomp = r$decomp,
          interp = r$interp, plot_path = plot_path, alpha = r$alpha),
        envir = new.env(parent = globalenv()), quiet = TRUE)
      if (identical(input$gre_report_format, "Word")) {
        raisins_font_docx(out, font)
        raisins_tstyle_docx(out, style)
      }
      file.copy(out, file, overwrite = TRUE)
    })

  # Shared with the invoice / Arrange Data / Datasets servers, like RBD.
  error_support
}

## ============================================================================
##  RAISINS Growth Rate Estimation — error support + data checks
##  The support module (raisins_support_server / raisins_support_ui) and the
##  check box display (raisins_check_boxes) come from greyskull. The panel,
##  alert and user-details helpers are ported from RAISINS RBD (they are
##  app-local there too). gre_data_checks() is this module's own hook: the
##  greyskull raisins_data_checks() is written for ANOVA designs.
## ============================================================================

gre_error_alert_styles <- function() shiny::tags$style(shiny::HTML('
.sweet-alert.gre-support-alert {width:460px!important;max-width:calc(100vw - 32px)!important;box-sizing:border-box;
  left:50%!important;right:auto!important;margin-left:0!important;transform:translateX(-50%)!important;padding:28px 28px 24px;
  border-radius:16px;box-shadow:0 18px 70px rgba(38,28,51,.18);max-height:calc(100vh - 32px);
  overflow-y:auto;font-family:Arial,sans-serif;}
.sweet-alert.gre-support-alert:before {content:"RAISINS SUPPORT";display:block;
  color:#786787;font-size:11px;font-weight:600;letter-spacing:1.4px;margin-bottom:14px;}
.sweet-alert.gre-support-alert h2 {font-size:23px;line-height:1.35;font-weight:600;
  color:#382c46;margin:0 0 14px;}
.sweet-alert.gre-support-alert p {margin:0;color:#5f5967;font-size:15px;line-height:1.6;}
.gre-alert-summary {display:block;}
.gre-alert-next {display:block;margin-top:14px;color:#77707e;font-size:14px;}
.sweet-alert.gre-support-alert button {border-radius:8px;font-size:14px;font-weight:600;
  padding:11px 18px;margin:22px 5px 0;box-shadow:none!important;white-space:normal;}
.sweet-alert.gre-support-alert button.cancel {background:#f1eef5!important;color:#554762;}
.sweet-alert.gre-support-alert button.confirm {background:#70508b!important;color:#fff;}
.sweet-alert.gre-support-alert button:focus-visible {outline:3px solid #c7b3d9;outline-offset:3px;}
'))

## "Oho! Something went wrong." alert; "Ask RAISINS" opens the support form.
gre_error_alert_server <- function(error_support, session = shiny::getDefaultReactiveDomain(),
                                   assistance_id = "gre_error_support") {
  seen <- character()
  shiny::insertUI(selector = "head", where = "beforeEnd", ui = gre_error_alert_styles(),
                  immediate = TRUE, session = session)
  open_assistance <- function() {
    # Opens the existing contact form; it never submits a ticket.
    button_id <- jsonlite::toJSON(paste0(assistance_id, "-yes"), auto_unbox = TRUE)
    panel_id  <- jsonlite::toJSON(paste0(assistance_id, "-actions"), auto_unbox = TRUE)
    shinyjs::runjs(sprintf(paste0(
      '(function(){var tries=0;function open(){var panel=document.getElementById(%s);',
      'var button=document.getElementById(%s);if(button){button.click();}',
      'if(panel && $(panel).is(":visible") && (button || panel.querySelector("input,select"))){raisinsScrollTo(panel,"center");',
      'setTimeout(function(){var field=panel.querySelector("input:not([type=hidden]),select,button");',
      'if(field)field.focus({preventScroll:true});},150);return;}',
      'if(++tries<20)setTimeout(open,100);}open();})();'), panel_id, button_id))
  }
  shiny::observeEvent(error_support$current(), {
    record <- error_support$current()
    if (is.null(record) || !identical(record$status, "offered") || record$id %in% seen) return()
    seen <<- tail(c(seen, record$id), 64L)
    incident <- record$id
    copy <- shiny::tagList(
      shiny::tags$span(class = "gre-alert-summary", "Don’t worry—we’re here to make statistics sweet."),
      shiny::tags$span(class = "gre-alert-next", "Return to the app and try again, or ask the RAISINS team for help."))
    shinyalert::shinyalert(title = "Oho! Something went wrong.", text = copy,
      type = "", html = TRUE, showCancelButton = TRUE, animation = FALSE,
      className = "gre-support-alert", confirmButtonCol = "#70508b",
      confirmButtonText = "Ask RAISINS", cancelButtonText = "Back to app",
      closeOnClickOutside = FALSE, inputId = "gre_unexpected_error_alert", session = session,
      callbackR = function(confirmed) {
        current <- shiny::isolate(error_support$current())
        if (isTRUE(confirmed) && !is.null(current) && identical(current$id, incident) &&
            current$status %in% c("offered", "details", "failed")) {
          shiny::updateTabsetPanel(session, "tabs", selected = "Analysis")
          shiny::updateTabsetPanel(session, "subtabs", selected = "Analysis Results")
          open_assistance()
        }
      })
  }, ignoreNULL = TRUE)
}

gre_error_panel <- function(message, title = "Check your data", assistance = NULL) {
  tags$div(id = "gre_data_error", role = "alert",
    style = "margin:12px 0;padding:14px 18px;border:1px solid #e4b5b5;border-radius:6px;background:#fff5f5;color:#7a201b;font-size:15px;line-height:1.5;",
    tags$strong(title), tags$div(message), assistance)
}

## Account details attached to a support ticket (same fields as RBD).
gre_support_user_details <- function(user, user_info, users, verified_demo_email = NULL) {
  first_value <- function(...) {
    for (value in list(...)) {
      if (!is.null(value) && length(value) && !is.na(value[1L]) &&
          nzchar(trimws(as.character(value[1L])))) return(as.character(value[1L]))
    }
    NULL
  }
  account <- first_value(user)
  details <- list()
  if (!is.null(account) && !is.null(users) && "Email ID" %in% names(users)) {
    lookup <- if (identical(account, "demo")) first_value(verified_demo_email, "demo@statoberry.com") else account
    matches <- which(tolower(trimws(as.character(users[["Email ID"]]))) == tolower(trimws(lookup)))
    if (!length(matches) && identical(account, "demo"))
      matches <- which(users[["Email ID"]] == "demo@statoberry.com")
    if (length(matches)) {
      index <- matches[1L]
      for (field in intersect(c("Full name", "Select Your Plan", "payID", "Institution", "Institution name"), names(users)))
        details[[field]] <- users[[field]][index]
    }
  }
  plan <- first_value(user_info$comment, details[["Select Your Plan"]])
  plan_label <- if (identical(plan, "23600")) "Monthly" else if (identical(plan, "94400")) "Basic" else plan
  account_type <- if (identical(account, "demo")) "Preview" else if (!is.null(plan) &&
    grepl("Inst plan", plan, ignore.case = TRUE)) "Institutional" else if (!is.null(account)) "Subscriber" else NULL
  list(name = first_value(details[["Full name"]]), account_type = account_type,
       plan = plan_label, pay_id = first_value(details[["payID"]]), valid_until = first_value(user_info$expire),
       institution = first_value(details[["Institution"]], details[["Institution name"]]))
}

## Time-series data checks, in the list shape greyskull::raisins_check_boxes() draws:
## list(issues = list(list(title, entries, action)), valid, skipped, blocked).
gre_data_checks <- function(df, year_col, series, grouped = FALSE) {
  out <- list(issues = list(), valid = series, skipped = character(), blocked = FALSE)
  add <- function(title, entries, action, blocking = FALSE) {
    out$issues[[length(out$issues) + 1L]] <<- list(title = title, entries = entries, action = action)
    if (blocking) out$blocked <<- TRUE
  }
  if (is.null(df) || !length(year_col) || !year_col %in% names(df)) return(out)

  yr  <- df[[year_col]]
  num <- suppressWarnings(as.numeric(as.character(yr)))
  bad_year <- which(is.na(num))
  if (length(bad_year))
    add("Missing or invalid years", paste0(year_col, ": data row ", bad_year, " = ", as.character(yr[bad_year])),
        "Every row needs a numeric year such as 2001. Please correct these rows in your original file.", TRUE)
  if (!grouped) {
    dup <- unique(num[duplicated(num) & !is.na(num)])
    if (length(dup))
      add("Repeated years", paste("Year", dup),
          "Each year should appear once. If the file has one row per district or region, choose that column in the Group column picker.", TRUE)
  }
  if (length(unique(num[!is.na(num)])) < 3L)
    add("Too few years", paste(length(unique(num[!is.na(num)])), "year(s) found"),
        "Growth and trend analysis needs at least 3 years of data.", TRUE)

  rejected <- character()
  prints <- list()
  for (nm in intersect(series, setdiff(names(df), year_col))) {
    x <- df[[nm]]
    probe <- if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
    blank <- which(is.na(x) | (is.character(x) & !nzchar(trimws(as.character(x)))))
    bad   <- setdiff(which(!is.finite(probe)), blank)
    if (!is.numeric(x) && !length(bad)) {
      add("Numbers saved as text", nm,
          "The numbers in this column are saved as text. Please save them as numbers in your original file. This column will be skipped.")
      rejected <- c(rejected, nm); next
    }
    if (length(bad)) {
      add("Invalid numbers", paste0(nm, ": data row ", bad, " = ", as.character(x[bad])),
          "Some entries are not valid numbers. Please check the entries listed above. This column will be skipped.")
      rejected <- c(rejected, nm); next
    }
    if (length(blank))
      add("Missing values", paste0(nm, ": year ", as.character(yr[blank])),
          "These years have no value and are left out of the calculations for this series. Leave them blank rather than typing 0.")
    nonpos <- which(is.finite(probe) & probe <= 0)
    if (length(nonpos))
      add("Zero or negative values", paste0(nm, ": year ", as.character(yr[nonpos]), " = ", probe[nonpos]),
          "Compound growth rates (CGR, CAGR) need positive values, so these years are skipped for those rates. Trend and instability use them as they are.")
    same <- names(prints)[vapply(prints, function(p) identical(p, as.numeric(probe)), logical(1))]
    if (length(same))
      add("Identical columns", paste(c(same[1], nm), collapse = " and "),
          "These columns contain exactly the same values. Please check whether one was copied by mistake. Both columns will remain selected.")
    prints[[nm]] <- as.numeric(probe)
  }
  out$valid   <- setdiff(series, rejected)
  out$skipped <- unique(rejected)
  if (length(series) && !length(out$valid)) {
    out$blocked <- TRUE
    add("No columns available for analysis", "None of the selected series can be analysed.",
        "Please correct the issues above and upload the file again, or select other series.")
  }
  if (out$blocked) out$valid <- character()
  out
}

## ============================================================================
##  RAISINS Growth Rate Estimation — interpretation text (as in RAISINS RBD)
##  Builds the plain-text write-up for the Interpretation tab's typewriter and
##  the Statistical Report. Like RBD, the opening and closing wording is picked
##  at random from a few variants; every number comes from the results.
## ============================================================================

gre_analysis_citation <- function(cfg = raisins_app())
  paste0("Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025). ",
         cfg$app_name, " (Version ", cfg$version, ").")

gre_p_text <- function(p) {
  if (is.null(p) || !length(p) || is.na(p)) return("p not available")
  if (p < 0.001) "p < 0.001" else paste0("p = ", formatC(p, digits = 3, format = "g"))
}

gre_join <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste0(paste(head(x, -1), collapse = ", "), " and ", tail(x, 1))
}

gre_interpretation_text <- function(r) {
  alpha <- r$alpha
  years <- r$year[is.finite(r$year)]
  span  <- if (length(years)) paste0(min(years), "–", max(years)) else "the study period"
  n_yr  <- length(unique(years))
  paras <- character()

  openings <- c(
    "Hi, please relate the explanation below with the tables in the 'Analysis Results', 'Trend Fitting', 'Trend Test' and 'Instability' tabs.",
    "Dear user, kindly connect the explanation below with the corresponding tables in the analysis subtabs.",
    "Hello, the tables in the analysis subtabs should be read together with the explanation below.",
    "Dear researcher, please interpret the results presented in the analysis subtabs based on the following explanation.")
  closings <- c(
    "If you have any doubts, please feel free to contact us. Our statisticians are there to help you.\nHAPPY STATISTICS with STATOBERRY!",
    "If any questions arise, please reach out to us. Our statisticians are available to help.\nWE MAKE SOUR STATISTICS SWEET AT STATOBERRY!",
    "For further clarification, feel free to contact us. Our team is happy to assist.\nHAPPY STATISTICS with STATOBERRY!",
    "Got questions? Reach out to us! We're here to help.\nWE MAKE SOUR STATISTICS SWEET AT STATOBERRY!")

  paras <- c(paras, paste0(
    sample(openings, 1), " Yearly data on ", gre_join(r$series), " covering ", span, " (", n_yr,
    " years) were analysed. The selected significance level is ", alpha, "."))

  # Growth rates
  g <- r$growth
  if (!is.null(g) && nrow(g)) {
    lines <- vapply(seq_len(nrow(g)), function(i) {
      cgr <- g$`CGR (%/yr)`[i]
      if (is.na(cgr))
        return(paste0(g$Series[i], ": the compound growth rate could not be estimated because fewer than three positive values were available."))
      dir <- if (cgr > 0) "grew" else if (cgr < 0) "declined" else "remained unchanged"
      sig <- if (!is.na(g$`CGR p`[i]) && g$`CGR p`[i] < alpha) "statistically significant" else "not statistically significant"
      paste0(g$Series[i], " ", dir, " at a compound growth rate (CGR) of ", abs(cgr), "% per year (", sig, ", ",
             gre_p_text(g$`CGR p`[i]), "). The endpoint CAGR was ", g$`CAGR (%/yr)`[i],
             "% per year and the average year-on-year change was ", g$`Annual avg (%/yr)`[i], "%.")
    }, character(1))
    paras <- c(paras, paste0(
      "Growth rates. The CGR is estimated from the log-linear model ln(y) = a + bt fitted to all years, so it reflects the whole period; ",
      "the CAGR uses only the first and last years and is therefore more sensitive to unusual end points. ",
      paste(lines, collapse = " ")))
  }

  # Trend fitting
  if (length(r$trend_fits)) {
    t <- r$trend
    lines <- vapply(names(r$trend_fits), function(s) {
      best <- r$trend_fits[[s]]$best
      adj  <- if (!is.null(t)) t$`Adj R2`[t$Series == s & t$Model == best] else NA
      paste0("For ", s, " the ", tolower(best), " trend fitted best (adjusted R² = ", adj[1], ").")
    }, character(1))
    paras <- c(paras, paste0(
      "Trend fitting. Linear, quadratic, cubic and exponential trends were fitted and ranked by adjusted R². ",
      paste(lines, collapse = " "),
      " When two models have almost the same adjusted R², the simpler one is usually preferred."))
  }

  # Mann-Kendall + Sen's slope
  m <- r$mk
  if (!is.null(m) && nrow(m)) {
    lines <- vapply(seq_len(nrow(m)), function(i) {
      trend <- if (m$Trend[i] == "No significant trend") "showed no significant monotonic trend" else
        paste0("showed a significant ", tolower(m$Trend[i]), " trend")
      paste0(m$Series[i], " ", trend, " (Kendall's τ = ", m$`Kendall tau`[i], ", ", gre_p_text(m$`p-value`[i]),
             "), with a Sen's slope of ", m$`Sen slope/yr`[i], " units per year (confidence interval ",
             m$`Sen CI low`[i], " to ", m$`Sen CI high`[i], ").")
    }, character(1))
    paras <- c(paras, paste0(
      "Trend test. The non-parametric Mann-Kendall test checks for a steady rise or fall without assuming a straight line or normal errors, ",
      "and Sen's slope gives a robust estimate of the change per year in the series' own units. ",
      paste(lines, collapse = " ")))
  }

  # Instability
  ins <- r$instab
  if (!is.null(ins) && nrow(ins)) {
    lines <- vapply(seq_len(nrow(ins)), function(i)
      paste0(ins$Series[i], " had a CV of ", ins$`CV (%)`[i], "% and a CDVI of ", ins$`CDVI (%)`[i], "% (",
             tolower(as.character(ins$Stability[i])), ")."), character(1))
    paras <- c(paras, paste0(
      "Instability. The coefficient of variation (CV) includes variation caused by the trend itself, whereas the ",
      "Cuddy-Della Valle Index (CDVI) removes the trend and so reflects genuine year-to-year instability. ",
      "A CDVI below 15% indicates low, 15% to 30% moderate, and above 30% high instability. ",
      paste(lines, collapse = " ")))
  }

  # Decomposition
  d <- r$decomp
  if (!is.null(d) && nrow(d)) {
    share <- function(e) d$`Share (%)`[d$Effect == e]
    ae <- share("Area effect"); ye <- share("Yield effect"); ie <- share("Interaction effect")
    driver <- if (length(ae) && length(ye)) (if (ae > ye) "area expansion" else "yield improvement") else "mixed factors"
    paras <- c(paras, paste0(
      "Area-yield decomposition. The total change in production between the first and last year was split into an area effect (",
      ae, "%), a yield effect (", ye, "%) and an interaction effect (", ie, "%). The change was driven mainly by ", driver, "."))
  }

  paras <- c(paras, gre_analysis_citation(), paste(
    "References:",
    "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis. Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128",
    "2. R Core Team. 2024. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/.",
    "3. Mann, H. B. (1945). Nonparametric tests against trend. Econometrica, 13(3), 245–259.",
    "4. Kendall, M. G. (1975). Rank Correlation Methods. Charles Griffin, London.",
    "5. Sen, P. K. (1968). Estimates of the regression coefficient based on Kendall's tau. Journal of the American Statistical Association, 63(324), 1379–1389.",
    "6. Cuddy, J. D. A., & Della Valle, P. A. (1978). Measuring the instability of time series data. Oxford Bulletin of Economics and Statistics, 40(1), 79–85.",
    sep = "\n"),
    sample(closings, 1))
  paste(paras, collapse = "\n\n")
}

## ---- Quick Tour: steps, dialogs and server events -------------------------
gre_tour_steps <- function() {
  data.frame(
    element = c(
      NA,
      "a[data-value='Datasets']",
      "a[data-value='Arrange Data']",
      "a[data-value='Feedback']",
      "a[data-value='User']",
      "a[data-value='retake_tour']",
      "a[data-value='View Data']",
      "a[data-value='FAQs']",
      "a[data-value='Analysis']",
      "#subtabs",
      "a[data-value='Analysis Results']",
      "#sidebar",
      "#gre_mainpanel",
      "a[data-value='Trend']",
      "a[data-value='MannKendall']",
      "a[data-value='Instability']",
      "a[data-value='Decomposition']",
      "a[data-value='Interpretation']",
      "a[data-value='Report']",
      "#rcbd_logout"
    ),
    intro = c(
      # Welcome
      paste0(
        "<div style='text-align:center;'>",
        "<img src='guide1.gif'",
        "style='height:300px; object-fit:contain; margin-bottom:2px; transform:translateX(-21%);'>",
        "<h3 style='color:#6f2c91; margin:0; padding:0; line-height:1.2;'>",
        "Welcome to RAISINS Growth Rate Estimation!",
        "</h3>",
        "<p style='font-size:14px;color:#555;'>",
        "This quick tour will walk you through every section of the app.<br>",
        "Use <b>Next &#9654;</b> / <b>&#9664; Back</b> to navigate, or press <b>Esc</b> to skip.",
        "</p></div>"
      ),
      # Datasets
      paste0(
        "<b>&#128193; Datasets</b><br><br>",
        "This tab contains <b>3 model datasets</b> of coconut area, production and yield.<br><br>",
        "You can <b>download these files</b> to see how a yearly time series should be ",
        "<b>structured and arranged</b> for analysis.<br><br>",
        "Use them as a <b>reference template</b> and organize your own data ",
        "<b>in the same format</b> before uploading it."
      ),
      # Arrange Data
      paste0(
        "<b>&#9997; Arrange Data</b><br><br>",
        "This tab allows you to <b>create a dataset template directly in RAISINS</b>.<br><br>",
        "You can then <b>enter values manually</b> or <b>paste data from a spreadsheet</b>, ",
        "and <b>download it as a CSV file</b> that can be uploaded for analysis."
      ),
      # Feedback
      paste0(
        "<b>&#128172; Feedback</b><br><br>",
        "Use this tab to <b>share your experience</b> or <b>request a feature</b>.<br><br>",
        "Choose a category, enter your <b>email</b>, a <b>subject</b> and a short description, then click ",
        "<b>Send to RAISINS team</b>."
      ),
      # User Panel
      paste0(
        "<b>&#128100; User Panel</b><br><br>",
        "This section displays your <b>RAISINS account and subscription details</b>.<br><br>",
        "You can view your <b>billing information, plan, order ID, payment method</b>, ",
        "and <b>platform usage statistics</b>, and <b>download your invoice</b>."
      ),
      # Quick Tour
      paste0(
        "<b>&#129517; Quick Tour</b><br><br>",
        "Click <b>Quick Tour</b> at any time to <b>replay this guided tour</b> ",
        "of the app's sections."
      ),
      # View Data
      paste0(
        "<b>&#128065;&#65039; View Data Tab</b><br><br>",
        "Use this tab to <b>inspect the uploaded dataset</b> before analysis.<br><br>",
        "Check that the <b>year column</b> and every <b>numeric series</b> were read correctly."
      ),
      # FAQs
      paste0(
        "<b>&#10067; FAQs Tab</b><br><br>",
        "Answers to <b>frequently asked questions</b>: how to <b>prepare the file</b>, ",
        "<b>CGR vs CAGR</b>, and how the <b>trend test, instability and decomposition</b> work."
      ),
      # Analysis
      paste0(
        "<b>&#128202; Analysis Workspace</b><br><br>",
        "This is the <b>main analysis workspace</b> of the module.<br>",
        "Upload your yearly data, select the year and series columns, ",
        "and click on Run Analysis, that's it.<br><br>",
        "The results are organized across <b>dedicated subtabs</b> under this."
      ),
      # Subtabs
      paste0(
        "<b>&#128196; Analysis Subtabs</b><br><br>The analysis subtabs follow this order:<br>",
        "1. <b>Analysis Results</b><br>2. <b>Trend Fitting</b><br>3. <b>Trend Test</b><br>",
        "4. <b>Instability</b><br>5. <b>Decomposition</b><br>6. <b>Interpretation</b><br>",
        "7. <b>Statistical Report</b><br><br>View Data and FAQs are available in the main navigation."
      ),
      # Analysis Results
      paste0(
        "<b>&#128202; Analysis Results Tab</b><br><br>",
        "The <b>primary analysis workspace</b> of the module.<br><br>",
        "After you click <b>Run Analysis</b>, the <b>growth rates</b> of every series ",
        "(CGR, CAGR and average annual growth) appear here."
      ),
      # Sidebar
      paste0(
        "<b>&#9881;&#65039; Sidebar - Variable Setup</b><br><br>",
        "Use this panel to configure the analysis.<br><br>",
        "&#8226; Upload your dataset (CSV / Excel).<br>",
        "&#8226; Select the <b>Year</b> column and the <b>Series</b> to analyse.<br>",
        "&#8226; Optionally pick a <b>Group</b> column to sum districts or regions by year.<br>",
        "&#8226; Tick the <b>trend analyses</b> you need.<br>",
        "&#8226; Click <b>Run Analysis</b>."
      ),
      # Results panel
      paste0(
        "<b>&#128203; Results Panel</b><br><br>",
        "Once the analysis is submitted, the <b>growth-rate table</b> is displayed here.<br><br>",
        "Use the controls above the table to change the ",
        "<b>level of significance, font and table style</b>."
      ),
      # Trend Fitting
      paste0(
        "<b>&#128200; Trend Fitting Tab</b><br><br>",
        "<b>Linear, quadratic, cubic and exponential</b> trends ranked by adjusted R&sup2;, ",
        "with a <b>fitted-trend plot</b> you can download."
      ),
      # Trend Test
      paste0(
        "<b>&#8599;&#65039; Trend Test Tab</b><br><br>",
        "The non-parametric <b>Mann-Kendall test</b> for a steady rise or fall, ",
        "with <b>Sen's slope</b> as the change per year."
      ),
      # Instability
      paste0(
        "<b>&#12336;&#65039; Instability Tab</b><br><br>",
        "<b>Coefficient of variation</b> and the <b>Cuddy-Della Valle Index</b>, ",
        "which removes the trend to show genuine year-to-year instability."
      ),
      # Decomposition
      paste0(
        "<b>&#129383; Decomposition Tab</b><br><br>",
        "Splits the change in <b>production</b> into <b>area</b>, <b>yield</b> and ",
        "<b>interaction</b> effects. Needs columns named like <i>Area</i> and <i>Production</i>."
      ),
      # Interpretation
      paste0(
        "<b>&#128172; Interpretation Tab</b><br><br>",
        "A <b>plain-language summary</b> of every analysis that was run."
      ),
      # Statistical Report
      paste0(
        "<b>&#128196; Statistical Report Tab</b><br><br>",
        "Select the <b>tables and sections</b> to include and download a ",
        "<b>Word, PDF or HTML</b> report."
      ),
      # Sign Out
      paste0(
        "<b>&#128682; Sign Out</b><br><br>",
        "Use the <b>Sign Out</b> button located at the top-right ",
        "to securely log out of the RAISINS platform at any time.<br><br>",
        "<span style='color:#6f2c91;font-weight:bold;'>",
        "You are now ready to begin your analysis. Happy analysing! &#127881;</span>"
      )
    ),
    stringsAsFactors = FALSE
  )
}

gre_tour_end_modal <- function(button_id, button_label, body) {
  modalDialog(
    title = NULL,
    footer = tagList(
      tags$style(HTML("
.btn-custom-violet {
  background-color: #6610f2;
  color: white !important;
  border-color: #6f2c91 !important;
  transition: transform 0.2s ease-in-out !important;
}
.btn-custom-violet:hover {
  background-color: #5a2375 !important;
  border-color: #4a1d60 !important;
  transform: scale(1.05) !important;
}")),
      actionButton(inputId = button_id, label = HTML(button_label), class = "btn-custom-violet")
    ),
    easyClose = TRUE,
    tags$div(style = "text-align:center; padding:20px 10px; font-family:sans-serif;", body)
  )
}

gre_tour_server <- function(input, session, current_subscriber) {
  tour_shown <- reactiveVal(FALSE)
  run_tour <- function() {
    shinyjs::runjs("window.raisinTourCompleted = false;")
    raisins_run_tour(session, steps = gre_tour_steps())
  }

  # Show the welcome choice box on login
  observeEvent(current_subscriber(), {
    req(current_subscriber())
    if (tour_shown()) return()
    tour_shown(TRUE)
    if (raisins_auto_logged_in(session) && !raisins_website_sso_requested(session)) return() # returning user, no tour box

    user_name <- as.character(current_subscriber())
    user_name <- paste0(toupper(substr(user_name, 1, 1)), substr(user_name, 2, nchar(user_name)))
    user_name <- ifelse(grepl("@", user_name), sub("@.*$", "", user_name), user_name)

    shinyjs::delay(600, {
      shinyjs::runjs(sprintf("
    var el = document.getElementById('welcome_modal_overlay');
    var nameEl = document.getElementById('welcome_user_name');
    if (nameEl) nameEl.innerHTML = 'Hello, %s! &#128075;';
    el.style.display = 'flex';
  ", user_name))
    })
  })

  # User chose: Take the tour
  observeEvent(input$welcome_take_tour, {
    shinyjs::runjs("document.getElementById('welcome_modal_overlay').style.display = 'none';")
    shinyjs::delay(300, run_tour())
  })

  # User chose: Skip tour, go to analysis
  observeEvent(input$welcome_skip_tour, {
    shinyjs::runjs("document.getElementById('welcome_modal_overlay').style.display = 'none';")
    updateTabsetPanel(session, "subtabs", selected = "Analysis Results")
  })

  # Retake tour button
  observeEvent(input$retake_tour, {
    run_tour()
  })

  observeEvent(input$tour_completed, {
    showModal(gre_tour_end_modal("complete_tour", "&#9654; Let's Go", tagList(
      tags$div(style = "font-size:52px; margin-bottom:12px;", "\U0001F389"),
      tags$h3("Tour Complete!", style = "color:#6f2c91; margin-bottom:8px;"),
      tags$p("You're all set to use", tags$b("RAISINS Growth Rate Estimation."),
             style = "color:#555; font-size:14px; margin-bottom:6px;"),
      tags$p("Happy analysing! \U0001F31F", style = "color:#6f2c91; font-size:15px; font-weight:bold;"),
      tags$hr(style = "border-color:#e8d5f5; margin:16px 0;"),
      tags$p("\U0001F4A1 Tip: You can retake this tour anytime using the",
             tags$b("Quick Tour"), "tab.", style = "color:#888; font-size:12px;")
    )))
  })
  observeEvent(input$complete_tour, { removeModal() })

  observeEvent(input$tour_skipped, {
    showModal(gre_tour_end_modal("continue_app", "&#9654; Continue to App", tagList(
      tags$div(style = "font-size:48px; margin-bottom:12px;", HTML("&#128075;")),
      tags$h3("Tour Skipped", style = "color:#6f2c91; margin-bottom:8px;"),
      tags$p("No worries! You can explore", tags$b("RAISINS Growth Rate Estimation"), "on your own.",
             style = "color:#555; font-size:14px; margin-bottom:6px;"),
      tags$p("If you need guidance later, you can restart the tour anytime.",
             style = "color:#555; font-size:14px;"),
      tags$hr(style = "border-color:#e8d5f5; margin:16px 0;"),
      tags$p(HTML("&#128161;"), " Tip: Click the", tags$b("Quick Tour"),
             "tab to start the guided walkthrough again.", style = "color:#888; font-size:12px;")
    )))
  })
  observeEvent(input$continue_app, { removeModal() })

  # The "Quick Tour" navbar tab replays the tour from the Analysis tab.
  observeEvent(input$tabs, {
    if (raisins_signed_out(session)) return()   # signed out by another login
    if (input$tabs == "retake_tour") {
      updateTabsetPanel(session, inputId = "tabs", selected = "Analysis")
      updateTabsetPanel(session, inputId = "subtabs", selected = "Analysis Results")
      run_tour()
    }
  })
}
