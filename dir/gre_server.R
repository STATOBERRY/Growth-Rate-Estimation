## ============================================================================
##  RAISINS Growth Rate Estimation — Analysis engine  (gre_server)
##  Base-R statistics (no extra package installs). Plugs into greyskull shell.
##  Signature matches the shell:  gre_server(input, output, session, shared)
## ============================================================================

## ---- statistical helpers (base R) ------------------------------------------

# Growth rates use the observed series in chronological order.
gre_cgr <- function(year, y, alpha = 0.05) {
  keep <- is.finite(y)
  year <- year[keep]; y <- y[keep]
  result <- list(rate = NA_real_, p = NA_real_, ci = rep(NA_real_, 2), residuals = NULL,
                 lag1 = NA_real_, pairs = 0L)
  if (length(y) < 3 || any(y <= 0)) return(result)
  t <- year - min(year)
  fit <- stats::lm(log(y) ~ t)
  errors <- stats::residuals(fit)
  result$rate <- 100 * expm1(stats::coef(fit)[["t"]])
  result$residuals <- data.frame(Year = year, Residual = unname(errors))
  if (sqrt(mean(errors^2)) <= sqrt(.Machine$double.eps) * stats::sd(log(y))) return(result)
  result$p <- summary(fit)$coefficients["t", "Pr(>|t|)"]
  result$ci <- 100 * expm1(stats::confint(fit, "t", level = 1 - alpha))
  adjacent <- diff(year) == 1
  result$pairs <- sum(adjacent)
  previous <- head(errors, -1)[adjacent]
  current <- tail(errors, -1)[adjacent]
  if (result$pairs >= 3 && stats::sd(previous) > 0 && stats::sd(current) > 0)
    result$lag1 <- stats::cor(previous, current)
  result
}

gre_cagr <- function(year, y) {
  keep <- is.finite(y)
  year <- year[keep]; y <- y[keep]
  n <- length(y)
  if (n < 2 || y[1] <= 0 || y[n] <= 0) return(NA_real_)
  100 * expm1((log(y[n]) - log(y[1])) / (year[n] - year[1]))
}

gre_annual_growth <- function(year, y) {
  if (length(y) < 2) return(NA_real_)
  previous <- head(y, -1)
  current <- tail(y, -1)
  use <- diff(year) == 1 & is.finite(previous) & is.finite(current) &
    previous > 0 & current >= 0
  if (!any(use)) return(NA_real_)
  100 * mean((current[use] - previous[use]) / previous[use])
}

# Compare Gaussian and lognormal likelihoods on the observed response scale.
gre_trend_fit <- function(year, y) {
  keep <- is.finite(y)
  year <- year[keep]; y <- y[keep]
  n <- length(y)
  if (n < 3 || all(y == y[1])) return(NULL)
  t <- year - min(year)
  models <- list(
    Linear = stats::lm(y ~ t),
    Quadratic = if (n > 3) stats::lm(y ~ t + I(t^2)),
    Cubic = if (n > 4) stats::lm(y ~ t + I(t^2) + I(t^3)),
    Exponential = if (all(y > 0)) stats::lm(log(y) ~ t)
  )
  models <- Filter(Negate(is.null), models)
  rows <- lapply(names(models), function(name) {
    model <- models[[name]]
    ll <- stats::logLik(model)
    k <- attr(ll, "df")
    aicc <- NA_real_
    response <- stats::model.response(stats::model.frame(model))
    residual_scale <- sqrt(mean(stats::residuals(model)^2))
    if (n > k + 1 && is.finite(ll) && residual_scale > sqrt(.Machine$double.eps) * stats::sd(response)) {
      aic <- -2 * as.numeric(ll) + 2 * k
      if (name == "Exponential") aic <- aic + 2 * sum(log(y))
      aicc <- aic + 2 * k * (k + 1) / (n - k - 1)
    }
    data.frame(Model = name, AICc = aicc)
  })
  tab <- do.call(rbind, rows)
  eligible <- which(is.finite(tab$AICc))
  best_name <- if (length(eligible)) tab$Model[eligible[which.min(tab$AICc[eligible])]] else NA_character_
  fitted <- NULL
  if (!is.na(best_name)) {
    model <- models[[best_name]]
    prediction <- stats::fitted(model)
    if (best_name == "Exponential") prediction <- exp(prediction + mean(stats::residuals(model)^2) / 2)
    fitted <- data.frame(year = year, y = y, fit = as.numeric(prediction))
  }
  list(table = tab, best = best_name, fitted = fitted)
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
  p <- if (n < 10L && all(ties == 1L))
    stats::cor.test(seq_len(n), y, method = "kendall", exact = TRUE)$p.value else
    2 * stats::pnorm(-abs(Z))
  pairs <- choose(n, 2)
  untied <- pairs - sum(choose(ties, 2))
  tau <- if (untied > 0) S / sqrt(pairs * untied) else NA_real_
  list(S = S, Z = Z, p = p, tau = tau, varS = varS, n = n)
}

# Theil-Sen slope with (1-alpha) confidence interval
gre_sens_slope <- function(year, y, alpha = 0.05) {
  ok <- is.finite(y) & is.finite(year)
  year <- year[ok]; y <- y[ok]
  n <- length(y)
  if (n < 3) return(NULL)
  slopes <- unlist(lapply(seq_len(n - 1L), function(i) {
    j <- seq.int(i + 1L, n)
    (y[j] - y[i]) / (year[j] - year[i])
  }), use.names = FALSE)
  slopes <- sort(slopes)
  mk <- gre_mann_kendall(y)
  Calpha <- stats::qnorm(1 - alpha / 2) * sqrt(mk$varS)
  N <- length(slopes)
  M1 <- ceiling((N - Calpha) / 2)
  M2 <- floor((N + Calpha) / 2) + 1L
  lo <- if (M1 >= 1 && M1 <= N) slopes[M1] else NA
  hi <- if (M2 >= 1 && M2 <= N) slopes[M2] else NA
  list(slope = stats::median(slopes), lower = lo, upper = hi)
}

# Coefficient of variation and Cuddy-Della Valle instability index (%)
gre_instability <- function(year, y) {
  ok <- is.finite(y) & is.finite(year)
  year <- year[ok]; y <- y[ok]
  if (length(y) < 3 || any(y < 0) || mean(y) <= 0) return(list(cv = NA_real_, cdvi = NA_real_))
  cv <- stats::sd(y) / mean(y) * 100
  if (cv == 0) return(list(cv = 0, cdvi = 0))
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
        else gre_read_csv(path),
        error = function(e) {
          error_support$report(e, context = "Data upload",
            public_message = "The file could not be read. Save it as CSV UTF-8 or Excel (.xlsx / .xls) and upload it again.")
          NULL
        })
    } else if (nzchar(input$demo_data_select %||% "")) {
      fp <- file.path("data", input$demo_data_select)
      if (file.exists(fp)) df <- gre_read_csv(fp)
      else error_support$report("The selected model dataset is unavailable. Choose another dataset.",
                                context = "Data upload")
    }
    if (!is.null(df)) {
      record <- isolate(error_support$current())
      if (!is.null(record) && identical(record$context, "Data upload")) error_support$clear()
    }
    df
  })

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
        selected = if (is.null(input$rcbd_file1) && identical(input$demo_data_select, "dataset2.csv")) "Year" else character(0),
        options = pickerOptions(container = "body", title = "Select year column",
                                liveSearch = TRUE, size = 6),
        width = "100%"
      ),
      class = "brand-picker1"
    )
  })
  output$gre_series_ui <- renderUI({
    df <- gre_raw(); if (is.null(df)) return(NULL)
    nc <- setdiff(series_cols(df), c(input$gre_year, input$gre_group))
    selected <- intersect(isolate(input$gre_series), nc)
    if (!length(selected) && is.null(input$rcbd_file1) && identical(input$demo_data_select, "dataset2.csv"))
      selected <- intersect(c("Area", "Production"), nc)
    tags$div(
      pickerInput(
        "gre_series",
        label = HTML("Series columns: <br><small style='font-size: 10px; color: gray;'>(You can select all the numeric series for analysis at once.)</small>"),
        choices = nc, selected = selected,
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
    group_cols <- setdiff(names(df), input$gre_year %||% "")
    previous <- isolate(input$gre_group) %||% ""
    detected <- gre_detect_group(df, input$gre_year %||% "")
    selected <- if (nzchar(detected)) detected else if (previous %in% group_cols) previous else ""
    tags$div(
      actionButton("gre_group_help", label = NULL, icon = icon("circle-question"),
                   class = "btn-link btn-xs", title = "How grouping works",
                   `aria-label` = "How grouping works", style = "float: right;"),
      pickerInput(
        "gre_group",
        label = "Group column (optional)",
        choices = c("No grouping" = "", group_cols),
        selected = selected,
        options = pickerOptions(container = "body", title = "No grouping (optional)", size = 6),
        width = "100%"
      ),
      class = "brand-picker3"
    )
  })

  observeEvent(input$gre_group_help, {
    showModal(modalDialog(
      title = "How grouping works",
      tags$p("Leave Group column blank if your file has one row per year. If it has one row per group in each year, select the column containing the group names or IDs."),
      tags$table(class = "table table-bordered table-condensed",
        tags$caption("Model dataset with one row per group per year"),
        tags$thead(tags$tr(
          tags$th(scope = "col", "Year"),
          tags$th(scope = "col", class = "info", "Group"),
          tags$th(scope = "col", "Count")
        )),
        tags$tbody(
          tags$tr(tags$td("2023"), tags$td(class = "info", "A"), tags$td("30")),
          tags$tr(tags$td("2023"), tags$td(class = "info", "B"), tags$td("50")),
          tags$tr(tags$td("2024"), tags$td(class = "info", "A"), tags$td("40")),
          tags$tr(tags$td("2024"), tags$td(class = "info", "B"), tags$td("60"))
        )
      ),
      tags$p("Select ", tags$strong("Year"), " as the year column, ", tags$strong("Group"),
             " as the group column, and ", tags$strong("Count"), " as the series. ",
             "The combined counts are 80 for 2023 and 100 for 2024."),
      tags$p("Add counts or amounts that represent distinct groups. For rates, percentages, or averages, supply an annual series calculated from the underlying totals or with the appropriate weights."),
      easyClose = TRUE, footer = modalButton("Close")
    ))
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
                    group_col = input$gre_group %||% "")
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

  data_filt <- reactive({
    df <- gre_raw()
    req(df)
    gre_prepare_data(df, input$gre_year, gre_checks()$valid, input$gre_group %||% "")
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
    trend_fits <- residuals <- list()

    for (s in series) {
      y <- suppressWarnings(as.numeric(df[[s]]))

      if ("growth" %in% mods && any(is.finite(y))) {
        cg <- gre_cgr(year, y, alpha)
        growth <- rbind(growth, gre_growth_row(s, year, y, cg))
        if (!is.null(cg$residuals)) residuals[[s]] <- cg$residuals
      }
      if ("trend" %in% mods) {
        tf <- gre_trend_fit(year, y)
        if (!is.null(tf)) {
          if (!is.null(tf$fitted)) trend_fits[[s]] <- tf
          trend <- rbind(trend, cbind(Series = s, tf$table,
                                      Best = ifelse(tf$table$Model %in% tf$best, "Yes", "")))
        }
      }
      if ("mk" %in% mods) {
        m <- gre_mann_kendall(y[is.finite(y)])
        ss <- gre_sens_slope(year, y, alpha)
        if (!is.null(m))
          mk <- rbind(mk, data.frame(
            Series = s,
            `Kendall tau` = m$tau,
            `S` = m$S,
            `Z` = m$Z,
            `p-value` = m$p,
            `Trend` = ifelse(m$p < alpha,
                             ifelse(m$S > 0, "Increasing", "Decreasing"),
                             "No significant trend"),
            `Sen slope/yr` = if (!is.null(ss)) ss$slope else NA,
            `Sen CI low`  = if (!is.null(ss)) ss$lower else NA,
            `Sen CI high` = if (!is.null(ss)) ss$upper else NA,
            check.names = FALSE))
      }
      if ("instab" %in% mods) {
        ii <- gre_instability(year, y)
        instab <- rbind(instab, data.frame(
          Series = s,
          `CV (%)`   = ii$cv,
          `CDVI (%)` = ii$cdvi,
          check.names = FALSE))
      }
    }

    ## area-yield decomposition (auto-detect Area / Production [/ Yield])
    decomp <- NULL; decomp_msg <- NULL
    if ("decomp" %in% mods) {
      decomposition <- gre_decomposition(df, yc)
      decomp <- decomposition$table
      decomp_msg <- decomposition$message
    }

    # Plain-language write-up (gre_interpretation_text below), shown on the
    # Interpretation tab and used in the Statistical Report.
    interp <- gre_interpretation_text(list(year = year, series = series, growth = growth,
                                           trend = trend, trend_fits = trend_fits, mk = mk,
                                           instab = instab, decomp = decomp, alpha = alpha))

    group <- input$gre_group %||% ""
    panel <- NULL
    if ("growth" %in% mods && nzchar(group) && group %in% names(gre_raw())) {
      panel <- gre_panel_growth(gre_raw(), df, yc, group, series, alpha)
    }
    quality <- gre_quality_summary(gre_raw(), df, yc, group,
      input$gre_series %||% character(), gre_checks())
    list(raw = df, year_col = yc, growth = growth, trend = trend,
         trend_fits = trend_fits, mk = mk, instab = instab, residuals = residuals,
         group_comparison = panel$table, group_data = panel$data, quality = quality,
         decomp = decomp, decomp_msg = decomp_msg, interp = interp, alpha = alpha, series = series)
  })

  ## -- output renderers ------------------------------------------------------
  output$gre_table_css <- renderUI({
    css <- raisins_tstyle_css(input$gre_table_style, scope = c("#gre_mainpanel", ".gre-styled-results"))
    if (nzchar(css)) tags$style(HTML(css))
  })
  result_table <- function(d, key) {
    req(!is.null(d))
    cells <- gre_table_cells(d, input$gre_digits %||% 3L)
    table <- knitr::kable(cells, format = "html", row.names = FALSE, escape = TRUE,
                          align = ifelse(vapply(d, is.numeric, logical(1)), "r", "l"))
    font <- raisins_font_value(input$gre_font %||% "cambria")
    table <- kableExtra::kable_styling(table, bootstrap_options = "bordered",
                                       full_width = FALSE, position = "center")
    table <- kableExtra::kable_classic(table, full_width = FALSE, html_font = font)
    formulas <- gre_table_math(key)
    if (key %in% c("growth", "group")) formulas <- c(formulas, gre_table_math("diagnostics"))
    htmltools::attachDependencies(
      tagList(div(style = "overflow-x: auto;", HTML(as.character(table))),
              tags$p(class = "text-muted", tags$small(tags$strong("Note. "), gre_table_note(key, res()$alpha))),
              if (length(formulas))
                shiny::withMathJax(tags$div(tags$strong("Formulas used"),
                  lapply(formulas, function(eq) tags$div(style = "overflow-x: auto;", paste0("\\[", eq, "\\]")))))),
      kableExtra::html_dependency_lightable()
    )
  }

  output$gre_run_hint <- renderUI({
    if (!gre_submitted() && !is.null(gre_raw()))
      div(style = "color:#2E7D32;", "Set your columns and analyses, then click Run Analysis.")
  })
  # Drives RBD's "results will appear shortly" note until the run has finished.
  output$gre_results_ready <- reactive({
    isTRUE(gre_run_ready()) && !is.null(tryCatch(res(), error = function(e) NULL))
  })
  outputOptions(output, "gre_results_ready", suspendWhenHidden = FALSE)
  output$gre_growth_table <- renderUI({ req(res()$growth); result_table(res()$growth, "growth") })
  # Calculate with the run (not after the browser reports the panel visible),
  # so the calculator loader shows on this table while results are worked out.
  # Higher priority so it starts first: its "recalculating" signal (which opens
  # the GIF) then reaches the browser before the calculation, not after it.
  outputOptions(output, "gre_growth_table", suspendWhenHidden = FALSE, priority = 10)
  output$gre_trend_table <- renderUI({
    r <- res()
    if (is.null(r$trend)) return(tags$p("Run Trend fitting on a nonconstant series to compare models."))
    result_table(r$trend, "trend")
  })
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

  output$gre_trend_interactive <- plotly::renderPlotly({
    shiny::validate(need(length(res()$trend_fits), "Run Trend fitting on an eligible series to see this plot."))
    plotly::ggplotly(gre_trend_plot_obj())
  })
  output$gre_diagnostic_series <- renderUI({
    choices <- names(res()$residuals)
    if (!length(choices)) return(tags$p("Run Growth rates on a positive series to see diagnostics."))
    selectInput("gre_diag_series", "Series", choices, selected = isolate(input$gre_diag_series))
  })
  diagnostic_data <- reactive({
    series <- res()$residuals
    req(length(series))
    selected <- input$gre_diag_series %||% names(series)[1]
    if (selected %in% names(series)) series[[selected]] else series[[1]]
  })
  output$gre_residual_plot <- plotly::renderPlotly({
    d <- diagnostic_data()
    plotly::ggplotly(gre_chart(d, ggplot2::aes(Year, Residual),
                              "Residuals over time", "Year", "Log-scale residual") +
      ggplot2::geom_hline(yintercept = 0, colour = "#83949b", linetype = 2) +
      ggplot2::geom_line(colour = "#16655f") +
      ggplot2::geom_point(colour = "#16655f", size = 2.8))
  })
  output$gre_lag_plot <- plotly::renderPlotly({
    d <- diagnostic_data()
    adjacent <- diff(d$Year) == 1
    lagged <- data.frame(Previous = head(d$Residual, -1)[adjacent],
                         Current = tail(d$Residual, -1)[adjacent])
    shiny::validate(need(nrow(lagged) >= 3, "At least three adjacent-year pairs are needed."))
    plotly::ggplotly(gre_chart(lagged, ggplot2::aes(Previous, Current),
                              "One-year residual lag", "Previous year", "Current year") +
      ggplot2::geom_hline(yintercept = 0, colour = "#a9b6bc", linetype = 2) +
      ggplot2::geom_vline(xintercept = 0, colour = "#a9b6bc", linetype = 2) +
      ggplot2::geom_point(colour = "#d47434", size = 3))
  })
  output$gre_group_comparison_table <- renderUI({
    r <- res()
    if (is.null(r$group_comparison)) return(tags$p("Choose a grouping column and run Growth rates."))
    result_table(r$group_comparison, "group")
  })
  output$gre_group_plot_series <- renderUI({
    d <- res()$group_comparison
    if (is.null(d)) return(tags$p("Choose a grouping column and run Growth rates to see group plots."))
    selectInput("gre_group_plot_variable", "Series", unique(d$Series),
                selected = isolate(input$gre_group_plot_variable))
  })
  group_plot_series <- reactive({
    r <- res()
    req(r$group_comparison)
    choices <- unique(r$group_comparison$Series)
    selected <- input$gre_group_plot_variable %||% choices[1]
    if (selected %in% choices) selected else choices[1]
  })
  output$gre_group_forest <- plotly::renderPlotly({
    d <- res()$group_comparison
    d <- d[d$Series == group_plot_series() & is.finite(d$`CGR (%/yr)`), , drop = FALSE]
    shiny::validate(need(nrow(d), "No compound growth rate could be estimated for this series."))
    d$Group <- factor(d$Group, levels = rev(d$Group))
    ci <- d[is.finite(d$`CGR CI low`) & is.finite(d$`CGR CI high`), , drop = FALSE]
    plotly::ggplotly(gre_chart(d, ggplot2::aes(`CGR (%/yr)`, Group, colour = Scope),
                              "Compound growth by group", "CGR (% per year)", NULL) +
      ggplot2::geom_vline(xintercept = 0, colour = "#83949b", linetype = 2) +
      ggplot2::geom_segment(data = ci, ggplot2::aes(x = `CGR CI low`, xend = `CGR CI high`, yend = Group),
                            linewidth = 1) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::scale_colour_manual(values = c(Combined = "#d47434", Group = "#16655f")))
  })
  output$gre_group_paths <- plotly::renderPlotly({
    d <- res()$group_data
    d <- d[d$Series == group_plot_series() & d$Scope == "Group", , drop = FALSE]
    shiny::validate(need(any(is.finite(d$Value)), "No observations are available for this series."))
    plotly::ggplotly(gre_chart(d, ggplot2::aes(Year, Value, colour = Group),
                              "Group time series", "Year", group_plot_series()) +
      ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 2.1) +
      ggplot2::scale_colour_viridis_d(end = 0.85))
  })
  output$gre_quality_table <- renderUI(result_table(res()$quality, "quality"))
  output$gre_decomp_plot <- plotly::renderPlotly({
    d <- res()$decomp
    shiny::validate(need(d, res()$decomp_msg %||% "Run Area-Yield decomposition to see this plot."))
    plotly::plot_ly(d, x = ~Effect, y = ~Contribution, type = "waterfall",
      measure = c("relative", "relative", "relative", "total"),
      increasing = list(marker = list(color = "#16655f")),
      decreasing = list(marker = list(color = "#c35849")),
      totals = list(marker = list(color = "#254663"))) |>
      plotly::layout(title = "From area and yield to total production change",
                     yaxis = list(title = "Change in production units"), xaxis = list(title = ""))
  })

  output$gre_explore_options <- renderUI({
    r <- res()
    kind <- input$gre_explore_kind %||% "indexed"
    if (kind == "indexed") {
      years <- gre_index_years(r)
      if (!length(years)) return(tags$p("There is no common year with positive values for all selected series."))
      return(selectInput("gre_explore_base", "Base year (index = 100)", years,
                         selected = isolate(input$gre_explore_base)))
    }
    if (kind == "scatter" && length(r$series) >= 2L) {
      return(tagList(tags$p("Each point represents a year. Shared time trends can produce apparent associations."),
                     selectInput("gre_explore_x", "Horizontal series", r$series,
                                 selected = isolate(input$gre_explore_x) %||% r$series[1]),
                     selectInput("gre_explore_y", "Vertical series", r$series,
                                 selected = isolate(input$gre_explore_y) %||% r$series[2])))
    }
    if (kind == "heatmap") tags$p("Grey cells mark unavailable changes. Percentage changes use adjacent calendar years only.")
  })
  explore_plot <- reactive(gre_explore_plot(res(), input))
  output$gre_explore_plot <- plotly::renderPlotly(plotly::ggplotly(explore_plot()))

  publication_series <- reactive({
    r <- res()
    switch(input$gre_pub_kind %||% "trend",
      trend = names(r$trend_fits),
      growth = unique(r$group_comparison$Series),
      character())
  })
  output$gre_pub_series_ui <- renderUI({
    choices <- publication_series()
    if (length(choices)) selectInput("gre_pub_series", "Series", choices,
      selected = isolate(input$gre_pub_series))
  })
  publication_plot <- reactive({
    choices <- publication_series()
    series <- input$gre_pub_series %||% ""
    if (length(choices) && !series %in% choices) series <- choices[1]
    gre_publication_plot(res(), input$gre_pub_kind %||% "trend", series, input)
  })
  output$gre_pub_plot <- renderPlot(publication_plot())
  output$gre_pub_download_ui <- renderUI({
    req(publication_plot())
    gre_publication_export_ui(input)
  })
  output$gre_pub_download <- downloadHandler(
    filename = function() paste0("GRE_", input$gre_pub_kind %||% "trend", ".",
                                 input$gre_pub_format %||% "pdf"),
    content = function(file) {
      p <- publication_plot()
      d <- gre_trend_dimensions(list(
        gre_trend_width = input$gre_pub_width %||% 7,
        gre_trend_height = input$gre_pub_height %||% 4.5,
        gre_trend_dpi = input$gre_pub_dpi %||% 600, gre_trend_format = input$gre_pub_format %||% "pdf"))
      wrap <- function(text) {
        if (is.null(text)) return(NULL)
        width <- max(20L, floor(d$width * 11))
        lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
        paste(unlist(lapply(lines, strwrap, width = width)), collapse = "\n")
      }
      p <- p + ggplot2::labs(title = wrap(p$labels$title), caption = wrap(p$labels$caption))
      device <- gre_open_device(file, d$format, d)
      on.exit(grDevices::dev.off(device), add = TRUE)
      print(p)
    }
  )

  output$gre_mk_intro <- renderUI(HTML(
    "<p>The <b>Mann-Kendall</b> test checks for a monotonic trend. <b>Sen's slope</b> estimates change per year. Inference assumes independent observations. For fewer than 10 observations without ties the p-value is exact. The approximate p-value and slope interval can be unreliable for short series or many ties.</p>"))
  output$gre_mk_table <- renderUI({
    r <- res()
    if (is.null(r$mk)) return(tags$p("Select Mann-Kendall and Sen's slope, then run the analysis."))
    result_table(r$mk, "mk")
  })

  output$gre_instab_table <- renderUI({
    r <- res()
    if (is.null(r$instab)) return(tags$p("Run Instability to see CV and CDVI."))
    result_table(r$instab, "instab")
  })
  output$gre_decomp_table <- renderUI({
    r <- res()
    notice <- function(message) div(class = "well well-sm text-muted", role = "status", message)
    if (is.null(r$decomp))
      return(notice(r$decomp_msg %||% "Select Area-Yield decomposition and run the analysis. Your file needs numeric Area and Production columns."))
    tagList(result_table(r$decomp, "decomp"), if (!is.null(r$decomp_msg)) notice(r$decomp_msg))
  })

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
                         decomp = "Area-Yield decomposition", group_comparison = "Group and combined growth")
  gre_report_notes  <- c(growth = "CGR, CAGR and average annual growth for each series",
                         trend  = "Linear, quadratic, cubic and exponential fits compared by AICc on the original scale",
                         mk     = "Kendall's τ, p-value and Sen's slope with its confidence interval",
                         instab = "Coefficient of variation and Cuddy-Della Valle Index",
                         decomp = "Area, yield and interaction effects on production change",
                         group_comparison = "Growth estimates, confidence intervals and derived yield by group and for the combined series")
  gre_report_option <- function(label, description)
    tagList(span(class = "stat-report-option-title", label),
            span(class = "stat-report-option-description", description))

  output$gre_report_body <- renderUI({
    req(gre_submitted())
    r <- res(); req(r)
    avail <- names(gre_report_tables)[vapply(names(gre_report_tables),
                                             function(k) !is.null(r[[k]]), logical(1))]
    sections <- c(interp = "Interpretation", plot = "Fitted-trend figure",
                  publication = "Current publication figure", exploration = "Current exploratory chart")
    section_notes <- c(interp = "Plain-language summary of every analysis that was run",
                       plot = "Observed values with the best-fit trend for each series",
                       publication = "Uses the current selections in Plots & Graphs → Publication Figures",
                       exploration = "Uses the current selections in Plots & Graphs → Explore Data")
    if (!length(r$trend_fits)) sections <- sections[names(sections) != "plot"]
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
            selected = if (is.null(prev_sections)) intersect(c("interp", "plot"), names(sections)) else intersect(prev_sections, names(sections)),
            width = "100%"))),
      div(class = "stat-report-export",
        radioButtons("gre_report_format", "Document format", c("HTML", "PDF", "Word", "Excel"),
                     inline = TRUE, selected = isolate(input$gre_report_format) %||% "HTML"),
        uiOutput("gre_report_download_ui"))
    )
  })

  output$gre_report_download_ui <- renderUI({

    downloadButton("gre_download_report", "Download statistical report", class = "btn-success")
  })

  export_report <- function(file, format, title, keep, secs, tab = NULL) {
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
    for (k in setdiff(names(gre_report_tables), keep)) r[k] <- list(NULL)
    findings <- gre_report_findings(r, input$gre_digits %||% 3L)
    if (identical(tab, "interpretation")) {
      r$interp <- res()$interp
    } else if ("interp" %in% secs && length(intersect(keep, names(gre_report_tables)))) {
      r$interp <- gre_interpretation_text(c(r[setdiff(names(r), "trend_fits")], list(
        year = r$raw[[r$year_col]], trend_fits = if ("trend" %in% keep) r$trend_fits else list())))
    } else r$interp <- NULL
    font  <- raisins_font_value(input$gre_font %||% "cambria")
    style <- raisins_tstyle_value(input$gre_table_style)
    # As in RBD: PDF via xelatex with the chosen font's twin (www/fonts);
    # Word gets font and table style applied to the .docx after rendering.
    header <- input$gre_header_mode %||% "default"
    logo_alignment <- match.arg(input$gre_logo_alignment %||% "center", c("left", "center", "right"))
    td <- tempfile("gre-report-")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    logo <- if (format != "Excel" && header == "custom") tryCatch(
      gre_report_logo(input$gre_header_logo, td),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        stop(e)
      })
    source_default <- if (!is.null(input$rcbd_file1)) input$rcbd_file1$name else
      paste("Bundled example", input$demo_data_select %||% "")
    metadata <- data.frame(Item = c("Data source", "Units", "Known changes in coverage or definitions"),
      Detail = c(gre_report_text(input$gre_report_source, source_default),
                 gre_report_text(input$gre_report_units, "Not supplied"),
                 gre_report_text(input$gre_report_changes, "Not supplied")))
    quality <- rbind(metadata, r$quality)
    fmt <- switch(format,
                  HTML = rmarkdown::html_document(df_print = "kable"),
                  Word = rmarkdown::word_document(reference_docx = if (header == "default" && file.exists("styles.docx"))
                    normalizePath("styles.docx", winslash = "/") else "default"),
                  PDF  = rmarkdown::pdf_document(
                    latex_engine = "xelatex",
                    pandoc_args  = raisins_font_pdf_args(font, normalizePath("www/fonts", winslash = "/"))))
    # The report uses the trend plot exactly as customised in its workspace.
    plot_path <- NA
    if ("plot" %in% secs && length(r$trend_fits)) {
      plot_path <- file.path(td, "gre_trend.png")
      ggplot2::ggsave(plot_path, gre_trend_plot_obj(), width = 8, height = 5, dpi = 150, bg = "white")
    }
    extra_figures <- list()
    for (key in intersect(c("publication", "exploration"), secs)) {
      plot <- tryCatch(if (key == "publication") publication_plot() else explore_plot(),
                       shiny.silent.error = function(e) NULL)
      if (is.null(plot)) {
        showNotification(paste("The", key, "figure was unavailable and was omitted."), type = "warning")
        next
      }
      path <- file.path(td, paste0(key, ".png"))
      ggplot2::ggsave(path, plot, width = 8, height = 5, dpi = 200, bg = "white")
      extra_figures[[key]] <- path
    }
    if (format == "Excel") {
      tables <- r[intersect(keep, names(gre_report_tables))]
      tables <- Filter(Negate(is.null), tables)
      if (identical(tab, "plots")) tables$annual_data <- r$raw[c(r$year_col, r$series)]
      if (!is.null(r$interp))
        tables$interpretation <- data.frame(Interpretation = strsplit(r$interp, "\n\n", fixed = TRUE)[[1]])
      gre_export_xlsx(file, tables, quality, c(trend = plot_path, unlist(extra_figures)),
                     style, as.integer(input$gre_digits %||% 3L), r$alpha, title)
      return(invisible(NULL))
    }
    file.copy("growth_rate_report.Rmd", file.path(td, "growth_rate_report.Rmd"), overwrite = TRUE)
    out <- rmarkdown::render(
      input = file.path(td, "growth_rate_report.Rmd"),
      output_format = fmt,
      params = list(
        report_title = title, table_style = style,
        font        = font,
        digits      = as.integer(input$gre_digits %||% 3L),
        growth = r$growth, trend = r$trend, mk = r$mk,
        instab = r$instab, decomp = r$decomp, group_comparison = r$group_comparison, quality = quality,
        header_text = switch(header, custom = gre_report_text(input$gre_header_text, ""),
                             default = if (format != "Word") "Statoberry" else "",
                             ""),
        logo_path = logo, logo_alignment = logo_alignment,
        interp = r$interp, plot_path = plot_path, extra_figures = extra_figures,
        findings = findings, alpha = r$alpha),
      envir = new.env(parent = globalenv()), quiet = TRUE)
    if (identical(format, "Word")) {
      raisins_font_docx(out, font)
      raisins_tstyle_docx(out, style)
      if (!is.null(logo)) gre_align_word_logo(out, td, logo_alignment)
    }
    if (!file.copy(out, file, overwrite = TRUE)) stop("Could not write the report download.")
  }

  extension <- function(format) switch(format, PDF = "pdf", Word = "docx", Excel = "xlsx", "html")
  output$gre_download_report <- downloadHandler(
    filename = function() paste0("RAISINS_GRE_report_", Sys.Date(), ".",
                                 extension(input$gre_report_format %||% "HTML")),
    content = function(file) export_report(file, input$gre_report_format %||% "HTML",
      gre_report_text(input$gre_report_title, "Growth Rate and Trend Analysis Report"), input$gre_report_tables %||% character(),
      input$gre_report_sections %||% character())
  )

  tab_exports <- list(
    growth = list(title = "Growth Rates", tables = "growth"),
    trend = list(title = "Trend", tables = c("trend", "mk")),
    instability = list(title = "Instability and Decomposition", tables = c("instab", "decomp")),
    groups = list(title = "Group Comparison", tables = "group_comparison"),
    plots = list(title = "Plots and Graphs", sections = c("plot", "publication", "exploration")),
    quality = list(title = "Data Quality"),
    interpretation = list(title = "Interpretation", sections = "interp")
  )
  for (tab in names(tab_exports)) local({
    key <- tab
    spec <- tab_exports[[key]]
    format <- function() input[[paste0("gre_format_", key)]] %||% "HTML"
    output[[paste0("gre_download_", key)]] <- downloadHandler(
      filename = function() paste0("RAISINS_GRE_", key, "_", Sys.Date(), ".", extension(format())),
      content = function(file) export_report(file, format(), spec$title,
        spec$tables %||% character(), spec$sections %||% character(), tab = key))
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
gre_read_csv <- function(path) {
  data <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  names(data) <- sub("^\uFEFF", "", names(data))
  valid <- function(x) all(is.na(x) | validUTF8(x))
  if (!valid(names(data)) || !all(vapply(Filter(is.character, data), valid, logical(1))))
    stop("The file contains invalid UTF-8 text. Save it as CSV UTF-8 or upload an Excel copy.")
  data
}

gre_align_word_logo <- function(path, directory, alignment) {
  unpacked <- file.path(directory, "word-package")
  dir.create(unpacked)
  utils::unzip(path, exdir = unpacked)
  document <- file.path(unpacked, "word", "document.xml")
  xml <- xml2::read_xml(document)
  ns <- xml2::xml_ns(xml)
  paragraph <- xml2::xml_find_first(xml, "(//w:body/w:p[w:r/w:drawing])[1]", ns)
  if (inherits(paragraph, "xml_missing")) stop("Could not locate the report logo in the Word document.")
  properties <- xml2::xml_find_first(paragraph, "w:pPr", ns)
  if (inherits(properties, "xml_missing")) properties <- xml2::xml_add_child(paragraph, "w:pPr", .where = 0)
  justify <- xml2::xml_find_first(properties, "w:jc", ns)
  if (inherits(justify, "xml_missing")) justify <- xml2::xml_add_child(properties, "w:jc")
  xml2::xml_set_attr(justify, "w:val", alignment)
  xml2::write_xml(xml, document)
  updated <- file.path(directory, "aligned.docx")
  zip::zipr(updated, list.files(unpacked, all.files = TRUE, no.. = TRUE),
            root = unpacked, include_directories = FALSE)
  if (!file.copy(updated, path, overwrite = TRUE)) stop("Could not save the Word logo alignment.")
}

gre_report_text <- function(value, fallback) {
  if (is.null(value) || !nzchar(trimws(value))) return(fallback)
  substr(trimws(gsub("[[:cntrl:]]+", " ", value)), 1, 2000)
}

gre_report_logo <- function(upload, directory) {
  if (is.null(upload)) return(NULL)
  path <- upload$datapath
  size <- file.info(path)$size
  if (is.na(size) || size > 5 * 1024^2) stop("Use a PNG or JPEG logo no larger than 5 MB.")
  signature <- readBin(path, "raw", n = 8L)
  png <- identical(signature, as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  jpeg <- identical(head(signature, 3L), as.raw(c(255, 216, 255)))
  if (!png && !jpeg) stop("The logo file must contain a PNG or JPEG image.")
  image <- tryCatch(magick::image_read(path), error = function(e) NULL)
  if (is.null(image)) stop("The logo could not be read. Try a different PNG or JPEG.")
  logo <- file.path(directory, "report-logo.png")
  image <- magick::image_strip(magick::image_resize(image[1], "1200x600>"))
  magick::image_write(image, logo, format = "png")
  logo
}

gre_growth_row <- function(series, year, y, cg) {
  observed <- year[is.finite(y)]
  data.frame(
    Series = series, Observations = length(observed),
    `Start year` = if (length(observed)) min(observed) else NA_real_,
    `End year` = if (length(observed)) max(observed) else NA_real_,
    Mean = if (length(observed)) mean(y[is.finite(y)]) else NA_real_,
    `CGR (%/yr)` = cg$rate, `CGR CI low` = cg$ci[1], `CGR CI high` = cg$ci[2],
    `CGR p` = cg$p, `CAGR (%/yr)` = gre_cagr(year, y),
    `Annual avg (%/yr)` = gre_annual_growth(year, y),
    `Lag-1 residual r` = cg$lag1, `Lag pairs` = cg$pairs, check.names = FALSE)
}

gre_panel_growth <- function(raw, combined, year_col, group_col, series, alpha) {
  raw[[year_col]] <- as.numeric(as.character(raw[[year_col]]))
  raw <- raw[order(raw[[year_col]]), , drop = FALSE]
  groups <- split(raw, trimws(as.character(raw[[group_col]])))
  frames <- c(list(combined), unname(groups))
  area <- gre_measure_column(combined, year_col, "area")
  production <- gre_measure_column(combined, year_col, "prod")
  if (!is.na(area) && !is.na(production)) {
    yield_name <- make.unique(c(series, "Derived yield (production / area)"))[length(series) + 1L]
    frames <- lapply(frames, function(frame) {
      a <- frame[[area]]
      p <- frame[[production]]
      frame[[yield_name]] <- ifelse(is.finite(a) & a > 0 & is.finite(p) & p >= 0, p / a, NA_real_)
      frame
    })
    series <- c(series, yield_name)
  }
  group_names <- make.unique(c("Combined total", names(groups)))
  tables <- paths <- list()
  for (i in seq_along(frames)) {
    frame <- frames[[i]]
    year <- frame[[year_col]]
    scope <- if (i == 1L) "Combined" else "Group"
    for (name in series) {
      y <- frame[[name]]
      key <- length(tables) + 1L
      cg <- gre_cgr(year, y, alpha)
      tables[[key]] <- cbind(Group = group_names[i], Scope = scope,
                            gre_growth_row(name, year, y, cg))
      paths[[key]] <- data.frame(Group = group_names[i], Scope = scope,
                                Year = year, Series = name, Value = y)
    }
  }
  list(table = do.call(rbind, tables), data = do.call(rbind, paths))
}

gre_quality_summary <- function(raw, annual, year_col, group, requested, checks) {
  years <- annual[[year_col]]
  grouped <- nzchar(group) && group %in% names(raw)
  group_names <- if (grouped) sort(unique(trimws(as.character(raw[[group]])))) else character()
  series_notes <- vapply(requested, function(name) {
    if (!name %in% checks$valid) return("Excluded by data checks")
    y <- annual[[name]]
    paste(sum(is.finite(y)), "usable annual values;", sum(!is.finite(y)), "missing annual values;",
          sum(!is.finite(raw[[name]])), "missing input values;",
          sum(is.finite(y) & y <= 0), "nonpositive annual values")
  }, character(1))
  data.frame(
    Item = c("Year column", "Period", "Input rows", "Recorded years", "Unrecorded calendar years",
             "Grouping", "Groups covered", names(series_notes)),
    Detail = c(year_col, paste(min(years), max(years), sep = "–"), nrow(raw), length(years),
               max(years) - min(years) + 1 - length(years),
               if (grouped) group else "Single annual series",
               if (grouped) paste(length(group_names), "—", paste(group_names, collapse = ", ")) else "Not applicable",
               series_notes),
    row.names = NULL)
}

gre_chart <- function(data, mapping, title, x, y) {
  ggplot2::ggplot(data, mapping) +
    ggplot2::labs(title = title, x = x, y = y) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = "#23465e"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom", legend.title = ggplot2::element_blank())
}

gre_measure_column <- function(df, year_col, pattern) {
  hits <- setdiff(grep(pattern, names(df), value = TRUE, ignore.case = TRUE), year_col)
  hits <- hits[vapply(df[hits], is.numeric, logical(1)) & !gre_is_ratio(hits)]
  if (length(hits) == 1L) hits else NA_character_
}

gre_is_ratio <- function(name) {
  grepl("yield|rate|ratio|percent|%|per[ _/-]|productivity|price|index|average|mean|share",
        name, ignore.case = TRUE)
}

gre_prepare_data <- function(df, year_col, series, group_col = "") {
  df[[year_col]] <- as.numeric(as.character(df[[year_col]]))
  if (nzchar(group_col) && group_col %in% names(df)) {
    area_prod <- grep("area|prod", names(df), value = TRUE, ignore.case = TRUE)
    area_prod <- area_prod[vapply(df[area_prod], is.numeric, logical(1)) & !gre_is_ratio(area_prod)]
    cols <- setdiff(union(series, area_prod), c(year_col, group_col))
    df <- stats::aggregate(df[cols], by = setNames(list(df[[year_col]]), year_col),
                           FUN = function(x) if (all(is.finite(x))) sum(x) else NA_real_)
  }
  df[order(df[[year_col]]), , drop = FALSE]
}

gre_decomposition <- function(df, year_col) {
  area_col <- gre_measure_column(df, year_col, "area")
  prod_col <- gre_measure_column(df, year_col, "prod")
  if (is.na(area_col) || is.na(prod_col))
    return(list(message = "Decomposition needs one identifiable numeric Area column and one Production column."))
  endpoints <- c(1L, nrow(df))
  area <- df[[area_col]][endpoints]
  prod <- df[[prod_col]][endpoints]
  if (!all(is.finite(area) & area > 0 & is.finite(prod) & prod >= 0))
    return(list(message = "Both endpoints need positive area and nonnegative production, with no missing components."))
  yield <- prod / area
  change <- diff(prod)
  effects <- c(yield[1] * diff(area), area[1] * diff(yield), diff(area) * diff(yield), change)
  list(table = data.frame(
    Effect = c("Area effect", "Yield effect", "Interaction effect", "Total change"),
    Contribution = effects,
    `Share (%)` = if (change == 0) rep(NA_real_, 4) else 100 * effects / change,
    check.names = FALSE
  ), message = if (change == 0) "Percentage shares are undefined when total production change is zero.")
}

gre_table_cells <- function(d, digits) {
  digits <- as.integer(digits)
  for (nm in names(d)[vapply(d, is.numeric, logical(1))]) {
    x <- d[[nm]]
    d[[nm]] <- if (nm %in% c("CGR p", "p-value")) {
      p_digits <- max(3L, digits)
      threshold <- formatC(10^-p_digits, digits = p_digits, format = "f")
      ifelse(x < as.numeric(threshold), paste("<", threshold),
             formatC(x, digits = p_digits, format = "f"))
    } else {
      formatC(x, digits = if (nm %in% c("Start year", "End year", "S", "Observations", "Lag pairs")) 0L else digits, format = "f")
    }
    d[[nm]][!is.finite(x)] <- "—"
  }
  d
}

gre_detect_group <- function(df, year_col) {
  if (!year_col %in% names(df)) return("")
  year <- suppressWarnings(as.numeric(as.character(df[[year_col]])))
  if (any(!is.finite(year)) || !anyDuplicated(year)) return("")
  candidates <- setdiff(names(df), year_col)
  named <- grepl("(^|[^a-z])(group|district|region)([^a-z]|$)",
                 tolower(candidates))
  categorical <- vapply(df[candidates], function(x) is.character(x) || is.factor(x), logical(1))
  eligible <- candidates[named | categorical]
  valid <- eligible[vapply(eligible, function(name) {
    group <- trimws(as.character(df[[name]]))
    if (anyNA(group) || any(!nzchar(group))) return(FALSE)
    n_groups <- length(unique(group))
    n_groups > 1L && nrow(df) == length(unique(year)) * n_groups &&
      !anyDuplicated(data.frame(year, group))
  }, logical(1))]
  preferred <- intersect(valid, candidates[named])
  if (length(preferred) == 1L) return(preferred)
  if (length(valid) == 1L) valid else ""
}

gre_data_checks <- function(df, year_col, series, group_col = "") {
  series <- intersect(series, setdiff(names(df), c(year_col, group_col)))
  out <- list(issues = list(), valid = series, skipped = character(), blocked = FALSE)
  add <- function(title, entries, action, blocking = FALSE) {
    out$issues[[length(out$issues) + 1L]] <<- list(title = title, entries = entries, action = action)
    if (blocking) out$blocked <<- TRUE
  }
  if (is.null(df) || !length(year_col) || !year_col %in% names(df)) return(out)

  yr  <- df[[year_col]]
  num <- suppressWarnings(as.numeric(as.character(yr)))
  bad_year <- which(!is.finite(num) | num != trunc(num))
  if (length(bad_year)) num[bad_year] <- NA_real_
  if (length(bad_year))
    add("Missing or invalid years", paste0(year_col, ": data row ", bad_year, " = ", as.character(yr[bad_year])),
        "Every row needs a numeric year such as 2001. Please correct these rows in your original file.", TRUE)
  grouped <- nzchar(group_col) && group_col %in% setdiff(names(df), year_col)
  if (grouped && !length(bad_year)) {
    group <- trimws(as.character(df[[group_col]]))
    counts <- table(num, group)
    if (anyNA(group) || any(!nzchar(group)) || any(counts != 1L))
      add("Incomplete or duplicate group-year records", group_col,
          "Annual totals require exactly one row for every group in every year and no missing group labels.", TRUE)
  }
  if (!grouped) {
    dup <- unique(num[duplicated(num) & !is.na(num)])
    if (length(dup))
      add("Repeated years", paste("Year", dup),
          "Each year should appear once. If the file has one row per group, choose that column in the Group column picker.", TRUE)
  }
  if (length(unique(num[is.finite(num)])) < 3L)
    add("Too few years", paste(length(unique(num[is.finite(num)])), "year(s) found"),
        "Growth and trend analysis needs at least 3 years of data.", TRUE)

  rejected <- character()
  prints <- list()
  for (nm in intersect(series, setdiff(names(df), year_col))) {
    if (grouped && gre_is_ratio(nm)) {
      add("Non-additive series", nm,
          "Yield, rates and other ratios cannot be summed across groups. Supply a correctly weighted annual series instead.")
      rejected <- c(rejected, nm); next
    }
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
    usable_years <- if (grouped) sum(!vapply(split(probe, num), function(x) any(!is.finite(x)), logical(1))) else sum(is.finite(probe))
    if (usable_years < 3L) {
      add("Too few usable observations", nm, "At least three years with usable values are required for this series.")
      rejected <- c(rejected, nm); next
    }
    if (length(blank))
      add("Missing values", paste0(nm, ": year ", as.character(yr[blank])),
          "These years are omitted for this series. In grouped data a missing component makes the annual total unavailable.")
    nonpos <- which(is.finite(probe) & probe <= 0)
    if (length(nonpos))
      add("Zero or negative values", paste0(nm, ": year ", as.character(yr[nonpos]), " = ", probe[nonpos]),
          "CGR requires every observed value to be positive. CAGR requires positive endpoints. CV and CDVI require nonnegative values and a positive mean.")
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
    "Hi, please relate the explanation below with the tables in the 'Analysis Results', 'Trend' and 'Instability' tabs.",
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
        return(paste0(g$Series[i], ": the compound growth rate could not be estimated because it requires at least three observations and all observed values must be positive."))
      dir <- if (cgr > 0) "grew" else if (cgr < 0) "declined" else "remained unchanged"
      sig <- if (is.na(g$`CGR p`[i])) "inference unavailable" else if (g$`CGR p`[i] < alpha) "statistically significant" else "not statistically significant"
      paste0(g$Series[i], " ", dir, " at a compound growth rate (CGR) of ", signif(abs(cgr), 4), "% per year (", sig, ", ",
             gre_p_text(g$`CGR p`[i]), "). The endpoint CAGR was ", signif(g$`CAGR (%/yr)`[i], 4),
             "% per year and the average year-on-year change was ", signif(g$`Annual avg (%/yr)`[i], 4), "%.")
    }, character(1))
    paras <- c(paras, paste0(
      "Growth rates. The CGR is estimated from the log-linear model ln(y) = a + bt fitted to all years, so it reflects the whole period; ",
      "the CAGR uses only the first and last years and is therefore more sensitive to unusual end points. ",
      paste(lines, collapse = " ")))
  }

  # Trend fitting
  if (length(r$trend_fits)) {
    lines <- vapply(names(r$trend_fits), function(s) {
      best <- r$trend_fits[[s]]$best
      paste0("For ", s, " the ", tolower(best), " trend had the lowest AICc among eligible models.")
    }, character(1))
    paras <- c(paras, paste0(
      "Trend fitting. Eligible trend models were compared by AICc on the original response scale. ",
      paste(lines, collapse = " ")))
  }

  # Mann-Kendall + Sen's slope
  m <- r$mk
  if (!is.null(m) && nrow(m)) {
    lines <- vapply(seq_len(nrow(m)), function(i) {
      ci <- signif(c(m$`Sen CI low`[i], m$`Sen CI high`[i]), 4)
      trend <- if (m$Trend[i] == "No significant trend") "showed no significant monotonic trend" else
        paste0("showed a significant ", tolower(m$Trend[i]), " trend")
      paste0(m$Series[i], " ", trend, " (Kendall's τ = ", signif(m$`Kendall tau`[i], 4), ", ", gre_p_text(m$`p-value`[i]),
             "), with a Sen's slope of ", signif(m$`Sen slope/yr`[i], 4), " units per year",
             if (all(is.finite(ci))) paste0(" (confidence interval ", ci[1], " to ", ci[2], ").")
             else " (confidence interval unavailable).")
    }, character(1))
    paras <- c(paras, paste0(
      "Trend test. The non-parametric Mann-Kendall test checks for a steady rise or fall without assuming a straight line or normal errors, ",
      "and Sen's slope gives a robust estimate of change per year. The p-value and interval assume independent observations. ",
      paste(lines, collapse = " ")))
  }

  # Instability
  ins <- r$instab
  if (!is.null(ins) && nrow(ins)) {
    lines <- vapply(seq_len(nrow(ins)), function(i)
      paste0(ins$Series[i], " had a CV of ", signif(ins$`CV (%)`[i], 4), "% and a CDVI of ", signif(ins$`CDVI (%)`[i], 4), "%."), character(1))
    paras <- c(paras, paste0(
      "Instability. The coefficient of variation (CV) includes variation caused by the trend itself, whereas the ",
      "Cuddy-Della Valle Index (CDVI) adjusts CV for the fitted linear trend. ",
      paste(lines, collapse = " ")))
  }

  # Decomposition
  d <- r$decomp
  if (!is.null(d) && nrow(d)) {
    paras <- c(paras, paste0(
      "Area-yield decomposition. The first-to-last production change is the sum of the area, yield and interaction effects. ",
      "These are accounting contributions. Offsetting effects can produce negative shares or shares above 100%."))
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
      "a[data-value='Plots']",
      "a[data-value='Instability']",
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
        "1. <b>Analysis Results</b><br>2. <b>Trend</b><br>3. <b>Plots &amp; Graphs</b><br>",
        "4. <b>Instability &amp; Decomposition</b><br>5. <b>Interpretation</b><br>",
        "6. <b>Statistical Report</b><br><br>View Data and FAQs are available in the main navigation."
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
        "&#8226; Optionally pick a <b>Group</b> column to sum groups by year.<br>",
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
        "<b>&#128200; Trend Tab</b><br><br>",
        "<b>Linear, quadratic, cubic and exponential</b> trends compared by AICc, ",
        "alongside the Mann-Kendall trend test and Sen's slope."
      ),
      # Plots & Graphs
      paste0(
        "<b>Plots &amp; Graphs</b><br><br>",
        "Explore annual trajectories and change patterns, inspect diagnostics, and export publication figures."
      ),
      # Instability and decomposition
      paste0(
        "<b>Instability &amp; Decomposition</b><br><br>",
        "Compare the coefficient of variation and trend-adjusted instability, ",
        "then see how area, yield and their interaction contribute to the change in production."
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
gre_table_math <- function(key) {
  growth <- c(
    r"(\ln y_t = a + bt + \varepsilon_t, \qquad \mathrm{CGR}(\%)=100(e^b-1))",
    r"(\mathrm{CI}_{b}=b\pm t_{1-\alpha/2,n-2}\,\mathrm{SE}(b),\qquad t_{\mathrm{obs}}=\frac{b}{\mathrm{SE}(b)})",
    r"(\mathrm{CI}_{\mathrm{CGR}}=\left[100(e^{b_{\mathrm{lo}}}-1),\;100(e^{b_{\mathrm{hi}}}-1)\right])",
    r"(\mathrm{CAGR}(\%)=100\left[\left(\frac{y_{\mathrm{last}}}{y_{\mathrm{first}}}\right)^{1/(t_{\mathrm{last}}-t_{\mathrm{first}})}-1\right])",
    r"(\text{Average annual change }(\%)=\frac{100}{|\mathcal A|}\sum_{t\in\mathcal A}\frac{y_t-y_{t-1}}{y_{t-1}})",
    r"(\mathcal A=\{t:\;y_{t-1}>0,\;y_t\ge 0,\;\text{both adjacent years observed}\})"
  )
  switch(key,
    growth = growth,
    diagnostics = r"(r_1=\operatorname{cor}(e_{t-1},e_t)\quad\text{over adjacent calendar years})",
    trend = c(
      r"(\begin{aligned}\text{Linear: }&a+bt\\\text{Quadratic: }&a+bt+ct^2\\\text{Cubic: }&a+bt+ct^2+dt^3\\\text{Exponential: }&\ln y_t=a+bt+\varepsilon_t\end{aligned})",
      r"(\mathrm{AICc}=-2\ln L+2k+\frac{2k(k+1)}{n-k-1})",
      r"(\ln L_{\mathrm{original}}=\ln L_{\log}-\sum_t \ln y_t\quad\text{for the exponential fit})"
    ),
    mk = c(
      r"(S=\sum_{i<j}\operatorname{sgn}(y_j-y_i),\qquad \tau_b=\frac{S}{\sqrt{NU}})",
      r"(\operatorname{Var}(S)=\frac{n(n-1)(2n+5)-\sum_g m_g(m_g-1)(2m_g+5)}{18})",
      r"(Z=\begin{cases}\frac{S-\operatorname{sgn}(S)}{\sqrt{\operatorname{Var}(S)}}& S\ne0\\0&S=0\end{cases},\qquad p_{\mathrm{approx}}=2\Phi(-|Z|))",
      r"(\widehat\beta_{\mathrm{Sen}}=\operatorname{median}_{i<j}\left(\frac{y_j-y_i}{t_j-t_i}\right))",
      r"(C=z_{1-\alpha/2}\sqrt{\operatorname{Var}(S)},\qquad M_1=\left\lceil\frac{N-C}{2}\right\rceil,\quad M_2=\left\lfloor\frac{N+C}{2}\right\rfloor+1)"
    ),
    instab = r"(\mathrm{CV}(\%)=100\frac{s}{\bar y},\qquad \mathrm{CDVI}(\%)=\mathrm{CV}\sqrt{1-R^2_{\mathrm{adj,\ bounded}}})",
    decomp = c(r"(Y=\frac{P}{A},\qquad \Delta P=Y_0\Delta A+A_0\Delta Y+\Delta A\Delta Y)",
               r"(\text{Share}(\%)=100\frac{\text{contribution}}{\Delta P})"),
    group = c(r"(Y_{\mathrm{combined},t}=\frac{\sum_g P_{g,t}}{\sum_g A_{g,t}})", growth),
    character()
  )
}

gre_table_formulas <- function(key) {
  growth <- c("Log-linear model: ln(y) = a + b t + error, with t in years.",
              "CGR (%) = 100 [exp(b) - 1]. Transform both slope limits b +/- t* SE(b), with n - 2 degrees of freedom.",
              "CGR p-value = two-sided t-test of the slope, using t = b / SE(b).",
              "CAGR (%) = 100 [(last value / first value)^(1 / elapsed years) - 1].",
              "Average annual change (%) = mean of 100 (current - previous) / previous over eligible adjacent years.")
  switch(key,
    growth = growth,
    diagnostics = "Lag-1 residual r = Pearson correlation of residuals from adjacent calendar years.",
    trend = c("Linear: a + b t. Quadratic: a + b t + c t^2. Cubic: a + b t + c t^2 + d t^3. Exponential: ln(y) = a + b t + error.",
              "AICc = -2 log L + 2k + 2k(k + 1)/(n - k - 1), with n observations and k parameters including variance. For the exponential model, log L on the observed scale equals log L on the log scale minus sum[ln(y)]."),
    mk = c("S = sum of sign(y[j] - y[i]) over i < j. Tau-b = S / sqrt(N U), with N total pairs and U pairs untied in value.",
           "Var(S) = {n(n - 1)(2n + 5) - sum[m(m - 1)(2m + 5)]}/18, with m the size of each tie group.",
           "Z = [S - sign(S)] / sqrt(Var(S)) when S is nonzero, and 0 otherwise. Approximate two-sided p = 2 Phi(-abs(Z)). For fewer than 10 observations without ties, the exact test is used.",
           "Sen's slope = median of (y[j] - y[i]) / (year[j] - year[i]) over i < j.",
           "Sen interval: let C = z* sqrt(Var(S)), where z* is the two-sided normal critical value. Take the ordered slopes at ranks ceil((N - C)/2) and floor((N + C)/2) + 1 when those ranks exist."),
    instab = c("CV (%) = 100 x sample standard deviation / mean.",
               "CDVI (%) = CV x sqrt(1 - R2adj), where R2adj is the linear trend's adjusted R-squared bounded between 0 and 1."),
    decomp = c("Yield = production / area. Changes are end minus start.",
               "Area effect = starting yield x area change. Yield effect = starting area x yield change. Interaction = area change x yield change.",
               "Total production change = area effect + yield effect + interaction.",
               "Share (%) = 100 x contribution / total production change."),
    group = c("Combined yield = sum of production / sum of area within each year.", growth),
    character()
  )
}

gre_export_xlsx <- function(file, tables, quality, figures, style, digits, alpha, title) {
  wb <- openxlsx::createWorkbook(title = title)
  labels <- c(growth = "Growth rates", trend = "Trend models", mk = "Trend test",
              instab = "Instability", decomp = "Decomposition", group_comparison = "Group comparison",
              annual_data = "Annual data", interpretation = "Interpretation", quality = "Data quality")
  keys <- c(growth = "growth", trend = "trend", mk = "mk", instab = "instab",
            decomp = "decomp", group_comparison = "group", quality = "quality")
  tables$quality <- quality
  for (name in names(tables)) {
    d <- tables[[name]]
    sheet <- labels[[name]]
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, d)
    raisins_tstyle_xlsx(wb, sheet, nrow = nrow(d) + 1L, ncol = ncol(d), style = style)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    numbers <- which(vapply(d, is.numeric, logical(1)) &
      !names(d) %in% c("Start year", "End year", "Observations", "Lag pairs", "S", "CGR p", "p-value"))
    if (length(numbers) && nrow(d) && !identical(name, "annual_data")) {
      format <- if (digits > 0) paste0("0.", strrep("0", digits)) else "0"
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = format),
                         rows = seq_len(nrow(d)) + 1L, cols = numbers, gridExpand = TRUE, stack = TRUE)
    }
    if (name %in% names(keys)) {
      key <- keys[[name]]
      formulas <- gre_table_formulas(key)
      if (key %in% c("growth", "group")) formulas <- c(formulas, gre_table_formulas("diagnostics"))
      notes <- c(paste("Note.", gre_table_note(key, alpha)),
                 if (length(formulas)) c("Formulas used", formulas))
      openxlsx::writeData(wb, sheet, notes, startRow = nrow(d) + 3L, colNames = FALSE)
    }
    openxlsx::setColWidths(wb, sheet, seq_len(ncol(d)), widths = if (name == "interpretation") 100 else 19)
    if (name %in% c("quality", "interpretation")) {
      column <- if (name == "quality") 2L else 1L
      openxlsx::setColWidths(wb, sheet, column, widths = 95)
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                         rows = seq_len(nrow(d)) + 1L, cols = column, stack = TRUE)
    }
  }
  for (name in names(figures)[!is.na(figures)]) {
    sheet <- paste("Figure", name)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::insertImage(wb, sheet, figures[[name]], width = 8, height = 5)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

gre_table_note <- function(key, alpha) {
  level <- paste0(100 * (1 - alpha), "%")
  switch(key,
    growth = paste(level, "CGR intervals and p-values assume independent, constant-variance normal errors on the log scale. CAGR uses the observed endpoints. Average annual change uses adjacent calendar years."),
    diagnostics = "Lag-1 r is the descriptive correlation of log-scale residuals in adjacent calendar years. Lag pairs is the number of pairs available for that calculation.",
    trend = "Lower AICc is preferred among eligible fits on the original response scale. Here n is the number of observations and k counts all fitted parameters, including variance. A dash indicates an unavailable criterion.",
    mk = paste("Tau-b adjusts for ties. N counts all pairs, U counts untied pairs, and m is a tie-group size. The Sen interval uses the ordered slopes at the two stated ranks. Sen's slope is in series units per year. The", level,
               "interval and the p-value assume independent observations. P-values are unadjusted across series."),
    instab = "CV is the coefficient of variation. CDVI adjusts CV using the fitted linear trend. Both require nonnegative observations and a positive mean.",
    decomp = "Shares use total endpoint production change as the denominator and are undefined when it is zero. Offsetting contributions can yield negative shares or shares above 100%.",
    group = paste("Combined yield is total production divided by total area. The", level,
                  "intervals describe individual model estimates. They do not test differences between groups."),
    quality = "For grouped data, an annual total is unavailable if a component value is missing."
  )
}

gre_report_findings <- function(r, digits) {
  findings <- character()
  if (!is.null(r$growth)) {
    g <- gre_table_cells(r$growth, digits)
    ok <- is.finite(r$growth$`CGR (%/yr)`)
    if (any(ok)) {
      estimates <- paste0(g$Series[ok], " ", g$`CGR (%/yr)`[ok], "% per year")
      findings <- c(findings, paste0("Estimated compound growth: ", paste(estimates, collapse = "; "), "."))
    }
  }
  if (!is.null(r$trend)) {
    best <- r$trend[r$trend$Best == "Yes", , drop = FALSE]
    if (nrow(best)) findings <- c(findings, paste0("Lowest AICc: ",
      paste(paste0(best$Series, " (", tolower(best$Model), ")"), collapse = "; "), "."))
  }
  if (!is.null(r$mk)) {
    m <- r$mk[is.finite(r$mk$`p-value`), , drop = FALSE]
    significant <- m[m$`p-value` < r$alpha, , drop = FALSE]
    if (nrow(m)) findings <- c(findings, if (nrow(significant)) paste0(
      "Mann-Kendall at the selected significance level: ",
      paste(paste0(significant$Series, " (", tolower(significant$Trend), ")"), collapse = "; "), ".")
      else "No tested series met the selected Mann-Kendall significance threshold.")
  }
  if (!is.null(r$decomp)) {
    d <- gre_table_cells(r$decomp, digits)
    findings <- c(findings, paste0("Endpoint production change: ",
      d$Contribution[d$Effect == "Total change"], " in the source production units."))
  }
  findings
}
