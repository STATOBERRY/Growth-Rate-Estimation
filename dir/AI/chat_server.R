library(httr2)
library(jsonlite)

## change api

OPENAI_API_KEY    <- Sys.getenv("OPENAI_API_KEY")
DASHBOARD_DB_BASE    <- "https://dashboard-raisins.firebaseio.com"
DASHBOARD_API_KEY    <- Sys.getenv("DASHBOARD_API_KEY")
DASHBOARD_SVC_EMAIL  <- "statistician@statoberry.com"
DASHBOARD_SVC_PWD    <- Sys.getenv("firepwd")

MODULE_ID            <- "rmtwoway_chat"




QUOTA_LIMITS <- list(
  inst = list(daily = 150000L, monthly = 2000000L)
)
QUOTA_EXCEEDED_DAILY   <- "You have reached your daily message limit. Please try again tomorrow or contact support@statoberry.com."
QUOTA_EXCEEDED_MONTHLY <- "You have reached your monthly message limit. Please contact support@statoberry.com."




BOT_IMG <- paste0(
  "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/",
  "o/RAISINS_GENERAL%2Fbot.webp?alt=media"
)

# ─── Dashboard Firebase helpers ──────────────────────────────────────────────
.dash_token_cache <- new.env(parent = emptyenv())

.get_dash_token <- function() {
  cached <- .dash_token_cache$tok
  issued <- .dash_token_cache$issued
  if (!is.null(cached) && !is.null(issued) &&
      as.numeric(Sys.time()) - issued < 3000) return(cached)
  tok <- tryCatch({
    url <- paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=",
                  DASHBOARD_API_KEY)
    res <- httr::POST(url,
                      body   = jsonlite::toJSON(list(
                        email            = DASHBOARD_SVC_EMAIL,
                        password         = DASHBOARD_SVC_PWD,
                        returnSecureToken = TRUE), auto_unbox = TRUE),
                      encode = "json")
    httr::content(res)$idToken
  }, error = function(e) NULL)
  .dash_token_cache$tok    <- tok
  .dash_token_cache$issued <- as.numeric(Sys.time())
  tok
}

.ts_dash <- function() format(Sys.time(), "%Y%m%d_%H%M%S_") |>
  paste0(as.integer((as.numeric(Sys.time()) %% 1) * 1000))

.fb_dash_get <- function(path, token) {
  url <- paste0(DASHBOARD_DB_BASE, path, "?auth=", token)
  tryCatch(httr::GET(url), error = function(e) NULL)
}
.fb_dash_patch <- function(path, data, token) {
  url <- paste0(DASHBOARD_DB_BASE, path, "?auth=", token)
  tryCatch(
    httr::PATCH(url,
                body   = jsonlite::toJSON(data, auto_unbox = TRUE),
                encode = "json", httr::content_type_json()),
    error = function(e) NULL)
}
.fb_dash_put <- function(path, data, token) {
  url <- paste0(DASHBOARD_DB_BASE, path, "?auth=", token)
  tryCatch(
    httr::PUT(url,
              body   = jsonlite::toJSON(data, auto_unbox = TRUE),
              encode = "json", httr::content_type_json()),
    error = function(e) NULL)
}

# ─── Quota ───────────────────────────────────────────────────────────────────
.fb_read_quota <- function(uid, token) {
  res <- .fb_dash_get(paste0("/Modules/", MODULE_ID, "/quota/", uid, ".json"), token)
  if (is.null(res)) return(list())
  parsed <- httr::content(res, as = "parsed")
  if (is.null(parsed) || identical(parsed, FALSE)) list() else parsed
}

.resolve_quota_counts <- function(quota) {
  today <- format(Sys.Date(), "%Y%m%d")
  month <- format(Sys.Date(), "%Y%m")
  list(
    today        = today,
    month        = month,
    tokens_today = if (identical(quota$lastResetDay,   today))
      as.integer(quota$tokensToday  %||% 0L) else 0L,
    tokens_month = if (identical(quota$lastResetMonth, month))
      as.integer(quota$tokensMonth  %||% 0L) else 0L
  )
}

check_quota_dash <- function(uid, token) {
  if (is.null(uid) || !nzchar(uid)) return(list(ok = TRUE, quota = list(), counts = NULL))
  tryCatch({
    quota  <- .fb_read_quota(uid, token)
    counts <- .resolve_quota_counts(quota)
    limits <- QUOTA_LIMITS[["inst"]]
    if (is.finite(limits$daily)   && counts$tokens_today >= limits$daily)
      return(list(ok = FALSE, msg = QUOTA_EXCEEDED_DAILY,   quota = quota, counts = counts))
    if (is.finite(limits$monthly) && counts$tokens_month >= limits$monthly)
      return(list(ok = FALSE, msg = QUOTA_EXCEEDED_MONTHLY, quota = quota, counts = counts))
    list(ok = TRUE, quota = quota, counts = counts)
  }, error = function(e) list(ok = TRUE, quota = list(), counts = NULL))
}

update_quota_dash <- function(uid, token, tokens_used, check_result = NULL, email = NULL) {
  if (is.null(uid) || !nzchar(uid) || tokens_used <= 0L) return(invisible(NULL))
  tryCatch({
    counts <- if (!is.null(check_result$counts))
      check_result$counts else .resolve_quota_counts(.fb_read_quota(uid, token))
    new_today <- counts$tokens_today + as.integer(tokens_used)
    new_month <- counts$tokens_month + as.integer(tokens_used)
    limits    <- QUOTA_LIMITS[["inst"]]
    if (is.finite(limits$daily))   new_today <- min(new_today, limits$daily   + 1L)
    if (is.finite(limits$monthly)) new_month <- min(new_month, limits$monthly + 1L)
    quota_data <- list(plan           = "inst",
                       tokensToday    = new_today,
                       tokensMonth    = new_month,
                       lastResetDay   = counts$today,
                       lastResetMonth = counts$month)
    if (!is.null(email) && nzchar(trimws(email)))
      quota_data$email <- tolower(trimws(email))
    .fb_dash_patch(paste0("/Modules/", MODULE_ID, "/quota/", uid, ".json"),
                   quota_data, token)
    if (!is.null(email) && nzchar(trimws(email))) {
      existing_enable <- tryCatch({
        res <- .fb_dash_get(paste0("/Modules/", MODULE_ID, "/users/", uid, "/chat_enable.json"), token)
        httr::content(res, as = "parsed")
      }, error = function(e) NULL)
      user_data <- list(email = tolower(trimws(email)))
      if (is.null(existing_enable)) user_data$chat_enable <- TRUE
      .fb_dash_patch(paste0("/Modules/", MODULE_ID, "/users/", uid, ".json"),
                     user_data, token)
    }
  }, error = function(e) NULL)
}

# ─── Chat enable/disable gate ────────────────────────────────────────────────
check_global_chat_enabled_dash <- function() {
  tryCatch({
    tok <- .get_dash_token()
    if (is.null(tok)) return(TRUE)   # fail open
    res <- .fb_dash_get(paste0("/Modules/", MODULE_ID, "/enabled.json"), tok)
    if (is.null(res)) return(TRUE)
    val <- httr::content(res, as = "parsed")
    if (is.null(val)) return(TRUE)
    isTRUE(val)
  }, error = function(e) TRUE)
}

check_user_chat_enabled_dash <- function(uid, token) {
  if (is.null(uid) || !nzchar(uid)) return(TRUE)  # no uid → pass through
  tryCatch({
    if (is.null(token)) return(TRUE)
    res <- .fb_dash_get(paste0("/Modules/", MODULE_ID, "/users/", uid, "/chat_enable.json"), token)
    if (is.null(res)) return(TRUE)
    val <- httr::content(res, as = "parsed")
    if (is.null(val)) return(TRUE)   # node absent → default enabled
    isTRUE(val)
  }, error = function(e) TRUE)
}



#### Changed by Claude AI (ppg)
chat_server <- function(id,
                        context        = reactive(NULL),
                        api_key        = OPENAI_API_KEY,
                        model          = "gpt-4o-mini",
                        user_rv        = reactive(NULL),
                        plan_rv        = reactive(NULL),
                        email_rv       = reactive(NULL),
                        model_data_dir = reactive(NULL),
                        parent_session = NULL,
                        results_rv     = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── State ─────────────────────────────────────────────────────────────────
    chat_history    <- reactiveVal(list())
    is_waiting      <- reactiveVal(FALSE)
    conv_counter    <- reactiveVal(1)
    pending_datagen <- reactiveVal(NULL)   # list(n_trt, n_rep) while waiting for n_resp
    dl_counter      <- reactiveVal(0L)
    ##
    consent_given   <- reactiveVal(FALSE)
    last_intent     <- reactiveVal("none")
    
    dash_uid <- reactive({
      em <- tryCatch(email_rv(), error = function(e) NULL)
      if (is.null(em) || !nzchar(trimws(em))) return(NULL)
      digest::digest(tolower(trimws(em)), algo = "sha256")
    })
    
    
    
    
    observe({
      req(!is.null(parent_session))
      tab <- parent_session$input$tabs
      req(identical(tab, "RA-One"))
      
      # Only if analysis has been completed (context is non-NULL)
      ctx <- tryCatch(context(), error = function(e) NULL)
      req(!is.null(ctx) && nchar(trimws(ctx)) > 0)
      
      # Only show once per session
      if (isTRUE(isolate(consent_given()))) return()
      
      shinyjs::runjs("
        (function() {
          var style = document.getElementById('shinyalert-cancel-style');
          if (!style) {
            style = document.createElement('style');
            style.id = 'shinyalert-cancel-style';
            style.textContent = '.sa-button-container .cancel { background-color: #2d4ecf !important; }';
            document.head.appendChild(style);
          }
        })();
      ")
      shinyalert::shinyalert(
        session           = session,
        title             = "Welcome to RAISINS AI ENGINE",
        text = paste0(
          "<b style='color:#1a6fba;'>RA-One </b> helps you analyse your data, ask questions, and generate insights through simple conversation.<br><br>",
          "<b style='color:#137525;'>How it works</b><br>",
          "Your data is analysed in real time in an R environment, while an AI service assists in interpreting results and answering your queries.<br><br>",
          "<b style='color:#b5650d;'>Privacy & Security</b><br>",
          "RAISINS does not store your data or chats; everything is cleared when your session ends. Your questions and a summary of your analysis results are sent to our AI service provider solely to generate responses.<br><br>",
          "<b style='color:#c0392b;'>Use of AI (LLM) models</b><br>",
          "RA-One is powered by a third-party large language model (LLM). The text of your questions and a summary of your analysis results are transmitted to that external AI provider to produce answers, and may be processed, logged, or used by them in accordance with their own terms and privacy policies.<br><br>",
          "<b style='color:#8e44ad;'>Need Help?</b><br>",
          "If you face any issues, contact us at <a href='mailto:statistician@statoberry.com' style='color:#c0392b;'>statistician@statoberry.com</a><br><br>",
          "By continuing, you agree to our <a href='https://www.raisins.live/policy.html' target='_blank' style='color:#137525;'> Data Privacy Policy</a>."
        ),
        html              = TRUE,    
        type              = "success",
        showCancelButton  = TRUE,
        confirmButtonText = "Agree & Continue",
        cancelButtonText  = "Go Back",
        confirmButtonCol  = "#137525",
        callbackR = function(confirmed) {
          if (isTRUE(confirmed)) {
            consent_given(TRUE)
          } else {
            if (!is.null(parent_session)) {
              updateTabsetPanel(parent_session, inputId = "tabs", selected = "Analysis")
            }
          }
        }
      )
      # shinyalert::shinyalert(
      #   session           = session,
      #   title             = "Welcome to AI Mode",
      #   text = paste0(
      #     "Please read carefully before proceeding.\n\n",
      #     "Until now, your data has not been stored anywhere and is lost after the session.\n\n",
      #     "For a better user experience, your analysis results will now be securely stored in our database.\n\n",
      #     "By clicking Agree, you consent to our Terms and Conditions."
      #   ),
      #   type              = "success",
      #   showCancelButton  = TRUE,
      #   confirmButtonText = "OK, Continue",
      #   cancelButtonText  = "Go Back",
      #   confirmButtonCol  = "#137525",
      #   callbackR = function(confirmed) {
      #     if (isTRUE(confirmed)) {
      #       consent_given(TRUE)
      #     } else {
      #       if (!is.null(parent_session)) {
      #         updateTabsetPanel(parent_session, inputId = "tabs", selected = "Analysis")
      #       }
      #     }
      #   }
      # )
    })
    
    ## end 
    
    #### 
    plot_counter    <- reactiveVal(0L)     # unique IDs for plotOutput in chat
    active_plot_id  <- reactiveVal(NULL)   # current plotOutput ID for re-render
    current_gg      <- reactiveVal(NULL)  
    settings_open   <- reactiveVal(FALSE) #track if Plot Settings panel is open
    
    # ── System prompt ─────────────────────────────────────────────────────────
    
    system_prompt <- reactive({
      usr  <- tryCatch(user_rv(), error = function(e) NULL)
      plan <- tryCatch(plan_rv(), error = function(e) NULL)
      ctx  <- tryCatch(context(), error = function(e) NULL)
      get_rcbd_system_prompt(user = usr, plan = plan, context = ctx)
    })
    
    # ── Access gate ───────────────────────────────────────────────────────────
    is_allowed <- reactive({
      plan <- tryCatch(plan_rv(), error = function(e) NULL)
      if (is.null(plan) || nchar(trimws(plan)) == 0) return(FALSE)
      grepl("Inst plan", plan, ignore.case = TRUE) 
      # ||
      # grepl("94400",     plan, ignore.case = TRUE)
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # SIDEBAR / TOPBAR / WELCOME  (unchanged logic - kept compact)
    # ══════════════════════════════════════════════════════════════════════════
    output$ctx_badge <- renderUI({
      ctx <- context()
      if (!is.null(ctx) && nchar(trimws(ctx)) > 0)
        div(class = "rch-ctx-on", div(class = "cdot"), "Analysis context loaded")
      else
        div(class = "rch-ctx-off", "No analysis context yet")
    })
    
    output$history_items <- renderUI({
      msgs <- chat_history()
      if (length(msgs) == 0)
        return(div(style = "padding:8px 10px;font-size:12px;color:#5C5855;",
                   "No conversations yet"))
      first_user <- Filter(function(m) m$role == "user", msgs)
      label <- if (length(first_user) > 0) {
        txt <- first_user[[1]]$content
        if (nchar(txt) > 36) paste0(substr(txt, 1, 36), "\u2026") else txt
      } else paste("Conversation", conv_counter())
      div(class = "rch-hist active", icon("comment-dots"),
          div(class = "rch-hist-txt", label))
    })
    
    output$conv_title <- renderUI({
      msgs <- chat_history()
      if (length(msgs) == 0) return(span("New conversation", style = "color:#9A9790;"))
      first_user <- Filter(function(m) m$role == "user", msgs)
      if (length(first_user) > 0) {
        txt <- first_user[[1]]$content
        span(if (nchar(txt) > 60) paste0(substr(txt, 1, 60), "\u2026") else txt)
      } else span(paste("Conversation", conv_counter()))
    })
    
    # ══════════════════════════════════════════════════════════════════════════
    # MESSAGE RENDERING
    # ══════════════════════════════════════════════════════════════════════════
    output$messages <- renderUI({
      msgs <- chat_history()
      
      if (length(msgs) == 0 && !is_waiting()) {
        ctx      <- tryCatch(context(), error = function(e) NULL)
        has_ctx  <- !is.null(ctx) && nchar(trimws(ctx)) > 0
        usr      <- tryCatch(user_rv(), error = function(e) NULL)
        greet    <- if (!is.null(usr) && nchar(trimws(usr)) > 0)
          paste0("Hello, ", usr, "!") else "Hello!"
        
        if (!isTRUE(is_allowed())) return(
          div(id = ns("welcome"),
              div(class = "rch-wlc-icon", icon("lock")),
              div(class = "rch-wlc-h", "Feature Locked"),
              div(class = "rch-wlc-p",
                  "The AI Assistant is available only to",
                  tags$br(), tags$strong("Institutional"), " and ",
                  tags$strong("Individual Full Licence"), " users.",
                  tags$br(), tags$br(),
                  "Please upgrade your plan to access AI-powered analysis.")))
        
        if (!has_ctx) return(
          div(id = ns("welcome"),
              div(class = "rch-wlc-icon",
                  tags$img(src = BOT_IMG,
                           style = "width:100%;height:100%;object-fit:cover;border-radius:14px;",
                           onerror = "this.style.display='none'")),
              div(class = "rch-wlc-h", "RAISINS Assistant"),
              div(class = "rch-wlc-p", greet, tags$br(),
                  "No analysis results loaded yet.", tags$br(), tags$br(),
                  tags$strong("To get started:"),
                  tags$ol(style = "text-align:left;margin-top:10px;",
                          tags$li("Go to the ", tags$strong("Analysis Results"), " tab"),
                          tags$li("Upload your data file (CSV or Excel)"),
                          tags$li("Select Treatment column and Response Variable(s)"),
                          tags$li("Click ", tags$strong("Run Analysis"))),
                  tags$br(),
                  "Or ask me about ", tags$strong("data preparation"),
                  ", ", tags$strong("replication advice"),
                  ", or ", tags$strong("model datasets"), "!"),
              div(class = "rch-chips",
                  div(class = "rch-chip",
                      `data-prompt` = "What is Repeated Measures One-way ANOVA?",
                      tags$strong("What is Repeated Measures One-way ANOVA?"),
                      "Basic explanation of Repeated Measures One-way ANOVA."),
                  div(class = "rch-chip",
                      `data-prompt` = "Create a data template for 5 treatments, 4 time points, 3 replications, 3 variables",
                      tags$strong("Create data template"),
                      "Generate a downloadable CSV."),
                  div(class = "rch-chip",
                      `data-prompt` = "How many replications do I need for 6 treatments?",
                      tags$strong("What should be the replication?"),
                      "Minimum reps for error df \u2265 12."),
                  div(class = "rch-chip",
                      `data-prompt` = "Show me the model datasets",
                      tags$strong("Model datasets"),
                      "Download built-in example data."))))
        
        # ── Ready state with chips ───────────────────────────────────────────
        return(
          div(id = ns("welcome"),
              div(class = "rch-wlc-icon",
                  tags$img(src = BOT_IMG,
                           style = "width:100%;height:100%;object-fit:cover;border-radius:14px;",
                           onerror = "this.style.display='none'")),
              div(class = "rch-wlc-h", "RAISINS Assistant"),
              div(class = "rch-wlc-p", greet, tags$br(),
                  "Your analysis results are loaded. Ask me anything!"),
              div(class = "rch-chips",
                  div(class = "rch-chip",
                      `data-prompt` = "Interpret my results",
                      tags$strong("Interpret results"),
                      "Summarise significance of my characters."),
                  div(class = "rch-chip",
                      `data-prompt` = "Create a data template for 5 treatments, 4 time points, 3 replications, 3 variables",
                      tags$strong("Create data template"),
                      "Generate a downloadable CSV."), 
                  div(class = "rch-chip",
                      `data-prompt` = "Show me the model datasets",
                      tags$strong("Model datasets"),
                      "Download built-in example data."),
                  div(class = "rch-chip",
                      `data-prompt` = "How should I report this results in a paper?",
                      tags$strong("Report results"),
                      "Help me write up the results section."))))
      }
      
      # ── Build message rows ─────────────────────────────────────────────────
      rows <- lapply(msgs, function(m) {
        is_user  <- m$role == "user"
        msg_type <- m$type %||% "text"
        
        av <- div(class = paste("rch-av", if (is_user) "user" else "ai"),
                  if (is_user) "You"
                  else tags$img(src = BOT_IMG,
                                style = "width:100%;height:100%;object-fit:cover;border-radius:50%;",
                                onerror = "this.outerHTML='R1'"))
        
        #### 
        body_content <- if (msg_type == "plot" && !is.null(m$plot_id)) {
          # Full plot card with toolbar (built by rcbd_ai_plot.R)
          is_active <- identical(m$plot_id, active_plot_id())
          toolbar   <- if (is_active) rcbd_ai_build_toolbar(
            current_plot_type() %||% "boxplot",
            current_plot_opts() %||% rcbd_ai_default_opts(),
            ns(""),
            settings_open = settings_open()   ## pass open state
          ) else NULL
          ##  build settings panel, keep it open if user had it open
          settings <- if (is_active) rcbd_ai_build_plot_settings(
            ns(""), current_plot_type() %||% "boxplot", is_open = settings_open()
          ) else NULL
          rcbd_ai_build_plot_card(m, is_active, toolbar, ns, settings_panel = settings)
        } else if (msg_type == "image") {
          tagList(
            if (!is.null(m$src))
              tags$img(src = m$src,
                       style = paste0("max-width:100%;border-radius:10px;",
                                      "border:1px solid #e8d9ed;margin-top:6px;",
                                      "display:block;box-shadow:0 2px 10px rgba(0,0,0,0.10);"))
            else NULL,
            if (!is.null(m$caption) && nchar(m$caption) > 0)
              div(style = "font-size:12px;color:#9A9790;margin-top:5px;font-style:italic;",
                  m$caption)
            else NULL
          )
        } else {
          tagList(
            div(class = "rch-text", HTML(fmt_msg(m$content %||% ""))),
            m$extra_ui)
        }
        #### 
        
        div(class = paste("rch-row", if (is_user) "u-row" else "a-row"),
            av,
            div(class = "rch-body",
                div(class = "rch-name", if (is_user) "You" else "RAISINS Assistant"),
                body_content))
      })
      
      if (is_waiting()) rows <- c(rows, list(
        div(class = "rch-row a-row",
            div(class = "rch-av ai",
                tags$img(src = BOT_IMG,
                         style = "width:100%;height:100%;object-fit:cover;border-radius:50%;",
                         onerror = "this.outerHTML='R1'")),
            div(class = "rch-body",
                div(class = "rch-name", "RAISINS Assistant"),
                div(class = "rch-typing", tags$span(), tags$span(), tags$span())))))
      
      tagList(rows)
    })
    
    
    # ══════════════════════════════════════════════════════════════════════════
    # DOWNLOAD-UI BUILDERS  (shared helpers for intercept handlers)
    # ══════════════════════════════════════════════════════════════════════════
    
    ## CSV download button for a data.frame
    
    ## Model-dataset download card
    make_model_datasets_ui <- function(datasets) {
      if (length(datasets) == 0) return(NULL)
      buttons <- lapply(names(datasets), function(nm) {
        ds <- datasets[[nm]]
        cnt <- dl_counter() + 1L; dl_counter(cnt)
        dl_id <- paste0("dl_model_", nm, "_", cnt)
        output[[dl_id]] <- downloadHandler(
          filename = function() paste0(nm, ".csv"),
          content  = function(file) file.copy(ds$path, file))
        div(style = "display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #e8d9ed;",
            div(style = "flex:1;",
                div(style = "font-size:13px;font-weight:600;color:#2d1a30;", ds$label),
                if (nzchar(ds$desc))
                  div(style = "font-size:11.5px;color:#5e4363;margin-top:2px;", ds$desc)),
            downloadButton(ns(dl_id), label = tagList(" CSV"),
                           style = paste0(
                             "background:linear-gradient(135deg,#560466,#2d1a30);",
                             "color:#fff;border:none;border-radius:20px;",
                             "padding:6px 14px;font-size:12px;font-weight:600;",
                             "cursor:pointer;white-space:nowrap;")))
      })
      div(style = "margin-top:10px;padding:12px;background:#faf0fc;border-radius:10px;border:1px solid #e8d9ed;",
          div(style = "font-size:12.5px;font-weight:600;color:#560466;margin-bottom:8px;",
              icon("database"), " Model Datasets"),
          tagList(buttons))
    }
    
    
    # ══════════════════════════════════════════════════════════════════════════
    # PUSH HELPERS
    # ══════════════════════════════════════════════════════════════════════════
    .clean_md_reply <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(txt)
      lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      n <- length(lines)
      if (n == 0L) return(txt)
      
      is_one_item  <- grepl("^\\s*1\\.\\s+\\S",                lines, perl = TRUE)
      is_list_item <- grepl("^\\s*([-*+]|\\d+\\.)\\s+\\S",    lines, perl = TRUE)
      is_blank     <- grepl("^\\s*$",                          lines, perl = TRUE)
      
      # ── Step 1: renumber runs of "1." items ─────────────────────────────────
      out <- lines
      i <- 1L
      while (i <= n) {
        if (is_one_item[i]) {
          group_idx <- integer(0)
          j <- i; last_one <- i
          while (j <= n) {
            if (is_one_item[j]) {
              group_idx <- c(group_idx, j); last_one <- j; j <- j + 1L
            } else if (is_blank[j]) {
              j <- j + 1L
            } else break
          }
          if (length(group_idx) >= 2L) {
            for (k in seq_along(group_idx)) {
              idx <- group_idx[k]
              out[idx] <- sub("^(\\s*)1\\.\\s+",
                              paste0("\\1", k, ". "),
                              out[idx], perl = TRUE)
            }
          }
          i <- last_one + 1L
        } else {
          i <- i + 1L
        }
      }
      
      # Renumbering may have turned a "1." item into "2.", "3." etc., so the
      # list-item mask must be rebuilt before step 2.
      is_list_item <- grepl("^\\s*([-*+]|\\d+\\.)\\s+\\S", out, perl = TRUE)
      
      # ── Step 2: drop blank lines that sit between two list items ────────────
      keep <- rep(TRUE, n)
      for (i in seq_len(n)) {
        if (is_blank[i]) {
          p <- i - 1L
          while (p >= 1L && is_blank[p]) p <- p - 1L
          if (p < 1L) next
          q <- i + 1L
          while (q <= n && is_blank[q]) q <- q + 1L
          if (q > n) next
          if (is_list_item[p] && is_list_item[q]) keep[i] <- FALSE
        }
      }
      
      paste(out[keep], collapse = "\n")
    }
    
    push_reply <- function(text, extra_ui = NULL) {
      text <- .clean_md_reply(text)
      chat_history(c(chat_history(), list(list(
        role = "assistant", content = text, extra_ui = extra_ui))))
    }
    
    #### 
    # ── Push a live plot with toolbar (UI built by rcbd_ai_plot.R) ─────────
    push_plot <- function(gg, caption = "", plot_height = "450px") {
      ## Anti-flicker: only ONE plot (the latest) tracks the shared current_gg()
      ## reactive so live settings edits refresh it in place. Every previously
      ## pushed plot is FROZEN to a static render at push time, so it no longer
      ## re-executes when current_gg() changes — that shared dependency across
      ## all history plots was the flicker (each settings-apply repainted them all).
      prev_id <- isolate(active_plot_id())
      if (!is.null(prev_id)) {
        local({
          pid0 <- prev_id
          g0   <- isolate(current_gg()) %||% gg
          output[[pid0]] <- renderPlot(g0, bg = "white")
        })
      }

      cnt <- plot_counter() + 1L; plot_counter(cnt)
      pid <- paste0("chatplot_", cnt, "_", as.integer(Sys.time()))
      active_plot_id(pid)
      current_gg(gg)

      # The active (latest) plot tracks current_gg() for live in-place editing.
      local({
        pidA <- pid
        output[[pidA]] <- renderPlot({ req(current_gg()); current_gg() }, bg = "white")
      })

      dl_id <- paste0("dl_", pid)
      local({
        pidD <- pid; ggD <- gg
        output[[paste0("dl_", pidD)]] <- downloadHandler(
          filename = function() paste0("RAISINS_plot_", Sys.Date(), ".png"),
          content  = function(file) {
            p <- if (identical(isolate(active_plot_id()), pidD)) isolate(current_gg()) else ggD
            ggplot2::ggsave(file, plot = p, device = "png",
                            width = 10, height = 7, dpi = 300, bg = "white")
          }
        )
      })

      chat_history(c(chat_history(), list(list(
        role     = "assistant",
        type     = "plot",
        plot_id  = pid,
        dl_id    = dl_id,
        plot_h   = plot_height,
        caption  = caption,
        content  = caption
      ))))
    }
    
    # Pending environment stages values; apply flushes them
    plot_pending <- new.env(parent = emptyenv())
    
    # Track settings panel toggle from JS
    observeEvent(input$plt_settings_toggle, {
      settings_open(!settings_open())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Observer 1: stage the value
    observeEvent(input$plt_ctrl_update, {
      req(!is.null(input$plt_ctrl_update))
      pkg <- input$plt_ctrl_update
      fld <- pkg$field; val <- pkg$value; typ <- pkg$vtype %||% "S"
      if (is.null(fld) || !nzchar(fld)) return()
      
      if (fld == "reset") {
        rm(list = ls(plot_pending), envir = plot_pending)
        current_plot_opts(rcbd_ai_default_opts())
        cur_type <- current_plot_type()
        results  <- tryCatch(results_rv(), error = function(e) NULL)
        if (!is.null(cur_type) && !is.null(results)) {
          gg <- rcbd_ai_make_plot(results, cur_type, rcbd_ai_default_opts())
          if (!is.null(gg)) current_gg(gg)
        }
        return()
      }
      
      coerced <- tryCatch(switch(typ,
                                 "I" = as.integer(val),
                                 "N" = as.numeric(val),
                                 "B" = isTRUE(val),
                                 as.character(val)),
                          error = function(e) val
      )
      assign(fld, coerced, envir = plot_pending)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Observer 2: flush pending → opts → re-render
    observeEvent(input$plt_ctrl_apply, {
      pending_fields <- ls(plot_pending)
      if (length(pending_fields) == 0) return()
      
      if ("plot_type" %in% pending_fields) {
        new_type <- get("plot_type", envir = plot_pending)
        rm(list = "plot_type", envir = plot_pending)
        results <- tryCatch(results_rv(), error = function(e) NULL)
        if (!is.null(results)) {
          current_plot_type(new_type)
          opts <- current_plot_opts() %||% rcbd_ai_default_opts()
          for (nm in ls(plot_pending)) {
            if (nm %in% names(opts)) opts[[nm]] <- get(nm, envir = plot_pending)
          }
          rm(list = ls(plot_pending), envir = plot_pending)
          current_plot_opts(opts)
          gg <- rcbd_ai_make_plot(results, new_type, opts)
          if (!is.null(gg)) {
            cap <- rcbd_AI_PLOT_NAMES[new_type] %||% new_type
            ph  <- if (new_type == "summary") "800px" else "450px"
            push_plot(gg, caption = paste0(cap, " \u2014 generated from your analysis data"),
                      plot_height = ph)
          }
        }
        return()
      }
      
      if ("treatment_order" %in% pending_fields) {
        results <- tryCatch(results_rv(), error = function(e) NULL)
        cur_type <- current_plot_type()
        if (!is.null(results) && !is.null(cur_type)) {
          opts <- current_plot_opts() %||% rcbd_ai_default_opts()
          for (nm in pending_fields) {
            if (nm %in% names(opts)) opts[[nm]] <- get(nm, envir = plot_pending)
          }
          rm(list = ls(plot_pending), envir = plot_pending)
          current_plot_opts(opts)
          gg <- rcbd_ai_make_plot(results, cur_type, opts)
          if (!is.null(gg)) {
            order_label <- switch(opts$treatment_order %||% "alpha",
                                  "data"      = "data order",
                                  "mean_desc" = "mean descending",
                                  "mean_asc"  = "mean ascending",
                                  "alphabetical"
            )
            current_gg(gg)
          }
        }
        return()
      }
      
      opts <- current_plot_opts() %||% rcbd_ai_default_opts()
      for (nm in pending_fields) {
        val <- get(nm, envir = plot_pending)
        if (nm %in% names(opts)) opts[[nm]] <- val
      }
      rm(list = ls(plot_pending), envir = plot_pending)
      current_plot_opts(opts)
      
      cur_type <- current_plot_type()
      results  <- tryCatch(results_rv(), error = function(e) NULL)
      if (!is.null(cur_type) && !is.null(results)) {
        gg <- rcbd_ai_make_plot(results, cur_type, opts)
        if (!is.null(gg)) current_gg(gg)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)  
    
    # ══════════════════════════════════════════════════════════════════════════
    # SEND - step 1: intercepts
    # ══════════════════════════════════════════════════════════════════════════
    pending_msg <- reactiveVal(NULL)
    
    # Tracks current plot type and opts for editing
    current_plot_type <- reactiveVal(NULL)
    current_plot_opts <- reactiveVal(NULL)
    
    observeEvent(input[["send_click"]], {
      req(!is_waiting())
      msg <- trimws(input[["send_click"]]$msg %||% "")
      req(nchar(msg) > 0)
      
      # Access gate
      if (!isTRUE(is_allowed())) {
        chat_history(c(chat_history(), list(list(role = "user", content = msg))))
        push_reply(paste0(
          "The AI Assistant is available only to **Institutional** and ",
          "**Individual Full Licence** users. ",
          "Please upgrade your plan to access this feature."))
        return()
      }
      
      ###200-word limit on user messages
      word_count <- length(strsplit(trimws(msg), "\\s+")[[1]])
      if (word_count > 200L) {
        chat_history(c(chat_history(), list(list(role = "user", content = msg))))
        push_reply(paste0(
          "Your message has **", word_count, " words**, which exceeds the ",
          "**200-word limit**. Please shorten your message and try again."))
        return()
      }
      ###end change 21-03-26 claude
      
      # Add user message 
      
      # Add user message
      chat_history(c(chat_history(), list(list(role = "user", content = msg))))
      
      
      # ── INTERCEPT 0: pending datagen follow-up (user typing n_resp) ────────
      if (!is.null(pending_datagen())) {
        
        # Try plain integer first: "3"
        nums_reply <- suppressWarnings(as.integer(trimws(msg)))
        
        # If not plain integer, try extracting a number from the message: "3 variables", "I want 3"
        if (is.na(nums_reply)) {
          extracted <- regmatches(msg, regexpr("\\d+", msg))
          if (length(extracted) > 0)
            nums_reply <- suppressWarnings(as.integer(extracted[1]))
        }
        
        if (!is.na(nums_reply) && nums_reply >= 1L && nums_reply <= 50L) {
          pd <- pending_datagen(); pending_datagen(NULL)
          n_rep <- pd$n_rep; n_resp <- nums_reply
          ## days default to 3 when the user did not say - the reply states it
          n_day <- if (is.null(pd$n_day)) 3L else pd$n_day
          ## n_rep is the SUBJECT count here; never coerced, only flagged
          min_rep <- compute_min_replication(n_day)
          low_df  <- (n_rep < min_rep)
          error_df <- (n_rep - 1L) * (n_day - 1L)
          cnt <- dl_counter() + 1L; dl_counter(cnt)
          gen_id <- paste0("chatgen_", cnt, "_", as.integer(Sys.time()))
          card_ui <- build_rm_datagen_card(gen_id, n_day, n_rep, n_resp, ns, output)
          push_reply(build_datagen_reply(n_day, n_rep, n_resp, error_df, low_df), card_ui)
          return()
        }
        
        # Number out of range or non-numeric reply → re-ask instead of silently dropping
        if (!is.na(nums_reply) && (nums_reply < 1L || nums_reply > 50L)) {
          push_reply(paste0(
            "Please enter a number between **1 and 50** for the number of response variables.\n\n",
            "How many response variables (characters/traits) do you want to study?"))
          return()   # ← keep pending_datagen() alive, don't clear it
        }
        
        # Truly non-numeric reply (user asked something else) → gently remind
        push_reply(paste0(
          "I'm still waiting for the **number of response variables** to build your data template.\n\n",
          "Just reply with a number (e.g. `3` for Yield, Plant Height, Ear Length). ",
          "Or say **cancel** to start over."))
        return()   # ← keep pending_datagen() alive
      }      
      
      # ── CLASSIFY INTENT (runs BEFORE plot handlers) ────────────────────────
      # These have their own handlers below; classify_chat_intent would misroute them
      interpret_kws <- c("interpret", "interpretion", "Interpret my results", "explain", "report", "write up", "how to",
                         "Interpret results","what does", "what is", "why is", "summarise", "summarize",
                         "tell me about", "help me understand", "meaning of", "interpret anova","interpret table",
                         "interpret the plot", "interpret the graph", "interpret the chart",
                         "explain the plot", "explain the graph", "explain the chart",
                         "what does the plot", "what does the graph",
                         "understand the plot", "understand the graph",
                         "significance of", "letter grouping", "grouping pattern")
      is_interpret <- any(sapply(interpret_kws, function(k) grepl(k, tolower(msg), fixed = TRUE)))
      
      .datagen_kws <- c(
        "prepare data", "prepare for", "prepare a data", "prepare crd",
        "create data", "create a data", "generate data", "make data",
        "create csv", "create a csv", "generate csv", "make csv", "csv file",
        "data template", "crd template", "data sheet",
        "create a template", "create template", "create a templete",
        "create templete", "make a template", "make template",
        "generate template", "generate a template",
        "template for", "templete for", "template", "templete"
      )
      is_datagen_kw <- any(sapply(.datagen_kws, function(k)
        grepl(k, tolower(msg), fixed = TRUE)))
      
      is_followup_msg <- !is_datagen_kw && !is_interpret &&
        grepl("\\by\\d+\\b|\\bfor\\s+\\w+|\\bonly\\s+\\w+|\\bjust\\s+\\w+|\\bshow\\s+\\w+",
              tolower(msg), perl = TRUE)
      
      is_char_filter_followup <- is_followup_msg &&
        !is.null(current_plot_type()) &&
        isolate(last_intent()) == "plot"
      
      is_table_char_followup <- is_followup_msg &&
        isolate(last_intent()) == "table"
      
      has_plot_kw  <- !is_datagen_kw && !is.null(rcbd_ai_detect_plot(msg))
      has_table_kw <- !is_datagen_kw &&
        (!is.null(crd_detect_table_request(msg)) || isTRUE(is_table_char_followup))
      has_edit_kw  <- !is_datagen_kw &&
        (!is.null(current_plot_type()) && rcbd_ai_detect_edit(msg) || is_char_filter_followup)
      
      # If the message carries a subject count, it's a datagen request, not a
      # replication question. One-way RM has no treatment factor to key off.
      has_datagen_params <- {
        .dp <- parse_datagen_params(msg)
        !is.null(.dp$n_rep)
      }
      
      intent <- if (is_datagen_kw || has_datagen_params) "datagen"   # datagen wins first
      else if (!is_interpret && (has_plot_kw || has_table_kw || has_edit_kw)) "none"
      else if (detect_replication_question(msg)) "replication"
      else classify_chat_intent(msg)
      
      
      # ── INTERCEPT 1: CSV template generation ───────────────────────────────
      if (intent == "datagen") {
        params <- parse_datagen_params(msg)
        
        if (!is.null(params$n_rep) && !is.null(params$n_resp)) {
          n_rep <- params$n_rep; n_resp <- params$n_resp
          n_day <- if (is.null(params$n_day)) 3L else params$n_day
          ## n_rep is the SUBJECT count here; never coerced, only flagged
          min_rep <- compute_min_replication(n_day)
          low_df  <- (n_rep < min_rep)
          error_df <- (n_rep - 1L) * (n_day - 1L)
          cnt <- dl_counter() + 1L; dl_counter(cnt)
          gen_id <- paste0("chatgen_", cnt, "_", as.integer(Sys.time()))
          card_ui <- build_rm_datagen_card(gen_id, n_day, n_rep, n_resp, ns, output)
          push_reply(build_datagen_reply(n_day, n_rep, n_resp, error_df, low_df),
                     card_ui)
          return()
          
        } else if (!is.null(params$n_rep)) {
          n_rep <- params$n_rep
          n_day <- if (is.null(params$n_day)) 3L else params$n_day
          min_rep <- compute_min_replication(n_day)
          pending_datagen(list(n_day = n_day, n_rep = n_rep))
          push_reply(paste0(
            "\U0001f4cb **One-way Repeated Measures ANOVA data template** \u2014 ", n_rep,
            " subjects \u00d7 ", n_day, " days",
            " (error df = ", (n_rep - 1L) * (n_day - 1L), ").",
            if (n_rep < min_rep) paste0(" _That is under the usual df rule of thumb of 12; ",
                                        min_rep, " subjects would reach it, but I will build what you asked._") else "",
            if (is.null(params$n_day)) " _Days not specified - assumed 3; say e.g. \"5 days\" to change._" else "",
            "\n\n",
            "**How many response variables (characters/traits) do you want to study?**\n\n",
            "Just reply with a number (e.g. `3` for Yield, Plant Height, Ear Length)."))
          return()
          
        } else if (!is.null(params$n_day)) {
          n_day <- params$n_day
          min_rep <- compute_min_replication(n_day)
          pending_datagen(list(n_day = n_day, n_rep = min_rep))
          push_reply(paste0(
            "\U0001f4cb **One-way Repeated Measures ANOVA data template** \u2014 ", min_rep,
            " subjects \u00d7 ", n_day, " days. _Subjects not specified - assumed ", min_rep,
            " (the smallest giving an error df of 12); say e.g. \"8 subjects\" to change._\n\n",
            "**How many response variables (characters/traits) do you want to study?**\n\n",
            "Just reply with a number (e.g. `3`)."))
          return()
          
        } else {
          push_reply(paste0(
            "**Repeated Measures Two-way data format for RAISINS:**\n\n",
            "| Treatment | Day | Block | y1 | y2 | y3 |\n",
            "| --- | --- | --- | --- | --- | --- |\n",
            "| T1 | day1 | B1 | 12.5 | 65.3 | 8.2 |\n",
            "| T1 | day1 | B2 | 13.1 | 63.1 | 7.9 |\n",
            "| T1 | day2 | B1 | 14.0 | 66.8 | 8.6 |\n",
            "| T2 | day1 | B1 | 15.2 | 70.2 | 9.1 |\n\n",
            "Every **Block** must appear on every **Day** within its Treatment - that ",
            "repeated measurement on the same subject is what the design needs.\n\n",
            "**Rules:** First column = **Treatment**, rest = numeric response columns.\n\n",
            "**Want a ready-to-fill data entry sheet?** Tell me:\n",
            "- Number of **treatments** (e.g. 5)\n",
            "- Number of **replications** (e.g. 4)\n",
            "- Number of **response variables** (e.g. 3)\n\n",
            "Example: *\"Create data for 5 treatments, 4 replications, 3 variables\"*"))
          return()
        }
      }
      
      
      # ── INTERCEPT 2: Replication advice ────────────────────────────────────
      if (intent == "replication") {
        params <- parse_datagen_params(msg)
        push_reply(build_replication_reply(params$n_trt))
        return()
      }
      
      
      # ── INTERCEPT 3: Model datasets ────────────────────────────────────────
      if (intent == "model_data") {
        data_dir <- tryCatch(model_data_dir(), error = function(e) NULL)
        datasets <- get_rcbd_model_datasets(data_dir)
        push_reply(build_model_dataset_reply(datasets),
                   make_model_datasets_ui(datasets))
        return()
      }
      
      
      # ── INTERCEPT 4: Format info (no numbers) ─────────────────────────────
      if (intent == "format_info") {
        push_reply(paste0(
          "**Repeated Measures Two-way data format for RAISINS:**\n\n",
          "Your CSV should have:\n",
          "- First column: **Treatment** (group labels like T1, T2, T3\u2026)\n",
          "- Remaining columns: numeric response variables (Yield, PlantHeight\u2026)\n",
          "- Each row = one observation\n",
          "- Equal or unequal replications are both supported\n\n",
          "| Treatment | Yield | PlantHeight |\n",
          "| --- | --- | --- |\n",
          "| T1 | 42.1 | 65.3 |\n",
          "| T1 | 39.8 | 63.1 |\n",
          "| T2 | 51.4 | 70.2 |\n",
          "| T2 | 48.9 | 68.7 |\n\n",
          "Want a ready-to-fill CSV? Just tell me your treatments, replications, and variables!\n\n",
          "You can also go to the {{tab:Create Data}} tab or download a **model dataset** ",
          "from the {{tab:Datasets}} tab."))
        return()
      }
      
      
      ## - INTERCEPT 5: Table display in chat─────────────────────────────
      # table_req <- crd_detect_table_request(msg)
      table_req <- if (!has_plot_kw) crd_detect_table_request(msg) else NULL
      if (is.null(table_req) && isTRUE(is_table_char_followup)) {
        after <- trimws(gsub(".*\\b(for|of|only|just|show)\\s+", "", tolower(msg), perl = TRUE))
        after <- trimws(gsub("[^a-z0-9 _%-]", "", after))
        table_req <- list(key = "individual", char = after)
      }
      if (!is.null(table_req) && !is.null(results_rv)) {
        results <- tryCatch(results_rv(), error = function(e) NULL)
        if (!is.null(results)) {
          card <- crd_build_table_card(results, table_req$key, table_req$char)
          if (!is.null(card)) {
            # Always show the table card first.
            push_reply(crd_table_caption(table_req$key, table_req$char),
                       extra_ui = card)
            last_intent("table")
            
            # If the user also asked for interpretation (e.g. "interpret anova table",
            # "explain the manova"), continue to the OpenAI call so the AI replies with
            # the interpretation right after the card.  Otherwise stop here as before.
            if (isTRUE(is_interpret)) {
              pending_msg(msg)
              shinyjs::delay(300, {
                is_waiting(TRUE)
                session$sendCustomMessage("triggerCrdApi", list())
              })
            }
            return()
          }
        }
      }
      
      #### 
      # ── Plot EDIT first (if active plot exists AND edit keywords found) ────
      # This runs BEFORE plot request so "change title to X" doesn't
      # accidentally trigger a new plot because X contains a plot keyword
      cur_type <- current_plot_type()
      if (!is_interpret && !is.null(cur_type) && (rcbd_ai_detect_edit(msg) || is_char_filter_followup) && !is.null(results_rv)) {
        is_waiting(TRUE)
        results <- tryCatch(results_rv(), error = function(e) NULL)
        opts    <- current_plot_opts() %||% rcbd_ai_default_opts()
        
        # ── Variable filter request ──────────────────────────────────────────
        m_low <- tolower(msg)
        is_show_all <- grepl("all variable|all char|show all|every variable", m_low)
        char_filter <- if (!is_show_all) rcbd_ai_extract_chars(msg, results) else NULL
        is_filter_req <- is_show_all || !is.null(char_filter)
        
        if (is_filter_req && !is.null(results)) {
          opts$filter_chars <- char_filter
          gg <- rcbd_ai_make_plot(results, cur_type, opts)
          if (!is.null(gg)) {
            current_plot_opts(opts)
            cap <- if (is_show_all) "Showing all variables"
            else paste0("Filtered to: ", paste(char_filter, collapse = ", "))
            push_plot(gg, caption = cap)
          } else {
            push_reply("Could not render the filtered plot. Try again.")
          }
          is_waiting(FALSE)
          return()
        }
        
        # ── GPT-based style edit ─────────────────────────────────────────────
        edit_sys  <- rcbd_ai_edit_prompt(cur_type)
        gpt_json  <- tryCatch(
          call_openai_raw(list(list(role = "user", content = msg)),
                          edit_sys, api_key, model),
          error = function(e) "{}"
        )
        edits <- tryCatch({
          clean <- trimws(gsub("```json|```", "", gpt_json))
          if (nchar(clean) > 2) jsonlite::fromJSON(clean) else list()
        }, error = function(e) list())
        
        if (length(edits) > 0) {
          opts <- rcbd_ai_apply_opts(opts, edits)
          gg   <- if (!is.null(results)) rcbd_ai_make_plot(results, cur_type, opts) else NULL
          if (!is.null(gg)) {
            current_plot_opts(opts)
            change_desc <- paste(names(edits), unlist(edits), sep = " \u2192 ", collapse = ", ")
            push_plot(gg, caption = paste0("Updated: ", change_desc))
          } else {
            push_reply("Plot updated but could not render. Try again.")
          }
        }  else {
          push_reply("For more plot customisations, use the **Plot Settings** panel in the plot card above, or visit the **Analysis** tab.")
        }
        is_waiting(FALSE)
        return()
      }
      
      # ── Plot REQUEST (new plot - only reaches here if NOT an edit) ─────────
      plot_key <- rcbd_ai_detect_plot(msg)
      if (is.null(plot_key) && !is.null(current_plot_type()) && is_char_filter_followup) {
        plot_key <- current_plot_type()   # reuse the active plot type
      }
      
      if (!is_interpret && !is.null(plot_key) && !is.null(results_rv)) {
        is_waiting(TRUE)
        
        results <- tryCatch(results_rv(), error = function(e) NULL)
        
        if (is.null(results)) {
          push_reply("No analysis results yet. Please run the analysis first.")
          is_waiting(FALSE)
          return()
        }
        
        if (plot_key == "generic") {
          plot_key <- current_plot_type() %||% "boxplot"
        }
        
        opts <- current_plot_opts() %||% rcbd_ai_default_opts()
        
        char_filter <- rcbd_ai_extract_chars(msg, results)
        opts$filter_chars <- char_filter
        
        gg <- rcbd_ai_make_plot(results, plot_key, opts)
        
        if (!is.null(gg)) {
          current_plot_type(plot_key)
          current_plot_opts(opts)
          cap <- rcbd_AI_PLOT_NAMES[plot_key] %||% plot_key
          if (!is.null(char_filter))
            cap <- paste0(cap, " for ", paste(char_filter, collapse = ", "))
          cap <- paste0(cap, " \u2014 generated from your analysis data")
          ph  <- if (plot_key == "summary") "800px" else "450px"
          push_plot(gg, caption = cap, plot_height = ph)
          last_intent("plot")
        } else {
          push_reply(paste0("Sorry, I could not generate the **", plot_key,
                            "** plot. Please make sure analysis is complete."))
        }
        is_waiting(FALSE)
        return()
      }
      #### 
      
      
      # ── No local intercept - forward to API ───────────────────────────────
      is_waiting(TRUE)
      pending_msg(msg)
      session$sendCustomMessage("triggerCrdApi", list())
    })
    
    
    # ══════════════════════════════════════════════════════════════════════════
    # SEND - step 2: API call  (after typing dots are painted)
    # ══════════════════════════════════════════════════════════════════════════
    observeEvent(input$crd_do_api, {
      msg <- pending_msg(); req(!is.null(msg)); pending_msg(NULL)
      
      # ── Chat enable/disable gate ─────────────────────────────────────────────
      if (!check_global_chat_enabled_dash()) {
        push_reply("The AI assistant is temporarily unavailable. Please try again later or contact support@statoberry.com.")
        is_waiting(FALSE)
        return()
      }
      uid <- dash_uid()
      tok <- tryCatch(.get_dash_token(), error = function(e) NULL)
      if (!check_user_chat_enabled_dash(uid, tok)) {
        push_reply("Your access to the AI assistant has been disabled. Please contact support@statoberry.com for assistance.")
        is_waiting(FALSE)
        return()
      }
      
      # ── Quota gate ──────────────────────────────────────────────────────────
      qr  <- check_quota_dash(uid, tok)
      if (!isTRUE(qr$ok)) {
        push_reply(qr$msg)
        is_waiting(FALSE)
        return()
      }
      
      ctx <- tryCatch(context(), error = function(e) NULL)
      ctx_missing <- is.null(ctx) || nchar(trimws(ctx)) == 0
      
      if (ctx_missing) {
        result_kws <- c("my result","my anova","my analysis","my data",
                        "my treatment","my p-value","my mean","my cv",
                        "interpret my","best treatment","which treatment")
        if (any(sapply(result_kws, function(k) grepl(k, tolower(msg), fixed = TRUE)))) {
          push_reply(paste0(
            "I don\u2019t have any analysis results loaded yet.\n\n",
            "Please go to the **Analysis** tab, upload your CSV file, ",
            "select the **Treatment** column and **Response Variable(s)**, ",
            "and click **Run Analysis**.\n\n",
            "If you need help preparing your data file first, just ask me!"))
          is_waiting(FALSE)
          return()
        }
      }
      
      history <- chat_history()
      result  <- tryCatch(
        call_openai(history, system_prompt(), api_key, model),
        error = function(e) list(
          content = paste("Sorry, I encountered an error:", conditionMessage(e)),
          usage   = NULL))
      
      push_reply(result$content)
      
      # ── Update quota ────────────────────────────────────────────────────────
      tokens <- as.integer(result$usage$total_tokens %||% 0L)
      if (!is.null(uid) && !is.null(tok) && tokens > 0L)
        update_quota_dash(uid, tok, tokens, qr,
                          email = tryCatch(email_rv(), error = function(e) NULL))
      
      is_waiting(FALSE)
    })
    
    # ── New / Clear ──────────────────────────────────────────────────────────
    observeEvent(input[["new-btn"]], {
      chat_history(list()); is_waiting(FALSE)
      conv_counter(conv_counter() + 1); pending_datagen(NULL)
      current_plot_type(NULL); current_plot_opts(NULL)
      active_plot_id(NULL); current_gg(NULL); settings_open(FALSE)   ### current changes by claude
    })
    observeEvent(input$clear_btn, {
      chat_history(list()); is_waiting(FALSE); pending_datagen(NULL)
      current_plot_type(NULL); current_plot_opts(NULL)
      active_plot_id(NULL); current_gg(NULL); settings_open(FALSE)   ### current changes by claude 
    })
    
    #### Changed by Claude AI (ppg)
    # Paperclip button: switch to Analysis tab and glow the file upload input
    observeEvent(input$goto_analysis_btn, {
      sess <- if (!is.null(parent_session)) parent_session else session
      updateTabsetPanel(sess, inputId = "tabs", selected = "Analysis")
      shinyjs::delay(100, {
        updateTabsetPanel(sess, inputId = "subtabs", selected = "Analysis Results")
      })
      shinyjs::delay(300, {
        shinyjs::runjs("
          (function() {
            var el = document.getElementById('rcbd_file1');
            if (!el) return;
            var box = el.closest('.shiny-input-container') || el;

            // Inject keyframe once
            if (!document.getElementById('raisins-glow-style')) {
              var s = document.createElement('style');
              s.id = 'raisins-glow-style';
              s.textContent =
                '@keyframes raisinsGlowPulse {' +
                '  0%,100% { box-shadow: 0 0 0 3px rgba(46,139,87,0.15); outline-color: rgba(46,139,87,0.5); }' +
                '  50%     { box-shadow: 0 0 0 7px rgba(46,139,87,0.45); outline-color: #2E8B57; }' +
                '}';
              document.head.appendChild(s);
            }

            // Apply glow
            box.style.outline        = '2.5px solid #2E8B57';
            box.style.borderRadius   = '6px';
            box.style.animation      = 'raisinsGlowPulse 1.2s ease-in-out infinite';
            box.style.transition     = 'box-shadow 0.3s';

            // Remove after 5 s
            setTimeout(function() {
              box.style.outline    = '';
              box.style.animation  = '';
              box.style.boxShadow  = '';
              box.style.borderRadius = '';
            }, 5000);
          })();
        ")
      })
    })
    #### end change
    
  })
}

# =============================================================================
# Lightweight single-turn OpenAI call for JSON-only responses (plot edits)
# =============================================================================
call_openai_raw <- function(messages, system, api_key, model) {
  api_msgs <- c(list(list(role = "system", content = system)), messages)
  resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers("Authorization" = paste("Bearer", api_key),
                       "Content-Type"  = "application/json") |>
    httr2::req_body_json(list(model = model, messages = api_msgs,
                              max_tokens = 256, temperature = 0.0)) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  httr2::resp_body_json(resp)$choices[[1]]$message$content
}


# =============================================================================
# OpenAI API call
# =============================================================================
call_openai <- function(messages, system_prompt, api_key, model) {
  if (is.null(api_key) || nchar(api_key) == 0)
    stop("RA-ONE is currently unavailable")
  
  text_msgs <- Filter(function(m) {
    msg_type <- m$type %||% "text"
    msg_type == "text" && !is.null(m$content) && nchar(m$content) > 0
  }, messages)
  
  api_msgs <- c(
    list(list(role = "system", content = system_prompt)),
    lapply(text_msgs, function(m) list(role = m$role, content = m$content))
  )
  
  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers("Authorization" = paste("Bearer", api_key),
                "Content-Type"  = "application/json") |>
    req_body_json(list(model       = model,
                       messages    = api_msgs,
                       max_tokens  = 1024,
                       temperature = 0.4)) |>
    req_timeout(60) |>
    req_perform()
  
  body <- resp_body_json(resp)
  list(
    content = body$choices[[1]]$message$content,
    usage   = body$usage
  )
}

# =============================================================================
# Markdown -> HTML formatter  
# =============================================================================
parse_md_table <- function(lines) {
  lines <- lines[!grepl("^\\s*\\|[-| :]+\\|\\s*$", lines)]
  rows  <- lapply(lines, function(l) {
    cells <- strsplit(trimws(gsub("^\\|(.*)\\|$", "\\1", l)), "\\|")[[1]]
    paste0("<td style='padding:6px 12px;border:1px solid #ddd;'>",
           trimws(cells), "</td>", collapse = "")
  })
  hdr <- paste0("<th style='padding:6px 12px;border:1px solid #ddd;background:#f3eaf6;'>",
                strsplit(trimws(gsub("^\\|(.*)\\|$","\\1",lines[1])),"\\|")[[1]],
                "</th>", collapse="")
  body_rows <- if (length(rows) > 1) paste0("<tr>", rows[-1], "</tr>", collapse="") else ""
  paste0("<div style='overflow-x:auto;margin:10px 0;'>",
         "<table style='border-collapse:collapse;font-size:13.5px;width:auto;'>",
         "<thead><tr>", hdr, "</tr></thead>",
         "<tbody>", body_rows, "</tbody></table></div>")
}

fmt_msg <- function(txt) {
  txt <- gsub("(?m)^[|\\s]*[-|: ]+[|\\s]*$", "", txt, perl = TRUE)
  txt <- gsub("^#{1,4} +",  "", txt)       # strip leading # symbols
  txt <- gsub("\n#{1,4} +", "\n", txt)     # strip # after newlines
  
  txt <- gsub("\\*\\*(.+?)\\*\\*", "\\1", txt)          # strip **bold**
  txt <- gsub("(?<!\\*)\\*(?!\\*)(.+?)(?<!\\*)\\*(?!\\*)", "\\1", txt, perl=TRUE)  # strip *italic*
  txt <- gsub("`([^`]+)`", "<code>\\1</code>", txt)
  txt <- gsub("\\[([^\\]]+)\\]\\((https?://[^)]+)\\)",
              "<a href=\\'\\2\\' target=\\'_blank\\'>\\1</a>", txt)
  lines <- strsplit(txt, "\n")[[1]]
  
  # Helper: find next non-blank line index from position k
  next_nonblank <- function(k) {
    while (k <= length(lines) && nchar(trimws(lines[k])) == 0) k <- k + 1L
    k
  }
  
  out <- character(0); in_ol <- FALSE; in_ul <- FALSE; in_nested_ul <- FALSE; i <- 1L
  while (i <= length(lines)) {
    ln <- lines[i]
    
    # ── Markdown table block ──
    if (grepl("^\\s*\\|", ln)) {
      j <- i; while (j <= length(lines) && grepl("^\\s*\\|", lines[j])) j <- j + 1
      if (in_nested_ul) { out <- c(out, "</ul>"); in_nested_ul <- FALSE }
      if (in_ol) { out <- c(out, "</ol>"); in_ol <- FALSE }
      if (in_ul) { out <- c(out, "</ul>"); in_ul <- FALSE }
      out <- c(out, parse_md_table(lines[i:(j-1)])); i <- j; next
    }
    
    # ── Numbered list item (e.g. "1. Eigenvalues:") ──
    if (grepl("^\\d+\\.\\s", ln)) {
      if (in_nested_ul) { out <- c(out, "</ul>"); in_nested_ul <- FALSE }
      if (in_ul)  { out <- c(out, "</ul>"); in_ul <- FALSE }
      if (!in_ol) { out <- c(out, "<ol>"); in_ol <- TRUE }
      out <- c(out, paste0("<li>", sub("^\\d+\\.\\s*", "", ln), ""))
      # Don't close </li> yet — sub-bullets may follow
      # Look ahead: if next content is bullets or blank+bullets, keep <li> open
      nxt <- next_nonblank(i + 1L)
      if (nxt <= length(lines) && grepl("^[-\\*]\\s", lines[nxt], perl = TRUE)) {
        # Sub-bullets will follow; leave <li> open
      } else {
        out[length(out)] <- paste0(out[length(out)], "</li>")
      }
      i <- i + 1L; next
    }
    
    # ── Bullet/dash item (e.g. "- PC1 has...") ──
    if (grepl("^[-\\*]\\s", ln, perl=TRUE)) {
      bullet_text <- sub("^[-\\*]\\s*", "", ln, perl=TRUE)
      if (in_ol) {
        # Inside an ordered list: nest as sub-list within current <li>
        if (!in_nested_ul) { out <- c(out, "<ul>"); in_nested_ul <- TRUE }
        out <- c(out, paste0("<li>", bullet_text, "</li>"))
      } else {
        # Standalone bullet list
        if (!in_ul) { out <- c(out, "<ul>"); in_ul <- TRUE }
        out <- c(out, paste0("<li>", bullet_text, "</li>"))
      }
      i <- i + 1L; next
    }
    
    # ── Blank line ──
    if (nchar(trimws(ln)) == 0) {
      # Inside a list: skip blank lines if the list continues after them
      if (in_ol || in_ul || in_nested_ul) {
        nxt <- next_nonblank(i + 1L)
        if (nxt <= length(lines)) {
          nxt_ln <- lines[nxt]
          nxt_is_ol <- grepl("^\\d+\\.\\s", nxt_ln)
          nxt_is_ul <- grepl("^[-\\*]\\s", nxt_ln, perl = TRUE)
          if (in_ol && (nxt_is_ol || nxt_is_ul)) {
            # Close nested <ul> and current <li> before next numbered item
            if (nxt_is_ol && in_nested_ul) {
              out <- c(out, "</ul>"); in_nested_ul <- FALSE
              out <- c(out, "</li>")
            }
            i <- i + 1L; next
          }
          if (in_ul && nxt_is_ul) { i <- i + 1L; next }
        }
      }
      # Otherwise close open lists and emit <br>
      if (in_nested_ul) { out <- c(out, "</ul>"); in_nested_ul <- FALSE; out <- c(out, "</li>") }
      if (in_ol) { out <- c(out, "</ol>"); in_ol <- FALSE }
      if (in_ul) { out <- c(out, "</ul>"); in_ul <- FALSE }
      # Avoid consecutive <br> tags
      if (length(out) == 0 || out[length(out)] != "<br>") out <- c(out, "<br>")
      i <- i + 1L; next
    }
    
    # ── Regular text ──
    if (in_nested_ul) { out <- c(out, "</ul>"); in_nested_ul <- FALSE; out <- c(out, "</li>") }
    if (in_ol) { out <- c(out, "</ol>"); in_ol <- FALSE }
    if (in_ul) { out <- c(out, "</ul>"); in_ul <- FALSE }
    if (grepl("^<div ", ln)) out <- c(out, ln)
    else out <- c(out, paste0("<p>", ln, "</p>"))
    i <- i + 1L
  }
  # Close any remaining open tags
  if (in_nested_ul) { out <- c(out, "</ul>"); out <- c(out, "</li>") }
  if (in_ol) out <- c(out, "</ol>")
  if (in_ul) out <- c(out, "</ul>")
  paste(out, collapse="")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b