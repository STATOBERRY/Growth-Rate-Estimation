# =============================================================================
# RA-One for Growth Rate Estimation (server). Same chat, consent, access gate, quota and
# formatting as CRD's chat_server(); the CRD-only helpers (ANOVA plots, CRD
# templates, replication advice, ANOVA table cards) are not used here.
# Helpers (quota, OpenAI call, fmt_msg) are in chat_server.R.
# =============================================================================

chat_server <- function(id,
                        context        = reactive(NULL),
                        api_key        = OPENAI_API_KEY,
                        model          = "gpt-4o-mini",
                        user_rv        = reactive(NULL),
                        plan_rv        = reactive(NULL),
                        email_rv       = reactive(NULL),
                        parent_session = NULL,
                        error_support  = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- State -----------------------------------------------------------------
    chat_history    <- reactiveVal(list())
    is_waiting      <- reactiveVal(FALSE)
    if(!is.null(error_support)) {
      observe <- error_support$observe
      observeEvent <- error_support$observeEvent
      downloadHandler <- error_support$downloadHandler
      shiny::observeEvent(error_support$current(), {
        is_waiting(FALSE)
      }, ignoreInit=TRUE)
    }
    conv_counter    <- reactiveVal(1)
    consent_given   <- reactiveVal(FALSE)
    pending_msg     <- reactiveVal(NULL)

    dash_uid <- reactive({
      em <- tryCatch(email_rv(), error = function(e) NULL)
      if (is.null(em) || !nzchar(trimws(em))) return(NULL)
      digest::digest(tolower(trimws(em)), algo = "sha256")
    })

    # -- Consent box on the first visit with results (as CRD) ------------------
    observe({
      req(!is.null(parent_session))
      tab <- parent_session$input$tabs
      req(identical(tab, "RA-One"))

      ctx <- tryCatch(context(), error = function(e) NULL)
      req(!is.null(ctx) && nchar(trimws(ctx)) > 0)

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
    })

    # -- System prompt ---------------------------------------------------------
    system_prompt <- reactive({
      usr  <- tryCatch(user_rv(), error = function(e) NULL)
      plan <- tryCatch(plan_rv(), error = function(e) NULL)
      ctx  <- tryCatch(context(), error = function(e) NULL)
      get_gre_system_prompt(user = usr, plan = plan, context = ctx)
    })

    # -- Access gate (as CRD: institutional plans) -----------------------------
    is_allowed <- reactive({
      plan <- tryCatch(plan_rv(), error = function(e) NULL)
      if (is.null(plan) || nchar(trimws(plan)) == 0) return(FALSE)
      grepl("Inst plan", plan, ignore.case = TRUE)
    })

    # ==========================================================================
    # SIDEBAR / TOPBAR / WELCOME
    # ==========================================================================
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

    chip <- function(prompt, title, text)
      div(class = "rch-chip", `data-prompt` = prompt, tags$strong(title), text)
    bot_icon <- function()
      div(class = "rch-wlc-icon",
          tags$img(src = BOT_IMG,
                   style = "width:100%;height:100%;object-fit:cover;border-radius:14px;",
                   onerror = "this.style.display='none'"))

    # ==========================================================================
    # MESSAGE RENDERING
    # ==========================================================================
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
                  tags$br(), tags$strong("Institutional"), " users.",
                  tags$br(), tags$br(),
                  "Please contact your institution or RAISINS to access AI-powered analysis.")))

        if (!has_ctx) return(
          div(id = ns("welcome"),
              bot_icon(),
              div(class = "rch-wlc-h", "RAISINS Assistant"),
              div(class = "rch-wlc-p", greet, tags$br(),
                  "No analysis results loaded yet.", tags$br(), tags$br(),
                  tags$strong("To get started:"),
                  tags$ol(style = "text-align:left;margin-top:10px;",
                          tags$li("Go to the ", tags$strong("Data & Settings"), " tab of Analysis"),
                          tags$li("Upload your data file (CSV or Excel)"),
                          tags$li("Select the year column and the series, and tick the analyses you need"),
                          tags$li("Click ", tags$strong("Run Analysis!"))),
                  tags$br(),
                  "Or ask me about ", tags$strong("growth rates"),
                  ", ", tags$strong("trend tests"),
                  ", or ", tags$strong("data preparation"), "!"),
              div(class = "rch-chips",
                  chip("What is the difference between CGR and CAGR?", "CGR and CAGR",
                       "How the two growth rates differ."),
                  chip("What do the Mann-Kendall test and Sen's slope tell me?", "Trend test",
                       "Direction of a trend and its size per year."),
                  chip("How should I arrange my yearly data?", "Data preparation",
                       "One row per year, with a column for each series."),
                  chip("What is the difference between CV and the Cuddy-Della Valle Index?", "Instability",
                       "Variation with and without the trend."))))

        # -- Ready state with chips -------------------------------------------
        return(
          div(id = ns("welcome"),
              bot_icon(),
              div(class = "rch-wlc-h", "RAISINS Assistant"),
              div(class = "rch-wlc-p", greet, tags$br(),
                  "Your analysis results are loaded. Ask me anything!"),
              div(class = "rch-chips",
                  chip("Interpret my results", "Interpret results",
                       "Summarise the growth and trend of each series."),
                  chip("Which series grew fastest, and is that growth reliable?", "Compare series",
                       "Growth rates with their intervals and checks."),
                  chip("Explain the trend test and the best-fitting trend for my data", "Trend",
                       "Mann-Kendall, Sen's slope and AICc."),
                  chip("How should I report these results in a paper?", "Report results",
                       "Help me write up the results section."))))
      }

      # -- Build message rows -------------------------------------------------
      rows <- lapply(msgs, function(m) {
        is_user <- m$role == "user"
        av <- div(class = paste("rch-av", if (is_user) "user" else "ai"),
                  if (is_user) "You"
                  else tags$img(src = BOT_IMG,
                                style = "width:100%;height:100%;object-fit:cover;border-radius:50%;",
                                onerror = "this.outerHTML='R1'"))
        div(class = paste("rch-row", if (is_user) "u-row" else "a-row"),
            av,
            div(class = "rch-body",
                div(class = "rch-name", if (is_user) "You" else "RAISINS Assistant"),
                tagList(div(class = "rch-text", HTML(fmt_msg(m$content %||% ""))),
                        m$extra_ui)))
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

    push_reply <- function(text, extra_ui = NULL) {
      chat_history(c(chat_history(), list(list(
        role = "assistant", content = text, extra_ui = extra_ui))))
    }

    # ==========================================================================
    # SEND - step 1: checks, then paint the typing dots
    # ==========================================================================
    observeEvent(input[["send_click"]], {
      req(!is_waiting())
      msg <- trimws(input[["send_click"]]$msg %||% "")
      req(nchar(msg) > 0)

      # Access gate
      if (!isTRUE(is_allowed())) {
        chat_history(c(chat_history(), list(list(role = "user", content = msg))))
        push_reply(paste0(
          "The AI Assistant is available only to **Institutional** users. ",
          "Please contact your institution or RAISINS to access this feature."))
        return()
      }

      # 200-word limit on user messages
      word_count <- length(strsplit(trimws(msg), "\\s+")[[1]])
      if (word_count > 200L) {
        chat_history(c(chat_history(), list(list(role = "user", content = msg))))
        push_reply(paste0(
          "Your message has **", word_count, " words**, which exceeds the ",
          "**200-word limit**. Please shorten your message and try again."))
        return()
      }

      chat_history(c(chat_history(), list(list(role = "user", content = msg))))
      is_waiting(TRUE)
      pending_msg(msg)
      session$sendCustomMessage("triggerCrdApi", list())
    })

    # ==========================================================================
    # SEND - step 2: API call  (after typing dots are painted)
    # ==========================================================================
    observeEvent(input$crd_do_api, {
      on.exit(is_waiting(FALSE), add=TRUE)
      if(!is.null(error_support) && identical(isolate(error_support$current())$context,'RA-One')) error_support$clear()
      msg <- pending_msg(); req(!is.null(msg)); pending_msg(NULL)

      # -- Chat enable/disable gate ---------------------------------------------
      if (!check_global_chat_enabled_dash()) {
        push_reply("The AI assistant is temporarily unavailable. Please try again later or contact support@statoberry.com.")
        return()
      }
      uid <- dash_uid()
      tok <- tryCatch(.get_dash_token(), error = function(e) NULL)
      if (!check_user_chat_enabled_dash(uid, tok)) {
        push_reply("Your access to the AI assistant has been disabled. Please contact support@statoberry.com for assistance.")
        return()
      }

      # -- Quota gate ----------------------------------------------------------
      qr  <- check_quota_dash(uid, tok)
      if (!isTRUE(qr$ok)) {
        push_reply(qr$msg)
        return()
      }

      ctx <- tryCatch(context(), error = function(e) NULL)
      ctx_missing <- is.null(ctx) || nchar(trimws(ctx)) == 0

      if (ctx_missing) {
        result_kws <- c("my result", "my analysis", "my data", "my series",
                        "my growth", "my trend", "my cgr", "my cagr",
                        "interpret my", "my decomposition", "my instability")
        if (any(sapply(result_kws, function(k) grepl(k, tolower(msg), fixed = TRUE)))) {
          push_reply(paste0(
            "I don\u2019t have any analysis results loaded yet.\n\n",
            "Please go to the **Analysis** tab, upload your CSV file, ",
            "select the **year column** and the **series**, ",
            "and click **Run Analysis!**.\n\n",
            "If you need help preparing your data file first, just ask me!"))
          return()
        }
      }

      history <- chat_history()
      result  <- tryCatch(
        call_openai(history, system_prompt(), api_key, model),
        error = function(e) {
          if(inherits(e,'shiny.silent.error')) stop(e)
          if(!is.null(error_support)) error_support$report(e, context='RA-One',
            public_message='RA-One could not complete this request. Please try again or request assistance.')
          list(content='RA-One could not complete this request. Please try again or request assistance.', usage=NULL)
        })

      push_reply(result$content)

      # -- Update quota --------------------------------------------------------
      tokens <- as.integer(result$usage$total_tokens %||% 0L)
      if (!is.null(uid) && !is.null(tok) && tokens > 0L)
        update_quota_dash(uid, tok, tokens, qr,
                          email = tryCatch(email_rv(), error = function(e) NULL))
    })

    # -- New / Clear ----------------------------------------------------------
    observeEvent(input[["new-btn"]], {
      chat_history(list()); is_waiting(FALSE)
      conv_counter(conv_counter() + 1)
    })
    observeEvent(input$clear_btn, {
      chat_history(list()); is_waiting(FALSE)
    })

    # Paperclip button: switch to Analysis > Data & Settings and glow the file upload input
    observeEvent(input$goto_analysis_btn, {
      sess <- if (!is.null(parent_session)) parent_session else session
      updateTabsetPanel(sess, inputId = "tabs", selected = "Analysis")
      shinyjs::delay(100, {
        updateTabsetPanel(sess, inputId = "subtabs", selected = "Setup")
      })
      shinyjs::delay(300, {
        shinyjs::runjs("
          (function() {
            var el = document.getElementById('rcbd_file1') ||
                     document.getElementById('demo_data_select');
            if (!el) return;
            var box = el.closest('.shiny-input-container') || el;

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

            box.style.outline        = '2.5px solid #2E8B57';
            box.style.borderRadius   = '6px';
            box.style.animation      = 'raisinsGlowPulse 1.2s ease-in-out infinite';
            box.style.transition     = 'box-shadow 0.3s';

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
  })
}
