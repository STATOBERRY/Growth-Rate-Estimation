# =============================================================================
# RAISINS - Claude-Style Chat Module  (UI)
# File: chat_ui.R
#
# Drop into any tab:
#   tabPanel("RA-One", icon = icon("rocketchat"), chat_ui("gre_chat"))
#
# Server:
#   chat_server("gre_chat",
#     context = reactive({ paste("Results:", your_results()) }),
#     api_key = Sys.getenv("OPENAI_API_KEY")
#   )
# =============================================================================

chat_ui <- function(id) {
  ns <- NS(id)
  
  # Build the CSS string - using paste0 so %% issues are avoided entirely
  css <- paste0("

/* ============================================================
   RAISINS CHAT  - Claude-accurate layout
   ============================================================ */

#", ns("wrap"), " * { box-sizing: border-box; }
#", ns("wrap"), " .btn { line-height: 1; box-shadow: none; }

#", ns("wrap"), " {
  --bg:         #f9f5fa;
  --sidebar-bg: #2d1a30;
  --sidebar-w:  256px;
  --canvas:     #fdf8fe;
  --surface:    #FFFFFF;
  --border:     #e8d9ed;
  --bd-dark:    #3d2440;
  --txt:        #2d1a30;
  --txt2:       #5e4363;
  --txt3:       #9A9790;
  --inv:        #f2eaf5;
  --inv2:       #c18bcc;
  --accent:     #560466;
  --user-bg:    #ecdff3;
  --font:       'DM Sans','Helvetica Neue',Arial,sans-serif;
  --mono:       'DM Mono','Fira Code',monospace;
  --r:          12px;
  --rsm:        8px;
  display: flex;
  height: calc(100vh - 70px);
  min-height: 560px;
  font-family: var(--font);
  background: var(--bg);
  overflow: hidden;
}

/* -- SIDEBAR --------------------------------------------------- */
#", ns("sidebar"), " {
  width: var(--sidebar-w);
  min-width: var(--sidebar-w);
  background: var(--sidebar-bg);
  display: flex;
  flex-direction: column;
  border-right: 1px solid var(--bd-dark);
  overflow: hidden;
  flex-shrink: 0;
}

#", ns("sb-top"), " {
  padding: 16px 14px 12px;
  border-bottom: 1px solid var(--bd-dark);
  flex-shrink: 0;
}

.rch-brand {
  display: flex; align-items: center; gap: 9px;
  margin-bottom: 14px; padding: 2px 4px;
}
.rch-brand-icon {
  width: 28px; height: 28px; border-radius: 7px;
  background: var(--accent);
  display: flex; align-items: center; justify-content: center;
  font-size: 12px; font-weight: 700; color: #fff; flex-shrink: 0;
}
.rch-brand-name {
  font-size: 13.5px; font-weight: 600;
  color: var(--inv); letter-spacing: .01em;
}

#", ns("new-btn"), " {
  width: 100%;
  display: flex !important; align-items: center; gap: 8px;
  padding: 8px 12px !important;
  background: rgba(255,255,255,.06) !important;
  border: 1px solid rgba(255,255,255,.10) !important;
  border-radius: var(--rsm) !important;
  color: var(--inv) !important;
  font-family: var(--font) !important;
  font-size: 13px !important; font-weight: 450 !important;
  cursor: pointer; transition: background .15s !important;
  text-align: left !important; justify-content: flex-start !important;
}
#", ns("new-btn"), ":hover {
  background: rgba(255,255,255,.11) !important;
  border-color: rgba(255,255,255,.18) !important;
  color: #fff !important;
}

.rch-sec-label {
  padding: 16px 16px 6px;
  font-size: 11px; font-weight: 600;
  color: var(--inv2);
  text-transform: uppercase; letter-spacing: .08em;
  flex-shrink: 0;
}

#", ns("hist-list"), " {
  flex: 1; overflow-y: auto;
  padding: 4px 8px 12px;
}
#", ns("hist-list"), "::-webkit-scrollbar { width: 4px; }
#", ns("hist-list"), "::-webkit-scrollbar-thumb {
  background: rgba(255,255,255,.12); border-radius: 2px;
}

.rch-hist {
  display: flex; align-items: center; gap: 8px;
  padding: 8px 10px; border-radius: var(--rsm);
  cursor: pointer; transition: background .12s; margin-bottom: 2px;
}
.rch-hist:hover  { background: rgba(255,255,255,.07); }
.rch-hist.active { background: rgba(255,255,255,.11); }
.rch-hist .fa    { font-size: 12px; color: var(--inv2); flex-shrink: 0; }
.rch-hist-txt {
  font-size: 12.5px; color: var(--inv2);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1;
}
.rch-hist.active .rch-hist-txt { color: var(--inv); }

#", ns("sb-bottom"), " {
  padding: 12px 14px;
  border-top: 1px solid var(--bd-dark); flex-shrink: 0;
}
.rch-ctx-on {
  display: flex; align-items: center; gap: 7px;
  padding: 7px 10px;
  background: rgba(86,4,102,.15);
  border: 1px solid rgba(86,4,102,.30);
  border-radius: var(--rsm);
  font-size: 11.5px; color: #c18bcc;
}
.rch-ctx-on .cdot {
  width: 6px; height: 6px; border-radius: 50%;
  background: #560466; flex-shrink: 0;
  animation: ctxPulse 2.5s infinite;
}
@keyframes ctxPulse { 0%,100%{opacity:1} 50%{opacity:.35} }
.rch-ctx-off { font-size: 11.5px; color: var(--inv2); text-align: center; padding: 4px 0; }

/* -- CANVAS ---------------------------------------------------- */
#", ns("canvas"), " {
  flex: 1; display: flex; flex-direction: column;
  background: var(--canvas); overflow: hidden; min-width: 0;
}

#", ns("topbar"), " {
  display: flex; align-items: center;
  justify-content: space-between;
  padding: 13px 24px;
  border-bottom: 1px solid var(--border);
  background: var(--canvas); flex-shrink: 0; gap: 12px;
}
.rch-tb-title { font-size: 14px; font-weight: 500; color: var(--txt); }
.rch-tb-model {
  font-size: 12px; color: var(--txt3);
  display: flex; align-items: center; gap: 5px;
}
.rch-mdot {
  width: 5px; height: 5px; border-radius: 50%;
  background: #560466; display: inline-block;
}

/* -- Scroll area -- */
#", ns("msg-scroll"), " { flex:1; overflow-y:auto; scroll-behavior:smooth; }
#", ns("msg-scroll"), "::-webkit-scrollbar { width:6px; }
#", ns("msg-scroll"), "::-webkit-scrollbar-track { background:transparent; }
#", ns("msg-scroll"), "::-webkit-scrollbar-thumb { background:#d4b8da; border-radius:3px; }
#", ns("msg-scroll"), "::-webkit-scrollbar-thumb:hover { background:#c18bcc; }

#", ns("msg-inner"), " {
  max-width: 720px; margin: 0 auto;
  padding: 32px 24px 16px;
  display: flex; flex-direction: column; gap: 0;
}

/* -- Welcome screen -- */
#", ns("welcome"), " {
  max-width: 580px; margin: 0 auto;
  padding: 56px 24px 24px; text-align: center;
}
.rch-wlc-icon {
  width: 52px; height: 52px; border-radius: 14px;
  background: transparent;
  display: flex; align-items: center; justify-content: center;
  font-size: 22px; color: #fff; margin: 0 auto 20px;
  overflow: hidden;
}
.rch-wlc-h {
  font-size: 22px; font-weight: 600; color: var(--txt);
  margin-bottom: 10px; letter-spacing: -.02em;
  font-family: 'Lora', Georgia, serif;
}
.rch-wlc-p {
  font-size: 14px; color: var(--txt2);
  line-height: 1.65; margin-bottom: 28px;
}
.rch-chips {
  display: grid; grid-template-columns: 1fr 1fr; gap: 10px; text-align: left;
}
.rch-chip {
  background: var(--surface); border: 1px solid var(--border);
  border-radius: var(--r); padding: 13px 15px;
  font-size: 13px; color: var(--txt2); cursor: pointer;
  transition: border-color .15s, box-shadow .15s, background .15s;
  line-height: 1.5;
}
.rch-chip:hover {
  border-color: var(--accent); background: #faf0fc;
  color: var(--txt); box-shadow: 0 2px 8px rgba(86,4,102,.10);
}
.rch-chip strong {
  display: block; font-weight: 500; color: var(--txt);
  margin-bottom: 2px; font-size: 13px;
}

/* -- Message rows -- */
.rch-row {
  display: flex; gap: 14px; padding: 18px 0;
  align-items: flex-start; border-bottom: 1px solid transparent;
  animation: rchFade .2s ease-out;
}
@keyframes rchFade { from{opacity:0;transform:translateY(7px)} to{opacity:1;transform:none} }

.rch-av {
  width: 32px; height: 32px; border-radius: 50%; flex-shrink: 0;
  display: flex; align-items: center; justify-content: center;
  font-size: 12px; font-weight: 600; margin-top: 1px;
}
.rch-av.ai   { background: transparent; color: #fff; overflow: hidden; }
.rch-av.user { background: #5e4363; color: #fff; }

.rch-body { flex: 1; min-width: 0; }
.rch-name {
  font-size: 13px; font-weight: 600; color: var(--txt);
  margin-bottom: 6px; letter-spacing: .005em;
}
.rch-text {
  font-size: 14.5px; line-height: 1.72; color: var(--txt);
  word-break: break-word; overflow-wrap: break-word;
}

/* User bubble */
.rch-row.u-row { flex-direction: row-reverse; }
.rch-row.u-row .rch-body { display: flex; flex-direction: column; align-items: flex-end; }
.rch-row.u-row .rch-name  { text-align: right; }
.rch-row.u-row .rch-text  {
  display: inline-block;
  background: var(--user-bg);
  border-radius: 18px 18px 4px 18px;
  padding: 11px 17px; font-size: 14px;
  line-height: 1.6; max-width: 100%;
  word-break: break-word; overflow-wrap: break-word; white-space: pre-wrap;
}

/* AI prose */
.rch-row.a-row .rch-text p { margin: 0 0 12px; }
.rch-row.a-row .rch-text p:last-child { margin-bottom: 0; }
.rch-row.a-row .rch-text ul,
.rch-row.a-row .rch-text ol { padding-left: 22px; margin: 8px 0 12px; }
.rch-row.a-row .rch-text li  { margin-bottom: 4px; }
.rch-row.a-row .rch-text strong { font-weight: 600; }
.rch-row.a-row .rch-text code {
  font-family: var(--mono); font-size: 12.5px;
  background: #f3eaf6; border: 1px solid #dcc8e2;
  border-radius: 4px; padding: 1px 6px;
}

/* Typing animation */
.rch-typing { display:flex; gap:5px; align-items:center; padding:6px 2px; }
.rch-typing span {
  width:7px; height:7px; border-radius:50%; background:#c18bcc;
  animation: rchDot 1.4s ease-in-out infinite;
}
.rch-typing span:nth-child(2) { animation-delay:.2s; }
.rch-typing span:nth-child(3) { animation-delay:.4s; }
@keyframes rchDot { 0%,60%,100%{opacity:.2;transform:scale(.85)} 30%{opacity:1;transform:scale(1.2)} }


/* -- INPUT BAR ------------------------------------------------- */
#", ns("input-area"), " {
  padding: 14px 24px 18px;
  background: var(--canvas);
  border-top: 1px solid var(--border); flex-shrink: 0;
}

#", ns("input-pill"), " {
  max-width: 720px; margin: 0 auto;
  background: var(--surface);
  border: 1.5px solid var(--border);
  border-radius: 24px;
  display: flex; flex-direction: column;
  transition: border-color .2s, box-shadow .2s;
  box-shadow: 0 2px 10px rgba(0,0,0,.06);
}
#", ns("input-pill"), ":focus-within {
  border-color: #9e5aab;
  box-shadow: 0 0 0 4px rgba(86,4,102,.08), 0 2px 10px rgba(0,0,0,.07);
}

#", ns("input-pill"), " textarea {
  font-family: var(--font) !important;
  font-size: 14.5px !important; line-height: 1.6 !important;
  color: var(--txt) !important; background: transparent !important;
  border: none !important; outline: none !important;
  box-shadow: none !important; resize: none !important;
  padding: 14px 20px 4px !important;
  min-height: 50px; max-height: 200px; width: 100%;
  overflow-y: auto;
}
#", ns("input-pill"), " textarea::placeholder { color: #c18bcc !important; }

#", ns("pill-foot"), " {
  display: flex; align-items: center;
  justify-content: space-between;
  padding: 5px 8px 8px 14px;
}

.rch-foot-left { display:flex; align-items:center; gap:2px; }

.rch-icon-btn {
  width: 30px; height: 30px; border-radius: 7px !important;
  background: transparent !important; border: none !important;
  color: var(--txt3) !important; font-size: 13px !important;
  display: flex; align-items: center; justify-content: center;
  cursor: pointer; transition: background .12s, color .12s !important;
  box-shadow: none !important; padding: 0 !important;
}
.rch-icon-btn:hover { background: var(--bg) !important; color: var(--txt2) !important; }

#", ns("send-btn"), " {
  width: 34px; height: 34px; border-radius: 50% !important;
  background: var(--txt) !important; border: none !important;
  color: #fff !important; font-size: 13px !important;
  display: flex; align-items: center; justify-content: center;
  padding: 0 !important; cursor: pointer;
  transition: background .15s, transform .1s !important;
  box-shadow: none !important; flex-shrink: 0;
}
#", ns("send-btn"), ":hover  { background: var(--accent) !important; transform:scale(1.06) !important; }
#", ns("send-btn"), ":active { transform:scale(.95) !important; }
#", ns("send-btn"), "[disabled] { background:#d4b8da !important; cursor:not-allowed !important; transform:none !important; }

.rch-hint {
  max-width: 720px; margin: 7px auto 0;
  font-size: 11.5px; color: var(--txt3); text-align: center;
}

/* -- Responsive -- */
@media (max-width:900px) { #", ns("sidebar"), " { display:none; } }
@media (max-width:600px) {
  #", ns("msg-inner"), ", #", ns("input-pill"), " { padding-left:12px; padding-right:12px; }
  .rch-chips { grid-template-columns:1fr; }
}
")
  
  tagList(
    
    # -- Fonts ----------------------------------------------------------------
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js"),
      tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
      tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
      tags$link(rel="stylesheet",
                href="https://fonts.googleapis.com/css2?family=Lora:wght@400;600&family=DM+Sans:wght@300;400;450;500;600&family=DM+Mono:wght@400;500&display=swap"),
      tags$style(HTML(css))
    ),
    
    # -- Shell ----------------------------------------------------------------
    div(id = ns("wrap"),
        
        # ======= SIDEBAR =====================================================
        div(id = ns("sidebar"),
            
            div(id = ns("sb-top"),
                div(class = "rch-brand",
                    div(class = "rch-brand-icon", "R"),
                    div(class = "rch-brand-name", "RAISINS")
                ),
                actionButton(ns("new-btn"), class = "",
                             label = tagList(icon("plus"), " New conversation")
                )
            ),
            
            div(class = "rch-sec-label", "Recent"),
            div(id = ns("hist-list"),
                uiOutput(ns("history_items"))
            ),
            
            div(id = ns("sb-bottom"),
                uiOutput(ns("ctx_badge"))
            )
        ),
        
        # ======= CANVAS ======================================================
        div(id = ns("canvas"),
            
            # Topbar
            div(id = ns("topbar"),
                div(class = "rch-tb-title",
                    uiOutput(ns("conv_title"), inline = TRUE)
                ),
                div(class = "rch-tb-model",
                    div(class = "rch-mdot"), "RAISINS AI ENGINE"
                )
            ),
            
            # Messages
            div(id = ns("msg-scroll"),
                div(id = ns("msg-inner"),
                    uiOutput(ns("messages"))
                )
            ),
            
            # Input bar
            div(id = ns("input-area"),
                div(id = ns("input-pill"),
                    tags$textarea(
                      id          = ns("user_input"),
                      placeholder = "Ask about your analysis results...",
                      rows        = "1"
                    ),
                    div(id = ns("pill-foot"),
                        div(class = "rch-foot-left",
                            actionButton(ns("clear_btn"),
                                         label = icon("rotate-left"),
                                         class = "rch-icon-btn",
                                         title = "Clear conversation"
                            ),
                            #### Changed by Claude AI (ppg)
                            actionButton(ns("goto_analysis_btn"),
                                         label = icon("paperclip"),
                                         class = "rch-icon-btn",
                                         title = "Go to Analysis tab to upload file"
                            )
                            #### end change
                        ),
                        actionButton(ns("send-btn"),
                                     label = icon("arrow-up"),
                                     title = "Send (Enter)"
                        )
                    )
                ),
                div(class = "rch-hint",
                    "RAISINS Assistant  .  Enter to send  .  Shift+Enter for new line"
                )
            )
            
        ) # /canvas
    ),  # /wrap
    
    # -- JS -------------------------------------------------------------------
    tags$script(HTML(paste0("
(function () {
  var NS = '", ns(""), "';

  /* Auto-resize textarea */
  $(document).on('input', '#' + NS + 'user_input', function () {
    this.style.height = 'auto';
    this.style.height = Math.min(this.scrollHeight, 200) + 'px';
  });

  /* -- Send helper: reads textarea value and bundles it into one event -- */
  function doSend() {
    var ta = document.getElementById(NS + 'user_input');
    if (!ta) return;
    var msg = $.trim(ta.value);
    if (msg.length === 0) return;
    Shiny.setInputValue(NS + 'send_click',
      { msg: msg, n: Math.random() },
      { priority: 'event' });
    ta.value = '';
    ta.style.height = 'auto';
  }

  /* Enter = send */
  $(document).on('keydown', '#' + NS + 'user_input', function (e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      doSend();
    }
  });

  /* Send button = send (intercept before actionButton fires) */
  $(document).on('click', '#' + NS + 'send-btn', function (e) {
    e.preventDefault();
    e.stopImmediatePropagation();
    doSend();
  });

  /* Auto-scroll */
  var obs = new MutationObserver(function () {
    var s = document.getElementById(NS + 'msg-scroll');
    if (s) s.scrollTop = s.scrollHeight;
  });
  $(document).ready(function () {
    var t = document.getElementById(NS + 'msg-inner');
    if (t) obs.observe(t, { childList: true, subtree: true });

   /* Suggestion chip -> send straight away (no need to click Send) ##chipsend(06-10-26) */
    $(document).on('click', '.rch-chip', function () {
      var p = $(this).data('prompt');
      var ta = document.getElementById(NS + 'user_input');
      if (ta && p) {
        $('.rch-chip').css('pointer-events', 'none');  /* no double send while the chat starts */
        ta.value = p;
        doSend();
      }
    });
    /* Round-trip: server signals to trigger API after dots are painted */
    Shiny.addCustomMessageHandler('triggerCrdApi', function (_) {
      setTimeout(function () {
        Shiny.setInputValue(NS + 'crd_do_api', Math.random(), { priority: 'event' });
      }, 50);
    });

     /* Clear input from server side */
    Shiny.addCustomMessageHandler('clearChatInput', function (msg) {
      var ta = document.getElementById(msg.id);
      if (ta) { ta.value = ''; ta.style.height = 'auto'; }
    });

    /*  render math formulas after each message update */
    var mathObs = new MutationObserver(function () {
      var el = document.getElementById(NS + 'msg-inner');
      if (el && typeof renderMathInElement === 'function') {
        renderMathInElement(el, {
          delimiters: [
            {left: '\\\\[', right: '\\\\]', display: true},
            {left: '\\\\(', right: '\\\\)', display: false},
            {left: '$$',  right: '$$',  display: true},
            {left: '$',   right: '$',   display: false}
          ],
          throwOnError: false
        });
      }
    });
    var mathEl = document.getElementById(NS + 'msg-inner');
    if (mathEl) mathObs.observe(mathEl, { childList: true, subtree: true });
    /* ##end change 22-03-26 */

  });
}());
    ")))
    
  )
}
