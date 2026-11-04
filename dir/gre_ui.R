## ============================================================================
##  RAISINS Growth Rate Estimation — Quick Tour  (ported from RAISINS RBD)
##  Tour options, events and look come from greyskull (raisins_run_tour);
##  only the steps and the welcome / complete / skipped boxes are this app's.
## ============================================================================

## Welcome "take a tour?" box, shown once after login (placed in gre_ui()).
gre_welcome_overlay <- function() {
  tags$div(
    id = "welcome_modal_overlay",
    onclick = "if(event.target === this) this.style.display = 'none';",
    style = "display:none; position:fixed; top:0; left:0; width:100%; height:100%;
           background:rgba(0,0,0,0.55); z-index:9999; justify-content:center;
           align-items:center;",
    tags$div(
      style = "background:#fff; border-radius:12px; padding:40px 36px; max-width:500px;
             width:90%; box-sizing:border-box; max-height:90vh; overflow:auto;
             text-align:center; box-shadow:0 8px 32px rgba(0,0,0,0.25);
             font-family:sans-serif;",

      # Dynamic greeting injected by JS
      tags$h3(id = "welcome_user_name",
              HTML("Welcome! &#128075;"),
              style = "color:#6f2c91; margin-bottom:6px;"),

      tags$h4("Welcome to RAISINS Growth Rate Estimation!",
              style = "color:#444; margin-bottom:10px; font-weight:normal;"),

      tags$p("Would you like a quick guided tour of the app before you begin?",
             style = "color:#555; font-size:14px; margin-bottom:24px;"),

      tags$div(
        actionButton(
          "welcome_skip_tour",
          HTML("&#9889; Go straight to analysis"),
          style = "flex:1 1 180px; background:#28a745; color:#ffffff; border:none;
             padding:10px 20px; border-radius:8px; font-size:14px; cursor:pointer;"
        ),
        actionButton(
          "welcome_take_tour",
          HTML("&#128506; Give me a quick tour"),
          style = "flex:1 1 180px; background:#6f2c91; color:#ffffff; border:none;
             padding:10px 20px; border-radius:8px; font-size:14px; cursor:pointer;"
        ),
        style = "display:flex; flex-wrap:wrap; gap:14px; justify-content:center;"
      )
    )
  )
}

## ============================================================================
##  RAISINS Growth Rate Estimation — Analysis tab UI  (gre_ui)
##  Layout follows RAISINS RBD (Single Universe): centred pill subtabs, the
##  sidebar inside "Analysis Results", an options well above the results.
##  Shared input IDs reused so the greyskull invoice / View Data / login wiring
##  keeps working unchanged:
##     rcbd_file1  (upload)   rcbd_submit (run)   demo_data_select (demo picker)
##     rcbd_ViewData / rcbd_instr_filecheck (View Data)   instr_bttns_FAQs (FAQs)
##     rcbd_logout (logout fab rendered by greyskull::raisins_page)
## ============================================================================

# Calculator GIF loader for results tables (RBD's rcbd_calculator_loader);
# its sizing CSS is in ui.R. Plots keep the plain spinner, as in RBD.
gre_calculator_loader <- function(output) {
  div(
    class = "rcbd-calculator-loader",
    shinycustomloader::withLoader(
      output,
      type = "image",
      loader = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fcrd.GIF?alt=media",
      proxy.height = "0px"
    )
  )
}

gre_ui <- function() {
  fluidPage(
    titlePanel(tags$div(tags$b("Growth Rate Estimation & Trend Analysis", style = "color:#000000"))),
    # Dynamic login page----
    useShinyjs(),
    introjsUI(),
    raisins_login_card(),
    ############################### suppress warning messages
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    ################ centering tabset panel
    tags$head(
      tags$style(HTML("
ul.nav-pills{
  display: flex !important;
  justify-content: center !important;
}
#subtabs { flex-wrap: wrap; }
.gre-analysis-options { display:flex; flex-wrap:wrap; }
.gre-analysis-options > .col-sm-3 { flex:1 1 160px; min-width:0; }
#gre_mainpanel { min-width:0; }

/*pickerinput styles*/
.brand-picker1 button.btn.dropdown-toggle.btn-default {
  background-color: #b8fffa;
  color: black;
}
.brand-picker2 button.btn.dropdown-toggle.btn-default {
  background-color: #e4ccff;
  color: black;
}
.brand-picker3 button.btn.dropdown-toggle.btn-default {
  background-color: #ecffa6;
  color: black;
}

/* Guided picker highlight (as in RBD); @keyframes violet-neon-pulse is in greyskull.css */
.brand-picker1.violet-neon-highlight button.dropdown-toggle,
.brand-picker2.violet-neon-highlight button.dropdown-toggle,
.brand-picker3.violet-neon-highlight button.dropdown-toggle {
  background-color: #b8fffa;
  color: black;
  border: 2px solid #a855f7 !important;
  box-shadow:
    0 0 0 3px #7c3aed,
    0 0 18px 6px rgba(139,92,246,0.7),
    0 0 40px 14px rgba(167,139,250,0.45) !important;
  animation: violet-neon-pulse 1s infinite alternate;
}
")),
      tags$script(HTML("
// Scroll only the Shiny app or its nearest scroll container, never the hosting page.
// Used by the error-support alert to bring the assistance form into view.
function raisinsScrollTo(el, block) {
  if (!el) return;
  var parent = el.parentElement;
  while (parent && parent !== document.body && parent !== document.documentElement) {
    var oy = window.getComputedStyle(parent).overflowY;
    if ((oy === 'auto' || oy === 'scroll') && parent.scrollHeight > parent.clientHeight) break;
    parent = parent.parentElement;
  }
  var useWindow = !parent || parent === document.body || parent === document.documentElement;
  var rect = el.getBoundingClientRect();
  var viewTop = useWindow ? 0 : parent.getBoundingClientRect().top;
  var viewH   = useWindow ? window.innerHeight : parent.clientHeight;
  var offset  = rect.top - viewTop;
  if (block === 'center') offset -= (viewH - rect.height) / 2;
  if (useWindow) {
    window.scrollTo({ top: window.pageYOffset + offset, behavior: 'smooth' });
  } else {
    parent.scrollTo({ top: parent.scrollTop + offset, behavior: 'smooth' });
  }
}

// Guided picker walk-through (as in RBD). The pickers come from renderUI, so
// retry until the element exists.
Shiny.addCustomMessageHandler('highlightControl', function(message) {
  var id = message.id;
  function tryVioletHighlight(attempts){
    if(attempts === undefined) attempts = 0;
    var sel = document.getElementById(id);
    if(sel){
      var container = sel.closest('.shiny-input-container') ||
                      sel.closest('.form-group') || sel;
      var brandWrapper = container.closest('.brand-picker1') ||
                         container.closest('.brand-picker2') ||
                         container.closest('.brand-picker3') ||
                         container;
      var btn = brandWrapper.querySelector('button.dropdown-toggle') ||
                container.querySelector('button.dropdown-toggle') ||
                brandWrapper.querySelector('button') ||
                container.querySelector('button') ||
                container;
      $('.violet-neon-highlight').removeClass('violet-neon-highlight');
      brandWrapper.classList.add('violet-neon-highlight');
      if(btn){ btn.classList.add('violet-neon-highlight'); }
      raisinsScrollTo(brandWrapper, 'center');
    } else if(attempts < 30){
      setTimeout(function(){ tryVioletHighlight(attempts+1); }, 200);
    }
  }
  tryVioletHighlight(0);
});

Shiny.addCustomMessageHandler('removeHighlight', function(message){
  $('.violet-neon-highlight').removeClass('violet-neon-highlight');
});

// Calculator loader: shinycustomloader looks for '.recalculating' when Shiny
// fires shiny:recalculating, but Shiny 1.14 adds that class afterwards, so the
// GIF never opened. Drive it from the event's own output instead.
// Deferred one tick so the other loader scripts' handlers have already run.
$(document).on('shiny:recalculating', function (e) {
  var out = $(e.target);
  if (!out.closest('.rcbd-calculator-loader').length) return;
  setTimeout(function () {
    if (!out.hasClass('recalculating')) return;
    out.siblings('.load-container').removeClass('shiny-spinner-hidden').css('display', 'block');
    out.css('visibility', 'hidden');
  }, 0);
});
$(document).on('shiny:value shiny:error', function (e) {
  var out = $(e.target);
  if (!out.closest('.rcbd-calculator-loader').length) return;
  setTimeout(function () {
    out.siblings('.load-container').css('display', 'none');
    out.css('visibility', 'visible');
  }, 0);
});

// Series is a multi-select: advance only when its dropdown closes with a choice.
$(document).on('hidden.bs.select', '#gre_series', function () {
  var selected = $('#gre_series').val();
  if(selected && selected.length > 0){
    Shiny.setInputValue('variables_dropdown_closed', Math.random());
  }
});
"))
    ),
    gre_welcome_overlay(),
    ######################
    tabsetPanel(
      id = "subtabs",
      type = "pills",
      tabPanel("Analysis Results",
        icon = icon("american-sign-language-interpreting"),
        tags$br(),
        sidebarPanel(
          id = "sidebar",
          uiOutput("uploadfile"),
          uiOutput("gre_year_ui"),
          uiOutput("gre_series_ui"),
          uiOutput("gre_group_ui"),
          uiOutput("gre_modules_ui"),
          tags$br(),
          conditionalPanel("output.gre_can_run === true",
            div(id = "rcbd_submit_wrapper", style = "display:flex;justify-content:center;",
                actionBttn(inputId = "rcbd_submit", label = "Run Analysis!",
                           color = "danger", style = "jelly"))),
          uiOutput("gre_sidebar_msg"),
          tags$br(),
          tags$br(),
          tags$br(),
          HTML(paste(
            "<br/>",
            "<span style='font-size: small;'>Developed and maintained by:</span><br/>"
          )),
          div(
            style = "text-align: center;",
            img(src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fstatoberry.png?alt=media", width = "200px", height = "50px"),
            HTML(paste(
              "<div style='display: block;'>",
              "<a href='http://www.statoberry.com' style='font-family: \"Times New Roman\", Times, serif; color: blue; text-decoration: none;'>www.statoberry.com</a>",
              "</div>",
              "<div style='display: block; margin-top: 10px;'>",
              "<i class='fas fa-question-circle' style='color: #343aeb;'></i> Help at: <a href='mailto:statistician@statoberry.com' style='font-family: \"Times New Roman\", Times, serif; color:#343aeb'>statistician@statoberry.com</a>",
              "</div>"
            ))
          ),
          style = "background-color: #fff7db;"
        ),
        mainPanel(
          id = "gre_mainpanel",
          uiOutput("gre_support_panel"),
          uiOutput("gre_checks_ui"),
          uiOutput("gre_run_hint"),
          conditionalPanel(
            condition = "output.submitClicked == true",
            wellPanel(
              fluidRow(class = "gre-analysis-options",
                column(width = 3,
                       selectInput("gre_alpha", "Level of significance (α):",
                                   choices = c("0.05", "0.01", "0.10"), selected = "0.05")),
                column(width = 3,
                       selectInput("gre_font", "Select Font:",
                                   choices = raisins_font_choices, selected = "cambria")),
                column(width = 3,
                       selectInput("gre_table_style", "Table Style:",
                                   choices = raisins_tstyle_choices, selected = "classic"))
              ),
              style = "background-color: #eef2da; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
            ),
            # RBD's wait note, shown until the results have been calculated.
            conditionalPanel(
              condition = "!output.gre_results_ready",
              div(
                style = "display: flex; justify-content: center; align-items: center; margin: 6px 0; gap: 10px;",
                div(
                  style = "
        border: 4px solid #f3f3f3; /* Light grey */
        border-top: 4px solid #3498db; /* Blue */
        border-radius: 50%;
        width: 30px;
        height: 30px;
        animation: spin 1s linear infinite;
      ",
                  tags$style(HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"))
                ),
                span(
                  style = "color:#666; font-style: italic;",
                  "If your dataset is correctly formatted for growth rate analysis, the results will appear shortly. Please wait while the analysis is being performed."
                )
              )
            ),
            tags$br(),
            gre_calculator_loader(DT::DTOutput("gre_growth_table")),
            tags$br(),
            uiOutput("gre_growth_note")
          )
        )
      ),

      tabPanel("Trend Fitting", value = "Trend",
        icon = icon("chart-line"),
        conditionalPanel(
          condition = "output.submitClicked == true",
          tags$br(),
          gre_calculator_loader(DT::DTOutput("gre_trend_table")),
          tags$br(),
          gre_trend_workspace_ui()
        )
      ),

      tabPanel("Trend Test", value = "MannKendall",
        icon = icon("arrow-trend-up"),
        conditionalPanel(
          condition = "output.submitClicked == true",
          tags$br(), uiOutput("gre_mk_intro"),
          gre_calculator_loader(DT::DTOutput("gre_mk_table"))
        )
      ),

      tabPanel("Instability", value = "Instability",
        icon = icon("wave-square"),
        conditionalPanel(
          condition = "output.submitClicked == true",
          tags$br(), uiOutput("gre_instab_intro"),
          gre_calculator_loader(DT::DTOutput("gre_instab_table"))
        )
      ),

      tabPanel("Decomposition", value = "Decomposition",
        icon = icon("chart-pie"),
        conditionalPanel(
          condition = "output.submitClicked == true",
          tags$br(), uiOutput("gre_decomp_intro"),
          gre_calculator_loader(DT::DTOutput("gre_decomp_table")),
          tags$br(), uiOutput("gre_decomp_note")
        )
      ),

      ## Same flow as RBD's Interpretation tab: robot check, button, typewriter
      ## with Copy / Quick, and the review warning.
      tabPanel("Interpretation", value = "Interpretation",
        icon = icon("comment-dots"),
        tags$br(),
        conditionalPanel(
          condition = "output.submitClicked == true",
          tags$div(id = "var_AI_path_intro_gre",
            tags$h3(
              style = "color: purple; text-align: justify; font-weight: bold;",
              "Welcome to RAISINS Assistant, your personal statistical interpretation assistant!"),
            tags$p(style = "color: blue; text-align: justify;",
              "With RAISINS Assistant, you can receive insightful and accurate interpretations of your data. Whether you're a student, researcher, or simply looking to gain better insights from your data, RAISINS Assistant has got you covered. Click on the checkbox below to ensure you are not a robot.")
          ),
          tags$div(id = "var_checkbox_path_gre",
            checkboxInput("checkforAI_gre",
              "I'm not a robot and I have checked that on running analysis there was no error reported",
              FALSE)
          ),
          uiOutput("var_AI_path_actionbttn_gre"),
          tags$style("
  #loading-spinner_gre {
    position: absolute;
    top: 60%;
    left: 50%;
    margin-top: -20px;
    margin-left: -20px;
    border: 4px solid #f3f3f3;
    border-top: 4px solid #3498db;
    border-radius: 50%;
    width: 40px;
    height: 40px;
    animation: spin 2s linear infinite;
  }
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  #loading-text_gre {
    position: absolute;
    top: 70%;
    left: 50%;
    transform: translate(-50%, -50%);
    font-size: 20px;
    font-weight: bold;
    text-align: center;
    color: #3498db;
  }
  #output_path_typing_gre {
    white-space: pre-line;
  }
"),
          conditionalPanel(
            condition = "output.gre_interpretation_ready && input.checkforAI_gre && document.getElementById('output_path_typing_gre') && document.getElementById('output_path_typing_gre').textContent == ''",
            tags$div(id = "loading-spinner_gre"),
            tags$div(id = "loading-text_gre", "Yes boss I'm reading your data, it will take few minutes, please wait...")
          ),
          tags$br(),
          conditionalPanel(
            condition = "output.gre_interpretation_ready && input.checkforAI_gre",
            wellPanel(
              uiOutput("AI_typing_path_gre"),
              tags$div(id = "output_path_typing_gre"),
              style = "background-color: #ffe8b8; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
            )
          ),
          tags$br(),
          tags$br(),
          tags$br(),
          uiOutput("var_AI_path_end_gre"),
          tags$br(),
          tags$br()
        )
      ),

      ## Same look as RBD's raisins_full_report_ui(); the body renders only after Run.
      tabPanel("Statistical Report", value = "Report",
        icon = icon("file-lines"),
        tags$style(HTML(paste0(
          '#gre_report_body{max-width:1180px;margin:24px auto 36px;line-height:1.55;} ',
          '.stat-report-heading{font-family:Cambria,Georgia,serif;font-size:29px;color:#293c31;margin:0 0 10px;} ',
          '.stat-report-intro{color:#59655e;margin-bottom:24px;max-width:960px;} ',
          '.stat-report-grid{display:grid;grid-template-columns:minmax(0,1.6fr) minmax(285px,1fr);gap:24px;align-items:start;} ',
          '.stat-report-panel{border:1px solid #dce3dc;border-radius:6px;padding:20px 22px;background:#fff;min-width:0;} ',
          '.stat-report-panel legend{font-family:Cambria,Georgia,serif;font-size:19px;font-weight:600;color:#31483b;border:0;margin:0 0 4px;padding:0;width:auto;} ',
          '.stat-report-note{font-size:13px;color:#6b756f;margin-bottom:16px;} ',
          '.stat-report-tables .shiny-options-group{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px 14px;} ',
          '.stat-report-panel .checkbox{margin:0;padding:11px 10px;border:1px solid #e5e9e4;border-radius:4px;background:#fcfdfb;} ',
          '.stat-report-panel .checkbox:focus-within,.stat-report-panel .checkbox:hover{border-color:#a8bcae;background:#f5f8f3;} ',
          '.stat-report-panel .checkbox label{padding-left:24px;display:block;line-height:1.45;} ',
          '.stat-report-panel input[type=checkbox]{accent-color:#276344;margin-top:5px;} ',
          '.stat-report-option-title{display:block;font-size:15px;font-weight:600;color:#304338;} ',
          '.stat-report-option-description{display:block;font-size:12px;color:#738078;font-weight:400;margin-top:3px;} ',
          '.stat-report-sections .shiny-options-group{display:grid;gap:9px;} ',
          '.stat-report-export{margin-top:24px;padding:18px 22px;border:1px solid #cbd7cd;border-radius:4px;background:#f6f8f4;display:flex;align-items:center;justify-content:space-between;gap:24px;flex-wrap:wrap;} ',
          '.stat-report-export .shiny-input-container{margin-bottom:0;} .stat-report-export .btn{padding:11px 22px;font-weight:600;} ',
          '@media(max-width:850px){.stat-report-grid{grid-template-columns:1fr}.stat-report-export{justify-content:center}} ',
          '@media(max-width:520px){.stat-report-tables .shiny-options-group{grid-template-columns:1fr}.stat-report-panel{padding:16px}}'))),
        uiOutput("gre_report_body")
      )
    )
  )
}
