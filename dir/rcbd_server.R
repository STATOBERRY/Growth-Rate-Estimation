library(shiny)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           rcbd_server of RAISINS RM-ANOVA                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rcbd_server = function(input, output,session,shared){
  ## Dynamic auth----
  observeEvent(shared$action_trigger, {
    if (shared$action_trigger == 0) return()
    
    current_action <- isolate(shared$last_action)
    req(current_action)
    js <- switch(current_action,
                 "individual" = '
    setTimeout(function() {
      $("#auth-shinymanager-auth-head").text("");
      $("#dynamic_link").text("Forgot password?");
      $("label[for=\'auth-user_id\']").text("E-mail ID:").show();
      $("label[for=\'auth-user_pwd\']").text("Password:");
      $("#auth-user_id").attr("placeholder", "Your email ID")
                        .prop("readonly", false)
                        .show()
                        .closest(".form-group, .form-floating, .mb-3").show();
      $("#auth-user_pwd").attr("placeholder", "Your password");
      $("#demo-hint").remove();
    }, 100);
  ',
                 
                 "institutional" = '
    setTimeout(function() {
      $("#auth-shinymanager-auth-head").text("Institutional Login");
      $("#dynamic_link").text(" ");
      $("label[for=\'auth-user_id\']").text("Inst. mail ID:").show();
      $("label[for=\'auth-user_pwd\']").text("Email OTP:");
      $("#auth-user_id").attr("placeholder", "Institutional e-mail id")
                        .prop("readonly", false)
                        .show()
                        .closest(".form-group, .form-floating, .mb-3").show();
      $("#auth-user_pwd").attr("placeholder", "OTP received in your mail");
      $("#demo-hint").remove();
    }, 100);
  ',
                 
                 "demo" = '
    setTimeout(function() {
      $("#auth-shinymanager-auth-head").text("Preview Mode");
      $("#dynamic_link").text(" ");

      // force username = demo, make it readonly, then hide the username control
      $("#auth-user_id").val("demo").prop("readonly", true).trigger("input");
      $("label[for=\'auth-user_id\']").hide();
      // hide the whole container if present (covers different bootstrap wrappers)
      $("#auth-user_id").hide()
        .closest(".form-group, .form-floating, .mb-3").hide();

      $("label[for=\'auth-user_pwd\']").text("Email OTP:");
      $("#auth-user_pwd").attr("placeholder", "OTP received in your mail");

      if (!$("#demo-hint").length) {
        $("<div id=\'demo-hint\' style=\'font-size:12px;color:#888;margin-top:4px;\'>Enter the OTP sent to your<b>Email ID</b></div>")
          .insertAfter("#auth-user_pwd");
      }
    }, 100);
  '
    )
    runjs(js)
    
    shared$last_action <- NULL
  })
  # remove email modal on cancel----
  observeEvent(input$cancel_modal, {
    removeModal()
  })
  
  # remove demo email modal on cancel----
  observeEvent(input$demo_cancel_modal, {
    removeModal()
  })
  
   # authentication module ----
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = raisins_check_credentials(shared)
  )

  ## RAISINS session kit: stay signed in + newest login wins + leave guard----
  raisins_after_login(auth, input, session, shared)
  
  # Show RA-One bubble only after login
  observe({
    req(auth$user)
    session$sendCustomMessage("show_raone_bubble", list())
    shinyjs::show("logout_fab")
  })
  
  observeEvent(session$input$rcbd_logout,{
    session$reload()
  })
  #++++++++++++++++++++++++++++++++++++ checkVals test validity 
  check_vals <- reactiveValues(
    na = NULL,
    cell_na = NULL
  )
  
  ## current subscriber----
  current_subscriber <- reactive({
    authentication <- reactiveValuesToList(auth)
    subscriber <- authentication$user_info$user
  })
  
  tour_shown <- reactiveVal(FALSE)
  run_tour <- function() {
    shinyjs::runjs("window.raisinTourCompleted = false;")
    # Tour options, events and look come from greyskull; only the steps are this app's.
    raisins_run_tour(
      session,
      steps = data.frame(
          element = c(
            NA,
            "a[data-value='Datasets']",
            "a[data-value='Arrange Data']",
            "a[data-value='RA-One']",
            "a[data-value='User']",
            "a[data-value='Analysis']",
            "#subtabs",
            "a[data-value='Analysis Results']",
            "#sidebar",
            "#rcbd_content",
            "a[data-value='Summary stats']",
            "a[data-value='Basic Plots']",
            "a[data-value='Advanced Plots']",
            "a[data-value='Individual ANOVA']",
            "a[data-value='Interpretation']",
            "a[data-value='Multivariate']",
            "a[data-value='FAQs']",
            "a[data-value='View Data']",
            "#rcbd_logout"
          ),
          intro = c(
            # ── Step 1 : Welcome ───────────────────────────────────────────────
            paste0(
              "<div style='text-align:center;'>",
              "<img src='https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fguide1.gif?alt=media' ",
              "style='height:300px; object-fit:contain; margin-bottom:2px;'>",
              "<h3 style='color:#6f2c91; margin:0; padding:0; line-height:1.2;'>
               Welcome to RAISINS Repeated Measures ANOVA!
             </h3>",
              "<p style='font-size:14px;color:#555;'>",
              "This quick tour will walk you through every section of the app.<br>",
              "Use <b>Next &#9654;</b> / <b>&#9664; Back</b> to navigate, or press <b>Esc</b> to skip.",
              "</p></div>"
            ),
            
            # ── Step 2 : Datasets ──────────────────────────────────────────────
            paste0(
              "<b>&#128193; Datasets</b><br><br>",
              "This tab contains <b>3 model datasets</b>.<br><br>",
              
              "You can <b>download these files</b> to see how the data should be ",
              "<b>structured and arranged</b> for analysis.<br><br>",
              
              "Use them as a <b>reference template</b> and organize your own dataset ",
              "<b>in the same format</b> before uploading it to the platform.<br>",
              "<span style='color:#6f2c91;'>&#128161; Still unsure about the format?</span> ",
              "Head over to the <b>Arrange Data</b> tab where you can build a ",
              "dataset template directly."
            ),
            
            # ── Step 3 : Create Data ───────────────────────────────────────────
            paste0(
              "<b>&#9997; Arrange Data</b><br><br>",
              "This tab allows you to <b>create a dataset template directly in RAISINS</b>.<br><br>",
              "Enter the number of <b>Treatments, Days, and Characters</b>, ",
              "and specify whether replications are <b>equal or unequal</b>.<br><br>",
              "Click <b>Create</b> to generate the template. You can then ",
              "<b>enter values manually</b> or <b>paste data from a spreadsheet</b>.<br><br>",
              "The completed dataset can be <b>downloaded as a CSV file</b>. This dataset you can directly upload for analysis"
            ),
            
            # ── Step 4 : RA-One ───────────────────────────────────────────
            paste0(
              "<b>&#9997; RA-One</b><br><br>",
              "This tab provides access to the <b>RAISINS AI Assistant</b>, designed to support your data analysis workflow.<br><br>",
              "You can <b>ask questions</b>, seek guidance on <b>data preparation</b>, ",
              "<b>experimental design</b>, and <b>interpretation of results</b>.<br><br>",
              "For best results, ensure your <b>analysis is completed</b> before interacting, ",
              "so the assistant can give <b>context-aware insights</b> based on your data."
            ),
            
            # ── Step 5 : User Panel ────────────────────────────────────────────
            paste0(
              "<b>&#128100; User Panel</b><br><br>",
              "This section displays your <b>RAISINS account and subscription details</b>.<br><br>",
              "You can view your <b>billing information, plan, order ID, payment method</b>, ",
              "and <b>platform usage statistics</b>.<br><br>",
              "It also allows you to <b>download your invoice</b> and review the ",
              "<b>data policy</b>."
            ),
            
            # ── Step 6 : Analysis Workspace ────────────────────────────────────
            paste0(
              "<b>&#128202; Analysis Workspace</b><br><br>",
              
              "This is the <b>main analysis workspace</b> of the module.<br>",
              "Upload your dataset, select the treatment, time period column and required variables, ",
              "and click on Run Analysis, that's it.<br><br>",
              
              "The results are organized across <b>dedicated subtabs</b> under this<br><br>",
              "Navigate through these subtabs to explore the ",
              "<b>complete analytical output</b> generated from your dataset."
            ),
            
            # ── Step 7 : Navigation Tabs ───────────────────────────────────────
            paste0(
              "<b>&#128196; Navigation Tabs</b><br><br>",
              "The top bar gives you access to all modules:<br>",
              "&#8226; <b>Analysis Results</b> &#9654; run Repeated Measures ANOVA<br>",
              "&#8226; <b>Summary stats</b> &#9654; All the summary statistics of you dataset<br>",
              "&#8226; <b>Basic Plots</b> &#9654; standard visualisations<br>",
              "&#8226; <b>Advanced Plots</b> &#9654; specialised visualisations<br>",
              "&#8226; <b>Individual ANOVA</b> &#9654; per-character results<br>",
              "&#8226; <b>Interpretation</b> &#9654; AI-powered summary<br>",
              "&#8226; <b>Multivariate</b> &#9654; MANOVA and PCA &amp; diagnostics<br>",
              "&#8226; <b>FAQs</b> &#9654; help &amp; guidance<br>",
              "&#8226; <b>View Data</b> &#9654; inspect your dataset<br>"
              
            ),
            
            # ── Step 8 : Analysis Results Tab ──────────────────────────────────
            paste0(
              "<b>&#128202; Analysis Results Tab</b><br><br>",
              "This tab serves as the <b>primary analysis workspace</b> of the Repeated Measures ANOVA module.<br><br>",
              "Upload your dataset, select the <b>required columns </b>.<br><br>",
              "Click <b>Run Analysis</b> to perform the Repeated Measures ANOVA and generate the corresponding ",
              "<b>statistical results and plots</b>."
            ),
            
            # ── Step 9 : Sidebar ───────────────────────────────────────────────
            paste0(
              "<b>&#9881;&#65039; Sidebar - Variable Setup</b><br><br>",
              "Use this panel to configure the analysis.<br><br>",
              "&#8226; Upload your dataset (CSV / Excel).<br>",
              "&#8226; Select the <b>Treatment</b>, <b>Days</b>, and <b>Response</b> variables.<br>",
              "&#8226; Apply optional <b>data transformations</b> if necessary.<br>",
              "&#8226; Click <b>Run Analysis</b> to run the Repeated Measures ANOVA."
            ),
            
            # ── Step 10 : Results Panel ─────────────────────────────────────────
            paste0(
              "<b>&#128203; Results Panel</b><br><br>",
              "Once the analysis is submitted, the <b>Repeated Measures ANOVA results</b> ",
              "including the <b>ANOVA table, and multiple comparison outputs</b> are displayed here.<br><br>",
              "Use the controls above the table to modify the ",
              "<b>multiple comparison test, significance level, ",
              "and display settings</b>."
            ),
            
            # ── Step 11 : Summary Stats Tab ────────────────────────────────────
            paste0(
              "<b>&#128290; Summary Statistics Tab</b><br><br>",
              
              "This tab presents the <b>descriptive summary statistics</b> ",
              "of the uploaded dataset.<br><br>",
              
              "It provides key statistical measures such as ",
              "<b>mean, median, minimum, maximum, standard deviation, ",
              "skewness, kurtosis</b>, and other basic indicators that ",
              "help summarize the variables.<br><br>"
            ),
            
            # ── Step 13 : Basic Plots Tab ──────────────────────────────────────
            paste0(
              "<b>&#128200; Basic Plots Tab</b><br><br>",
              "This tab generates <b>publication-ready visualisations</b>:<br><br>",
              "&#8226; Boxplot, Violin-Box, and Bar plots with error bars<br>",
              "&#8226; Line Plot and Mean &amp; Error plot<br>",
              "&#8226; Customisable colours, themes, and labels.<br><br>",
              "All figures can be <b>downloaded directly</b> for reporting or publication."
            ),
            
            # ── Step 14 : Advanced Plots Tab  (UPDATED) ────────────────────────
            paste0(
              "<b>&#127775; Advanced Plots Tab</b><br><br>",
              
              "This tab provides additional visualisations for deeper data exploration:<br><br>",
              
              "&#8226; Summary Plot<br>",
              "&#8226; Raincloud Plot<br>",
              "&#8226; Advanced Raincloud Plot<br>",
              "&#8226; Circular Plot<br>",
              "&#8226; QQ Plot<br>",
              "&#8226; Distribution Plot<br>",
              "&#8226; Pair Plot<br>",
              "&#8226; Correlation Plot<br><br>",
              
              "All plots can be customised and downloaded for reporting."
            ),
            
            # ── Step 12 : Individual ANOVA Tab  (UPDATED) ──────────────────────
            paste0(
              "<b>&#128208; Individual ANOVA Tab</b><br><br>",
              
              "This tab provides a <b>detailed character-by-character breakdown</b> ",
              "of the ANOVA results.<br><br>",
              "This tab is especially useful when you need to <b>report or interpret ",
              "results for individual traits</b> in detail."
            ),
            
            
            # ── Step 15 : Interpretation Tab ───────────────────────────────
            paste0(
              "<b>&#129302; Interpretation Tab</b><br><br>",
              "This tab provides an <b>overall interpretation</b> ",
              "of the Repeated Measures ANOVA results.<br><br>",
              
              "The generated explanation helps users <b>understand the statistical output</b> ",
              "and the meaning of the results."
            ),
            
            # ── Step 16 : Multivariate Tab ─────────────────────────────────────
            paste0(
              "<b>&#128269; Multivariate Tab</b><br><br>",
              "This section provides multivariate analysis tools such as ",
              "<b>Principal Component Analysis (PCA)</b> ",
              "to explore the multivariate structure of the dataset.<br><br>",
              "These methods help examine relationships among variables, ",
              "identify major sources of variation, and assess overall ",
              "differences across treatments considering multiple ",
              "response variables simultaneously."
            ),
            
            # ── Step 17 : FAQs Tab ─────────────────────────────────────────────
            paste0(
              "<b>&#10067; FAQs Tab</b><br><br>",
              "This section provides answers to <b>frequently asked questions</b> ",
              "related to the Repeated Measures ANOVA module.<br><br>",
              "It includes guidance on <b>data preparation, analysis workflow, ",
              "result interpretation</b>, and information on obtaining ",
              "<b>support from the RAISINS team</b>."
            ),
            
            # ── Step 18 : View Data Tab ────────────────────────────────────────
            paste0(
              "<b>&#128065;&#65039; View Data Tab</b><br><br>",
              "Use this tab to <b>inspect the uploaded dataset</b> before analysis.<br><br>",
              "&#10004; Treatment columns appear in <b>yellow/green</b>.<br>",
              "&#10004; Numeric values appear in <b>green</b>.<br><br>",
              "&#10008; Red cells indicate <b>missing values</b>, which must be corrected before analysis."
            ),
            
            # ── Step 19 : Sign Out ─────────────────────────────────────────────
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
      )
  }
  
  # ── Show the welcome choice modal on login ──────────────────────────────────
  observeEvent(current_subscriber(), {
    req(current_subscriber())
    if (tour_shown()) return()
    tour_shown(TRUE)
    if (raisins_auto_logged_in(session)) return()   # RAISINS session kit: returning user, no tour box
    
    # ── Extract the user's name ──────────────────────────────────────
    user_info <- current_subscriber()
    user_name   <- as.character(user_info)
    
    user_name <- paste0(
      toupper(substr(user_name, 1, 1)),
      substr(user_name, 2, nchar(user_name))
    )
    
    # # If email → extract part before @, else use as-is
    user_name  <- ifelse(grepl("@", user_name), sub("@.*$", "", user_name), user_name)
    # 
    # # Capitalise first letter
    # user_name  <- paste0(toupper(substring(user_name, 1, 1)), substring(user_name, 2))
    shinyjs::delay(600, {
      shinyjs::runjs(sprintf("
    var el = document.getElementById('welcome_modal_overlay');
    var nameEl = document.getElementById('welcome_user_name');
    if (nameEl) nameEl.innerHTML = 'Hello, %s! &#128075;';
    el.style.display = 'flex';
  ", user_name))
    })
  })  
  # ── User chose: Take the tour ───────────────────────────────────────────────
  observeEvent(input$welcome_take_tour, {
    shinyjs::runjs("
    document.getElementById('welcome_modal_overlay').style.display = 'none';
  ")
    shinyjs::delay(300, run_tour())
  })
  
  # ── User chose: Skip tour, go to analysis ──────────────────────────────────
  observeEvent(input$welcome_skip_tour, {
    shinyjs::runjs("
    document.getElementById('welcome_modal_overlay').style.display = 'none';
  ")
    # Optionally navigate to the Analysis tab automatically:
    updateTabsetPanel(session, "subtabs", selected = "Analysis Results")
  })
  
  # ── Retake tour button (unchanged) ─────────────────────────────────────────
  observeEvent(input$retake_tour, {
    run_tour()
  })
  
  observeEvent(input$tour_completed, {
    showModal(modalDialog(
      title = NULL,
      footer = tagList(
        tags$style(HTML("
          .btn-custom-violet {
background-color: #6610f2;
hover: #0b5ed7;
  color: white !important;
  border-color: #6f2c91 !important;
  transition: transform 0.2s ease-in-out !important;
}

.btn-custom-violet:hover {
  background-color: #5a2375 !important;
  border-color: #4a1d60 !important;
  transform: scale(1.05) !important;
}

.btn-custom-violet:active {
  animation: pulse 0.1s ease-in-out !important;
}

@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(0.95); }
  100% { transform: scale(1); }
}")),
        
        actionButton(
          inputId = "complete_tour",
          label   = HTML("&#9654; Let's Go"),
          class = "btn-custom-violet"
        )),
      
      easyClose = TRUE,
      
      tags$div(
        style = "text-align:center; padding:20px 10px; font-family:sans-serif;",
        
        tags$div(style = "font-size:52px; margin-bottom:12px;", "\U0001F389"),
        
        tags$h3(
          "Tour Complete!",
          style = "color:#6f2c91; margin-bottom:8px;"
        ),
        
        tags$p(
          "You're all set to use", tags$b("RAISINS Repeated Measures ANOVA."),
          style = "color:#555; font-size:14px; margin-bottom:6px;"
        ),
        
        tags$p(
          "Happy analysing! \U0001F31F",
          style = "color:#6f2c91; font-size:15px; font-weight:bold;"
        ),
        
        tags$hr(style = "border-color:#e8d5f5; margin:16px 0;"),
        
        tags$p(
          "\U0001F4A1 Tip: You can retake this tour anytime using the",
          tags$b("Quick Tour"), "button from the top.",
          style = "color:#888; font-size:12px;"
        )
      )
    ))
  })
  
  observeEvent(input$complete_tour, {
    removeModal()
  })
  
  observeEvent(input$tour_skipped, {
    showModal(
      modalDialog(
        title = NULL,
        footer = tagList(
          tags$style(HTML("
          .btn-custom-violet {
background-color: #6610f2;
hover: #0b5ed7;
  color: white !important;
  border-color: #6f2c91 !important;
  transition: transform 0.2s ease-in-out !important;
}

.btn-custom-violet:hover {
  background-color: #5a2375 !important;
  border-color: #4a1d60 !important;
  transform: scale(1.05) !important;
}

.btn-custom-violet:active {
  animation: pulse 0.1s ease-in-out !important;
}

@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(0.95); }
  100% { transform: scale(1); }
}")),
          
          actionButton(
            inputId = "continue_app",
            label   = HTML("&#9654; Continue to App"),
            class = "btn-custom-violet"
          )),
        # footer = modalButton("\u25B6 Continue to App"),
        easyClose = TRUE,
        
        tags$div(
          style = "text-align:center; padding:20px 10px; font-family:sans-serif;",
          
          tags$div(
            style = "font-size:48px; margin-bottom:12px;",
            HTML("&#128075;")
          ),
          
          tags$h3(
            "Tour Skipped",
            style = "color:#6f2c91; margin-bottom:8px;"
          ),
          
          tags$p(
            "No worries! You can explore",
            tags$b("RAISINS Repeated Measures ANOVA"),
            "on your own.",
            style = "color:#555; font-size:14px; margin-bottom:6px;"
          ),
          
          tags$p(
            "If you need guidance later, you can restart the tour anytime.",
            style = "color:#555; font-size:14px;"
          ),
          
          tags$hr(
            style = "border-color:#e8d5f5; margin:16px 0;"
          ),
          
          tags$p(
            HTML("&#128161;"), " Tip: Click the",
            tags$b("Quick Tour"),
            "button to start the guided walkthrough again.",
            style = "color:#888; font-size:12px;"
          )
        )
      )
    )
  })
  
  observeEvent(input$continue_app, {
    removeModal()
  })
  
  
  # inside crd server
  observeEvent(input$tabs, {
    if (raisins_signed_out(session)) return()   # RAISINS session kit: signed out by another login
    
    if (input$tabs == "retake_tour") {
      
      # 1️⃣ Switch main tab
      updateTabsetPanel(
        session,
        inputId = "tabs",
        selected = "Analysis"
      )
      
      # 2️⃣ Switch sub tab
      updateTabsetPanel(
        session,
        inputId = "subtabs",
        selected = "Analysis Results"
      )
      
      # 3️⃣ Run the tour
      run_tour()
      
    }
    
  })
  
  
  ###Multi access detection----  
  # Attach passive multi-access guard only for institutional logins
  # replaced by the RAISINS session kit (newest login wins, all users),
  # started by raisins_after_login() above
  ###
  
  
  ## create file input based on user----
  output$uploadfile <- renderUI({
    req(current_subscriber())
    
    if (current_subscriber() == "demo") {
      demo_files <- list.files("data", pattern = "\\.csv$", full.names = FALSE)
      
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
  
  
  #create a reactive to identify upload and selecetd----
  uploaded_or_selected <- reactive({
    req(current_subscriber())
    if (current_subscriber() == "demo") {
      input$demo_data_select
    } else {
      input$rcbd_file1
    }
  })
  
  #rcbdcsv----
  rcbd_csvfile <- reactive({
    req(current_subscriber())
    
    if (current_subscriber() == "demo") {
      req(input$demo_data_select)
      file_path <- file.path("data", input$demo_data_select)
      if (!file.exists(file_path)) {
        showNotification("Demo file not found.", type = "error")
        return(NULL)
      }
      dt <- read.csv(file_path, header = TRUE, sep = ",", na.strings = c(""))
    } else {
      file <- input$rcbd_file1
      if (is.null(file)) return(NULL)
      
      file_ext <- tools::file_ext(file$datapath)
      
      if (file_ext == "csv") {
        dt <- read.csv(file$datapath, header = TRUE, sep = ",", na.strings = c(""))
      } else if (file_ext %in% c("xls", "xlsx")) {
        raw_dt <- readxl::read_excel(file$datapath, na = c(""))
        temp_csv <- tempfile(fileext = ".csv")
        write.csv(raw_dt, temp_csv, row.names = FALSE)
        dt <- read.csv(temp_csv, header = TRUE, sep = ",", na.strings = c(""))
      } else {
        showNotification("Unsupported file type", type = "error")
        return(NULL)
      }
    }
    
    # Clean leading/trailing spaces in all character columns
    dt[] <- lapply(dt, function(col) {
      if (is.character(col)) trimws(col) else col
    })
    
    dt
  })
  
  ## Output Notice for DEMO mode----
  output$demo_mode_notice <- renderUI({
    req(current_subscriber() == "demo")
    req(reactiveValSubmit() == FALSE)  # Show only before submit
    tags$div(
      style = "padding: 12px 16px; background-color: #f0f2f5; border-left: 5px solid #17a2b8; border-radius: 6px; font-size: 0.95rem; color: #333; font-family: 'Segoe UI', sans-serif;",
      HTML("
      <strong><i class='bi bi-eye'></i> Preview Mode</strong><br><br>
      Data upload is disabled in this mode. You can still explore how RAISINS works using demo datasets.
      Select a demo dataset, choose treatment, day, block and response variables and click <em>Run Analysis</em> to view results.
      <span style='font-size: 0.9rem; color: #555;'><em>To upload your own data, please subscribe and login via <strong>Get Started</strong>.</em></span>
    ")
    )
  })
  
  ## Selected dataset display----
  output$demo_dataset_info <- renderUI({
    req(current_subscriber() == "demo")
    req(input$demo_data_select)
    req(reactiveValSubmit() == FALSE)  # Show only before submit
    
    dataset_description <- switch(input$demo_data_select,
                                  "dataset1.csv" = "This dataset consists of 3 chemical treatments, replicated in 3 blocks. Observations are taken for five days. Characters under study include char1, char2, char3, and char4. When you create csv file for upload, create in this format. Also explore Create Data and Datasets for creating data and model datasets.",
                                  "dataset2.csv" = "This dataset consists of 3 treatments replicated in 3 blocks. Observations are from Day1, Day3, Day6 and Day7. Characters include obs1, obs2, obs3 and obs4. When you create csv file for upload, create in this format. Also explore Create Data and Datasets for creating data and model datasets.",
                                  "dataset3.csv" = "This dataset consists of 4 treatments in 3 blocks. Observations are taken for four different days. Parameters include Ca, Fe, Cu, N, P and Mg. When you create csv file for upload, create in this format. Also explore Create Data and Datasets for creating data and model datasets.",
                                  "No description available for this dataset."
    )    
    tags$div(
      style = "padding: 10px; background-color: #d1ecf1; border-left: 5px solid #0c5460; border-radius: 5px; margin-top: 10px; font-size: 0.9rem;",
      HTML(paste0("<strong>Selected Dataset:</strong> ", input$demo_data_select, "<br><br>", dataset_description))
    )
  })
  
  ## demo dataset preview----
  output$demo_dataset_preview <- renderReactable({
    req(current_subscriber() == "demo")
    req(input$demo_data_select)
    req(reactiveValSubmit() == FALSE)
    
    file_path <- file.path("data", input$demo_data_select)
    req(file.exists(file_path))
    df <- read.csv(file_path, header = TRUE)
    
    reactable::reactable(
      df,
      compact = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      defaultPageSize = 30,
      searchable = FALSE,
      paginationType = "simple"
    )
  })
  
  ## Shiny alert if submit pressed without proper selection----
  observeEvent(input$rcbd_submit, {
    if (is.null(input$rcbd_Subject) || input$rcbd_Subject == "" ||
        is.null(input$rcbd_Day) || input$rcbd_Day == "" ||
        is.null(input$rcbd_selvar) || length(input$rcbd_selvar) == 0) {

      shinyalert::shinyalert(
        title = "Missing Input",
        text = "Please select the Subject column, the Day/Time column and at least one response variable.",
        type = "warning"
      )

      return()
    }
  })
  
  ### Plus or minus toggle----
  output$toggle_mean_sd <- renderUI({
    if (is.null(rcbd_csvfile())) {
      return()
    }
    req(rcbd_csvfile(), input$rcbd_Subject, input$rcbd_Day, input$rcbd_selvar)
    
    div(
      style = "text-align: center; margin: 10px;",
      prettyToggle(
        inputId = "plusminus_std",
        label_on = HTML("Show mean &plusmn; standard deviation &#128071;"),  # ± and 👇
        icon_on = icon("check"),
        status_on = "info",
        status_off = "warning",
        label_off = HTML("Show only mean &#128071;"),
        icon_off = icon("remove"),
        value = TRUE
      )
    ) 
  })
  
  ### toggle for letter grouping
  output$toggle_letter_group <- renderUI({
    if (is.null(rcbd_csvfile())) {
      return()
    }
    req(rcbd_csvfile(), input$rcbd_Subject, input$rcbd_Day, input$rcbd_selvar)
    
    div(
      style = "text-align: center; margin: 10px;",
      prettyToggle(
        inputId = "letter_group",
        label_on = HTML("Show letter grouping &#128071;"),
        icon_on = icon("check"),
        status_on = "info",
        status_off = "warning",
        label_off = HTML("Remove letter grouping &#128071;"),
        icon_off = icon("remove"),
        value = TRUE
      )
    )
  })
  
  ### show pairwise TxD toggle----
  # output$toggle_pair <- renderUI({
  #   if (is.null(rcbd_csvfile())) {
  #     return()
  #   }
  #   req(rcbd_csvfile(), input$rcbd_Subject, input$rcbd_Day, input$rcbd_selvar)
  #   
  #   div(
  #     style = "text-align: center; margin: 10px;",
  #     prettyToggle(
  #       inputId = "pair_wise",
  #       label_on = HTML("Show Pairwise Comparison Treatment x Day &#128071;"),  # ± and 👇
  #       icon_on = icon("check"),
  #       status_on = "info",
  #       status_off = "warning",
  #       label_off = HTML("Pairwise Comparison Treatment x Day &#128071;"),
  #       icon_off = icon("remove"),
  #       value = FALSE
  #     )
  #   ) 
  # })
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                START IMAGE                               ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$rbd_startimage_content <- renderUI({  ##arshi(15-08-26) circle-icon start screen
    # Check if file is uploaded
    if (is.null(rcbd_csvfile())) {
      tagList(

        tags$style(HTML("

.circle-wrapper {
  text-align: center;
  width: 200px;
}

.circle-icon {
  width: 80px;
  height: 75px;
  border-radius: 50%;
  background: white;
  display: flex;
  align-items: center;
  justify-content: center;
  margin: 0 auto;
  cursor: pointer;
  transition: all 0.35s ease;
  box-shadow: 0 8px 20px rgba(0,0,0,0.08);
}

.circle-icon i {
  font-size: 45px;
  transition: all 0.35s ease;
}

/* Upload */
.circle-upload {
  border: 3px solid #2E8B57;
  color: #2E8B57;
}

.circle-upload:hover {
  background: #2E8B57;
  color: white;
  transform: translateY(-6px);
  box-shadow: 0 15px 30px rgba(0,0,0,0.2);
}

/* CSV */
.circle-csv {
  border: 3px solid #1f618d;
  color: #1f618d;
}

.circle-csv:hover {
  background: #1f618d;
  color: white;
  transform: translateY(-6px);
  box-shadow: 0 15px 30px rgba(0,0,0,0.2);
}

/* Transformations */
.circle-trans {
  border: 3px solid #e67e22;
  color: #e67e22;
}

.circle-trans:hover {
  background: #e67e22;
  color: white;
  transform: translateY(-6px);
  box-shadow: 0 15px 30px rgba(0,0,0,0.2);
}

/* Analysis */
.circle-analysis {
  border: 3px solid #8e44ad;
  color: #8e44ad;
}

.circle-analysis:hover {
  background: #8e44ad;
  color: white;
  transform: translateY(-6px);
  box-shadow: 0 15px 30px rgba(0,0,0,0.2);
}

.circle-label {
  margin-top: 12px;
  font-weight: 600;
  font-size: 14px;
  color: #496f9e;
}

.circle-wrapper a {
  color: black !important;
  text-decoration: none !important;
}

.circle-wrapper a:visited {
  color: black !important;
}

.circle-wrapper a:hover {
  color: inherit !important;  /* keeps your hover color */
}

")),
        div(
          style = "display:flex; justify-content:center; gap:80px; flex-wrap:wrap; margin-top:50px;",

          div(class = "circle-wrapper",
              div(class = "circle-icon circle-upload",
                  actionLink("btn_upload_instr", label = NULL,
                             icon = icon("file-upload"),
                             style = "font-size:45px; text-decoration:none;")
              ),
              div(class = "circle-label", "Data Upload Instructions")
          ),

          div(class = "circle-wrapper",
              div(class = "circle-icon circle-csv",
                  actionLink("btn_csv_help", label = NULL,
                             icon = icon("file-csv"),
                             style = "font-size:45px; text-decoration:none;")
              ),
              div(class = "circle-label", "How to Save as CSV")
          ),

          div(class = "circle-wrapper",
              div(class = "circle-icon circle-analysis",
                  actionLink("btn_analysis_help", label = NULL,
                             icon = icon("chart-line"),
                             style = "font-size:45px; text-decoration:none;")
              ),
              div(class = "circle-label", "How to Perform Analysis")
          ),
          div(class = "circle-wrapper",
              div(class = "circle-icon circle-trans",
                  actionLink("instr_rcbd_trans", label = NULL,
                             icon = icon("exchange"),
                             style = "font-size:45px; text-decoration:none;")
              ),
              div(class = "circle-label", "Transformations available")
          )

        ),

        div(
          style = "
    margin-top:60px;
    padding:40px;
    max-width:1100px;
    margin-left:auto;
    margin-right:auto;
    background:#ffffff;
    border-radius:16px;
    box-shadow:0 10px 25px rgba(0,0,0,0.08);
  ",


          # First paragraph
          p(
            style = "font-size: 16px; line-height: 1.6; margin-bottom: 15px; text-align: left;",
            "Before uploading your file for analysis,
          please read these instructions carefully.
        In RAISINS you can upload file in xls, xlsx or csv format.
        But we recommend a csv format as it will be much lighter."

          ),
          # second paragraph
          p(
            style = "font-size: 16px; line-height: 1.7; margin-bottom: 20px; text-align: left;",
            HTML(
              paste0(
                "RAISINS provides a built-in feature to create data ",
                "<a href='#' onclick=\"document.querySelector('a[data-value=\\'Create Data\\']').click(); return false;\" ",
                "style='font-weight: bold; color: #2E8B57; text-decoration: underline; cursor: pointer;'>",
                "<span style='display: inline-block; margin-left: 4px;'>&#128073;</span>",
                "click here</a>",
                " or select the <b>Create Data</b> tab above",
                "<span style='display: inline-block; margin-left: 4px;'>&#9757;&#65039;</span>",
                " to generate your dataset. To explore sample datasets instead, ",
                "<a href='#' onclick=\"document.querySelector('a[data-value=\\'Datasets\\']').click(); return false;\" ",
                "style='font-weight: bold; color: #2E8B57; text-decoration: underline; cursor: pointer;'>",
                "<span style='display: inline-block; margin-left: 4px;'>&#128073;</span>",
                "click here</a>",
                " or select the <b>Datasets</b> tab above",
                "<span style='display: inline-block; margin-left: 4px;'>&#9757;&#65039;</span>",
                "."
              )
            )
          ),
          p(
            style = "font-size: 16px; line-height: 1.6; margin-bottom: 15px; text-align: left; border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9;",
            span(style = "font-weight: bold; color: green;", "Column Naming Guidelines: "),
            "Always start the column names with a letter. Use only letters, numbers, dots (.) or underscores (_),
        kindly avoid spaces and special characters."
          ),

          h2(),

          # Final line
          p(
            style = "font-size: 14px; font-weight: bold; color: #27ae60; margin-top: 20px;font-family: 'Times New Roman', Times, serif;",
            "Once your data is ready, you're all set to enjoy Repeated Measures One Way ANOVA in RAISINS!")
        )

      )
    }
  })

  observeEvent(input$btn_upload_instr, {
    showModal(
      modalDialog(
        title = "Data Upload Instructions",
        tags$iframe(
          src = "dataprep_rcbd.html",
          style = "width:100%; height:80vh; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      )
    )
  })


  observeEvent(input$btn_analysis_help, {

    showModal(
      modalDialog(
        title = "How to Perform Repeated Measures One Way ANOVA",
        tags$iframe(
          src = "analysis_rcbd.html",
          style = "width:100%; height:80vh; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      )
    )

  })

  # opening csv preparation modal
  observeEvent(input$btn_csv_help, {
    showModal(
      modalDialog(
        title = "How to Save as CSV",
        tags$iframe(
          src = "csv.html",
          style = "width:100%; height:80vh; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      )
    )
  })
  #====================================================================================
  
  
    ####################HTML FILE OF INSTRUCTIONS in the start
  output$instr_bttns <- renderUI({
    if (is.null(rcbd_csvfile())) {
      list(
        tags$h4("FAQs"), 
        actionBttn(
          inputId = "instr_rcbd_fileprep",
          label = HTML("&#129300; How to prepare and upload file?"),  # 🤔 
          style = "stretch",
          color = "warning",
          size = "sm"
        ),
        tags$br(),
        
        actionBttn(
          inputId = "instr_rcbd_trans",
          label = HTML("&#129488; About the transformation Algorithm used?"),  # 🧐 
          style = "stretch",
          color = "warning",
          size = "sm"
        ),
        tags$br(),
        actionBttn(
          inputId = "instr_rcbd_cohenF",
          label = HTML("&#128528; What is Generalized Eta Squared in the results?"),  # 😐 
          style = "stretch",
          color = "warning",
          size = "sm"
        )
      )
    }
  })
  
  output$instr_bttns_FAQs <- renderUI({
    list(
      tags$h4("FAQs"), 
      actionBttn(
        inputId = "instr_rcbd_fileprep_FAQ",
        label = HTML("&#129300; How to prepare and upload file?"),  # 🤔 
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      
      actionBttn(
        inputId = "instr_rcbd_trans_FAQ",
        label = HTML("&#129488; About the transformation Algorithm used?"),  # 🧐
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      actionBttn(
        inputId = "instr_rcbd_cohenF_FAQ",
        label = HTML("&#128528; What is Generalized Eta Squared in the results?"),  # 😐  
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      actionBttn(
        inputId = "instr_rcbd_plot_FAQ",
        label = HTML("&#129327; How to master Plots in RAISINS?"),  # 🤯 
        style = "stretch",
        color = "warning",
        size = "sm"
      ),
      tags$br(),
      actionBttn(
        inputId = "instr_rcbd_pca_FAQ",
        label = HTML("&#129327; More on PCA based index score"),  # 🤯 
        style = "stretch",
        color = "warning",
        size = "sm"
      )
    )
  }) 
  
  output$instr_plots <- renderUI({
    if (!is.null(rcbd_csvfile())) {
      list(
        actionBttn(
          inputId = "instr_rcbd_plot",
          label = HTML("&#129327; How to master Plots in RAISINS?"),  # 🤯 
          style = "stretch",
          color = "success",
          size = "sm"
        )
      )
    }
  }) 
  
  observeEvent(input$instr_rcbd_fileprep, {
    showModal(
      modalDialog(
        title = "Instructions",
        tags$iframe(
          src = "dataprep_rcbd.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$instr_rcbd_trans, {
    showModal(
      modalDialog(
        title = "Transformation Algorithm",
        tags$iframe(
          src = "transformation.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  observeEvent(input$instr_rcbd_cohenF, {
    showModal(
      modalDialog(
        title = "Generalized Eta Squared",
        tags$iframe(
          src = "etasquare.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$instr_rcbd_fileprep_FAQ, {
    showModal(
      modalDialog(
        title = "Instructions",
        tags$iframe(
          src = "dataprep_rcbd.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$instr_rcbd_trans_FAQ, {
    showModal(
      modalDialog(
        title = "Transformation Algorithm",
        tags$iframe(
          src = "transformation.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  observeEvent(input$instr_rcbd_cohenF_FAQ, {
    showModal(
      modalDialog(
        title = "Generalized Eta Squared",
        tags$iframe(
          src = "etasquare.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }) 
  observeEvent(input$instr_rcbd_plot_FAQ, {
    showModal(
      modalDialog(
        title = "Plotting in RAISINS",
        tags$iframe(
          src = "plotting.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$instr_rcbd_pca_FAQ, {
    showModal(
      modalDialog(
        title = "PCA based index",
        tags$iframe(
          src = "pcaindexscore.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  observeEvent(input$instr_rcbd_plot, {
    showModal(
      modalDialog(
        title = "Plotting in RAISINS",
        tags$iframe(
          src = "plotting.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }) 
  
 
  ##################### view uploaded data
  output$rcbd_ViewData <- renderDT({
    if (is.null(rcbd_csvfile())) {
      return()
    }
    
    # Load the data
    dt <- rcbd_csvfile()
    # Identify character columns
    char_cols <- sapply(dt, is.character)
    
    # Create a vector for background color settings
    bg_colors <- sapply(seq_along(dt), function(i) {
      if (char_cols[i]) {
        'yellow'
      } else {
        NA
      }
    })
    
    # Define unique values for non-character columns and background colors for numeric values
    uval <- unique(unlist(dt[sapply(dt, is.numeric)]))
    uval <- uval[!is.na(uval)]  # Remove NA from unique values
    
    # Apply formatting to the datatable
    datatable(dt, 
              selection = 'none',
              options = list(
                scrollX = TRUE, 
                lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), 
                paging = TRUE, 
                searching = FALSE, 
                fixedColumns = TRUE, 
                autoWidth = FALSE, 
                ordering = TRUE, 
                dom = 'lfrtipB'
              ),
              class = 'cell-border stripe') %>% 
      formatStyle(names(dt),
                  backgroundColor = styleEqual(
                    c(as.character(uval), NA),
                    c(rep('lightgreen', length(uval)), 'red')
                  )) %>% 
      formatStyle(names(dt)[char_cols],
                  backgroundColor = 'yellow')
  })

    
  output$rcbd_instr_filecheck <- renderUI({
    if (is.null(rcbd_csvfile())) {
      return(NULL)
    }
    # Display the HTML file using an iframe
    tags$iframe(
      src = "filecheck.html",  # Ensure this file is in the www directory
      style = "width:100%; height:300px; border:none;",  # Adjust width and height as needed
      seamless = TRUE
    )
  })

  # Reactive value to control visibility of wellpanel in main panel
  rcbd_show_panel <- reactiveVal(FALSE)
  
  # When submit button is pressed, show the panel
  observeEvent(input$rcbd_submit, {
    rcbd_show_panel(TRUE)
  })
  
  # Reactive value to control visibility of wellpanel in sidebarpanel
  rcbd_show_panel_trans <- reactiveVal(FALSE)
  
  # When transswitch is pressed, show the panel
  observeEvent(input$rcbd_trans_switch, {
    rcbd_show_panel_trans(TRUE)
  })
  
 ################# variables appear after file upload
  output$rcbd_var <- renderUI({
    req(rcbd_csvfile())
    
    # Reactive expression for column names
    rcbd_colnames <- reactive({
      names(rcbd_csvfile())
    })
    
    # Reactive expressions for remaining choices
    ## Subject replaces Treatment; there is no block picker to exclude.
    rcbd_trt_choices <- reactive({
      setdiff(rcbd_colnames(), c(input$rcbd_selvar, input$rcbd_Day))
    })
    
    rcbd_selvar_choices <- reactive({
      setdiff(rcbd_colnames(), c(input$rcbd_Subject, input$rcbd_Day))
    })
    
    rcbd_factor_choices <- reactive({
      setdiff(rcbd_colnames(), c(input$rcbd_Subject, input$rcbd_selvar))
    })
    
    # Observers for dynamic updates
    observe({
      req(rcbd_colnames())
      updatePickerInput(
        session, 
        "rcbd_Subject", 
        choices = rcbd_trt_choices(), 
        selected = input$rcbd_Subject
      )
    })
    
    observe({
      req(rcbd_colnames())
      updatePickerInput(
        session, 
        "rcbd_Day", 
        choices = rcbd_factor_choices(), 
        selected = input$rcbd_Day
      )
    })
    
    observeEvent(list(input$rcbd_Subject, input$rcbd_Day), {
      req(rcbd_colnames())
      current_selvar <- input$rcbd_selvar  # Preserve current selections
      updated_choices <- rcbd_selvar_choices()
      valid_selected <- intersect(current_selvar, updated_choices)  # Keep valid selections
      updatePickerInput(
        session, 
        "rcbd_selvar", 
        choices = updated_choices, 
        selected = valid_selected
      )
    })
    
    # Render UI
    tagList(
      # Day picker
      tags$div(
        pickerInput(
          "rcbd_Day",
          label = HTML("Select Day/Time: <br><small style='font-size: 10px; color: gray;'>(Select the column with Day/Time)</small>"),
          choices = rcbd_colnames(),
          options = pickerOptions(
            container = "body",
            title = "Select Day/Time",
            liveSearch = TRUE,
            size = 6
          ),
          width = "100%"
        ),
        class = "brand-picker4"
      ),
      # Treatment picker
      tags$div(
        pickerInput(
          "rcbd_Subject",
          label = HTML("Select Subject: <br><small style='font-size: 10px; color: gray;'>(The column identifying each subject / plot / animal measured repeatedly)</small>"),
          choices = rcbd_colnames(),
          options = pickerOptions(
            container = "body",
            title = "Select Subject",
            liveSearch = TRUE,
            size = 6
          ),
          width = "100%"
        ),
        class = "brand-picker1"
      ),
      ## ##arshi(15-08-26) one-way RM - no block/replication picker: in a one-way repeated measures
      ## design the SUBJECT is the unit of replication, and it is already
      ## selected above.
      
      # Variables for analysis picker
      tags$div(
        pickerInput(
          "rcbd_selvar",
          label = HTML("Select variables: <br><small style='font-size: 10px; color: gray;'>(You can select all the variables for analysis at once.)</small>"),
          choices = rcbd_colnames(),
          multiple = TRUE,
          options = pickerOptions(
            container = "body",
            actionsBox = TRUE,
            liveSearch = FALSE,
            title = "Select variables",
            size = 6
          ),
          width = "100%"
        ),
        class = "brand-picker2"
      ),
      # Transformation toggle
      prettyToggle(
        inputId = "rcbd_trans_switch",
        label_on = "Yes I need Transformation",
        icon_on = icon("check"),
        status_on = "info",
        status_off = "warning",
        label_off = "Click for Transformation",
        icon_off = icon("remove")
      )
    )
  })
  
  
  ################# END: variables appear after file upload  
  
  output$rcbd_var_transform <- renderUI({
    req(rcbd_csvfile(), input$rcbd_trans_switch, input$rcbd_selvar)
    
    if (input$rcbd_trans_switch == TRUE && rcbd_show_panel_trans()) {
      rcbd_column_names_trans <- as.character(input$rcbd_selvar)
      
      # Reactive expressions for filtered variable choices using isolate()
      rcbd_sq_choices <- reactive({
        setdiff(rcbd_column_names_trans, c(isolate(input$rcbd_log_selvar), isolate(input$rcbd_sin_selvar)))
      })
      
      rcbd_log_choices <- reactive({
        setdiff(rcbd_column_names_trans, c(isolate(input$rcbd_sq_selvar), isolate(input$rcbd_sin_selvar)))
      })
      
      rcbd_sin_choices <- reactive({
        setdiff(rcbd_column_names_trans, c(isolate(input$rcbd_log_selvar), isolate(input$rcbd_sq_selvar)))
      })
      
      # Observer to update choices dynamically with isolate()
      observeEvent(input$rcbd_selvar, {
        updatePickerInput(session, "rcbd_sq_selvar", choices = rcbd_sq_choices(), selected = isolate(input$rcbd_sq_selvar))
        updatePickerInput(session, "rcbd_log_selvar", choices = rcbd_log_choices(), selected = isolate(input$rcbd_log_selvar))
        updatePickerInput(session, "rcbd_sin_selvar", choices = rcbd_sin_choices(), selected = isolate(input$rcbd_sin_selvar))
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      # UI elements for transformations
      list(
        pickerInput("rcbd_log_selvar", "Log transform:", 
                    choices = rcbd_column_names_trans, 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        uiOutput("doe_log_constant"),
        pickerInput("rcbd_sq_selvar", "Square-root transform:", 
                    choices = rcbd_column_names_trans, 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput("rcbd_sin_selvar", "Arcsin transform:", 
                    choices = rcbd_column_names_trans, 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE)
      )
    }
  })
  
  
  
  output$rcbd_var_submit <- renderUI({
    
    # Check if file is uploaded
    if (is.null(rcbd_csvfile())) {
      return(NULL)
    }
    # Function to check for blank cells or NA values
    validate_cell_file <- function(data) {
      if (any(is.na(data))) {
        return("Blank cells found")
      }
      return(NULL)
    }
    
    # Function to check for blank rows or columns
    validate_file <- function(data) {
      if (any(apply(data, 1, function(row) all(is.na(row)))) || 
          any(apply(data, 2, function(col) all(is.na(col))))) {
        return("Blank rows or columns")
      }
      return(NULL)
    }
    
    # Validate the data file
    rcbd_dt <- rcbd_csvfile()
    check_vals$na <- validate_file(rcbd_dt)
    check_vals$cell_na <- validate_cell_file(rcbd_dt)
    
    #usage limit based on plan
    authentication <- reactiveValuesToList(auth)
    subscriber_limit<-authentication$user_info$comment
    if (grepl("94400", subscriber_limit)) {
      duration_limit <- 105
    } else if (grepl("23600", subscriber_limit)) {
      duration_limit <- 35
    } else if (grepl("Inst plan", subscriber_limit)) {
      duration_limit <- 150
    }else if (grepl("demo plan", subscriber_limit)) {
      duration_limit <- 10000
    }
    else {
      duration_limit <- 35
    }
    # Check usage limit
    if (!is.null(shared$total_duration) && shared$total_duration >= duration_limit) {
      shinyalert::shinyalert(
        title = "Usage Hour Exceeded, we will help",
        text = HTML("You have reached your maximum usage limit. 
                    Hey, but dont worry we support students. Just give us a call."),
        type = "warning"
      )
      return(HTML('<div style="background-color:#f8ff75; padding:10px; border-radius:5px;">
        <strong style="color:red;">Usage limit exceeded. We\'re thrilled you\'re using RAISINS! Contact us</strong>
      </div>'))
    }
    
    # Check file validity
    if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      shinyalert::shinyalert(
        title = "Look Into Your File!",
        text = HTML(
          "The file you uploaded has some blank rows or columns. 
         It may be due to deleted cell values or some issue with the file. 
         In your Excel file, please select the area you deleted and then right-click and hit 'Clear Contents'. 
         You can also use the 'Create CSV' option in this app to create your file. 
         Don't worry, we are here to help you. It's easy!"
        ),
        type = "error"
      )
      return(HTML('<strong style="color:red;">Please check your file and try again.</strong'))
    }
    # Identify numeric columns
    numeric_cols <- rcbd_dt[, sapply(rcbd_dt, is.numeric), drop = FALSE]
    
    # Calculate variance for numeric columns
    variance_values <- sapply(numeric_cols, var, na.rm = TRUE)
    
    # Check for zero variance columns
    zero_var_cols <- names(variance_values[variance_values == 0])
    
    if (length(zero_var_cols) > 0) {
      shinyalert::shinyalert(
        title = "Issue in some column(s)",
        text = paste0(
          "Hey, for your column(s): ", paste(zero_var_cols, collapse = ", "), 
          " the variance is found to be 0. It may be due to all values being equal or all values being 0. But you can proceed with analysis.
         But don't use that column(s) for analysis as it can result in an error."
        ),
        type = "warning"
      )
    }
    # Render the submit button if no issues
    div(
      style = "display: flex; justify-content: center;",
      actionBttn(
        inputId = "rcbd_submit",
        label = "Run Analysis!",
        color = "danger",
        style = "jelly"
      )
    )
  })
  
  ################# capture submit click to use in UI
  # Define a reactive value
  reactiveValSubmit <- reactiveVal(FALSE)

  ## Store for RA-One chat context(updation March)
  rcbd_results_store <- reactiveVal(NULL)
  
  # ---- BIG DATA PERFORMANCE: hard reset + memory cleanup ----
  
  # A master reset key to invalidate heavy reactives (optional but strongly recommended)
  reset_key <- reactiveVal(0)
  
  # Store any large computed objects here (instead of leaving them hanging in reactive graph)
  # You can use this later to cache results, but for now we just clear it on new upload.
  session$userData$rcbd_cache <- NULL
  
  clear_rcbd_memory <- function() {
    
    # 1) Reset validation flags (these can block/trigger extra observers)
    check_vals$na      <- NULL
    check_vals$cell_na <- NULL
    
    # 2) Hide analysis panels (prevents heavy UI outputs from trying to render)
    rcbd_show_panel(FALSE)
    rcbd_show_panel_trans(FALSE)
    
    # 3) Reset key inputs (prevents old selections from re-triggering long chains)
    # Treatment & variables
    try(updatePickerInput(session, "rcbd_Subject", selected = character(0)), silent = TRUE)
    try(updatePickerInput(session, "rcbd_Day", selected = character(0)), silent = TRUE)
    try(updatePickerInput(session, "rcbd_selvar", selected = character(0)), silent = TRUE)
    
    # Transformation UI selections (only if those inputs exist)
    try(updatePickerInput(session, "rcbd_log_selvar", selected = character(0)), silent = TRUE)
    try(updatePickerInput(session, "rcbd_sq_selvar",  selected = character(0)), silent = TRUE)
    try(updatePickerInput(session, "rcbd_sin_selvar", selected = character(0)), silent = TRUE)
    
    # Toggle OFF transformation switch so transformation observers don’t keep running
    try(shinyWidgets::updatePrettyToggle(session, "rcbd_trans_switch", value = FALSE), silent = TRUE)
    
    # MCP / options that can trigger expensive recompute
    try(updateSelectInput(session, "rcbd_mcp", selected = character(0)), silent = TRUE)
    
    # 4) Drop any cached big objects you may have stored
    session$userData$rcbd_cache <- NULL
    
    # 5) Force garbage collection
    gc(verbose = FALSE)
  }
  

  # Observe the file input to reset reactiveValSubmit
  # ---- On new dataset upload/selection: reset + cleanup ##arshi(15-08-26) ----
  observeEvent(uploaded_or_selected(), {

    reactiveValSubmit(FALSE)

    # Invalidate heavy reactives downstream
    reset_key(reset_key() + 1)

    # ---- Show column names alert on file upload ----
    df <- tryCatch(rcbd_csvfile(), error = function(e) NULL)
    if (!is.null(df) && ncol(df) > 0) {
      col_list <- paste(names(df), collapse = " &nbsp;|&nbsp; ")
      shinyalert::shinyalert(
        title = "",
        text = paste0(
          "This file has <b>", ncol(df), " column(s)</b> and <b>", nrow(df), " row(s)</b>.<br><br>",
          "<div style='background:#f0f4f8; padding:10px; border-radius:6px; font-size:12px; color:#2c3e50;'>",
          "<b>Columns found:</b><br><br>",
          "<div style='max-height:150px; overflow-y:auto; padding-right:4px;'>",
          col_list,
          "</div>",
          "</div>",
          "<br>",
          "<div style='
      background: linear-gradient(135deg, #e8f5e9, #f1f8e9);
      border-left: 4px solid #2E8B57;
      border-radius: 8px;
      padding: 12px 14px;
      font-size: 13px;
      color: #1b5e20;
      display: flex;
      align-items: flex-start;
      gap: 8px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
    '>",
          "<span style='font-size:18px; line-height:1;'>&#128073;</span>",
          "<span>Now select the correct <b>Subject</b> and <b>Day/Time</b> columns and the <b>Response Variable(s)</b> from the dropdowns, then click <b>Run Analysis</b>.</span>",
          "</div>"
        ),
        type = "success",
        html = TRUE,
        closeOnEsc = TRUE,
        confirmButtonText = "Got it, let's go!",
        confirmButtonCol = "#047d48",
        callbackR = function(x) {
          later::later(function() {
            session$sendCustomMessage("highlightControl", list(
              id = "rcbd_Day",
              panel = NULL,
              persistent = TRUE
            ))
          }, delay = 0.3)
        }
      )
    }

  }, ignoreInit = TRUE)


  # ---- Guided pointer chain: Subject -> Day -> Variables ##arshi(15-08-26) one-way RM ----
  ## One-way repeated measures needs three columns picked before Run Analysis,
  ## so the pointer walks the row instead of jumping straight to the variables.
  ## nzchar() guards keep the chain from racing ahead if a picker reports "".
  rcbd_point_to <- function(next_id) {
    session$sendCustomMessage("removeHighlight", list())
    later::later(function() {
      session$sendCustomMessage("highlightControl", list(
        id = next_id,
        panel = NULL,
        persistent = FALSE
      ))
    }, delay = 0.3)
  }

  observeEvent(input$rcbd_Day, {
    req(rcbd_csvfile(), input$rcbd_Day, nzchar(input$rcbd_Day))
    if (!reactiveValSubmit()) {
      rcbd_point_to("rcbd_Subject")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$rcbd_Subject, {
    req(rcbd_csvfile(), input$rcbd_Subject, nzchar(input$rcbd_Subject))
    if (!reactiveValSubmit()) {
      rcbd_point_to("rcbd_selvar")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$variables_dropdown_closed, {
    if (length(input$rcbd_selvar) > 0 && !reactiveValSubmit()) {
      session$sendCustomMessage("removeHighlight", list())
    }
  }, ignoreInit = TRUE)

  
  # Use observeEvent to update the reactive value
  observeEvent(input$rcbd_submit, {
    # Update reactiveVal based on the value of input$rcbd_submit
    if (input$rcbd_submit > 0) {
      reactiveValSubmit(TRUE)
    } else {
      reactiveValSubmit(FALSE)
    }
  }, ignoreInit = TRUE)  # ignoreInit = TRUE ensures the observer does not run on app initialization
  
  # Define a reactive expression for output
  output$submitClicked <- reactive({
    return(reactiveValSubmit())
  })
  
  # Set the outputOptions
  outputOptions(output, 'submitClicked', suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$rcbd_submit, {
    if (is.null(rcbd_csvfile())) {
      return()
    }
    if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      return()
    }
    
    # ### deciding number of digits after decimals
    output$rcbd_digit <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      if (rcbd_show_panel()) {
        numericInput("rcbd_digit", "Digits after decimal", 2, min = 1, max = 4)
      }
    })
    # ###
    ################################## testing
    ##
    output$correction_method <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      if (rcbd_show_panel()) {
        selectInput(
          "correction_method",
          label = tags$span(
            "Sphericity Correction",
            actionLink(
              "info_sphericity", label = NULL, icon = icon("info-circle"),
              title = "What is Sphericity Correction?",
              style = "margin-left:6px; color:#17a2b8; text-decoration:none; font-size:15px; vertical-align:middle;"
            )
          ),
          choices = c(
            "None" = "none",
            "Greenhouse Geisser" = "GG",
            "Huynh Feldt" = "HF"
          ),
          selected = "none"
        )
      }
    })
    
    # ### deciding number of digits after decimals
    output$rcbd_mcp <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      if (rcbd_show_panel()) {
        selectInput("rcbd_mcp", "Multiple comparison test:", choices = c("LSD", "TUKEY"), selected = "LSD")
      }
    })
    # ###
    
    # ### Level of significance
    output$rcbd_alpha <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      if (rcbd_show_panel()) {
        level_choices <- c("0.05", "0.01")
        selectInput("rcbd_level", "Level of significance (\u03B1):", choices = level_choices, selected = "0.05")
      }
    })
    # ###
    
    # ### FONT selection
    output$rcbd_font <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      if (rcbd_show_panel()) {
        font_choices <- c(
          "cambria", "Arial", "Times New Roman", "Helvetica",
          "Courier New", "Georgia", "Verdana",
          "Tahoma", "Trebuchet MS"
        )
        selectInput("rcbd_font", "Select Font:", choices = font_choices, selected = "cambria")
      }
    })
    
    
    ################ Data reactive for ploting and a paragraph in AI ----
    
    rcbd_plotdata <- reactive({
      # if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      #   return()
      # }
      req(rcbd_csvfile(),
          input$rcbd_mcp,
          input$rcbd_level,
          input$rcbd_font,
          input$correction_method,
          input$rcbd_digit,
          input$rcbd_Subject,
          input$rcbd_Day,
          input$rcbd_selvar)

      ## ##arshi(15-08-26) one-way RM - a single within-subject factor, so the
      ## A / B / AB triple of the two-way version collapses to one set of
      ## frames. The A slot is kept so every plot builder downstream still
      ## finds df_summaryA / df_datalongA / trt_noA.
      rcbd_A       <- rcbd_csvfile()[, input$rcbd_Day,     drop = FALSE]
      rcbd_subject <- rcbd_csvfile()[, input$rcbd_Subject, drop = FALSE]
      rcbd_y       <- rcbd_csvfile()[, input$rcbd_selvar,  drop = FALSE]

      # Step 2: Rename
      colnames(rcbd_A)       <- "Treatment"   # plot builders expect a "Treatment" column
      colnames(rcbd_subject) <- "Subject"

      rcbd_df1 <- data.frame(rcbd_A, rcbd_subject, rcbd_y)

      rcbd_df1$Treatment <- factor(rcbd_df1$Treatment,
                                   levels = gtools::mixedsort(unique(rcbd_df1$Treatment)))

      rcbd_df1 <- rcbd_df1[order(rcbd_df1$Treatment), ]
      rcbd_df1 <- rcbd_df1[, c("Treatment", "Subject", colnames(rcbd_y))]

      Treatment <- rcbd_df1$Treatment   # already a factor
      t <- nlevels(Treatment)

      data_longA <- rcbd_df1[, !(names(rcbd_df1) %in% c("Subject"))] %>%
        pivot_longer(cols = -Treatment, names_to = "character", values_to = "value") %>%
        mutate(
          character = factor(character, levels = unique(character)),
          Treatment = factor(Treatment, levels = levels(Treatment))
        )

      df.summaryA <- data_longA %>%
        group_by(character, Treatment) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(character, Treatment)

      ### grouping should be taken from reactive table below as transformation may apply
      #data on which original analysis is done based on user selected transformation
      data_rcbd <- rcbd_table()

      rcbd_groups <- data_rcbd$rcbd_groups
      data_plotA  <- cbind(df.summaryA, group = as.vector(rcbd_groups))

      list(
        df_summaryA = data_plotA,
        df_linegraphA = df.summaryA,
        df_datalongA = data_longA,
        trt_noA = t
      )
    })
    
    
    ####### Output table reactive ----
    
    ## ---- letter grouping sanitiser ##arshi(15-08-26) ----
    ## With a large number of treatments cld() runs out of a-z and starts
    ## emitting symbols (#, $, %, [ ...). Those read as noise in the grouping
    ## column, so anything that is not a letter (or the alignment padding cld
    ## adds) is shown as "-" instead.
    rcbd_clean_group <- function(g) {
      g <- as.character(g)
      g[is.na(g)] <- ""
      gsub("[^A-Za-z ]", "-", g)
    }

    rcbd_table <- reactive({
      doe_exact_tables <- list()

      req(rcbd_csvfile(), 
          input$rcbd_mcp, 
          input$rcbd_level, 
          input$correction_method,
          input$rcbd_digit,
          input$rcbd_Subject, 
          input$rcbd_Day, 
          input$rcbd_selvar)
      
      # if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      #   return()
      # }
      
      rcbd_roundvalue <- if (is.null(input$rcbd_digit)) 2 else input$rcbd_digit
      rcbd_data <- rcbd_csvfile()
      
      if (!all(c(input$rcbd_Subject, input$rcbd_Day, input$rcbd_selvar) %in% names(rcbd_data))) {
        return()
      }
      
      ## ##arshi(15-08-26) one-way RM -------------------------------------------------
      ## One-way repeated measures: ONE within-subject factor (Day/Time) measured
      ## on each Subject. `rcbd_A` is that factor - keeping the A slot means every
      ## A-suffixed result vector downstream (df1, rcbd_groups, emm_meanA, ...)
      ## keeps working, while the B / AB machinery of the two-way version goes.
      rcbd_A       <- rcbd_csvfile()[, input$rcbd_Day,     drop = FALSE]
      rcbd_subject <- rcbd_csvfile()[, input$rcbd_Subject, drop = FALSE]
      rcbd_y       <- rcbd_csvfile()[, input$rcbd_selvar,  drop = FALSE]
      
      # Original names for interpretation
      org_Day       <- colnames(rcbd_A)
      org_Treatment <- org_Day   # kept: downstream text/report still reads org_Treatment
      org_Subject   <- colnames(rcbd_subject)
      
      # Standardizing column names
      colnames(rcbd_A) <- "Day"
      colnames(rcbd_subject) <- "Subject"
      colnames(rcbd_y) <- make.names(colnames(rcbd_y))
      
      # Creating the final dataframe - 2 id columns, then the responses
      rcbd_df1 <- data.frame(rcbd_subject, rcbd_A, rcbd_y)
      
      # Extracting selected variable names for AI processing
      rcbd_char_AI <- colnames(rcbd_y)
      
      Day     <- factor(rcbd_df1$Day)
      Subject <- factor(rcbd_df1$Subject)
      
      # Listing level names for interpretation
      levelnamesA <- levels(Day)        # the single (within-subject) factor
      levelnamesB <- NULL
      
      # Counting levels
      level_countsA <- nlevels(Day)
      level_countsB <- NULL
      subject_count <- nlevels(Subject)
      
      # ==================== OPTIMIZED HELPER FUNCTIONS ====================
      
      # Vectorized star generation - MUCH FASTER
      rcbd_generate_star <- function(rcbd_pval) {
        pval_numeric <- as.numeric(rcbd_pval)
        ifelse(pval_numeric <= 0.01, "**",
               ifelse(pval_numeric <= 0.05, "*", "NS"))
      }
      
      # Original concatenation functions (these work correctly)
      concatenate_with_superscript <- function(A, B) {
        result_rcbd <- matrix(ncol = nrow(A), nrow = ncol(A))
        for (i in 1:ncol(A)) {
          result_rcbd[i, ] <- paste0(A[, i], "<sup>", B[, i], "</sup>")
        }
        return(result_rcbd)
      }
      
      concatenate_with_superscript_matrix <- function(A, B) {
        result_rcbd <- matrix(ncol = ncol(A), nrow = nrow(A))
        for (i in 1:nrow(A)) {
          result_rcbd[i, ] <- doe_letter_cells(A[i, ], B[i, ], input$doe_letter_position)
        }
        return(result_rcbd)
      }
      
      # Function for parenthesis

      make_parenthesis <- function(A, B, SD) {
        doe_parenthesis_cells(A, B, SD, identical(input$doe_paren_value, "original"), input$doe_letter_position)
      }
      
      # ==================== TRANSFORMATION SECTION ====================
      
      if (input$rcbd_trans_switch == TRUE) { 
        if (is.null(input$rcbd_log_selvar) && is.null(input$rcbd_sq_selvar) && is.null(input$rcbd_sin_selvar)) {
          return()
        }
        
        rcbd_transformed_data <- rcbd_df1
        
        # Log transformation
        if (!is.null(input$rcbd_log_selvar)) {
          req(input$rcbd_log_selvar)
          selected_columns <- input$rcbd_log_selvar
          
          # Iterate over each selected column
          for (col in selected_columns) {
            column_data <- rcbd_transformed_data[[col]]
            
            # Check if any value in the column is less than 1
            if (any(column_data <=0)) {
              min_val <- min(column_data)
              # Apply log transformation with offset based on max_abs
              rcbd_transformed_data[[col]] <- log10(column_data - min_val + 1)
            } else {
              # Apply simple log transformation without offset
              rcbd_transformed_data[[col]] <- doe_log_positive(column_data, input$doe_log_c)
            }
          }
          
        }
        
        # Square-root transformation
        if (!is.null(input$rcbd_sq_selvar)) {
          req(input$rcbd_sq_selvar)
          for (col in input$rcbd_sq_selvar) {
            if (any(rcbd_transformed_data[[col]] < 0)) {
              showModal(modalDialog(
                title = "Warning",
                HTML(paste("There is a <b style='color:red;'>negative data</b> in column", col, 
                           "of your dataset. Square root transformation cannot be applied.")),
                easyClose = TRUE,
                footer = tagList(modalButton("Cancel"))
              ))
              break
            } else if (any(rcbd_transformed_data[[col]] == 0)) {
              rcbd_transformed_data[[col]] <- sqrt(rcbd_transformed_data[[col]] + 0.5)
            } else {
              rcbd_transformed_data[[col]] <- sqrt(rcbd_transformed_data[[col]])
            }
          }
        }
        
        # Arcsin transformation
        if (!is.null(input$rcbd_sin_selvar)) {
          req(input$rcbd_sin_selvar)
          rcbd_n_trns <- nrow(rcbd_df1)
          rcbd_replacement_value <- 1 / (4 * rcbd_n_trns)
          
          for (col in input$rcbd_sin_selvar) {
            column_data <- rcbd_transformed_data[[col]]
            
            if (any(column_data < 0) || any(column_data > 1)) {
              showModal(modalDialog(
                title = "Warning",
                HTML(paste("<b style='color:red;'>No Transformation is performed!</b> Data should be in proportions (between 0 and 1) for arcsin transformation.")),
                easyClose = TRUE,
                footer = tagList(modalButton("Cancel"))
              ))
              break
            }
            
            if (any(column_data == 0) || any(column_data == 1)) {
              column_data[column_data == 0] <- rcbd_replacement_value
              column_data[column_data == 1] <- 1 - rcbd_replacement_value
            }
            rcbd_transformed_data[[col]] <- asin(sqrt(column_data))
          }
        }
        
        # Return only selected columns after transformations
        selected_cols <- c(input$rcbd_log_selvar, input$rcbd_sq_selvar, input$rcbd_sin_selvar)
        if (length(selected_cols) > 0) {
          rcbd_transformed_data <- rcbd_transformed_data[, selected_cols, drop = FALSE]
        } else {
          rcbd_transformed_data <- NULL
        }
        
        common_cols <- intersect(names(rcbd_df1), names(rcbd_transformed_data))
        rcbd_df <- rcbd_df1
        rcbd_df[common_cols] <- rcbd_transformed_data[common_cols]
        
        # Transformed means - Day (the single within-subject factor)
        rcbd_trans <- cbind(rcbd_A, rcbd_transformed_data)
        rcbd_trans_mean <- rcbd_trans %>%
          group_by(Day) %>%
          summarise(across(.cols = everything(), .fns = list(mean = mean)))
        names(rcbd_trans_mean) <- gsub("_mean", "", names(rcbd_trans_mean))
        rcbd_trans_mean <- as.data.frame(rcbd_trans_mean)
        row.names(rcbd_trans_mean) <- rcbd_trans_mean[, 1]
        rcbd_trans_mean <- rcbd_trans_mean[gtools::mixedsort(row.names(rcbd_trans_mean)), ]
        rcbd_trans_mean <- rcbd_trans_mean[, -1, drop = FALSE]
        rcbd_trans_mean <- round(rcbd_trans_mean, rcbd_roundvalue)
        rcbd_trans_mean <- format(rcbd_trans_mean, nsmall = rcbd_roundvalue)
      # SD for the optional transformed main cell.
        rcbd_trans_sd <- rcbd_trans %>%
          group_by(Day) %>%
          summarise(across(.cols = everything(), .fns = list(sd = sd)))
        names(rcbd_trans_sd) <- gsub("_sd", "", names(rcbd_trans_sd))
        rcbd_trans_sd <- as.data.frame(rcbd_trans_sd)
        row.names(rcbd_trans_sd) <- rcbd_trans_sd[, 1]
        rcbd_trans_sd <- rcbd_trans_sd[gtools::mixedsort(row.names(rcbd_trans_sd)), ]
        rcbd_trans_sd <- rcbd_trans_sd[, -1, drop = FALSE]
        rcbd_trans_sd <- round(rcbd_trans_sd, rcbd_roundvalue)
        rcbd_trans_sd <- format(rcbd_trans_sd, nsmall = rcbd_roundvalue)

      } else {
        rcbd_df <- rcbd_df1
      }

      # ==================== DESCRIPTIVE STATISTICS ====================

      # Mean per Day
      rcbd_result_mean <- rcbd_df1[, !(names(rcbd_df1) %in% c("Subject"))] %>%
        group_by(Day) %>%
        summarise(across(.cols = everything(), .fns = list(mean = mean)))
      names(rcbd_result_mean) <- gsub("_mean", "", names(rcbd_result_mean))
      rcbd_result_mean <- as.data.frame(rcbd_result_mean)
      row.names(rcbd_result_mean) <- rcbd_result_mean[, 1]
      rcbd_result_mean <- rcbd_result_mean[gtools::mixedsort(row.names(rcbd_result_mean)), ]
      rcbd_result_mean <- rcbd_result_mean[, -1, drop = FALSE]

      # Standard deviation per Day
      rcbd_result_sd <- rcbd_df1[, !(names(rcbd_df1) %in% c("Subject"))] %>%
        group_by(Day) %>%
        summarise(across(everything(), .fns = list(se = sd)))
      names(rcbd_result_sd) <- gsub("_se", "", names(rcbd_result_sd))
      rcbd_result_sd <- as.data.frame(rcbd_result_sd)
      row.names(rcbd_result_sd) <- rcbd_result_sd[, 1]
      rcbd_result_sd <- rcbd_result_sd[gtools::mixedsort(row.names(rcbd_result_sd)), ]
      rcbd_result_sd <- rcbd_result_sd[, -1, drop = FALSE]

      # Combine mean and SD
      rcbd_result_mean <- round(rcbd_result_mean, rcbd_roundvalue)
      rcbd_result_mean <- format(rcbd_result_mean, nsmall = rcbd_roundvalue)
      rcbd_result_sd <- round(rcbd_result_sd, rcbd_roundvalue)
      rcbd_result_sd <- format(rcbd_result_sd, nsmall = rcbd_roundvalue)
      if (input$plusminus_std == FALSE) {
        rcbd_desc_result <- as.data.frame(rcbd_result_mean)
      } else {
        rcbd_desc_result <- as.data.frame(mapply(paste, rcbd_result_mean, rcbd_result_sd, sep = "&plusmn;"))
      }
      row.names(rcbd_desc_result) <- row.names(rcbd_result_sd)

      # ==================== ANOVA SECTION ====================

      ## ##arshi(15-08-26) one-way RM
      ## The subject IS the error stratum: y ~ Day + Error(Subject/Day).
      ## No between-subject factor, so there is no interaction to estimate.
      A       <- factor(rcbd_df$Day)
      id      <- factor(rcbd_df$Subject)
      t       <- length(levels(A))
      count_subject <- length(levels(id))
      rcbd_alpha <- as.numeric(input$rcbd_level)
      selected_correction <- input$correction_method
      n <- ncol(rcbd_df) - 2

      # CRITICAL OPTIMIZATION: Pre-compute sort orders ONCE
      treatment_levels_sorted <- levels(A)[gtools::mixedorder(levels(A))]
      day_levels_sorted <- treatment_levels_sorted

      # Initialize result vectors
      rcbd_pvalA <- rep(0, n)
      rcbd_FSTAT <- rep(0, n)
      rcbd_MSEA <- rcbd_MSE <- rep(0, n)
      rcbd_TMS <- rep(0, n)
      gesA <- rep(0, n)
      rcbd_groups <- matrix(0, t, n)
      emm_meanA <- matrix(0, t, n)
      emm_SEA <- matrix(0, t, n)

      rcbd_out_pair_list <- list()

      # Cache model objects to avoid re-fitting in indv_anova_results
      cached_afmod_list <- list()
      cached_emm_A_list <- list()
      cached_anova_tables <- list()

      # Determine grouping threshold
      groupingA <- t <= 30

      # Determine adjustment method once
      adjust_method <- if (input$rcbd_mcp == "LSD") "none" else "tukey"

      # Main analysis loop
      for (i in 1:n) {
        response_name <- names(rcbd_df)[2 + i]
        rcbd_df[[response_name]] <- as.numeric(as.character(rcbd_df[[response_name]]))

        formula_str <- paste0(response_name, " ~ Day + Error(Subject/Day)")
        afmod <- afex::aov_car(as.formula(formula_str), data = rcbd_df,
                               anova_table = list(correction = selected_correction, es = "ges"))
        rcbd_result <- as.data.frame(afmod$anova_table)

        ## One factor -> the afex table has a SINGLE row (Day). Every [2, ] and
        ## [3, ] extraction of the two-way version is gone with it.
        rcbd_MSEA[i]  <- format(round(rcbd_result[1, 3], rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_FSTAT[i] <- format(round(rcbd_result[1, 4], rcbd_roundvalue), nsmall = rcbd_roundvalue)
        gesA[i]       <- format(round(rcbd_result[1, 5], rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_pvalA[i] <- format(round(rcbd_result[1, 6], rcbd_roundvalue), nsmall = rcbd_roundvalue)

        ## residual mean square of the within-subject error stratum
        rcbd_MSE[i] <- format(round(rcbd_result[1, 3] / rcbd_result[1, 4], rcbd_roundvalue),
                              nsmall = rcbd_roundvalue)
        ## ##arshi(19-08-26) Mean Square of the effect = MSE * F  (col3 * col4).
        ## Previously this multiplied by num Df (col1) too, which yields the
        ## SUM OF SQUARES - mislabelled as "mean squares" in the summary table.
        rcbd_TMS[i] <- round(rcbd_result[1, 3] * rcbd_result[1, 4], rcbd_roundvalue)

        rcbd_ANOVA_Df <- setNames(data.frame(DF = round(rcbd_result[["num Df"]], rcbd_roundvalue)), "DF")
        ## den Df = df of the within-subject error stratum (denominator of the Day
        ## F-test); used for the Error (Within-subject) row of the ANOVA table.
        rcbd_dfE_within <- round(rcbd_result[1, 2], rcbd_roundvalue)

        # Estimated marginal means
        emm_A <- emmeans(afmod, "Day")
        emm_summaryA <- as.data.frame(summary(emm_A))
        emm_meanA[, i] <- emm_summaryA$emmean
        emm_SEA[, i] <- emm_summaryA$SE

        # Cache objects for reuse in indv_anova_results (avoids re-fitting)
        cached_afmod_list[[response_name]] <- afmod
        cached_emm_A_list[[response_name]] <- emm_A
        cached_anova_tables[[response_name]] <- rcbd_result

        # Multiple comparison - Day
        if (groupingA) {
          rcbd_out_mcp <- multcomp::cld(emm_A, Letters = letters, adjust = adjust_method, alpha = rcbd_alpha, decreasing = TRUE)
          rcbd_outgroup <- as.data.frame(rcbd_out_mcp[, c("Day", ".group")])
          rcbd_outgroup$.group <- rcbd_clean_group(rcbd_outgroup$.group)  ##arshi(15-08-26)
          row.names(rcbd_outgroup) <- rcbd_outgroup$Day
          rcbd_outgroup <- rcbd_outgroup[gtools::mixedsort(row.names(rcbd_outgroup)), ]
        } else {
          rcbd_outgroup <- NULL
        }

        # Pairwise comparisons - Day
        rcbd_out_pair <- pairs(emm_A, adjust = adjust_method)
        rcbd_out_pair <- as.data.frame(rcbd_out_pair)


        doe_exact_tables[[paste(input$rcbd_selvar[i], "Pairwise comparisons", sep = " - ")]] <- data.frame(Comparison = rcbd_out_pair$contrast, `CD (unrounded)` = doe_exact_character(rcbd_out_pair$SE * abs(rcbd_out_pair$t.ratio)), check.names = FALSE)


        rcbd_out_pair$CD <- format(round(rcbd_out_pair$SE * abs(rcbd_out_pair$t.ratio), rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_out_pair$estimate <- format(round(rcbd_out_pair$estimate, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_out_pair$SE <- format(round(rcbd_out_pair$SE, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_out_pair$t.ratio <- format(round(rcbd_out_pair$t.ratio, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        rcbd_out_pair$p.value <- as.matrix(format(round(rcbd_out_pair$p.value, rcbd_roundvalue), nsmall = rcbd_roundvalue))
        rcbd_out_pair$significance <- as.matrix(rcbd_generate_star(rcbd_out_pair$p.value))
        rcbd_out_pair$pval_with_star <- concatenate_with_superscript_matrix(rcbd_out_pair$p.value, rcbd_out_pair$significance)
        rcbd_out_pair <- rcbd_out_pair[, c("contrast", "estimate", "SE", "df", "t.ratio", "pval_with_star", "CD")]
        colnames(rcbd_out_pair) <- c("Day pairs", "Estimate", "SE(d)", "DF", "t ratio", "p value", "CD value")
        rcbd_out_pair <- rcbd_out_pair[gtools::mixedsort(row.names(rcbd_out_pair)), ]
        rcbd_out_pair_list[[names(rcbd_df)[i + 2]]] <- rcbd_out_pair

        # Assign groups
        if (rcbd_pvalA[i] <= rcbd_alpha && !is.null(rcbd_outgroup)) {
          rcbd_groups[, i] <- rcbd_outgroup$.group
        } else {
          rcbd_groups[, i] <- ""
        }
      }

      # ==================== POST-PROCESSING AND FINAL TABLES ====================

      # Format vectors
      rcbd_pvalA <- format(rcbd_pvalA, nsmall = rcbd_roundvalue)
      rcbd_FSTAT <- format(rcbd_FSTAT, nsmall = rcbd_roundvalue)
      rcbd_MSEA <- format(rcbd_MSEA, nsmall = rcbd_roundvalue)
      gesA <- format(gesA, nsmall = rcbd_roundvalue)

      # Generate star vector - VECTORIZED
      rcbd_star_vector <- rcbd_generate_star(rcbd_pvalA)

      # Convert to matrices
      rcbd_FSTAT_vector <- as.matrix(rcbd_FSTAT)
      rcbd_pval_vectorA <- as.matrix(rcbd_pvalA)
      rcbd_TMS_vector <- as.matrix(rcbd_TMS)
      rcbd_star_vector <- as.matrix(rcbd_star_vector)

      rcbd_desc_matrix <- as.matrix(rcbd_desc_result)
      rcbd_group_matrix <- as.matrix(rcbd_groups)

      # Apply concatenation
      rcbd_combined_FSTAT <- concatenate_with_superscript(rcbd_FSTAT_vector, rcbd_star_vector)
      rcbd_combined_TMS <- concatenate_with_superscript(rcbd_TMS_vector, rcbd_star_vector)
      colnames(rcbd_combined_FSTAT) <- colnames(rcbd_desc_result)
      ## ##arshi(22-08-26) one-way RM - "Show / Remove letter grouping" toggle.
      ## When the toggle is ON (input$letter_group == TRUE, default) the CLD
      ## letters are appended to each mean; when the user turns it OFF the table
      ## shows the descriptive cells (mean +/- SD) without the letters.
      if (isTRUE(input$letter_group)) {
        rcbd_desc_grouping <- concatenate_with_superscript_matrix(rcbd_desc_matrix, rcbd_group_matrix)
      } else {
        rcbd_desc_grouping <- rcbd_desc_matrix
      }
      colnames(rcbd_desc_grouping) <- colnames(rcbd_desc_result)
      row.names(rcbd_desc_grouping) <- row.names(rcbd_desc_result)

      # Apply transformation parenthesis if needed
      if (input$rcbd_trans_switch == TRUE) {
        rcbd_desc_grouping <- make_parenthesis(rcbd_desc_grouping, rcbd_trans_mean, rcbd_trans_sd)
      }

      colnames(rcbd_desc_grouping) <- colnames(rcbd_desc_result)
      row.names(rcbd_desc_grouping) <- row.names(rcbd_desc_result)

      # Build final combined table
      rcbd_combined_table <- rbind(
        rcbd_desc_grouping,
        `F stat` = as.data.frame(rcbd_combined_FSTAT),
        `p value` = rcbd_pvalA,
        MSE = rcbd_MSEA,
        `Generalized Eta Squared` = gesA
      )

      # Final table
      rcbd_final_table <- rcbd_combined_table

      # ANOVA TABLE
      ## Complete table: the Day effect (mean square of the effect, with stars)
      ## plus its Error (Within-subject) row (the within-subject error mean square,
      ## which is the denominator of the Day F-test). The between-subject stratum
      ## (Subjects) is intentionally not shown - it is not a test term here.
      rcbd_ANOVATABLE <- rbind(
        as.matrix(rcbd_combined_TMS),        # Day: effect MS (MSE * F) with stars
        matrix(rcbd_MSEA, nrow = 1)          # Error (Within-subject): within error MS
      )
      rcbd_ANOVATABLE <- as.data.frame(rcbd_ANOVATABLE, stringsAsFactors = FALSE)
      colnames(rcbd_ANOVATABLE) <- colnames(rcbd_desc_result)
      SoV <- c(org_Day, "Error (Within-subject)")
      DF  <- rbind(rcbd_ANOVA_Df,
                   setNames(data.frame(DF = rcbd_dfE_within), "DF"))
      rcbd_final_ANOVATABLE <- cbind(SoV, DF, rcbd_ANOVATABLE)

      ## ---- Unrounded (original) CD reference table ##arshi(15-08-26) ----
      rcbd_CD_exact_tbl  <- NULL
      rcbd_CD_exact_abbr <- NULL
      rcbd_CD_exact_full <- NULL
      rcbd_CD_exact_dmrt <- FALSE

      # ==================== MAUCHLY'S TEST OF SPHERICITY ====================
      ## One-way RM design (y ~ Day + Error(Subject/Day)): the ONLY within-subject
      ## effect is 'Day', so the sphericity table has a single effect row. It is
      ## read from the car::Anova.mlm object stored in each cached afex model.
      ## Columns of the output table are the response variables (one per character).
      rcbd_mauchly_table <- tryCatch({
        resp_names <- names(cached_afmod_list)
        mauchly_list <- lapply(cached_afmod_list, function(afm) {
          s <- suppressWarnings(summary(afm$Anova))
          s$sphericity.tests   # rows = within effects; cols = "Test statistic"(W), "p-value"
        })
        # Effect names shared across responses (take the first non-empty result)
        eff_names <- NULL
        for (m in mauchly_list) {
          if (!is.null(m) && nrow(m) > 0) { eff_names <- rownames(m); break }
        }
        if (is.null(eff_names)) {
          # Day has < 3 levels -> sphericity automatic, no Mauchly test exists.
          NULL
        } else {
          # W in (0,1] and p can both be tiny; show W to 3 sig figs and p with
          # the standard "<0.001" convention so nothing collapses to "0.00".
          fmt_w <- function(x) {
            x <- as.numeric(x)
            ifelse(is.na(x), "-", formatC(x, format = "g", digits = 3))
          }
          fmt_p <- function(x) {
            x <- as.numeric(x)
            ifelse(is.na(x), "-",
                   ifelse(x < 0.001, "<0.001",
                          formatC(x, format = "f", digits = max(3, rcbd_roundvalue))))
          }
          row_list     <- list()   # screen version (HTML icon verdict)
          row_list_rep <- list()   # report version (plain-text verdict for PDF/Word)
          viol_flag    <- FALSE     # TRUE if any character violates sphericity
          for (eff in eff_names) {
            p_raw <- sapply(mauchly_list, function(m) {
              if (!is.null(m) && eff %in% rownames(m)) as.numeric(m[eff, 2]) else NA_real_
            })
            viol_flag <- viol_flag || any(p_raw < rcbd_alpha, na.rm = TRUE)
            W_disp <- sapply(mauchly_list, function(m) {
              if (!is.null(m) && eff %in% rownames(m)) fmt_w(m[eff, 1]) else "-"
            })
            p_disp  <- ifelse(is.na(p_raw), "-", fmt_p(p_raw))
            verdict <- ifelse(is.na(p_raw), "-",
                              ifelse(p_raw >= rcbd_alpha,
                                     "<span style='color:#2e7d32; font-weight:bold;'>&#10004;</span>",
                                     "<span style='color:#c62828; font-weight:bold;'>&#10008;</span>"))
            verdict_txt <- ifelse(is.na(p_raw), "-",
                                  ifelse(p_raw >= rcbd_alpha, "Assumed", "Violated"))
            row_list[[paste0(eff, " - Mauchly's W")]] <- as.character(W_disp)
            row_list[[paste0(eff, " - p value")]]     <- as.character(p_disp)
            row_list[[paste0(eff, " - Sphericity")]]  <- as.character(verdict)
            row_list_rep[[paste0(eff, " - Mauchly's W")]] <- as.character(W_disp)
            row_list_rep[[paste0(eff, " - p value")]]     <- as.character(p_disp)
            row_list_rep[[paste0(eff, " - Sphericity")]]  <- as.character(verdict_txt)
          }
          mtab <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)
          colnames(mtab) <- resp_names
          mtab_report <- as.data.frame(do.call(rbind, row_list_rep), stringsAsFactors = FALSE)
          colnames(mtab_report) <- resp_names
          attr(mtab, "any_violated") <- isTRUE(viol_flag)
          attr(mtab, "report_table") <- mtab_report
          mtab
        }
      }, error = function(e) NULL)

      # ==================== RETURN LIST ====================

      list(
        doe_exact_tables = doe_exact_tables,
        df1 = rcbd_final_table,
        df4 = rcbd_final_ANOVATABLE,
        df_mauchly = rcbd_mauchly_table,
        no_trt = t,
        r = count_subject,
        # Balanced design = every subject measured once at every time point.
        # (One-way RM has no replication column; the subjects ARE the repeats.)
        all_equal = tryCatch(length(unique(as.vector(table(id, A)))) == 1,
                             error = function(e) NA),
        rcbd_groups = rcbd_groups,
        rcbd_out_pair = rcbd_out_pair_list,
        emm_meanA = emm_meanA,
        emm_SEA = emm_SEA,
        emm_summaryA = emm_summaryA,
        trans_data = rcbd_df,
        rcbd_df1 = rcbd_df,
        org_Treatment = org_Treatment,
        org_Day = org_Day,
        org_Subject = org_Subject,
        levelnamesA = levelnamesA,
        rcbd_char = rcbd_char_AI,
        rcbd_star = rcbd_star_vector,
        rcbd_pvalA = rcbd_pval_vectorA,
        cached_afmod = cached_afmod_list,
        cached_emm_A = cached_emm_A_list,
        cached_anova_tables = cached_anova_tables
      )
    })

    ## Mauchly's test of sphericity (single within-subject effect: Day)
    output$rcbd_mauchly <- renderUI({
      req(rcbd_table())
      data_rcbd <- rcbd_table()
      mtab <- data_rcbd$df_mauchly
      if (is.null(mtab)) {
        ## The sphericity table can be empty for TWO different reasons; report the
        ## right one instead of always blaming "fewer than three levels".
        n_lev  <- length(data_rcbd$levelnamesA)              # levels of the repeated factor (Day)
        n_subj <- suppressWarnings(as.numeric(data_rcbd$r))  # number of subjects
        msg <- if (isTRUE(n_lev < 3)) {
          "Mauchly's test of sphericity is not applicable (the repeated factor has fewer than three levels, so sphericity is automatically satisfied)."
        } else {
          sprintf(paste0("Mauchly's test of sphericity could not be computed for this data. ",
                         "The test needs at least as many subjects as there are levels of the repeated factor ",
                         "(here %d subject(s) for %d levels). The within-subject F-test is still valid; ",
                         "sphericity simply cannot be tested with this few subjects."),
                  n_subj, n_lev)
        }
        return(HTML(paste0(
          "<p style='color:#666; font-style:italic; text-align:center;'>", msg, "</p>"
        )))
      }
      rcbd_mauchly_tbl <- mtab %>%
        kableExtra::kbl(caption = "Mauchly's Test of Sphericity (within-subject effect)",
                        align = "c", escape = FALSE) %>%
        kableExtra::kable_styling(
          bootstrap_options = "bordered",
          full_width = FALSE,
          latex_options = "booktabs"
        ) %>%
        kableExtra::kable_classic(full_width = F, html_font = input$rcbd_font) %>%
        kableExtra::footnote(number = c(
          "Mauchly's W tests the sphericity assumption for the repeated (within-subject) factor.",
          "'Sphericity: Violated' means p is below the selected significance level; 'Assumed' means it is not.",
          "When sphericity is violated, the within-subject F-test is adjusted using the selected sphericity correction (e.g., Greenhouse-Geisser / Huynh-Feldt).",
          "Sphericity is automatic (no test) when the repeated factor has only two levels."
        ))

      any_violated <- isTRUE(attr(mtab, "any_violated"))
      corr <- input$correction_method
      if (is.null(corr) || !nzchar(corr)) corr <- "none"
      corr_name <- switch(corr, none = "None", GG = "Greenhouse–Geisser",
                          HF = "Huynh–Feldt", corr)
      link <- "<a href=\"#\" onclick=\"var e=document.getElementById('correction_method'); if(e){var t=e.closest('.form-group')||e; t.scrollIntoView({behavior:'smooth',block:'center'}); t.style.transition='box-shadow .3s ease, background-color .3s ease'; t.style.boxShadow='0 0 0 3px #17a2b8'; t.style.backgroundColor='#e8f7fa'; t.style.borderRadius='6px'; setTimeout(function(){t.style.boxShadow='none'; t.style.backgroundColor='';}, 2000);} return false;\" style=\"color:#0c5460;font-weight:bold;text-decoration:underline;cursor:pointer;\">Sphericity Correction</a>"
      if (any_violated) {
        note <- paste0(
          '<div style="margin-top:10px; padding:10px 14px; background-color:#fff3cd; border-left:5px solid #e0a800; border-radius:5px; font-size:13px; color:#5c4500; max-width:1000px;">',
          '<b>Sphericity correction needed.</b> One or more characters violate the sphericity assumption. ',
          'Apply a correction by choosing <b>Greenhouse&ndash;Geisser</b> or <b>Huynh&ndash;Feldt</b> in the ', link, ' input',
          if (corr == "none") ' (currently set to <b>None</b> - the within-subject F-test is <u>not</u> yet corrected).'
          else paste0(' (currently set to <b>', corr_name, '</b>).'),
          '</div>'
        )
      } else {
        note <- paste0(
          '<div style="margin-top:10px; padding:10px 14px; background-color:#d4edda; border-left:5px solid #28a745; border-radius:5px; font-size:13px; color:#155724; max-width:1000px;">',
          '<b>&#10004; Sphericity holds.</b> All characters meet the sphericity assumption, so no correction is required; the ', link,
          ' input can remain <b>None</b> (currently <b>', corr_name, '</b>).',
          '</div>'
        )
      }

      tagList(HTML(rcbd_mauchly_tbl), HTML(note))
    })

    output$rcbd_final_ANOVA <- renderUI({
      # if (is.null(rcbd_csvfile())) {
      #   return()
      # }
      # if (is.null(input$rcbd_mcp)) {
      #   return()
      # }
      # if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      #   return()
      # }
      req(rcbd_table())
      data_rcbd<-rcbd_table()
      rcbd_ANOVA1<-data_rcbd$df4 %>%
        kableExtra::kbl(caption = "ANOVA summary table: mean squares and significance", 
                        align = "c", 
                        escape = FALSE,
                        rownames = FALSE) %>%
        kableExtra::kable_styling(
          bootstrap_options = "bordered",
          full_width = FALSE,
          latex_options = "booktabs"
        ) %>%
        kableExtra::kable_classic(full_width = F, 
                                  html_font = input$rcbd_font)%>%
        kableExtra::footnote(number = c("SoV= Source of Variation", 
                                        "DF = Degrees of Freedom",
                                        "Mean squares is provided for each character",
                                        "* indicates significance at 5% level", 
                                        "** indicates significance at 1% level",
                                        "NS indicates non-significant")
        )
      if (is.null(rcbd_ANOVA1)) {
        return()
      }
      else{
        HTML(rcbd_ANOVA1)
      }
    })
    
    
    output$rcbd_table1 <- renderUI({
      # if (is.null(rcbd_csvfile())) {
      #   return()
      # }
      # if (is.null(input$rcbd_mcp)) {
      #   return()
      # }
      # if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
      #   return()
      # }
      #   if (input$twowayTable != FALSE) return()  # 👈 Add this line if two way pressed this disappears
      req(rcbd_table())
      data_rcbd<-rcbd_table()
      df_Day<-data_rcbd$df1
      
      rcbd_table1<-df_Day %>%
        kableExtra::kbl(caption = paste0("Detailed tabular representation with letter grouping and other important statistics of ", data_rcbd$org_Day), align = "c", escape = FALSE) %>%
        kableExtra::kable_styling(
          bootstrap_options = "bordered",
          full_width = FALSE,
          latex_options = "booktabs"
        ) %>%
        kableExtra::kable_classic(full_width = F, html_font = input$rcbd_font)
      
      ## one-way RM: a single within-subject factor means a single table
      tagList(
        HTML(rcbd_table1)
      )
    })
    
    ## ##arshi(15-08-26) one-way RM - output$table1AB removed: with one
    ## within-subject factor there is no interaction table to render.
    
    ####### Output table END
    
    ##Download report----
    output$rcbd_var1 <- renderUI({
      req(input$rcbd_selvar, input$rcbd_Subject, input$rcbd_Day)
      list(
        radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                     inline = TRUE
        ),
        downloadButton("downloadReport")
      )
      
    })
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste("statoberry_report",
              switch(input$format,
                     PDF = "pdf",
                     HTML = "html",
                     Word = "docx"),
              sep = ".")
      },
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
        
        rcbd_data <- rcbd_table()
        # Get two-way tables from reactive if available
        #two_way_tables <- NULL
        # if(input$twowayTable) {
        #   two_way_tables <- two_way_tables_reactive()
        # }
        
        # Path to the temporary directory
        tempDir <- tempdir()
        
        # Path to the temporary Rmd file
        tempReport <- file.path(tempDir, "rcbd_report.Rmd")
        file.copy("rcbd_report.Rmd", tempReport, overwrite = TRUE)
        
        #copying image file
        # Copy banner.jpg to the temp directory
        # Define the temporary image path (without "www" subdirectory)
        tempImage <- file.path(tempDir, "banner1.jpg")
        # Copy the image from "www" to tempDir
        file.copy("www/banner1.jpg", tempImage, overwrite = TRUE)
        # Use absolute path for styles.docx (do NOT copy to temp directory)
        styles_path <- normalizePath("styles.docx")
        out <- doe_render_report(tempReport, switch(input$format,
                                                    PDF = pdf_document(),
                                                    HTML = html_document(),
                                                    Word = word_document(reference_docx = styles_path)
        ),
        params = list(
          anova_table = as.data.frame(rcbd_data$df4),
          table = as.data.frame(rcbd_data$df1),
          #table1 = as.data.frame(rcbd_data$df2),
          #table2 = as.data.frame(rcbd_data$df3),
          
          ## ##arshi(15-08-26) one-way RM - single results table; the Day and
          ## TreatmentxDay params of the two-way version are gone. The report
          ## template still declares them, so they simply arrive as their
          ## NA defaults until rcbd_report.Rmd is converted too.
          
          #summary_text = rcbd_summary_text(),
          ##Mcp button
          rcbd_mcp = input$rcbd_mcp,

          # Mauchly's test of sphericity: plain-text table (for PDF/Word to
          # recolour), the HTML icon table (for an HTML report identical to the
          # app), the violation flag, and the currently selected correction.
          mauchly = if (!is.null(rcbd_data$df_mauchly))
                      as.data.frame(attr(rcbd_data$df_mauchly, "report_table")) else NULL,
          mauchly_html = if (!is.null(rcbd_data$df_mauchly))
                      as.data.frame(rcbd_data$df_mauchly) else NULL,
          mauchly_violated = isTRUE(attr(rcbd_data$df_mauchly, "any_violated")),
          correction = {
            cm <- input$correction_method
            if (is.null(cm) || !nzchar(cm)) cm <- "none"
            switch(cm, none = "None", GG = "Greenhouse-Geisser", HF = "Huynh-Feldt", cm)
          }
          ### two-way table button
          #twowayTable = input$twowayTable,
          # Add the two-way tables to params
          #two_way_tables = two_way_tables
        ),
        envir = new.env(parent = globalenv())
        )

        file.rename(out, file)
      }
    )
    
    ##calculations table ----
    
    ## ---- rounding note under the pairwise tables ##arshi(15-08-26) ----
    ## The stars here, and the letters in the summary tables, come from the fitted
    ## model's p-values (multcomp::cld on emmeans) - NOT from comparing estimates
    ## against the CD column. So the note must not claim the CD decides
    ## significance; it explains the rounding of the CD column itself.
    rcbd_cd_round_note <- function() {
      HTML(paste0(
        "<div style='",
        "background-color: #eaf6ff;",
        "border: 1px solid #b6dcf5;",
        "color: #0b4f6c;",
        "padding: 12px 15px;",
        "border-radius: 6px;",
        "margin: 12px auto;",
        "width: 95%;",
        "font-size: 14px;",
        "line-height: 1.6;",
        "'>",
        "<b>Note:</b> ",
        "Significance shown in the above table - the p values with their significance marks, ",
        "and the letter grouping in the summary tables - is computed from the fitted model.",
        " It is not obtained by comparing the estimates against the rounded ",
        "CD value shown here. The CD values displayed are rounded to the number of decimal places ",
        "selected in the ",
        "<a href='#' onclick='rcbdGoToDecimals(); return false;' ",
        "style='font-weight: bold; color: #2E8B57; text-decoration: underline; cursor: pointer;'>",
        "Digits after decimal option</a>",
        "<span style='display: inline-block; margin-left: 4px;'>&#128072;</span>",
        " for concise presentation. You can change the decimal and see the unrounded cd value",
        "</div>"
      ))
    }

    output$rcbd_pair <- renderUI({
      if (is.null(input$rcbd_file1$datapath)) {
        return()
      }
      if (is.null(input$rcbd_mcp)) {
        return()
      }
      if (!is.null(check_vals$na) || !is.null(check_vals$cell_na)) {
        return()
      }
      req(rcbd_table())
      data_rcbd <- rcbd_table()
      
      # Get list of pairwise comparison tables
      pair_list <- data_rcbd$rcbd_out_pair
      char_names <- names(pair_list)
      
      # Initialize with the first character's table
      combined_table <- pair_list[[1]][, c("Treatment pairs", "Estimate", "SE(d)", "DF", "t ratio", "p value", "CD value")]
      
      # Rename columns (except Treatment pairs) to include character name
      names(combined_table)[-1] <- paste0(char_names[1], "_", names(combined_table)[-1])
      
      # Merge remaining tables horizontally on Treatment pairs
      if (length(char_names) > 1) {
        for (i in 2:length(char_names)) {
          temp_table <- pair_list[[char_names[i]]][, c("Treatment pairs", "Estimate", "SE(d)", "DF", "t ratio", "p value", "CD value")]
          # Rename columns to include character name
          names(temp_table)[-1] <- paste0(char_names[i], "_", names(temp_table)[-1])
          # Merge on Treatment pairs
          combined_table <- merge(combined_table, temp_table, by = "Treatment pairs", all = TRUE)
        }
      }
      
      # Define the desired column order: Treatment pairs, then all Estimate, all SE(d), all DF, etc.
      col_order <- c(
        "Treatment pairs",
        paste0(char_names, "_Estimate"),
        paste0(char_names, "_SE(d)"),
        paste0(char_names, "_DF"),
        paste0(char_names, "_t ratio"),
        paste0(char_names, "_p value"),
        paste0(char_names, "_CD value")
      )
      
      # Reorder columns in combined_table
      combined_table <- combined_table[, col_order, drop = FALSE]
      
      # Tukey reports the HSD critical value; LSD/none reports CD. The internal
      # column name stays "CD value" (used by the merge / col_order logic above);
      # only the visible group header switches to "HSD value" under Tukey.
      cd_lab <- if ((input$rcbd_mcp %||% "LSD") == "TUKEY") "HSD value" else "CD value"

      # Define header structure for kableExtra
      header_above <- c(
        " " = 1,  # Treatment pairs
        setNames(length(char_names), "Estimate"),  # Sub-columns for Estimate
        setNames(length(char_names), "SE(d)"),    # Sub-columns for SE(d)
        setNames(length(char_names), "DF"),       # Sub-columns for DF
        setNames(length(char_names), "t ratio"),  # Sub-columns for t ratio
        setNames(length(char_names), "p value"),  # Sub-columns for p value
        setNames(length(char_names), cd_lab)       # Sub-columns for CD / HSD value
      )
      
      # Create sub-column names (character names for each group)
      sub_header <- c(
        "Treatment pairs",
        rep(char_names, times = 6)  # Repeat char_names for each group (Estimate, SE(d), DF, t ratio, p value, CD value)
      )
      
      # Create a single HTML table
      rcbd_table_pair <- combined_table %>%
        kableExtra::kbl(
          caption = paste0(
            "<p style='text-align: center; font-size: 14px; margin-bottom: 10px;'>",
            "This table presents the pairwise comparisons of treatments for different characters, ",
            "showing statistical differences between treatment means based on the selected multiple comparison procedure. ",
            "Each character's results are grouped under respective statistic columns.",
            "</p>",
            "<div style='text-align: center;'>Pairwise Comparison of Treatments Across All Characters</div>"
          ),
          align = "c", 
          escape = FALSE,
          rownames = FALSE,
          col.names = sub_header
        ) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("hover", "bordered"),
          full_width = FALSE,
          latex_options = "booktabs",
          position = "center"
        ) %>%
        kableExtra::add_header_above(header_above) %>%
        kableExtra::row_spec(0, bold = TRUE) %>%
        kableExtra::column_spec(1, bold = TRUE) %>%
        kableExtra::kable_classic(full_width = FALSE, html_font = input$rcbd_font)
      
      # Return as HTML
      tagList(HTML(rcbd_table_pair), rcbd_cd_round_note())  ##arshi(15-08-26)
    })
    
    ## ##arshi(15-08-26) one-way RM - output$rcbd_pair_day removed: the single
    ## factor's pairwise table is already rendered by output$rcbd_pair above.
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                         create Individual ANOVAS                    ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    
    
    # indv_anova_results <- reactive({
    #   req(rcbd_table(), input$rcbd_mcp, input$rcbd_level, input$rcbd_digit, input$correction_method)
    #   
    #   dataimport <- rcbd_table()
    #   original_data <- dataimport$rcbd_df1
    #   rcbd_trans_mean <- dataimport$rcbd_trans_mean
    #   df <- dataimport$trans_data
    #   alpha <- as.numeric(input$rcbd_level)
    #   digits <- input$rcbd_digit
    #   rcbd_roundvalue <- input$rcbd_digit
    #   
    #   # REUSE cached model objects from rcbd_table — NO re-fitting!
    #   cached_emm_A <- dataimport$cached_emm_A
    #   cached_emm_B <- dataimport$cached_emm_B
    #   cached_emm_AB <- dataimport$cached_emm_AB
    #   cached_anova_tables <- dataimport$cached_anova_tables
    #   
    #   adjustment_method <- if (input$rcbd_mcp == "LSD") "none" else "tukey"
    #   
    #   r <- length(unique(df$Block))
    #   char_cols <- names(cached_emm_A)  # use cached keys
    #   
    #   # ---- Helper functions defined ONCE outside loop ----
    #   rcbd_generate_star <- function(rcbd_pval) {
    #     pval_numeric <- as.numeric(rcbd_pval)
    #     ifelse(pval_numeric <= 0.01, "**", ifelse(pval_numeric <= 0.05, "*", "NS"))
    #   }
    #   
    #   concatenate_with_superscript_matrix <- function(A, B) {
    #     matrix(doe_letter_cells(A, B, input$doe_letter_position), nrow = nrow(A), ncol = ncol(A))
    #   }
    #   
    #   generate_summary_grouping <- function(emm_obj, grouping_vars, original_data, char, digits, 
    #                                         rcbd_trans_mean = NULL, adjustment = "tukey", 
    #                                         include_grouping = TRUE) {
    #     if (include_grouping) {
    #       rcbd_group <- multcomp::cld(emm_obj, alpha = alpha, Letters = letters, adjust = adjustment)
    #       group_df <- as.data.frame(rcbd_group)
    #       if (length(grouping_vars) == 1) {
    #         group_df$Level <- as.character(group_df[[grouping_vars[1]]])
    #       } else {
    #         group_df$Level <- apply(group_df[, grouping_vars], 1, paste, collapse = ":")
    #       }
    #       group_df <- group_df[, c("Level", ".group")]
    #       colnames(group_df)[2] <- "Grouping"
    #     }
    #     
    #     if (length(grouping_vars) == 1) {
    #       orig_subset <- original_data[, c(grouping_vars[1], char)]
    #       colnames(orig_subset)[1] <- "Level"
    #     } else {
    #       original_data$Interaction <- apply(original_data[, grouping_vars], 1, paste, collapse = ":")
    #       orig_subset <- original_data[, c("Interaction", char)]
    #       colnames(orig_subset)[1] <- "Level"
    #     }
    #     
    #     summary_table <- orig_subset %>%
    #       group_by(Level) %>%
    #       summarise(Mean = round(mean(.data[[char]], na.rm = TRUE), digits),
    #                 SD = round(sd(.data[[char]], na.rm = TRUE), digits), .groups = "drop")
    #     
    #     if (!is.null(rcbd_trans_mean) && char %in% colnames(rcbd_trans_mean)) {
    #       tryCatch({
    #         trans_values <- as.numeric(as.character(rcbd_trans_mean[[char]]))
    #         trans_data <- data.frame(Level = as.character(rownames(rcbd_trans_mean)),
    #                                  Trans = round(trans_values, digits), stringsAsFactors = FALSE)
    #         summary_table$Level <- as.character(summary_table$Level)
    #         summary_table <- dplyr::left_join(summary_table, trans_data, by = "Level")
    #         summary_table$Mean <- ifelse(is.na(summary_table$Trans), as.character(summary_table$Mean),
    #                                      paste0(summary_table$Mean, "<br> (", summary_table$Trans, ")"))
    #         summary_table$Trans <- NULL
    #       }, error = function(e) warning(paste("Trans mean error for", char, ":", e$message)))
    #     }
    #     
    #     if (include_grouping) summary_table <- dplyr::left_join(summary_table, group_df, by = "Level")
    #     return(summary_table)
    #   }
    #   
    #   generate_pairwise_comparison <- function(emm_obj, contrast_label, rcbd_roundvalue, adjustment = "none") {
    #     pairwise_df <- as.data.frame(pairs(emm_obj, adjust = adjustment))
    #     pairwise_df$CD <- format(round(pairwise_df$SE * abs(pairwise_df$t.ratio), rcbd_roundvalue), nsmall = rcbd_roundvalue)
    #     pairwise_df$estimate <- format(round(pairwise_df$estimate, rcbd_roundvalue), nsmall = rcbd_roundvalue)
    #     pairwise_df$SE <- format(round(pairwise_df$SE, rcbd_roundvalue), nsmall = rcbd_roundvalue)
    #     pairwise_df$t.ratio <- format(round(pairwise_df$t.ratio, rcbd_roundvalue), nsmall = rcbd_roundvalue)
    #     pairwise_df$p.value <- format(round(pairwise_df$p.value, rcbd_roundvalue), nsmall = rcbd_roundvalue)
    #     pairwise_df$significance <- rcbd_generate_star(pairwise_df$p.value)
    #     pairwise_df$pval_with_star <- concatenate_with_superscript_matrix(as.matrix(pairwise_df$p.value), as.matrix(pairwise_df$significance))
    #     pairwise_df <- pairwise_df[, c("contrast", "estimate", "SE", "df", "t.ratio", "pval_with_star", "CD")]
    #     colnames(pairwise_df) <- c(contrast_label, "Estimate", "SE(d)", "DF", "t ratio", "p value", "CD value")
    #     pairwise_df[gtools::mixedsort(row.names(pairwise_df)), ]
    #   }
    #   
    #   # ---- Main loop: reuses cached objects, no aov_car or emmeans calls ----
    #   results <- list()
    #   
    #   for (char in char_cols) {
    #     # REUSE cached objects — this is the key optimization
    #     emm_A <- cached_emm_A[[char]]
    #     emm_B <- cached_emm_B[[char]]
    #     emm_AB <- cached_emm_AB[[char]]
    #     anova_table <- cached_anova_tables[[char]]
    #     response <- as.numeric(as.character(df[[char]]))
    #     
    #     # Format ANOVA table
    #     num_Df <- round(anova_table[["num Df"]], digits = digits)
    #     MS_error <- anova_table$MSE
    #     F_values <- anova_table$F
    #     MS_effect <- MS_error * F_values
    #     SS_effect <- MS_effect * num_Df
    #     
    #     formatted_anova <- data.frame(
    #       Source = rownames(anova_table),
    #       DF = num_Df,
    #       SS = formatC(SS_effect, format = "f", digits = digits),
    #       MS = formatC(MS_effect, format = "f", digits = digits),
    #       MS_error = formatC(MS_error, format = "f", digits = digits),
    #       F = formatC(F_values, format = "f", digits = digits),
    #       p.value = formatC(anova_table[["Pr(>F)"]], format = "f", digits = digits)
    #     )
    #     
    #     # Extract p-values
    #     p_treatment <- anova_table["Treatment", "Pr(>F)"]
    #     p_day <- anova_table["Day", "Pr(>F)"]
    #     p_interaction <- anova_table["Treatment:Day", "Pr(>F)"]
    #     p_treatment <- ifelse(length(p_treatment) > 0, p_treatment, 1)
    #     p_day <- ifelse(length(p_day) > 0, p_day, 1)
    #     p_interaction <- ifelse(length(p_interaction) > 0, p_interaction, 1)
    #     
    #     # Groupings — unified LSD/TUKEY path
    #     original_data$Treatment <- df$Treatment
    #     original_data$Day <- df$Day
    #     
    #     groupings <- list(
    #       Treatment = generate_summary_grouping(emm_A, c("Treatment"), original_data, char, digits, rcbd_trans_mean, adjustment_method, include_grouping = p_treatment < alpha),
    #       Day = generate_summary_grouping(emm_B, c("Day"), original_data, char, digits, rcbd_trans_mean, adjustment_method, include_grouping = p_day < alpha),
    #       TreatmentxDay = generate_summary_grouping(emm_AB, c("Treatment", "Day"), original_data, char, digits, rcbd_trans_mean, adjustment_method, include_grouping = p_interaction < alpha)
    #     )
    #     
    #     # Stats
    #     mse_treatment <- anova_table["Treatment", "MSE"]
    #     mse_within <- anova_table["Day", "MSE"]
    #     mean_resp <- mean(response, na.rm = TRUE)
    #     
    #     stat_summary <- list(
    #       Treatment = data.frame(Stat = c("CV (%)", "MSE", "SE(m)", "SE(d)"),
    #                              Value = c(round(sqrt(mse_treatment) / mean_resp * 100, digits), round(mse_treatment, digits),
    #                                        round(sqrt(mse_treatment / r), digits), round(sqrt(2 * mse_treatment / r), digits))),
    #       Day = data.frame(Stat = c("CV (%)", "MSE", "SE(m)", "SE(d)"),
    #                        Value = c(round(sqrt(mse_within) / mean_resp * 100, digits), round(mse_within, digits),
    #                                  round(sqrt(mse_within / r), digits), round(sqrt(2 * mse_within / r), digits)))
    #     )
    #     stat_summary$TreatmentxDay <- stat_summary$Day
    #     
    #     # Pairwise comparisons — reuses cached emm objects (pairs() is cheap, aov_car was the expensive part)
    #     rcbd_out_pair <- generate_pairwise_comparison(emm_A, "Treatment pairs", rcbd_roundvalue, adjustment_method)
    #     rcbd_out_pair_day <- generate_pairwise_comparison(emm_B, "Day pairs", rcbd_roundvalue, adjustment_method)
    #     rcbd_out_pair_TD <- if (isTRUE(input$pair_wise)) {
    #       generate_pairwise_comparison(emm_AB, "Treatment x Day pairs", rcbd_roundvalue, adjustment_method)
    #     } else NULL
    #     
    #     results[[char]] <- list(
    #       ANOVA = formatted_anova,
    #       Summary = groupings,
    #       stat_summary = stat_summary,
    #       Pairwise_Treatment = rcbd_out_pair,
    #       Pairwise_Day = rcbd_out_pair_day,
    #       Pairwise_TD = rcbd_out_pair_TD
    #     )
    #   }
    #   
    #   return(results)
    # }) 
    
    indv_anova_results <- reactive({
      req(rcbd_table(), input$rcbd_mcp, input$rcbd_level, input$rcbd_digit)
      
      dataimport <- rcbd_table()
      rcbd_roundvalue <- input$rcbd_digit
      
      # REUSE cached emm objects from rcbd_table — NO re-fitting!
      cached_emm_A <- dataimport$cached_emm_A
      
      adjustment_method <- if (input$rcbd_mcp == "LSD") "none" else "tukey"
      char_cols <- names(cached_emm_A)
      alpha_value <- as.numeric(input$rcbd_level)
      
      if (is.na(alpha_value)) {
        alpha_value <- 0.05
      }
      
      # Helper functions defined ONCE
      rcbd_generate_star <- function(rcbd_pval) {
        pval_numeric <- as.numeric(rcbd_pval)
        ifelse(pval_numeric <= 0.01, "**", ifelse(pval_numeric <= 0.05, "*", "NS"))
      }
      
      concatenate_with_superscript_matrix <- function(A, B) {
        matrix(doe_letter_cells(A, B, input$doe_letter_position), nrow = nrow(A), ncol = ncol(A))
      }
      
      generate_pairwise_comparison <- function(emm_obj, contrast_label, rcbd_roundvalue, adjustment = "none",
                                               alpha = 0.05) {
        
        pairwise_df <- as.data.frame(pairs(emm_obj, adjust = adjustment))
        # Correct CD calculation
        if (adjustment == "none") {
          
          # LSD / CD value
          pairwise_df$CD <- qt(1 - alpha / 2, pairwise_df$df) * pairwise_df$SE
          
        } else if (adjustment == "tukey") {
          
          # Number of means being compared
          n_means <- nrow(as.data.frame(emm_obj))
          
          pairwise_df$CD <- qtukey(
            p = 1 - alpha,
            nmeans = n_means,
            df = pairwise_df$df
          ) * pairwise_df$SE / sqrt(2)
          
        } else {
          
          pairwise_df$CD <- qt(1 - alpha / 2, pairwise_df$df) * pairwise_df$SE
          
        }

        
        pairwise_df$CD <- format(round(pairwise_df$CD, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        # pairwise_df$CD <- format(round(pairwise_df$SE * abs(pairwise_df$t.ratio), rcbd_roundvalue), nsmall = rcbd_roundvalue)
        pairwise_df$estimate <- format(round(pairwise_df$estimate, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        pairwise_df$SE <- format(round(pairwise_df$SE, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        pairwise_df$t.ratio <- format(round(pairwise_df$t.ratio, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        pairwise_df$p.value <- format(round(pairwise_df$p.value, rcbd_roundvalue), nsmall = rcbd_roundvalue)
        pairwise_df$significance <- rcbd_generate_star(pairwise_df$p.value)
        pairwise_df$pval_with_star <- concatenate_with_superscript_matrix(as.matrix(pairwise_df$p.value), as.matrix(pairwise_df$significance))
        pairwise_df <- pairwise_df[, c("contrast", "estimate", "SE", "df", "t.ratio", "pval_with_star", "CD")]
        ## Tukey uses the HSD critical value (qtukey above), so label the column
        ## "HSD value"; LSD/none uses the CD critical value -> "CD value".
        cd_lab <- if (adjustment == "tukey") "HSD value" else "CD value"
        colnames(pairwise_df) <- c(contrast_label, "Estimate", "SE(d)", "DF", "t ratio", "p value", cd_lab)
        pairwise_df
      }
      
      ## ##arshi(19-08-26) Complete stratified ANOVA table (Df, SS, MSS, F, p)
      ## afex's $anova_table has no SS and afex 1.5-1 keeps $aov = NULL
      ## (include_aov = FALSE), so refit a base aov with the same Error strata
      ## to recover the full decomposition (between-subject + within-subject).
      rcbd_full_df <- dataimport$rcbd_df1
      rcbd_full_df$Day     <- factor(rcbd_full_df$Day)
      rcbd_full_df$Subject <- factor(rcbd_full_df$Subject)

      build_full_anova <- function(char) {
        dat <- rcbd_full_df
        dat$.resp <- suppressWarnings(as.numeric(as.character(dat[[char]])))
        aov_fit <- aov(.resp ~ Day + Error(Subject / Day), data = dat)
        s <- summary(aov_fit)

        parts <- lapply(names(s), function(stratum) {
          tab <- as.data.frame(s[[stratum]][[1]])
          src <- trimws(rownames(tab))
          is_within <- grepl(":", sub("^Error: ?", "", stratum))
          src[src == "Residuals"] <- if (is_within) "Error (Within-subject)" else "Error (Between-subject)"

          Fv <- tab[["F value"]]
          pv <- tab[["Pr(>F)"]]
          stars <- rcbd_generate_star(pv)
          stars[is.na(pv)] <- ""
          p_col <- ifelse(is.na(pv), "",
                          paste0(formatC(pv, format = "f", digits = rcbd_roundvalue),
                                 "<sup>", stars, "</sup>"))

          data.frame(
            `Source of Variation` = src,
            DF  = tab[["Df"]],
            SS  = formatC(tab[["Sum Sq"]],  format = "f", digits = rcbd_roundvalue),
            MSS = formatC(tab[["Mean Sq"]], format = "f", digits = rcbd_roundvalue),
            `F value` = ifelse(is.na(Fv), "", formatC(Fv, format = "f", digits = rcbd_roundvalue)),
            `p value` = p_col,
            check.names = FALSE, stringsAsFactors = FALSE
          )
        })

        out <- do.call(rbind, parts)
        ## Do not show the between-subject (Subjects) error row: in a one-way RM
        ## design it is not a test term, so the table keeps only Day and its
        ## within-subject error.
        out <- out[out[["Source of Variation"]] != "Error (Between-subject)", , drop = FALSE]
        rownames(out) <- NULL
        out
      }

      # Main loop — pairwise comparisons + complete ANOVA table
      results <- list()

      for (char in char_cols) {
        ## ##arshi(15-08-26) one-way RM - a single within-subject factor, so
        ## one pairwise table per character. Kept under Pairwise_Treatment so
        ## the existing output/report plumbing still finds it.
        rcbd_out_pair <- generate_pairwise_comparison(cached_emm_A[[char]], "Day pairs", rcbd_roundvalue, adjustment_method, alpha = alpha_value)

        rcbd_full_anova <- tryCatch(build_full_anova(char), error = function(e) NULL)

        results[[char]] <- list(
          Full_ANOVA = rcbd_full_anova,
          Pairwise_Treatment = rcbd_out_pair
        )
      }

      return(results)
    })
    
    ## Select character
    output$char_selector_ui1 <- renderUI({
      req(rcbd_table())
      
      df <- rcbd_table()$rcbd_df
      
      char_cols <- setdiff(names(df), c("Day", "Subject"))
      
      if (length(char_cols) == 0) {
        return(tags$p("No character columns found in the dataset. Please check your input file."))
      }
      
      selectInput("selected_character", "Select Character:",
                  choices = char_cols,
                  selected = char_cols[1])
    })
    

    
    ## ##arshi(19-08-26) Complete ANOVA table (Df, SS, MSS, F, p) for selected character
    output$indv_full_anova <- renderUI({
      req(input$selected_character)
      anova_tbl <- indv_anova_results()[[input$selected_character]]$Full_ANOVA
      req(!is.null(anova_tbl))

      styled_tbl <- anova_tbl %>%
        kableExtra::kbl(
          caption = paste0(
            "<div style='text-align: center;'>Complete ANOVA Table for ",
            input$selected_character, "</div>"
          ),
          align = c("l", rep("c", ncol(anova_tbl) - 1)),
          escape = FALSE,
          row.names = FALSE
        ) %>%
        kableExtra::kable_styling(
          bootstrap_options = "bordered",
          full_width = FALSE,
          latex_options = "booktabs"
        ) %>%
        kableExtra::kable_classic(full_width = FALSE, html_font = input$rcbd_font) %>%
        kableExtra::footnote(number = c(
          "DF = Degrees of Freedom, SS = Sum of Squares, MSS = Mean Sum of Squares",
          "Day is tested against the within-subject error",
          "* indicates significance at 5% level",
          "** indicates significance at 1% level"
        ))

      tagList(HTML(styled_tbl))
    })

    output$pairwise_Treatment <- renderUI({
      req(input$selected_character)
      pair_tbl <- indv_anova_results()[[input$selected_character]]$Pairwise_Treatment
      req(!is.null(pair_tbl))
      
      styled_tbl <- pair_tbl %>%
        kableExtra::kbl(
          caption = paste0(
            "<p style='text-align: center; font-size: 14px; margin-bottom: 10px;'>",
            " Tables below  represents the pairwise comparisons of treatments, days and treatment x day combinations. ",
            "showing statistical differences between treatment means based on the selected multiple comparison procedure. ",
            "Each character's results are grouped under respective statistic columns.",
            "</p>",
            "<div style='text-align: center;'>Pairwise Comparison of Treatments </div>"
          ),
          align = "c",
          escape = FALSE,
          row.names = FALSE
        ) %>%
        kableExtra::kable_styling(
          bootstrap_options = "bordered",
          full_width = FALSE,
          latex_options = "booktabs"
        ) %>%
        kableExtra::kable_classic(full_width = FALSE, html_font = input$rcbd_font)
      
      tagList(HTML(styled_tbl))  ##arshi(15-08-26)
    })
    
    
    ## ##arshi(15-08-26) one-way RM - output$pairwise_Day removed (no second
    ## factor / interaction in this design).
    
    ## ##arshi(15-08-26) one-way RM - output$pairwise_TD removed (no second
    ## factor / interaction in this design).
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                        Download Report individual                   ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$rcbd_var2 <- renderUI({
      req(input$rcbd_selvar, input$rcbd_Day, rcbd_table(), indv_anova_results(), input$selected_character, input$rcbd_Subject)
      list(
        radioButtons("format", "Download report:", c("HTML",
                                                     "PDF",
                                                     "Word"
        ),
        inline = TRUE
        ),
        downloadButton("downloadReport1")
      )

    })
    output$downloadReport1 <- downloadHandler(
      filename = function() {
        paste("statoberry_report",
              switch(input$format,
                     PDF = "pdf",
                     HTML = "html",
                     Word = "docx"
              ),
              sep = ".")
      },
      content = function(file) {
        showModal(modalDialog(
          title = "Downloading please wait...",
          tags$img(
            src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fdownload.gif?alt=media",
            style = "display: Replication; margin: 0 auto; width: 80%; height: auto;"
          ),
          footer = NULL
        ))
        on.exit(removeModal())

        rcbd_data1 <- indv_anova_results()
        sel_char <- input$selected_character

        # Path to the temporary directory
        tempDir <- tempdir()

        # Path to the temporary Rmd file
        tempReport <- file.path(tempDir, "rcbd_report1.Rmd")
        file.copy("rcbd_report1.Rmd", tempReport, overwrite = TRUE)

        #copying image file
        # Copy banner.jpg to the temp directory
        # Define the temporary image path (without "www" subdirectory)
        tempImage <- file.path(tempDir, "banner1.jpg")
        # Copy the image from "www" to tempDir
        file.copy("www/banner1.jpg", tempImage, overwrite = TRUE)
        # Use absolute path for styles.docx (do NOT copy to temp directory)
        styles_path <- normalizePath("styles.docx")
        out <- doe_render_report(tempReport, switch(input$format,
                                                    PDF = pdf_document(),
                                                    HTML = html_document(),
                                                    Word = word_document(reference_docx = styles_path)
        ),
        params = list(
          pairwise_Treatment = as.data.frame(indv_anova_results()[[sel_char]]$Pairwise_Treatment),

          char = sel_char,
          mcp = input$rcbd_mcp,
          pair_wise = input$pair_wise
        ),
        envir = new.env(parent = globalenv())
        )

        file.rename(out, file)
      }
    )
    
    
    #####################PROMPT AI ----
    output$var_AI_path_intro_rcbd = renderUI({
      HTML(paste0("<p><h3 style='color: purple; text-align: justify; font-weight: bold;'>Welcome to RAISINS Assistant, your personal statistical interpretation assistant!</h3></p>",
                  "<p style='color: blue; text-align: justify;'>With RAISINS Assistant, 
                  you can receive insightful and accurate interpretations 
                  of your data. Whether you're a student, researcher, 
                  or simply looking to gain better insights from your data, 
                  RAISINS Assistant has got you covered.
                  Click on the checkbox below to ensure you are not a robot.</p>"))
    })
    
    output$var_checkbox_path_rcbd = renderUI({
      checkboxInput("checkforAI_rcbd", "I'm not a robot and I have checked that on running analysis there was no error reported", FALSE)
    })
    
    output$var_AI_path_actionbttn_rcbd = renderUI({  
      if (is.null(input$checkforAI_rcbd)) {
        return()
      }
      if (input$checkforAI_rcbd > 0) {
        list(
          actionBttn(
            inputId = "aibttn_path_rcbd",
            label = "Click here for interpretation",
            color = "warning",
            size = "sm",
            style = "fill"
          )
        )
      }
    })
    ############## these AI prompts works after observing submit on file upload 
    ### obsereve event AI button pressed
    observeEvent(input$aibttn_path_rcbd, {
      data_rcbd<-rcbd_table()
      t_rcbd <- as.numeric(data_rcbd$no_trt)#no. of treatments
      r<-data_rcbd$r# replication
      ##for A
      rcbd_char<-data_rcbd$rcbd_char
      rcbd_star<-data_rcbd$rcbd_star
      rcbd_pvalA<-data_rcbd$rcbd_pvalA
      rcbd_char_sig <- rcbd_char[rcbd_star != "NS"]
      rcbd_pval_sig <- rcbd_pvalA[rcbd_star != "NS"]
     
      test_name <- input$rcbd_mcp
      level <- input$rcbd_level
      
      #Original names
      org_Treatment<-data_rcbd$org_Treatment
      org_Day<-data_rcbd$org_Day
      
      #levels
      levelsA<-as.numeric(data_rcbd$no_trt)
      levelsB<-as.numeric(data_rcbd$no_trtB)
      levelsAB<-as.numeric(data_rcbd$no_trtAB)
      
      t_A<-levelsA
      
      #level names
      levelnamesA<-data_rcbd$levelnamesA
      levelnamesB<-data_rcbd$levelnamesB
      
      # Sort level names in mixed order
      levelnamesA <- gtools::mixedsort(levelnamesA)
      levelnamesB <- gtools::mixedsort(levelnamesB)
      
      # Function to format level names with '&' before the last element
      format_levels <- function(levels) {
        if (length(levels) > 1) {
          paste(paste(levels[-length(levels)], collapse = ", "), "&", levels[length(levels)])
        } else {
          levels  # If there's only one level, return it as is
        }
      }
      
      # Apply function to both level sets
      formatted_levelsA <- format_levels(levelnamesA)
      formatted_levelsB <- format_levels(levelnamesB)
      
      # Define test descriptions
      test_descriptions <- list(
        "LSD" = "Least Significant Difference (LSD) test also known as Fisher's protected LSD.",
        "TUKEY" = "Tukey's Honestly Significant Difference (HSD) test."
        #"DMRT" = "Duncan's Multiple Range Test (DMRT)."
      )
      
      full_test_name <- test_descriptions[[test_name]]
      # Additional info based on selected test
      additional_info <- switch(test_name,
                                "LSD" = " LSD compares adjusted (EMM) means. It provides pairwise differences, standard errors, t ratios, and p values.",
                                "TUKEY" = " Tukey's HSD on adjusted means returns adjusted pairwise differences, standard errors, t ratios, and p values. "
                                #"DMRT" = " DMRT will give critical range tables, which is used for pairwise comparison of treatment means.Note that DMRT will give a range of values which different for different treatment pairs."
      )
      replication_text <- paste0(
        "This is a Repeated measures one way ANOVA. ", org_Day,
        " has ", levelsA, " levels namely ", formatted_levelsA,
        ". Each subject was measured repeatedly across all levels of ", org_Day,
        ". A total of ", unique(r), " subjects were used."
      )      
      ### for Treatment
      if (length(rcbd_char_sig) == 0) {
        sig_text <- " No significant differences between levels of Treatment were observed for any character in ANOVA."
      } else {
        char_pval_pairs <- paste0(rcbd_char_sig, " (p=", rcbd_pval_sig, ")")
        
        sig_text <- paste(
          " Significant differences among the levels of Treatment were observed in ANOVA for the following characters under study: ", 
          paste(char_pval_pairs, collapse = ", ") 
        )
      }
      
      
      ### for Treatment
      if (length(rcbd_char_sig) == 0) {
        rcbd_statement1 <- "For Treatment, as there is no significance detected for any character, no pairwise comparison test is performed."
      } else {
        rcbd_statement1 <- paste(
          ". Letter groupings were assigned to each levels of Treatment within each character under study based on the  post-hoc test, letters were given as superscripts in the table.",
          " Within each character under study, levels sharing at least one common letter are not significantly different and can be considered statistically on par."
        )
      }
      
      
      ## general statement about post hoc
      if (length(rcbd_char_sig) == 0) {
        paircomp <- NULL
      } else {
        paircomp <- paste(
          " In the table, significance is indicated by an asterisk (*) for the 5% level and two asterisks (**) for the 1% level of significance, displayed as superscripts for each corresponding F stat in the table, where F stat is the F value in the ANOVA table.",
          "For those characters in which significant difference is found, a post hoc pairwise comparison was performed to identify significant differences between treatments using",
          full_test_name, additional_info
        )
      }
      
      split_letters <- function(group) {
        if (is.na(group) || group == "") return(character(0))
        strsplit(tolower(trimws(as.character(group))), "")[[1]]
      }
      
      ############statement function for comparison
      create_rcbd_statements <- function(data) {
        # Filter the data for non-empty groups
        data <- data[data$group != "", ]
        
        # Get unique characters
        characters <- unique(data$character)
        
        # Initialize an empty list to store statements
        statements <- list()
        
        # Helper function to format values
        format_value <- function(value) {
          if (abs(value) < 0.01) {
            return(format(value, scientific = TRUE, digits = 2))
          } else {
            return(sprintf("%.2f", value))
          }
        }
        
        # Iterate over each character
        for (char in characters) {
          char_data <- data[data$character == char, ]
          
          # Identify the treatment with the highest mean
          highest_mean <- char_data[which.max(char_data$mean_value), ]
          # Identify the treatment with the lowest mean
          lowest_mean <- char_data[which.min(char_data$mean_value), ]
          
          # Extract letters from the groups of highest and lowest mean treatments
          # highest_group_letters <- strsplit(highest_mean$group, "")[[1]]
          # lowest_group_letters <- strsplit(lowest_mean$group, "")[[1]]
          highest_group_letters <- split_letters(highest_mean$group)
          lowest_group_letters <- split_letters(lowest_mean$group)
          
          # Find treatments that share common letters with the highest mean treatment
          on_par_high <- char_data[
            sapply(char_data$group, function(g) {
              any(split_letters(g) %in% highest_group_letters)
            }) & char_data$Treatment != highest_mean$Treatment, "Treatment"
          ]
          
          # Find treatments that share common letters with the lowest mean treatment
          on_par_low <- char_data[
            sapply(char_data$group, function(g) {
              any(split_letters(g) %in% lowest_group_letters)
            }) & char_data$Treatment != lowest_mean$Treatment, "Treatment"
          ]
          
          # Construct the on_par_high statement
          if (length(on_par_high) > 0) {
            on_par_high_statement <- paste0(highest_mean$Treatment, " is on par with ", paste(on_par_high, collapse = ", "), " as they have at least one letter in common.")
          } else {
            on_par_high_statement <- paste0(highest_mean$Treatment, " is significantly different from all others.")
          }
          
          # Construct the on_par_low statement
          if (length(on_par_low) > 0) {
            on_par_low_statement <- paste0(lowest_mean$Treatment, " is on par with ", paste(on_par_low, collapse = ", "), ".")
          } else {
            on_par_low_statement <- paste0(lowest_mean$Treatment, " is significantly different from all others.")
          }
          
          # Create the final statement
          statement <- paste0(
            "For character ", char, ", ",
            highest_mean$Treatment, " has the highest mean (", format_value(highest_mean$mean_value), " \u00B1 ", format_value(highest_mean$sd_value), ") and ",
            lowest_mean$Treatment, " has the lowest mean (", format_value(lowest_mean$mean_value), " \u00B1 ", format_value(lowest_mean$sd_value), "). ",
            on_par_high_statement, " ",
            on_par_low_statement
          )
          
          # Append the statement to the list
          statements[[char]] <- statement
        }
        
        # Combine the statements into paragraphs
        output <- paste(unlist(statements), collapse = " ")
        
        return(output)
      }
      ################################################    
      data_stmnt_AI1 <- rcbd_plotdata()
      data_stmnt_AI  <- data_stmnt_AI1$df_summaryA
      data_stmnt_AI  <- data_stmnt_AI %>% rename(Treatment = Treatment)
      
      statements <- create_rcbd_statements(data_stmnt_AI)
      rcbd_comp_state <- statements
      
      # Constructing 4 prompts
      prompt1 <- paste0(
        "Hi, you should relate the explanation below with the tables in the 'Analysis' tab. \\n",
        replication_text,"\\n",
        " \\n",
        "The studied characters were ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table is provided for each factor and also for interaction, may not be necessary for many studies but can be included if required to indicate effect size. \\n",
        # "Cohen's F Values less than 0.1 represent a very small effect, less than 0.25 a small effect, less than 0.4 a medium effect, \\n",
        "and 0.4 or higher a large effect. Interpret these effect sizes in the context of your study's objectives and findings. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If you have any doubts, please feel free to contact us. Our statisticians are there to help you. \\n\\n",
        "HAPPY STATISTICS with STATOBERRY!"
      )
      
      
      
      
      prompt2 <- paste0(
        "Hello, you should connect the explanation below with the tables in the analysis tab. \\n",
        replication_text, "\\n",
        " \\n",
        "The characters under study included ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size classifications: less than 0.1 is very small, less than 0.25 is small, less than 0.4 is medium, and 0.4 or higher is large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If you have any questions, please don't hesitate to reach out to us. Our team of statisticians is always ready to assist you. \\n\\n",
        "HAPPY STATISTICS with STATOBERRY!"
      )
      
      
      prompt3 <- paste0(
        "Hi, make sure to link the explanation below to the tables in the analysis section. \\n",
        replication_text, "\\n",
        " \\n",
        "The characters under study were ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "Should you have any queries, feel free to contact us. Our statisticians are here to support you. \\n\\n",
        "WE MAKE SOUR STATISTICS SWEET AT STATOBERRY!"
      )
      
      prompt4 <- paste0(
        "Hey, please relate the following explanation to the tables in the analysis tab. \\n",
        replication_text, "\\n",
        " \\n",
        "The characters under study were: ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If any questions arise, please reach out to us. Our statisticians are available to help. \\n\\n",
        "WE MAKE SOUR STATISTICS SWEET AT STATOBERRY!"
      )
      
      
      prompt5 <- paste0(
        "Hi, ensure that the explanation below corresponds with the tables in the analysis tab. \\n",
        replication_text, "\\n",
        " \\n",
        "The characters under study were: ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If you have any questions, feel free to contact us. Our statisticians are always ready to assist you. \\n\\n",
        "ENJOY STATISTICS WITH STATOBERRY!"
      )
      
      prompt6 <- paste0(
        "Hello, the explanation provided should be tied to the tables in the analysis tab. \\n",
        replication_text, "\\n",
        " \\n",
        "The characters under study were: ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "Should you have any questions, don't hesitate to reach out. Our statisticians are here to help. \\n\\n",
        "STAY SWEET WITH STATISTICS AT STATOBERRY!"
      )
      
      prompt7 <- paste0(
        "Hi, try to associate the explanation below with the tables in the analysis section. \\n",
        replication_text, "\\n",
        " \\n",
        "The study included the evaluation of ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If you need further clarification, please contact us. Our statisticians are happy to assist you. \\n\\n",
        "EMBRACE STATISTICS WITH STATOBERRY!"
      )
      
      
      prompt8 <- paste0(
        "Hey, you need to match the explanation below to the tables in the analysis tab. \\n",
        replication_text, "\\n",
        " \\n",
        "The characteristics we studied were ", paste(rcbd_char, collapse = ", "), ". \\n",
        paircomp,"\\n",
        sig_text, " ", rcbd_statement1, " ", rcbd_comp_state, " \\n",
        # "Cohen's F values in the table indicate effect size. Values less than 0.1 are very small, less than 0.25 are small, less than 0.4 are medium, and 0.4 or higher are large. Interpret this effect size as relevant to your study. \\n\\n",
        "Analysis was performed using RAISINS (R & AI Solutions for INferential Statistics) (Hisham et al., 2025; R Core Team, 2024).\\n",
        "References:\\n",
        "1. Hisham, M. M., Chandran, J., & Gopinath, P. P. (2025). RAISINS: Integrating R and AI for Agricultural Data Analysis.\\n",
        "   Journal of Sustainable Technology in Agriculture, 1(1). PAPAYA Academic Press. https://doi.org/10.5281/zenodo.15622128\\n",
        "2. R Core Team. 2024. R: A language and environment for statistical computing. \\n",
        "   R Foundation for Statistical Computing, Vienna, Austria. Available from https://www.R-project.org/. \\n\\n",
        "If you have any queries, please feel free to reach out. Our statisticians are available to help. \\n\\n",
        "HAPPY STATISTICS WITH STATOBERRY!"
      )
      
      # Store prompts in a list
      prompts <- list(prompt1, prompt2, prompt3, prompt4, prompt5,
                      prompt6, prompt7, prompt8)
      
      # Randomly select a prompt
      prompt <- sample(prompts, 1)[[1]]
      
      output$AI_typing_path_rcbd <- renderUI({  ##arshi(15-08-26) copy + stop toolbar

        # --- build plain-text version for Copy (prompt uses literal \n) ---
        final_plain <- gsub("\\\\n", "\n", prompt)   # prompt has literal \n
        final_plain <- trimws(final_plain)

        # --- HTML version for Stop (newlines -> <br>) ---
        final_html <- gsub("\n", "<br>", final_plain)

        # --- escaped string for the typing animation ---
        escaped_prompt <- gsub("\\\\", "\\\\\\\\", final_plain)
        escaped_prompt <- gsub("'", "\\\\'", escaped_prompt)
        escaped_prompt <- gsub("\n", "\\\\n", escaped_prompt)

        tagList(
          # toolbar
          HTML(
            "<div style='display:flex; gap:10px; margin-bottom:10px;'>
       <button type='button' onclick='raisinsRcbdCopy(this)'
         style='background:#4682B4; color:#fff; border:none; border-radius:6px;
                padding:6px 14px; font-size:13px; font-weight:600; cursor:pointer;'>
         <i class='fa fa-copy'></i> Copy
       </button>
       <button type='button' onclick='raisinsRcbdStop(this)'
         style='background:#c0392b; color:#fff; border:none; border-radius:6px;
                padding:6px 14px; font-size:13px; font-weight:600; cursor:pointer;'>
         <i class='fa fa-stop'></i> Stop
       </button>
     </div>"
          ),

          # hidden holders
          div(id = "typed-rcbd-full", style = "display:none;", HTML(final_html)),
          tags$textarea(id = "typed-rcbd-plain", style = "display:none;", final_plain),

          tags$script(HTML(paste0(
            "window.raisinsRcbdStop = function(btn){",
            "  window.raisinsRcbdStopped = true;",
            "  if(window.raisinsRcbdTimer){ clearTimeout(window.raisinsRcbdTimer); }",
            "  var full=document.getElementById('typed-rcbd-full');",
            "  var vis=document.getElementById('output_path_typing_rcbd');",
            "  if(full&&vis){ vis.innerHTML=full.innerHTML; }",
            "  var sp=document.getElementById('loading-spinner_rcbd'); if(sp){ sp.style.display='none'; }",
            "  var tx=document.getElementById('loading-text_rcbd'); if(tx){ tx.style.display='none'; }",
            "};",
            "window.raisinsRcbdCopy = function(btn){",
            "  var el=document.getElementById('typed-rcbd-plain');",
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
            "  window.raisinsRcbdStopped=false;",
            "  var i=0; var txt='", escaped_prompt, "'; var speed=10;",
            "  var target=document.getElementById('output_path_typing_rcbd');",
            "  if(target){ target.innerHTML='';",
            "    var sp=document.getElementById('loading-spinner_rcbd'); if(sp){ sp.style.display='none'; }",
            "    var tx=document.getElementById('loading-text_rcbd'); if(tx){ tx.style.display='none'; }",
            "    function typeWriter(){",
            "      if(window.raisinsRcbdStopped) return;",
            "      if(i<txt.length){",
            "        var c=txt.charAt(i);",
            "        target.innerHTML += (c==='\\n') ? '<br>' : c;",
            "        i++; window.raisinsRcbdTimer=setTimeout(typeWriter,speed);",
            "      }",
            "    }",
            "    typeWriter();",
            "  }",
            "})();"
          )))
        )
      })
      
      
      
      
      output$var_AI_path_end_rcbd <- renderUI({
        HTML(paste0("<p style='color: red; font-weight: bold; font-size: 10px; text-align: justify;'>Warning: Please note that the statistical interpretations generated by RAISINS AI are intended for reference purposes only and should be thoroughly reviewed and verified by a human expert before being utilized in any research publications. It is important to exercise caution and ensure the accuracy and validity of the output through rigorous human analysis and review. Also please note that if there is any error in uploaded csv file it will affect Interpretation. So confirm there is no error in Analysis.Our expert statisticians are available for consultation based on generated report.</p>"))
      })
    }) #observe event for AI button
    
    
    ###### display writing for selected multiple comparison test
    rcbd_summary_text <- reactive({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (!is.null(input$rcbd_mcp) && !is.null(input$rcbd_level)) {
        req(rcbd_table())
        selected_correction <- input$correction_method
        test_name <- input$rcbd_mcp
        level <- input$rcbd_level
        data_rcbd<-rcbd_table()
        
        t_rcbd <- as.numeric(data_rcbd$no_trt)#no. of treatments
        r<-as.numeric(data_rcbd$r)# replication
        block_no <-as.numeric(unique(r))
        #Original names
        org_Treatment<-data_rcbd$org_Treatment
        org_Day<-data_rcbd$org_Day
        
        ## ##arshi one-way RM - single within-subject factor (Day). The two-way
        ## B / AB fields (no_trtB, no_trtAB, levelnamesB) are not published by the
        ## one-way store; reading them yielded zero-length values that collapsed
        ## the sprintf below to character(0), so the summary rendered blank.
        #levels
        levelsA<-as.numeric(data_rcbd$no_trt)   # number of Day (within-subject) levels

        #level names
        levelnamesA<-data_rcbd$levelnamesA
        # Sort level names in mixed order
        levelnamesA <- gtools::mixedsort(levelnamesA)
        
        # Function to format level names with '&' before the last element
        format_levels <- function(levels) {
          if (length(levels) > 1) {
            paste(paste(levels[-length(levels)], collapse = ", "), "&", levels[length(levels)])
          } else {
            levels  # If there's only one level, return it as is
          }
        }
        
        # Format the Day level names
        formatted_levelsA <- format_levels(levelnamesA)
        
        # Define test descriptions
        test_descriptions <- list(
          "LSD" = "Least Significant Difference (LSD)",
          "TUKEY" = "Tukey's Honestly Significant Difference (HSD)"
          #"DMRT" = "Duncan's Multiple Range Test (DMRT)"
          
        )
        
        full_test_name <- test_descriptions[[test_name]]
        
        # Additional info based on selected test
        additional_info <- switch(test_name,
                                  "LSD" = "Critical difference (CD) values were used for pairwise comparisons.",
                                  "TUKEY" = "HSD values were used for pairwise comparisons."
                                  #"DMRT" = "DMRT critical range table was used for pairwise comparisons."
                                  
        )
        
        correction_descriptions <- list(
          "none" = "No correction",
          "GG" = "Greenhouse Geisser(GG) correction",
          "HF" = "Huynh Feldt(HF) correction"
          
        )
        
        full_correction_name <- correction_descriptions[[selected_correction]]
        # Create output text
        output_text <- sprintf(
          "A one way repeated measures ANOVA was conducted with
   <strong style='color:green;'>%s</strong> subjects, each measured over
   <strong style='color:green;'>%s</strong> <strong style='color:green;'>%s</strong>
   (<strong style='color:green;'>%s</strong>).
  Pairwise comparisons were performed using the <strong style='color:green;'>%s</strong> (post-hoc test)
  at a <strong style='color:green;'>%s</strong> level of significance. You have selected %s for Sphericity Correction.
  The first table presents the ANOVA summary, including the mean squares for the
  <strong style='color:green;'>%s</strong> effect and the error term along with their significance levels.
  The subsequent table provides character wise summaries and descriptive statistics, with significant
  differences indicated by letter groupings as superscripts based on the selected post-hoc test.",
          block_no, levelsA, org_Day, formatted_levelsA, full_test_name, level, full_correction_name, org_Day
        )
        return(output_text)
      } else {
        return(NULL)
      }
    })
    
    # In your UI output, use the reactive summary_text
    output$rcbd_selection_summary <- renderUI({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      if (input$rcbd_trans_switch == TRUE) { 
        if (is.null(input$rcbd_log_selvar) && is.null(input$rcbd_sq_selvar) && is.null(input$rcbd_sin_selvar)) {
          return()
        }
      }
      
      text <- rcbd_summary_text()  # Call the reactive expression
      if (!is.null(text)) {
        HTML(text)
      } else {
        return(NULL)
      }
    })
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                             PLOTTING                            ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ###Boxplot server----
    
    # Helper Functions
    get_boxplot_theme <- function(theme_name) {
      switch(theme_name,
             "normal" = theme_bw(),
             "economist" = theme_economist(),
             "minimal" = theme_minimal(),
             "gray" = theme_gray(),
             "light" = theme_light(),
             "void" = theme_void(),
             "tufte" = theme_tufte(),
             "stata" = theme_stata(),
             "wsj" = theme_wsj(),
             "calc" = theme_calc(),
             "hc" = theme_hc(),
             theme_bw())
    }
    
    get_line_type <- function(linetype_name) {
      switch(tolower(gsub(" ", "", linetype_name)),
             "solid" = "solid",
             "dotted" = "dotted",
             "dashed" = "dashed",
             "blank" = "blank",
             "twodash" = "twodash",
             "longdash" = "longdash",
             "dotdash" = "dotdash",
             "solid")
    }
    
    # Enhanced plot styling function with more customization options
    create_enhanced_plot_styling <- function(input, mycolors_boxplot, rcbd_box_theme, rcbd_box_linetype, is_individual = FALSE, y_var_name = NULL) {
      
      
      # Create styling components
      styling_list <- list(
        geom_boxplot(
          linetype = rcbd_box_linetype,
          size = input$rcbd_box_size,
          outlier.size = input$rcbd_outlier_size,
          outlier.colour = input$rcbd_outlier_color %||% "black",
          outlier.alpha = input$rcbd_outlier_alpha %||% 1,
          position = position_dodge2(preserve = "total"),
          alpha = input$rcbd_box_alpha %||% 1
        ),
        scale_fill_manual(values = mycolors_boxplot),
        scale_y_continuous(
          expand = expansion(mult = c(input$rcbd_y_expand_lower %||% 0.1,
                                      input$rcbd_y_expand_upper %||% 0.2))
        ),
        rcbd_box_theme,
        xlab(input$x_box_rcbd),
        ylab(input$y_box_rcbd),
        ggtitle(input$title_box_rcbd),
        labs(fill = input$side_box_rcbd),
        theme(
          # Plot title customization
          plot.title = element_text(
            size = input$rcbd_title_size %||% 14,
            color = input$rcbd_title_color %||% "black",
            face = if(input$rcbd_title_bold %||% FALSE) "bold" else "plain",
            hjust = input$rcbd_title_hjust %||% 0.5,
            vjust = input$rcbd_title_vjust %||% 0.5
          ),
          
          # Axis titles
          axis.title.x = element_text(
            size = input$rcbd_x_title_size %||% 12,
            color = input$rcbd_x_title_color %||% "black",
            face = if(input$rcbd_x_title_bold %||% FALSE) "bold" else "plain"
          ),
          axis.title.y = element_text(
            size = input$rcbd_y_title_size %||% 12,
            color = input$rcbd_y_title_color %||% "black",
            face = if(input$rcbd_y_title_bold %||% FALSE) "bold" else "plain"
          ),
          
          # Strip text (for faceted plots)
          strip.text.x = element_text(
            size = input$rcbd_box_wtextsize,
            colour = input$boxwtextcolr,
            face = if(input$rcbd_strip_text_bold %||% FALSE) "bold" else "plain"
          ),
          strip.background = element_rect(
            colour = input$boxwbordercolr,
            fill = input$boxwbgcolr,
            size = input$rcbd_strip_border_size %||% 0.5
          ),
          
          # Plot background
          plot.background = element_rect(
            fill = input$rcbd_plot_bg_color %||% "white",
            color = input$rcbd_plot_border_color %||% "white",
            size = input$rcbd_plot_border_size %||% 0
          ),
          panel.background = element_rect(
            fill = input$rcbd_panel_bg_color %||% "white",
            color = input$rcbd_panel_border_color %||% "grey50",
            size = input$rcbd_panel_border_size %||% 0.5
          ),
          
          # Grid lines
          panel.grid.major = if(input$rcbd_show_major_grid %||% TRUE) {
            element_line(
              color = input$rcbd_major_grid_color %||% "grey90",
              size = input$rcbd_major_grid_size %||% 0.5,
              linetype = input$rcbd_major_grid_linetype %||% "solid"
            )
          } else {
            element_blank()
          },
          panel.grid.minor = if(input$rcbd_show_minor_grid %||% TRUE) {
            element_line(
              color = input$rcbd_minor_grid_color %||% "grey95",
              size = input$rcbd_minor_grid_size %||% 0.25,
              linetype = input$rcbd_minor_grid_linetype %||% "solid"
            )
          } else {
            element_blank()
          },
          
          # Legend customization
          legend.position = input$rcbd_legend_position %||% "right",
          legend.title = element_text(
            size = input$rcbd_legend_title_size %||% 11,
            color = input$rcbd_legend_title_color %||% "black",
            face = if(input$rcbd_legend_title_bold %||% FALSE) "bold" else "plain"
          ),
          legend.text = element_text(
            size = input$rcbd_legend_text_size %||% 9,
            color = input$rcbd_legend_text_color %||% "black"
          ),
          legend.background = element_rect(
            fill = input$rcbd_legend_bg_color %||% "white",
            color = input$rcbd_legend_border_color %||% "black",
            size = input$rcbd_legend_border_size %||% 0.5
          ),
          
          # Aspect ratio with conditional logic
          aspect.ratio = if(input$rcbd_use_aspect_ratio %||% TRUE) {
            input$rcbd_aspect_ratio %||% 1
          } else {
            NULL
          },
          
          # X-axis text styling
          axis.text.x = if (input$rcbd_box_xaxislab) {
            element_blank()
          } else {
            element_text(
              angle = input$rcbd_boxplot_angle,
              size = input$rcbd_box_elSize,
              color = input$rcbd_box_x_axis_text_color,
              face = if(input$rcbd_box_x_axis_text_bold) "bold" else "plain",
              hjust = input$rcbd_x_text_hjust %||% 0.5,
              vjust = input$rcbd_x_text_vjust %||% 0.5
            )
          },
          
          # Y-axis text styling
          axis.text.y = element_text(
            color = input$rcbd_box_y_axis_text_color,
            face = if(input$rcbd_box_y_axis_text_bold) "bold" else "plain",
            size = input$rcbd_box_y_axis_text_size,
            hjust = input$rcbd_y_text_hjust %||% 0.5,
            vjust = input$rcbd_y_text_vjust %||% 0.5
          ),
          
          # Axis ticks styling
          axis.ticks.x = if (input$rcbd_box_xaxislab) {
            element_blank()
          } else {
            element_line(
              color = input$rcbd_box_x_axis_tick_color,
              size = input$rcbd_x_tick_size %||% 0.5
            )
          },
          axis.ticks.y = element_line(
            color = input$rcbd_box_y_axis_tick_color,
            size = input$rcbd_y_tick_size %||% 0.5
          ),
          
          # Axis line styling
          axis.line.x = element_line(
            color = input$rcbd_x_axis_line_color %||% "black",
            size = input$rcbd_x_axis_line_size %||% 0.5
          ),
          axis.line.y = element_line(
            color = input$rcbd_y_axis_line_color %||% "black",
            size = input$rcbd_y_axis_line_size %||% 0.5
          )
        )
      )
      
      return(styling_list)
    }
    
    # Enhanced data validation function - simplified for RCBD like CRD
    validate_plot_data <- function(rcbd_plotting) {
      if (is.null(rcbd_plotting)) {
        return(list(valid = FALSE, message = "No plot data available"))
      }
      
      # Check for required components - use factor A as primary source like CRD uses df_datalong
      if (!is.null(rcbd_plotting$df_datalongA)) {
        required_components <- c("df_datalongA", "df_summaryA", "trt_noA")
        missing_components <- required_components[!sapply(required_components, function(x) !is.null(rcbd_plotting[[x]]))]
        
        if (length(missing_components) > 0) {
          return(list(valid = FALSE, message = paste("Missing components:", paste(missing_components, collapse = ", "))))
        }
        
        if (nrow(rcbd_plotting$df_datalongA) == 0) {
          return(list(valid = FALSE, message = "No data in df_datalongA"))
        }
        
        required_columns_long <- c("Treatment", "value", "character")
        missing_cols_long <- required_columns_long[!required_columns_long %in% names(rcbd_plotting$df_datalongA)]
        
        if (length(missing_cols_long) > 0) {
          return(list(valid = FALSE, message = paste("Missing columns in df_datalongA:", paste(missing_cols_long, collapse = ", "))))
        }
      } else {
        return(list(valid = FALSE, message = "df_datalongA is missing"))
      }
      
      return(list(valid = TRUE, message = "Data is valid"))
    }
    
    # Helper function to create error plots
    create_error_plot <- function(error_message) {
      ggplot() +
        geom_text(aes(x = 0, y = 0, label = error_message),
                  size = 4, color = "red") +
        theme_void() +
        labs(title = "Plot Error")
    }
    
    # Main boxplot reactive function - simplified like CRD
    rcbd_boxplot_reactive <- reactive({
      
      # Validate input data availability
      req(rcbd_plotdata())
      
      tryCatch({
        # Get and validate the rcbd data
        rcbd_plotting <- rcbd_plotdata()
        validation_result <- validate_plot_data(rcbd_plotting)
        
        if (!validation_result$valid) {
          return(create_error_plot(validation_result$message))
        }
        
        # Select data based on the selected factor - but use same variable selection approach as CRD
        if (input$selected_factor_box == "A") {
          rcbd_data_long <- rcbd_plotting$df_datalongA
          rcbd_data_plot <- rcbd_plotting$df_summaryA
          t_rcbd <- rcbd_plotting$trt_noA
          Treatment <- "Treatment"
        } else {
          return(create_error_plot("Invalid selection"))
        }
        
        # Validate selected data
        if (is.null(rcbd_data_long) || is.null(t_rcbd)) {
          return(create_error_plot("Selected data is not available"))
        }
        
        if (nrow(rcbd_data_long) == 0) {
          return(create_error_plot("No data available for selected factor"))
        }
        
        # Create color palette with error handling
        mycolors_boxplot <- tryCatch({
          palette_name <- input$`pal.rcbd_BOXP` %||% "Dark2"
          colorRampPalette(brewer.pal(min(8, max(3, t_rcbd)), palette_name))(t_rcbd + 2)
        }, error = function(e) {
          rainbow(t_rcbd + 2)
        })
        
        # Sort treatments by their order
        rcbd_data_long <- rcbd_data_long[gtools::mixedorder(rcbd_data_long[[Treatment]]), ]
        rcbd_data_long[[Treatment]] <- factor(rcbd_data_long[[Treatment]], levels = unique(rcbd_data_long[[Treatment]]))
        
        # Filter data based on selected y-values (if individual mode is selected) - SAME AS CRD
        if (input$rcbd_box_mode == "individual" && !is.null(input$rcbd_selected_yvalues)) {
          rcbd_data_long <- rcbd_data_long[rcbd_data_long$character %in% input$rcbd_selected_yvalues, ]
          if (!is.null(rcbd_data_plot)) {
            rcbd_data_plot <- rcbd_data_plot[rcbd_data_plot$character %in% input$rcbd_selected_yvalues, ]
          }
        }
        
        # Validate filtered data
        if (nrow(rcbd_data_long) == 0) {
          return(create_error_plot("No data available for selected variables"))
        }
        
        # Get theme and linetype
        rcbd_box_theme <- get_boxplot_theme(input$rcbd_box_theme %||% "normal")
        rcbd_box_linetype <- get_line_type(input$rcbd_box_linetype %||% "Solid")
        
        # Create base plot
        p <- ggplot(rcbd_data_long, aes(x = .data[[Treatment]], y = value, fill = .data[[Treatment]]))
        
        # For individual mode, pass the y-variable name if there's only one selected - SAME AS CRD
        y_var_name <- if (input$rcbd_box_mode == "individual" &&
                          !is.null(input$rcbd_selected_yvalues) &&
                          length(input$rcbd_selected_yvalues) == 1) {
          input$rcbd_selected_yvalues[1]
        } else {
          NULL
        }
        
        # Add enhanced styling
        styling_components <- create_enhanced_plot_styling(
          input, mycolors_boxplot, rcbd_box_theme, rcbd_box_linetype,
          is_individual = (input$rcbd_box_mode == "individual"),
          y_var_name = y_var_name
        )
        
        p <- p + styling_components
        
        # Add faceting based on mode selection - SAME AS CRD
        if (input$rcbd_box_mode == "faceted") {
          p <- p + facet_wrap(~character, scales = "free_y", ncol = input$rcbd_box_ncol %||% 3)
        }
        
        # Add significant letters if needed - SAME AS CRD
        if ((input$rcbd_box_letter %||% TRUE) && !is.null(rcbd_data_plot) && nrow(rcbd_data_plot) > 0 && "group" %in% names(rcbd_data_plot)) {
          p <- p + geom_text(
            data = rcbd_data_plot,
            aes(
              x = .data[[Treatment]],
              y = mean_value + (sd_value + (sd_value / 3)),
              label = group
            ),
            color = input$col_box_colour %||% "black",
            size = input$rcbd_sig_size %||% 3,
            position = position_dodge(0.5),
            vjust = input$rcbd_box_vjust %||% -0.5,
            inherit.aes = FALSE
          )
        }
        
        # Add additional customization layers - SAME AS CRD
        if (input$rcbd_add_mean_points %||% FALSE) {
          p <- p + stat_summary(
            fun = mean, geom = "point",
            size = input$rcbd_mean_point_size %||% 2,
            color = input$rcbd_mean_point_color %||% "red",
            shape = input$rcbd_mean_point_shape %||% 16
          )
        }
        
        if (input$rcbd_add_median_line %||% FALSE) {
          p <- p + stat_summary(
            fun = median, geom = "crossbar",
            width = input$rcbd_median_line_width %||% 0.5,
            color = input$rcbd_median_line_color %||% "blue",
            size = input$rcbd_median_line_size %||% 0.5
          )
        }
        
        return(p)
        
      }, error = function(e) {
        return(create_error_plot(paste("Error creating plot:", e$message)))
      })
    })
    
    # Enhanced observer with comprehensive error handling
    observe({
      tryCatch({
        # Check if rcbd_plotdata exists and is not null
        if (exists("rcbd_plotdata") && is.function(rcbd_plotdata)) {
          plotting_data <- rcbd_plotdata()
          
          # Validate the plotting data
          validation_result <- validate_plot_data(plotting_data)
          
          if (validation_result$valid) {
            available_y_vars <- unique(plotting_data$df_datalongA$character)
            
            # Only update if we have valid data
            if (length(available_y_vars) > 0) {
              updateSelectInput(session, "rcbd_selected_yvalues",
                                choices = available_y_vars,
                                selected = if(length(available_y_vars) > 0) available_y_vars[1] else NULL)
            }
          } else {
            # Clear selections if data is invalid
            updateSelectInput(session, "rcbd_selected_yvalues",
                              choices = NULL,
                              selected = NULL)
          }
        } else {
          # Clear selections if function doesn't exist
          updateSelectInput(session, "rcbd_selected_yvalues",
                            choices = NULL,
                            selected = NULL)
        }
      }, error = function(e) {
        # Clear selections on any error
        updateSelectInput(session, "rcbd_selected_yvalues",
                          choices = NULL,
                          selected = NULL)
      })
    })
    
    # Render the rcbd boxplot with enhanced error handling
    output$rcbd_boxplot <- renderPlot({
      plot_obj <- rcbd_boxplot_reactive()
      req(plot_obj)
      plot_obj
    }, res = 96)
    
    # Enhanced download handler with quality settings
    output$downloadrcbd_boxplot <- downloadHandler(
      filename = function() {
        paste0("rcbd_boxplot-", Sys.Date(), ".", input$rcbd_box_plot_format %||% "png")
      },
      content = function(file) {
        # Show loading modal
        showModal(modalDialog(
          title = "Generating high-quality plot...",
          tags$div(
            style = "text-align: center;",
            tags$img(
              src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_rcbd%2Fdownloadplot.gif?alt=media",
              style = "width: 80%; height: auto;"
            )
          ),
          footer = NULL
        ))
        
        tryCatch({
          # Get the plot
          plot_obj <- isolate(rcbd_boxplot_reactive())
          
          # Validate plot object
          if (is.null(plot_obj)) {
            stop("Plot object is null")
          }
          
          # Get quality settings from inputs
          plot_width <- input$rcbd_plot_width %||% 10
          plot_height <- input$rcbd_plot_height %||% 8
          plot_dpi <- input$rcbd_plot_dpi %||% 300
          
          # Save with user-defined quality parameters
          fmt <- input$rcbd_box_plot_format %||% "png"
          switch(fmt,
                 "png"  = { png(file,  width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "jpeg" = { jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "tiff" = { tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw", bg = "white"); print(plot_obj); dev.off() },
                 "pdf"  = { cairo_pdf(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() },
                 "svg"  = { svglite::svglite(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() }
          )
          
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("Failed to save plot:", e$message),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, finally = {
          removeModal()
        })
      }
    )
    
    
    ############################################# rcbd_boxplot end
    ###########Violin Plot ----
    
    # Enhanced violin plot styling function with more customization options
    create_enhanced_violin_styling <- function(input, mycolors_violinplot, rcbd_violin_theme, is_individual = FALSE, y_var_name = NULL) {
      
      # Create styling components
      styling_list <- list(
        geom_violin(
          trim = input$rcbd_violin_trim %||% FALSE,
          position = position_dodge(input$rcbd_violin_dodge %||% 0.9),
          alpha = input$rcbd_violin_alpha %||% 1,
          scale = input$rcbd_violin_scale %||% "area",
          quantiles.linetype = if(input$rcbd_violin_show_quantiles %||% FALSE) {
            c(0.25, 0.5, 0.75)
          } else {
            NULL
          }
        ),
        
        # Conditional boxplot overlay
        if(input$rcbd_violin_show_boxplot %||% TRUE) {
          geom_boxplot(
            width = input$rcbd_violinbox_size %||% 0.1,
            position = position_dodge2(preserve = "total"),
            outlier.size = input$rcbd_violin_outlier_size %||% 1,
            outlier.colour = input$rcbd_violin_outlier_color %||% "black",
            outlier.alpha = input$rcbd_violin_outlier_alpha %||% 1,
            alpha = input$rcbd_violinbox_alpha %||% 1
          )
        },
        
        scale_fill_manual(values = mycolors_violinplot),
        scale_y_continuous(
          expand = expansion(mult = c(input$rcbd_violin_y_expand_lower %||% 0.1,
                                      input$rcbd_violin_y_expand_upper %||% 0.2))
        ),
        rcbd_violin_theme,
        xlab(input$x_violin_rcbd),
        ylab(input$y_violin_rcbd),
        ggtitle(input$title_violin_rcbd),
        labs(fill = input$side_violin_rcbd),
        theme(
          # Plot title customization
          plot.title = element_text(
            size = input$rcbd_violin_title_size %||% 14,
            color = input$rcbd_violin_title_color %||% "black",
            face = if(input$rcbd_violin_title_bold %||% FALSE) "bold" else "plain",
            hjust = input$rcbd_violin_title_hjust %||% 0.5,
            vjust = input$rcbd_violin_title_vjust %||% 0.5
          ),
          
          # Axis titles
          axis.title.x = element_text(
            size = input$rcbd_violin_x_title_size %||% 12,
            color = input$rcbd_violin_x_title_color %||% "black",
            face = if(input$rcbd_violin_x_title_bold %||% FALSE) "bold" else "plain"
          ),
          axis.title.y = element_text(
            size = input$rcbd_violin_y_title_size %||% 12,
            color = input$rcbd_violin_y_title_color %||% "black",
            face = if(input$rcbd_violin_y_title_bold %||% FALSE) "bold" else "plain"
          ),
          
          # Strip text (for faceted plots)
          strip.text.x = element_text(
            size = input$rcbd_violin_wtextsize,
            colour = input$violinwtextcolr,
            face = if(input$rcbd_violin_strip_text_bold %||% FALSE) "bold" else "plain"
          ),
          strip.background = element_rect(
            colour = input$violinwbordercolr,
            fill = input$violinwbgcolr,
            linewidth = input$rcbd_violin_strip_border_size %||% 0.5
          ),
          
          # Plot background
          plot.background = element_rect(
            fill = input$rcbd_violin_plot_bg_color %||% "white",
            color = input$rcbd_violin_plot_border_color %||% "white",
            size = input$rcbd_violin_plot_border_size %||% 0
          ),
          panel.background = element_rect(
            fill = input$rcbd_violin_panel_bg_color %||% "white",
            color = input$rcbd_violin_panel_border_color %||% "grey50",
            size = input$rcbd_violin_panel_border_size %||% 0.5
          ),
          
          # Grid lines
          panel.grid.major = if(input$rcbd_violin_show_major_grid %||% TRUE) {
            element_line(
              color = input$rcbd_violin_major_grid_color %||% "grey90",
              linewidth = input$rcbd_violin_major_grid_size %||% 0.5,
              linetype = input$rcbd_violin_major_grid_linetype %||% "solid"
            )
          } else {
            element_blank()
          },
          panel.grid.minor = if(input$rcbd_violin_show_minor_grid %||% TRUE) {
            element_line(
              color = input$rcbd_violin_minor_grid_color %||% "grey95",
              size = input$rcbd_violin_minor_grid_size %||% 0.25,
              linetype = input$rcbd_violin_minor_grid_linetype %||% "solid"
            )
          } else {
            element_blank()
          },
          
          # Legend customization
          legend.position = input$rcbd_violin_legend_position %||% "right",
          legend.title = element_text(
            size = input$rcbd_violin_legend_title_size %||% 11,
            color = input$rcbd_violin_legend_title_color %||% "black",
            face = if(input$rcbd_violin_legend_title_bold %||% FALSE) "bold" else "plain"
          ),
          legend.text = element_text(
            size = input$rcbd_violin_legend_text_size %||% 9,
            color = input$rcbd_violin_legend_text_color %||% "black"
          ),
          legend.background = element_rect(
            fill = input$rcbd_violin_legend_bg_color %||% "white",
            color = input$rcbd_violin_legend_border_color %||% "black",
            size = input$rcbd_violin_legend_border_size %||% 0.5
          ),
          
          # Aspect ratio with conditional logic
          aspect.ratio = if(input$rcbd_violin_use_aspect_ratio %||% TRUE) {
            input$rcbd_violin_aspect_ratio %||% 1
          } else {
            NULL
          },
          
          # X-axis text styling
          axis.text.x = if (input$rcbd_violin_xaxislab) {
            element_blank()
          } else {
            element_text(
              angle = input$rcbd_violin_angle,
              size = input$rcbd_violin_elSize,
              color = input$rcbd_violin_x_axis_text_color,
              face = if(input$rcbd_violin_x_axis_text_bold) "bold" else "plain",
              hjust = input$rcbd_violin_x_text_hjust %||% 0.5,
              vjust = input$rcbd_violin_x_text_vjust %||% 0.5
            )
          },
          
          # Y-axis text styling
          axis.text.y = element_text(
            color = input$rcbd_violin_y_axis_text_color,
            face = if(input$rcbd_violin_y_axis_text_bold) "bold" else "plain",
            size = input$rcbd_violin_y_axis_text_size,
            hjust = input$rcbd_violin_y_text_hjust %||% 0.5,
            vjust = input$rcbd_violin_y_text_vjust %||% 0.5
          ),
          
          # Axis ticks styling
          axis.ticks.x = if (input$rcbd_violin_xaxislab) {
            element_blank()
          } else {
            element_line(
              color = input$rcbd_violin_x_axis_tick_color,
              size = input$rcbd_violin_x_tick_size %||% 0.5
            )
          },
          axis.ticks.y = element_line(
            color = input$rcbd_violin_y_axis_tick_color,
            size = input$rcbd_violin_y_tick_size %||% 0.5
          ),
          
          # Axis line styling
          axis.line.x = element_line(
            color = input$rcbd_violin_x_axis_line_color %||% "black",
            size = input$rcbd_violin_x_axis_line_size %||% 0.5
          ),
          axis.line.y = element_line(
            color = input$rcbd_violin_y_axis_line_color %||% "black",
            size = input$rcbd_violin_y_axis_line_size %||% 0.5
          )
        )
      )
      
      return(styling_list)
    }
    
    # Main violin plot reactive function with enhanced error handling
    rcbd_violinplot_reactive <- reactive({
      
      # Validate input data availability
      req(rcbd_plotdata())
      
      tryCatch({
        # Get and validate the rcbd data
        rcbd_plotting <- rcbd_plotdata()
        validation_result <- validate_plot_data(rcbd_plotting)
        
        if (!validation_result$valid) {
          return(create_error_plot(validation_result$message))
        }
        
        # Get organization location info
        org_Location <- rcbd_plotting$org_Location %||% "Unknown"
        
        # Select data based on the selected factor
        if (input$selected_factor_violin == "A") {
          rcbd_data_long <- rcbd_plotting$df_datalongA
          rcbd_data_plot <- rcbd_plotting$df_summaryA
          t_rcbd <- rcbd_plotting$trt_noA
          Treatment <- "Treatment"
        } else {
          return(create_error_plot("Invalid selection"))
        }
        
        # Validate selected data
        if (is.null(rcbd_data_long) || is.null(t_rcbd)) {
          return(create_error_plot("Selected data is not available"))
        }
        
        if (nrow(rcbd_data_long) == 0) {
          return(create_error_plot("No data available for selected factor"))
        }
        
        # Create color palette with error handling
        mycolors_violinplot <- tryCatch({
          palette_name <- input$`pal.rcbd_VIOLINP` %||% "Dark2"
          colorRampPalette(brewer.pal(min(8, max(3, t_rcbd)), palette_name))(t_rcbd + 2)
        }, error = function(e) {
          rainbow(t_rcbd + 2)
        })
        
        # Sort treatments by their order
        rcbd_data_long <- rcbd_data_long[gtools::mixedorder(rcbd_data_long[[Treatment]]), ]
        rcbd_data_long[[Treatment]] <- factor(rcbd_data_long[[Treatment]], levels = unique(rcbd_data_long[[Treatment]]))
        
        # Filter data based on selected y-values (if individual mode is selected)
        if (input$rcbd_violin_mode == "individual" && !is.null(input$rcbd_violin_selected_yvalues)) {
          rcbd_data_long <- rcbd_data_long[rcbd_data_long$character %in% input$rcbd_violin_selected_yvalues, ]
          if (!is.null(rcbd_data_plot)) {
            rcbd_data_plot <- rcbd_data_plot[rcbd_data_plot$character %in% input$rcbd_violin_selected_yvalues, ]
          }
        }
        
        # Validate filtered data
        if (nrow(rcbd_data_long) == 0) {
          return(create_error_plot("No data available for selected variables"))
        }
        
        # Get theme
        rcbd_violin_theme <- get_boxplot_theme(input$rcbd_violin_theme %||% "normal")
        
        # Create base plot
        p <- ggplot(rcbd_data_long, aes(x = .data[[Treatment]], y = value, fill = .data[[Treatment]]))
        
        # For individual mode, pass the y-variable name if there's only one selected
        y_var_name <- if (input$rcbd_violin_mode == "individual" &&
                          !is.null(input$rcbd_violin_selected_yvalues) &&
                          length(input$rcbd_violin_selected_yvalues) == 1) {
          input$rcbd_violin_selected_yvalues[1]
        } else {
          NULL
        }
        
        # Add enhanced styling
        styling_components <- create_enhanced_violin_styling(
          input, mycolors_violinplot, rcbd_violin_theme,
          is_individual = (input$rcbd_violin_mode == "individual"),
          y_var_name = y_var_name
        )
        
        p <- p + styling_components
        
        # Add faceting based on mode selection
        if (input$rcbd_violin_mode == "faceted") {
          p <- p + facet_wrap(~character, scales = "free_y", ncol = input$rcbd_violin_ncol %||% 3)
        }
        
        # Add significant letters if needed
        if (input$rcbd_violin_letter && !is.null(rcbd_data_plot) && nrow(rcbd_data_plot) > 0) {
          p <- p + geom_text(
            data = rcbd_data_plot,
            aes(
              x = .data[[Treatment]],
              y = mean_value + (sd_value + (4 * sd_value / 2)),
              label = group
            ),
            color = input$col_violin_colour,
            size = input$rcbd_violin_sig_size,
            position = position_dodge(0.5),
            vjust = -0.5,
            inherit.aes = FALSE
          )
        }
        
        # Add additional customization layers
        if (input$rcbd_violin_add_mean_points %||% FALSE) {
          p <- p + stat_summary(
            fun = mean, geom = "point",
            size = input$rcbd_violin_mean_point_size %||% 2,
            color = input$rcbd_violin_mean_point_color %||% "red",
            shape = input$rcbd_violin_mean_point_shape %||% 16
          )
        }
        
        if (input$rcbd_violin_add_median_line %||% FALSE) {
          p <- p + stat_summary(
            fun = median, geom = "crossbar",
            width = input$rcbd_violin_median_line_width %||% 0.5,
            color = input$rcbd_violin_median_line_color %||% "blue",
            size = input$rcbd_violin_median_line_size %||% 0.5
          )
        }
        
        if (input$rcbd_violin_add_jitter %||% FALSE) {
          p <- p + geom_jitter(
            width = input$rcbd_violin_jitter_width %||% 0.2,
            height = input$rcbd_violin_jitter_height %||% 0,
            alpha = input$rcbd_violin_jitter_alpha %||% 0.5,
            size = input$rcbd_violin_jitter_size %||% 1,
            color = input$rcbd_violin_jitter_color %||% "black"
          )
        }
        
        return(p)
        
      }, error = function(e) {
        return(create_error_plot(paste("Error creating violin plot:", e$message)))
      })
    })
    
    # Enhanced individual violin plots function
    rcbd_individual_violin_plots <- reactive({
      req(input$rcbd_violin_mode == "individual", input$rcbd_violin_selected_yvalues)
      
      tryCatch({
        rcbd_plotting <- rcbd_plotdata()
        validation_result <- validate_plot_data(rcbd_plotting)
        
        if (!validation_result$valid) {
          return(NULL)
        }
        
        # Get data based on selected factor
        factor_selection <- input$selected_factor_violin %||% "A"
        
        if (factor_selection == "A") {
          rcbd_data_long <- rcbd_plotting$df_datalongA
          rcbd_data_plot <- rcbd_plotting$df_summaryA
          t_rcbd <- rcbd_plotting$trt_noA
          Treatment <- "Treatment"
        } else if (factor_selection == "B") {
          rcbd_data_long <- rcbd_plotting$df_datalongB
          rcbd_data_plot <- rcbd_plotting$df_summaryB
          t_rcbd <- rcbd_plotting$trt_noB
          Treatment <- "Day"
        } else if (factor_selection == "AB") {
          rcbd_data_long <- rcbd_plotting$df_datalongAB
          rcbd_data_plot <- rcbd_plotting$df_summaryAB
          t_rcbd <- rcbd_plotting$trt_noAB
          Treatment <- "TreatmentxDay"
        } else {
          return(NULL)
        }
        
        # Create color palette with error handling
        mycolors_violinplot <- tryCatch({
          palette_name <- input$`pal.rcbd_VIOLINP` %||% "Dark2"
          colorRampPalette(brewer.pal(min(8, max(3, t_rcbd)), palette_name))(t_rcbd + 2)
        }, error = function(e) {
          rainbow(t_rcbd + 2)
        })
        
        # Sort treatments
        rcbd_data_long <- rcbd_data_long[gtools::mixedorder(rcbd_data_long[[Treatment]]), ]
        rcbd_data_long[[Treatment]] <- factor(rcbd_data_long[[Treatment]], levels = unique(rcbd_data_long[[Treatment]]))
        
        # Get theme
        rcbd_violin_theme <- get_boxplot_theme(input$rcbd_violin_theme %||% "normal")
        
        # Generate individual plots
        plot_list <- list()
        
        for (y_val in input$rcbd_violin_selected_yvalues) {
          # Filter data for this y-value
          filtered_data_long <- rcbd_data_long[rcbd_data_long$character == y_val, ]
          filtered_data_plot <- rcbd_data_plot[rcbd_data_plot$character == y_val, ]
          
          # Skip if no data for this variable
          if (nrow(filtered_data_long) == 0) next
          
          # Create individual plot
          p_individual <- ggplot(filtered_data_long, aes(x = .data[[Treatment]], y = value, fill = .data[[Treatment]]))
          
          # Add enhanced styling for individual plots
          styling_components <- create_enhanced_violin_styling(
            input, mycolors_violinplot, rcbd_violin_theme,
            is_individual = TRUE, y_var_name = y_val
          )
          
          p_individual <- p_individual + styling_components
          
          # Add significant letters if needed
          if (input$rcbd_violin_letter && nrow(filtered_data_plot) > 0) {
            p_individual <- p_individual + geom_text(
              data = filtered_data_plot,
              aes(
                x = .data[[Treatment]],
                y = mean_value + (sd_value + (4 * sd_value / 2)),
                label = group
              ),
              color = input$col_violin_colour,
              size = input$rcbd_violin_sig_size,
              position = position_dodge(0.5),
              vjust = -0.5,
              inherit.aes = FALSE
            )
          }
          
          # Add additional customization layers for individual plots
          if (input$rcbd_violin_add_mean_points %||% FALSE) {
            p_individual <- p_individual + stat_summary(
              fun = mean, geom = "point",
              size = input$rcbd_violin_mean_point_size %||% 2,
              color = input$rcbd_violin_mean_point_color %||% "red",
              shape = input$rcbd_violin_mean_point_shape %||% 16
            )
          }
          
          if (input$rcbd_violin_add_median_line %||% FALSE) {
            p_individual <- p_individual + stat_summary(
              fun = median, geom = "crossbar",
              width = input$rcbd_violin_median_line_width %||% 0.5,
              color = input$rcbd_violin_median_line_color %||% "blue",
              size = input$rcbd_violin_median_line_size %||% 0.5
            )
          }
          
          if (input$rcbd_violin_add_jitter %||% FALSE) {
            p_individual <- p_individual + geom_jitter(
              width = input$rcbd_violin_jitter_width %||% 0.2,
              height = input$rcbd_violin_jitter_height %||% 0,
              alpha = input$rcbd_violin_jitter_alpha %||% 0.5,
              size = input$rcbd_violin_jitter_size %||% 1,
              color = input$rcbd_violin_jitter_color %||% "black"
            )
          }
          
          plot_list[[y_val]] <- p_individual
        }
        
        return(if(length(plot_list) > 0) plot_list else NULL)
        
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # Enhanced observer for violin plot y-values
    observe({
      tryCatch({
        # Check if rcbd_plotdata exists and is not null
        if (exists("rcbd_plotdata") && is.function(rcbd_plotdata)) {
          plotting_data <- rcbd_plotdata()
          
          # Validate the plotting data
          validation_result <- validate_plot_data(plotting_data)
          
          if (validation_result$valid) {
            available_y_vars <- unique(plotting_data$df_datalongA$character)
            
            # Only update if we have valid data
            if (length(available_y_vars) > 0) {
              updateSelectInput(session, "rcbd_violin_selected_yvalues",
                                choices = available_y_vars,
                                selected = if(length(available_y_vars) > 0) available_y_vars[1] else NULL)
            }
          } else {
            # Clear selections if data is invalid
            updateSelectInput(session, "rcbd_violin_selected_yvalues",
                              choices = NULL,
                              selected = NULL)
          }
        } else {
          # Clear selections if function doesn't exist
          updateSelectInput(session, "rcbd_violin_selected_yvalues",
                            choices = NULL,
                            selected = NULL)
        }
      }, error = function(e) {
        # Clear selections on any error
        updateSelectInput(session, "rcbd_violin_selected_yvalues",
                          choices = NULL,
                          selected = NULL)
      })
    })
    
    
    # Render the rcbd violin plot with enhanced error handling
    output$rcbd_violin <- renderPlot({
      plot_obj <- rcbd_violinplot_reactive()
      req(plot_obj)
      plot_obj
    }, res = 96)
    
    # Enhanced download handler with quality settings
    output$downloadrcbd_violinplot <- downloadHandler(
      filename = function() {
        paste0("rcbd_violinplot-", Sys.Date(), ".", input$rcbd_violin_plot_format %||% "png")
      },
      content = function(file) {
        # Show loading modal
        showModal(modalDialog(
          title = "Generating high-quality violin plot...",
          tags$div(
            style = "text-align: center;",
            tags$img(
              src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fdownloadplot.gif?alt=media",
              style = "width: 80%; height: auto;"
            )
          ),
          footer = NULL
        ))
        
        tryCatch({
          # Get the plot
          plot_obj <- isolate(rcbd_violinplot_reactive())
          
          # Validate plot object
          if (is.null(plot_obj)) {
            stop("Plot object is null")
          }
          
          # Get quality settings from inputs
          plot_width <- input$rcbd_violin_plot_width %||% 10
          plot_height <- input$rcbd_violin_plot_height %||% 8
          plot_dpi <- input$rcbd_violin_plot_dpi %||% 300
          
          # Save with user-defined quality parameters
          fmt <- input$rcbd_violin_plot_format %||% "png"
          switch(fmt,
                 "png"  = { png(file,  width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "jpeg" = { jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "tiff" = { tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw", bg = "white"); print(plot_obj); dev.off() },
                 "pdf"  = { cairo_pdf(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() },
                 "svg"  = { svglite::svglite(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() }
          )
          
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("Failed to save violin plot:", e$message),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, finally = {
          removeModal()
        })
      }
    )
    
    
    ##Line (mean value) plot ----
    rcbd_linegraph_reactive <- reactive({
      
      # ORIGINAL: Get the rcbd data using exact original variable name
      rcbd_plotting <- rcbd_plotdata()
      
      # ORIGINAL: Get org_Location (keeping original structure even if unused)
      org_Treatment <- rcbd_plotting$org_Treatment
      org_Day <- rcbd_plotting$org_Day
      
      # ORIGINAL: Select data based on the selected factor
      if (input$selected_factor_linegraph == "A") {
        df.summary <- rcbd_plotting$df_summaryA
        rcbd_data_plot <- rcbd_plotting$df_summaryA
        t_rcbd <- rcbd_plotting$trt_noA
        Treatment <- "Treatment"
      }

      ## ##arshi(20-08-26) one-way RM - in "individual" mode (the default) the
      ## plot must show ONLY the selected character. Without this filter the
      ## reactive ignores the "Select Y-variable" dropdown, drawing every
      ## character on one panel, so changing the character does nothing.
      ## box/violin already filter exactly this way.
      if (input$rcbd_line_mode == "individual" && !is.null(input$rcbd_line_selected_yvalues)) {
        df.summary     <- df.summary[df.summary$character %in% input$rcbd_line_selected_yvalues, ]
        rcbd_data_plot <- rcbd_data_plot[rcbd_data_plot$character %in% input$rcbd_line_selected_yvalues, ]
      }

      ## ##arshi(19-08-26) one-way RM - palette selection is independent of the
      ## factor branch (the botched two-way merge left palette_input undefined).
      palette_input <- if (!is.null(input$pal.rcbd_lineP)) input$pal.rcbd_lineP else "Set1"

      mycolors_line_plot <- colorRampPalette(brewer.pal(8, palette_input))(t_rcbd + 2)
      
      # ENHANCED: Theme switch with additional customization options
      base_theme <- if (!is.null(input$rcbd_line_theme)) input$rcbd_line_theme else "normal"
      
      rcbd_line_theme <- switch(base_theme,
                                "normal" = theme_bw(),
                                "economist" = theme_economist(),
                                "minimal" = theme_minimal(),
                                "gray" = theme_gray(),
                                "light" = theme_light(),
                                "void" = theme_void(),
                                "tufte" = theme_tufte(),
                                "stata" = theme_stata(),
                                "wsj" = theme_wsj(),
                                "calc" = theme_calc(),
                                "hc" = theme_hc(),
                                theme_bw())
      
      # ORIGINAL: Base ggplot structure (keeping exact same structure)
      p <- ggplot(
        df.summary,
        aes(
          x = mean_value,
          y = .data[[Treatment]],
          xmin = mean_value - sd_value,
          xmax = mean_value + sd_value
        )
      ) +
        # ENHANCED: geom_point with additional customization options
        geom_point(
          aes(color = .data[[Treatment]]),
          size = if (!is.null(input$rcbd_line_point_size)) input$rcbd_line_point_size else 2,
          alpha = if (!is.null(input$rcbd_line_point_alpha)) input$rcbd_line_point_alpha else 1,
          shape = if (!is.null(input$rcbd_line_point_shape)) input$rcbd_line_point_shape else 16
        ) +
        
        # ORIGINAL: scale_color_manual
        scale_color_manual(name = input$side_line_rcbd, values = mycolors_line_plot) +
        
        # ENHANCED: scale_x_continuous with customizable expansion
        scale_x_continuous(
          expand = expansion(mult = c(
            if (!is.null(input$rcbd_line_x_expand_lower)) input$rcbd_line_x_expand_lower else 0.1,
            if (!is.null(input$rcbd_line_x_expand_upper)) input$rcbd_line_x_expand_upper else 0.2
          ))
        ) +
        
        # ORIGINAL: Apply theme
        rcbd_line_theme +
        
        # ORIGINAL: Labels
        xlab(input$x_line_rcbd) +
        ylab(input$y_line_rcbd) +
        ggtitle(input$title_line_rcbd)
      
      # ORIGINAL: Add horizontal error bars with enhancement
      p <- p + geom_errorbarh(
        aes(color = .data[[Treatment]]),
        height = input$rcbd_line_size,
        size = if (!is.null(input$rcbd_line_errorbar_size)) input$rcbd_line_errorbar_size else 0.5
      )
      
      # ORIGINAL: Add significant letters for groups conditionally
      if (input$rcbd_line_letter == TRUE) {
        p <- p + geom_text(
          data = rcbd_data_plot, # Use the data_plot dataframe for annotations
          aes(
            x = mean_value + (sd_value + (sd_value / 4)),
            y = .data[[Treatment]],
            label = group
          ),
          color = input$col_line_colour,  # Text color
          size = input$rcbd_linesig_size, # Font size
          hjust = if (!is.null(input$rcbd_line_vjust)) input$rcbd_line_vjust else -0.5,
          inherit.aes = FALSE
        )
      }
      
      # ENHANCED: Add facet conditionally (original always added facets)
      if (is.null(input$rcbd_line_mode) || input$rcbd_line_mode == "faceted") {
        p <- p + facet_wrap(~character, scales = "free_x", ncol = input$rcbd_line_ncol) +
          theme(
            strip.text.x = element_text(
              size = input$rcbd_line_wtextsize,
              colour = input$linewtextcolr,
              face = if (!is.null(input$rcbd_line_strip_text_bold) && input$rcbd_line_strip_text_bold) "bold" else "plain"
            ),
            strip.background = element_rect(
              colour = input$linewbordercolr,
              fill = input$linewbgcolr,
              size = if (!is.null(input$rcbd_line_strip_border_size)) input$rcbd_line_strip_border_size else 0.5
            )
          )
      }
      
      # ORIGINAL: Conditionally customize y-axis labels and ticks
      if (input$rcbd_line_yaxislab == TRUE) {
        p <- p + theme(
          axis.text.y = element_blank(),  # Hide y-axis labels
          axis.ticks.y = element_blank(), # Hide y-axis ticks
          aspect.ratio = if (!is.null(input$rcbd_line_aspect_ratio)) input$rcbd_line_aspect_ratio else 1
        )
      } else {
        p <- p + theme(
          axis.text.x = element_text(
            angle = input$rcbd_line_angle,
            color = if (!is.null(input$rcbd_line_x_axis_text_color)) input$rcbd_line_x_axis_text_color else "black",
            size = if (!is.null(input$rcbd_line_x_axis_text_size)) input$rcbd_line_x_axis_text_size else 11,
            face = if (!is.null(input$rcbd_line_x_axis_text_bold) && input$rcbd_line_x_axis_text_bold) "bold" else "plain"
          ),
          axis.text.y = element_text(
            size = input$rcbd_line_elSize,
            color = if (!is.null(input$rcbd_line_y_axis_text_color)) input$rcbd_line_y_axis_text_color else "black",
            face = if (!is.null(input$rcbd_line_y_axis_text_bold) && input$rcbd_line_y_axis_text_bold) "bold" else "plain"
          ),
          aspect.ratio = if (!is.null(input$rcbd_line_aspect_ratio)) input$rcbd_line_aspect_ratio else 1
        )
      }
      
      # ENHANCED: Additional theme customizations (only if inputs exist)
      additional_theme_elements <- list()
      
      # Title customization
      if (!is.null(input$rcbd_line_title_size) || !is.null(input$rcbd_line_title_color) || !is.null(input$rcbd_line_title_bold)) {
        additional_theme_elements$plot.title <- element_text(
          size = if (!is.null(input$rcbd_line_title_size)) input$rcbd_line_title_size else 14,
          color = if (!is.null(input$rcbd_line_title_color)) input$rcbd_line_title_color else "black",
          face = if (!is.null(input$rcbd_line_title_bold) && input$rcbd_line_title_bold) "bold" else "plain",
          hjust = if (!is.null(input$rcbd_line_title_hjust)) input$rcbd_line_title_hjust else 0.5,
          vjust = if (!is.null(input$rcbd_line_title_vjust)) input$rcbd_line_title_vjust else 0.5
        )
      }
      
      # Axis title customization
      if (!is.null(input$rcbd_line_x_title_size) || !is.null(input$rcbd_line_x_title_color) || !is.null(input$rcbd_line_x_title_bold)) {
        additional_theme_elements$axis.title.x <- element_text(
          size = if (!is.null(input$rcbd_line_x_title_size)) input$rcbd_line_x_title_size else 12,
          color = if (!is.null(input$rcbd_line_x_title_color)) input$rcbd_line_x_title_color else "black",
          face = if (!is.null(input$rcbd_line_x_title_bold) && input$rcbd_line_x_title_bold) "bold" else "plain"
        )
      }
      
      if (!is.null(input$rcbd_line_y_title_size) || !is.null(input$rcbd_line_y_title_color) || !is.null(input$rcbd_line_y_title_bold)) {
        additional_theme_elements$axis.title.y <- element_text(
          size = if (!is.null(input$rcbd_line_y_title_size)) input$rcbd_line_y_title_size else 12,
          color = if (!is.null(input$rcbd_line_y_title_color)) input$rcbd_line_y_title_color else "black",
          face = if (!is.null(input$rcbd_line_y_title_bold) && input$rcbd_line_y_title_bold) "bold" else "plain"
        )
      }
      
      # Legend customization
      if (!is.null(input$rcbd_line_show_legend) && !input$rcbd_line_show_legend) {
        additional_theme_elements$legend.position <- "none"
      } else if (!is.null(input$rcbd_line_legend_position)) {
        additional_theme_elements$legend.position <- input$rcbd_line_legend_position
      }
      
      # Apply additional theme elements if any exist
      if (length(additional_theme_elements) > 0) {
        p <- p + do.call(theme, additional_theme_elements)
      }
      
      # ORIGINAL: Return the plot
      p
    })
    
    # Enhanced observer for line plot y-values
    observe({
      tryCatch({
        if (exists("rcbd_plotdata") && is.function(rcbd_plotdata)) {
          plotting_data <- rcbd_plotdata()
          
          if (!is.null(plotting_data)) {
            # Get available y-variables from the appropriate summary data
            available_y_vars <- NULL
            
            if (!is.null(input$selected_factor_linegraph)) {
              if (input$selected_factor_linegraph == "A" && !is.null(plotting_data$df_summaryA)) {
                available_y_vars <- unique(plotting_data$df_summaryA$character)
              } else if (input$selected_factor_linegraph == "B" && !is.null(plotting_data$df_summaryB)) {
                available_y_vars <- unique(plotting_data$df_summaryB$character)
              } else if (input$selected_factor_linegraph == "AB" && !is.null(plotting_data$df_summaryAB)) {
                available_y_vars <- unique(plotting_data$df_summaryAB$character)
              }
            }
            
            if (!is.null(available_y_vars) && length(available_y_vars) > 0) {
              available_y_vars <- available_y_vars[!is.na(available_y_vars)]
              if (length(available_y_vars) > 0) {
                updateSelectInput(session, "rcbd_line_selected_yvalues",
                                  choices = available_y_vars,
                                  selected = available_y_vars[1])
              }
            }
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })
    
    # ORIGINAL: Render plot
    output$rcbd_line <- renderPlot({
      rcbd_linegraph_reactive()
    }, res = 96)
    
    # ENHANCED: Download handler with customizable settings
    output$downloadrcbd_line <- downloadHandler(
      filename = function() {
        paste0("rcbd_meanValuePlot-", Sys.Date(), ".", input$rcbd_line_plot_format %||% "png")
      },
      content = function(file) {
        # Show loading modal (optional enhancement)
        if (!is.null(input$rcbd_line_plot_dpi) && input$rcbd_line_plot_dpi > 600) {
          showModal(modalDialog(
            title = "It may take a few seconds...",
            tags$img(
              src = "downloadplot.gif",
              style = "display: block; margin: 0 auto; width: 80%; height: auto;"
            ),
            footer = NULL
          ))
        }
        
        tryCatch({
          # Use custom download settings if available, otherwise use original defaults
          plot_width <- if (!is.null(input$rcbd_line_plot_width)) input$rcbd_line_plot_width else 10
          plot_height <- if (!is.null(input$rcbd_line_plot_height)) input$rcbd_line_plot_height else 8
          plot_dpi <- if (!is.null(input$rcbd_line_plot_dpi)) input$rcbd_line_plot_dpi else 300
          
          plot_obj <- isolate(rcbd_linegraph_reactive())

          fmt <- input$rcbd_line_plot_format %||% "png"
          switch(fmt,
                 "png"  = { png(file,  width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "jpeg" = { jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "tiff" = { tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw", bg = "white"); print(plot_obj); dev.off() },
                 "pdf"  = { cairo_pdf(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() },
                 "svg"  = { svglite::svglite(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() }
          )
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while saving the plot:", e$message),
            easyClose = TRUE
          ))
        }, finally = {
          if (!is.null(input$rcbd_line_plot_dpi) && input$rcbd_line_plot_dpi > 600) {
            removeModal()
          }
        })
      }
    )
    
    
    ############################################# Connected Line plot ----
    
    rcbd_lineplot_reactive <- reactive({
      
      # ORIGINAL STRUCTURE: Get the rcbd data (exact same variable name)
      rcbd_plotting <- rcbd_plotdata()
      
      # ORIGINAL: Get org_Location (keeping original structure even if unused)
      org_Location <- rcbd_plotting$org_Location
      
      # ORIGINAL: Select data based on the selected factor
      if (input$selected_factor_linecon == "A") {
        df.summary <- rcbd_plotting$df_summaryA
        rcbd_data_plot <- rcbd_plotting$df_summaryA
        t_rcbd <- rcbd_plotting$trt_noA
        Treatment <- "Treatment"
      }

      ## ##arshi(20-08-26) one-way RM - in "individual" mode (the default) show
      ## ONLY the selected character, like box/violin. Without this the reactive
      ## ignores the "Select Y-variable" dropdown and draws every character on
      ## one panel, so changing the character does nothing. Grouped mode keeps all.
      if (input$rcbd_lineCon_mode == "individual" && !is.null(input$rcbd_lineCon_selected_yvalues)) {
        df.summary     <- df.summary[df.summary$character %in% input$rcbd_lineCon_selected_yvalues, ]
        rcbd_data_plot <- rcbd_data_plot[rcbd_data_plot$character %in% input$rcbd_lineCon_selected_yvalues, ]
      }

      ## ##arshi(19-08-26) one-way RM - grouping flag is independent of the
      ## factor branch (the botched two-way merge left it undefined).
      actual_grouppbylineCon <- if (!is.null(input$grouppbylineCon)) input$grouppbylineCon else FALSE

      ## ##arshi(20-08-26) one-way RM - the botched two-way merge also dropped
      ## the palette / theme / face / linetype helpers this reactive relies on,
      ## so every render threw "object '...' not found" and no plot appeared.
      ## Rebuild them here, exactly like the working mean-value line plot does.
      palette_input <- if (!is.null(input$pal.rcbd_lineCon)) input$pal.rcbd_lineCon else "Dark2"
      mycolors_lineCon <- colorRampPalette(brewer.pal(8, palette_input))(t_rcbd + 2)

      base_theme <- if (!is.null(input$rcbd_lineCon_theme)) input$rcbd_lineCon_theme else "normal"
      rcbd_lineCon_theme <- switch(base_theme,
                                   "normal" = theme_bw(),
                                   "economist" = theme_economist(),
                                   "minimal" = theme_minimal(),
                                   "gray" = theme_gray(),
                                   "light" = theme_light(),
                                   "void" = theme_void(),
                                   "tufte" = theme_tufte(),
                                   "stata" = theme_stata(),
                                   "wsj" = theme_wsj(),
                                   "calc" = theme_calc(),
                                   "hc" = theme_hc(),
                                   theme_bw())

      x_axis_face <- if (isTRUE(input$rcbd_lineCon_x_axis_text_bold)) "bold" else "plain"
      y_axis_face <- if (isTRUE(input$rcbd_lineCon_y_axis_text_bold)) "bold" else "plain"
      rcbd_lineCon_linetype <- get_line_type(input$rcbd_lineCon_linetype)

      # ORIGINAL STRUCTURE: Plot the graph (EXACT SAME LOGIC)
      if (actual_grouppbylineCon) {
        # ORIGINAL: Grouped plot structure
        p <- ggplot(df.summary,
                    aes(x = .data[[Treatment]],
                        y = mean_value,
                        group = character,
                        color = character)) +
          geom_line(
            aes(group = character, color = character, linetype = character),
            size = input$rcbd_lineCon_size
          ) +
          {
            if (input$lineCon_show_ebar) {
              geom_errorbar(
                aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = input$rcbd_lineCon_size
              )
            }
          } +
          geom_point(
            aes(color = character),
            shape = input$rcbd_lineCon_poinshapes,
            size = input$rcbd_lineCon_pointsize
          ) +
          scale_color_manual(values = mycolors_lineCon) +
          {
            if (input$rcbd_lineCon_letter == TRUE) {
              if (input$lineCon_show_ebar) {
                geom_text(data = rcbd_data_plot,
                          aes(x = .data[[Treatment]],
                              y = mean_value + (sd_value + (sd_value / 4)),
                              label = group),
                          size = input$rcbd_lineConsig_size,
                          color = input$col_lineConLetter_colour,
                          vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else -0.5,
                          inherit.aes = FALSE)
              } else {
                geom_text(data = rcbd_data_plot,
                          aes(x = .data[[Treatment]],
                              y = mean_value,
                              label = group),
                          size = input$rcbd_lineConsig_size,
                          color = input$col_lineConLetter_colour,
                          vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else -0.5,
                          inherit.aes = FALSE)
              }
            }
          } +
          scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) + # ORIGINAL: Add 20% padding
          xlab(input$x_lineCon_rcbd) +
          ylab(input$y_lineCon_rcbd) +
          ggtitle(input$title_lineCon_rcbd) +
          rcbd_lineCon_theme +
          theme(
            axis.text.x = element_text(
              angle = input$rcbd_lineCon_angle,
              size = input$rcbd_lineCon_elSize,
              colour = input$rcbd_lineCon_x_axis_text_color,
              face = x_axis_face,
              hjust = if (input$rcbd_lineCon_angle %% 180 == 90) 1 else 0.5,
              vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else 0.5,
              margin = margin(t = 4)
            ),
            axis.text.y = element_text(
              size = input$rcbd_lineCon_y_axis_text_size,
              colour = input$rcbd_lineCon_y_axis_text_color,
              face = y_axis_face
            ),
            axis.ticks.x = element_line(colour = input$rcbd_lineCon_x_axis_tick_color),
            axis.ticks.y = element_line(colour = input$rcbd_lineCon_y_axis_tick_color),
            aspect.ratio = 1
          )
        
        p
      } else {
        # ORIGINAL: Base plot without grouping
        p <- ggplot(df.summary, aes(x = .data[[Treatment]], y = mean_value)) +
          scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) + # ORIGINAL: Add 20% padding
          xlab(input$x_lineCon_rcbd) +
          ylab(input$y_lineCon_rcbd) +
          ggtitle(input$title_lineCon_rcbd) +
          rcbd_lineCon_theme +
          theme(
            axis.text.x = element_text(
              angle = input$rcbd_lineCon_angle,
              size = input$rcbd_lineCon_elSize,
              colour = input$rcbd_lineCon_x_axis_text_color,
              face = x_axis_face,
              hjust = if (input$rcbd_lineCon_angle %% 180 == 90) 1 else 0.5,
              vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else 0.5,
              margin = margin(t = 4)
            ),
            axis.text.y = element_text(
              size = input$rcbd_lineCon_y_axis_text_size,
              colour = input$rcbd_lineCon_y_axis_text_color,
              face = y_axis_face
            ),
            axis.ticks.x = element_line(colour = input$rcbd_lineCon_x_axis_tick_color),
            axis.ticks.y = element_line(colour = input$rcbd_lineCon_y_axis_tick_color),
            aspect.ratio = 1
          ) +
          geom_line(
            aes(group = 1),
            linetype = rcbd_lineCon_linetype,
            color = input$col_lineConLine_colour
          ) +
          {
            if (input$lineCon_show_ebar) {
              geom_errorbar(
                aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = input$rcbd_lineCon_size,
                color = input$col_lineConerrorbar_colour
              )
            }
          } +
          geom_point(
            shape = input$rcbd_lineCon_poinshapes,
            size = input$rcbd_lineCon_pointsize,
            color = input$col_lineConPoint_colour
          )
        
        # ORIGINAL: Add significant letters conditionally
        if (input$rcbd_lineCon_letter) {
          if (input$lineCon_show_ebar) {
            p <- p + geom_text(
              data = rcbd_data_plot,
              aes(x = .data[[Treatment]],
                  y = mean_value + (sd_value + (sd_value / 4)),
                  label = group),
              size = input$rcbd_lineConsig_size,
              color = input$col_lineConLetter_colour,
              vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else -0.5,
              inherit.aes = FALSE
            )
          } else {
            p <- p + geom_text(
              data = rcbd_data_plot,
              aes(x = .data[[Treatment]],
                  y = mean_value,
                  label = group),
              size = input$rcbd_lineConsig_size,
              color = input$col_lineConLetter_colour,
              vjust = if (!is.null(input$rcbd_lineCon_vjust)) input$rcbd_lineCon_vjust else -0.5,
              inherit.aes = FALSE
            )
          }
        }
        
        # NEW: Conditional faceting - only add if NOT individual mode
        if (is.null(input$rcbd_lineCon_mode) || input$rcbd_lineCon_mode != "individual") {
          # ORIGINAL: Facet wrap if required
          p <- p + facet_wrap(~character, scales = "free_y", ncol = input$rcbd_lineCon_ncol) +
            theme(
              strip.text.x = element_text(
                size = input$rcbd_lineCon_wtextsize,
                colour = input$lineConwtextcolr
              ),
              strip.background = element_rect(
                colour = input$lineConwbordercolr,
                fill = input$lineConwbgcolr
              ),
              axis.text.y = element_text(
                size = input$rcbd_lineCon_y_axis_text_size,
                colour = input$rcbd_lineCon_y_axis_text_color,
                face = y_axis_face
              ),
              axis.ticks.x = element_line(colour = input$rcbd_lineCon_x_axis_tick_color),
              axis.ticks.y = element_line(colour = input$rcbd_lineCon_y_axis_tick_color)
            )
        }
        p
      }
    })
    
    # NEW: Enhanced observer for connected line plot y-values
    observe({
      tryCatch({
        if (exists("rcbd_plotdata") && is.function(rcbd_plotdata)) {
          plotting_data <- rcbd_plotdata()
          
          if (!is.null(plotting_data)) {
            # Get available y-variables from the appropriate summary data based on selected factor
            available_y_vars <- NULL
            
            if (!is.null(input$selected_factor_linecon)) {
              if (input$selected_factor_linecon == "A" && !is.null(plotting_data$df_summaryA)) {
                available_y_vars <- unique(plotting_data$df_summaryA$character)
              } else if (input$selected_factor_linecon == "B" && !is.null(plotting_data$df_summaryB)) {
                available_y_vars <- unique(plotting_data$df_summaryB$character)
              } else if (input$selected_factor_linecon == "AB" && !is.null(plotting_data$df_summaryAB)) {
                available_y_vars <- unique(plotting_data$df_summaryAB$character)
              }
            }
            
            if (!is.null(available_y_vars) && length(available_y_vars) > 0) {
              available_y_vars <- available_y_vars[!is.na(available_y_vars)]
              if (length(available_y_vars) > 0) {
                updateSelectInput(session, "rcbd_lineCon_selected_yvalues",
                                  choices = available_y_vars,
                                  selected = available_y_vars[1])
              }
            }
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })
    
    # NEW: Observer to sync grouppbylineCon with new mode system
    observe({
      if (!is.null(input$rcbd_lineCon_mode)) {
        if (input$rcbd_lineCon_mode == "grouped") {
          updateCheckboxInput(session, "grouppbylineCon", value = TRUE)
        } else {
          updateCheckboxInput(session, "grouppbylineCon", value = FALSE)
        }
      }
    })
    
    # ORIGINAL: Render plot
    output$rcbd_line_plot <- renderPlot({
      rcbd_lineplot_reactive()
    }, res = 96)
    
    
    # ORIGINAL: Download handler for rcbd line plot
    output$downloadrcbd_lineCon <- downloadHandler(
      filename = function() {
        paste0("rcbd_connectedPlot-", Sys.Date(), ".", input$rcbd_lineCon_plot_format %||% "png")
      },
      content = function(file) {
        showModal(modalDialog(
          title = "It may take a few seconds...",
          tags$img(
            src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fdownloadplot.gif?alt=media",
            style = "display: block; margin: 0 auto; width: 80%; height: auto;"
          ),
          footer = NULL
        ))
        
        tryCatch({
          plot_obj <- isolate(rcbd_lineplot_reactive())

          plot_width <- input$rcbd_lineCon_plot_width %||% 10
          plot_height <- input$rcbd_lineCon_plot_height %||% 8
          plot_dpi <- input$rcbd_lineCon_plot_dpi %||% 600

          fmt <- input$rcbd_lineCon_plot_format %||% "png"
          switch(fmt,
                 "png"  = { png(file,  width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "jpeg" = { jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "tiff" = { tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw", bg = "white"); print(plot_obj); dev.off() },
                 "pdf"  = { cairo_pdf(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() },
                 "svg"  = { svglite::svglite(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() }
          )
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while saving the plot:", e$message),
            easyClose = TRUE
          ))
        }, finally = {
          removeModal()
        })
      }
    )
    
    
    ############################################ connected Line Plot END
    
    
    ############## BAR GRAPH ----
    rcbd_barplot_reactive <- reactive({
      
      # ORIGINAL STRUCTURE: Get the rcbd data
      rcbd_plotting <- rcbd_plotdata()
      
      # ORIGINAL: Get org_Location (keeping original structure even if unused)
      org_Location <- rcbd_plotting$org_Location
      
      # ORIGINAL: Select data based on the selected factor
      if (input$selected_factor_bar == "A") {
        data_barplot <- rcbd_plotting$df_summaryA
        t_rcbd <- rcbd_plotting$trt_noA
        Treatment <- "Treatment"
      }

      ## ##arshi(19-08-26) one-way RM - single-barplot flag is independent of the
      ## factor branch (the botched two-way merge left it undefined).
      actual_singlebarplot <- if (!is.null(input$singlebarplot)) input$singlebarplot else FALSE

      ## ##arshi(20-08-26) one-way RM - the botched two-way merge dropped the
      ## data alias (d), palette (mycolors) and theme object this reactive uses,
      ## so every render threw "object '...' not found" and no plot appeared.
      ## Rebuild them here. NOTE: the one-way summary frame has a "Treatment"
      ## column (not the two-way "Treatments"), fixed in the aes() calls below.
      d <- data_barplot

      ## ##arshi(20-08-26) one-way RM - in "individual" mode (the default) show
      ## ONLY the selected character, like box/violin. Without this the reactive
      ## ignores the "Select Y-variable" dropdown and draws every character
      ## together, so changing the character does nothing. Single/faceted keep all.
      if (input$rcbd_barplot_mode == "individual" && !is.null(input$rcbd_barplot_selected_yvalues)) {
        d <- d[d$character %in% input$rcbd_barplot_selected_yvalues, ]
      }

      palette_input <- if (!is.null(input$pal.rcbd_barplot)) input$pal.rcbd_barplot else "Dark2"
      mycolors <- colorRampPalette(brewer.pal(8, palette_input))(t_rcbd + 2)

      base_theme <- if (!is.null(input$rcbd_barplot_theme)) input$rcbd_barplot_theme else "normal"
      rcbd_barplot_theme <- switch(base_theme,
                                   "normal" = theme_bw(),
                                   "economist" = theme_economist(),
                                   "minimal" = theme_minimal(),
                                   "gray" = theme_gray(),
                                   "light" = theme_light(),
                                   "void" = theme_void(),
                                   "tufte" = theme_tufte(),
                                   "stata" = theme_stata(),
                                   "wsj" = theme_wsj(),
                                   "calc" = theme_calc(),
                                   "hc" = theme_hc(),
                                   theme_bw())

      # ORIGINAL STRUCTURE: Check if single barplot is selected
      if (actual_singlebarplot) {
        # ORIGINAL: Combined graph
        dodge_width <- input$rcbd_barplot_barW # Bar width also used for dodge
        
        p_combined <- ggplot(d, aes(x = character,
                                    y = mean_value,
                                    fill = Treatment)) +
          scale_x_discrete(labels = unique(d$character)) +
          scale_fill_manual(values = mycolors) +
          scale_y_continuous(
            expand = expansion(mult = c(
              if (!is.null(input$rcbd_barplot_y_expand_lower)) input$rcbd_barplot_y_expand_lower else 0,
              if (!is.null(input$rcbd_barplot_y_expand_upper)) input$rcbd_barplot_y_expand_upper else 0.2
            ))
          ) +
          xlab(input$x_barplot_rcbd) +
          ylab(input$y_barplot_rcbd) +
          ggtitle(input$title_barplot_rcbd) +
          labs(fill = input$side_barplot_rcbd) +
          rcbd_barplot_theme +
          geom_bar(
            stat = "identity",
            position = position_dodge(width = dodge_width),
            width = input$rcbd_barplot_barW,
            colour = if (!is.null(input$rcbd_barplot_bar_border_color)) input$rcbd_barplot_bar_border_color else "black",
            alpha = if (!is.null(input$rcbd_barplot_bar_alpha)) input$rcbd_barplot_bar_alpha else 1,
            size = if (!is.null(input$rcbd_barplot_bar_border_size)) input$rcbd_barplot_bar_border_size else 0.5
          ) +
          geom_errorbar(aes(ymin = mean_value - sd_value,
                            ymax = mean_value + sd_value),
                        width = input$rcbd_barplot_size,
                        colour = input$rcbd_barerror_colour,
                        size = input$rcbd_barplot_Esize,
                        position = position_dodge(width = dodge_width)) +
          geom_text(aes(y = mean_value + sd_value,
                        label = group),
                    position = position_dodge(width = dodge_width),
                    vjust = if (!is.null(input$rcbd_bar_vjust)) input$rcbd_bar_vjust else -0.5,
                    color = input$col_barplotLetter_colour,
                    size = input$rcbd_barplot_lettrSZ) +
          theme(
            axis.text.x = element_text(
              angle = input$rcbd_barplot_angle,
              size = input$rcbd_barplot_elSize,
              hjust = if (input$rcbd_barplot_angle %% 180 == 90) 1 else 0.5,
              vjust = 0.5,
              margin = margin(t = 4)
            ),
            aspect.ratio = if (!is.null(input$rcbd_barplot_aspect_ratio)) input$rcbd_barplot_aspect_ratio else 1
          )
        
        return(p_combined)
      } else {
        # ORIGINAL STRUCTURE: Pattern vs regular plot logic
        if (input$gradientpattern) {
          # ORIGINAL: Pattern plot
          p <- ggplot(d, aes(x = .data[[Treatment]],
                             y = mean_value)) +
            scale_x_discrete(labels = unique(d$Treatment)) +
            scale_fill_manual(values = mycolors) +
            scale_y_continuous(
              expand = expansion(mult = c(
                if (!is.null(input$rcbd_barplot_y_expand_lower)) input$rcbd_barplot_y_expand_lower else 0,
                if (!is.null(input$rcbd_barplot_y_expand_upper)) input$rcbd_barplot_y_expand_upper else 0.2
              ))
            ) +
            xlab(input$x_barplot_rcbd) +
            ylab(input$y_barplot_rcbd) +
            ggtitle(input$title_barplot_rcbd) +
            labs(fill = input$side_barplot_rcbd) +
            rcbd_barplot_theme
          
          # ORIGINAL: Add pattern (EXACTLY AS IS)
          p <- p + geom_col_pattern(
            aes(pattern_type = Treatment),
            fill = 'white',
            colour = if (!is.null(input$rcbd_barplot_bar_border_color)) input$rcbd_barplot_bar_border_color else 'black',
            pattern = 'magick',
            pattern_scale = 1.5,
            pattern_density = input$rcbd_bar_pdensity,
            pattern_spacing = input$rcbd_bar_pspacing,
            pattern_fill = 'black',
            alpha = if (!is.null(input$rcbd_barplot_bar_alpha)) input$rcbd_barplot_bar_alpha else 1,
            size = if (!is.null(input$rcbd_barplot_bar_border_size)) input$rcbd_barplot_bar_border_size else 0.5
          ) +
            scale_pattern_type_discrete(choices = gridpattern::names_magick)
          
          # ORIGINAL: Add error bars
          p <- p + geom_errorbar(aes(ymin = mean_value - sd_value,
                                     ymax = mean_value + sd_value),
                                 width = input$rcbd_barplot_size,
                                 colour = input$rcbd_barerror_colour,
                                 size = input$rcbd_barplot_Esize)
          
          # ORIGINAL: Add letters for groups conditionally
          if (input$rcbd_barplot_letter) {
            p <- p + geom_text(aes(x = .data[[Treatment]],
                                   y = mean_value + (sd_value + (sd_value / 4)),
                                   label = group),
                               color = input$col_barplotLetter_colour,
                               size = input$rcbd_barplot_lettrSZ,
                               vjust = if (!is.null(input$rcbd_bar_vjust)) input$rcbd_bar_vjust else -0.5)
          }
          
          # NEW: Conditional faceting - only add if NOT individual mode
          if (is.null(input$rcbd_barplot_mode) || input$rcbd_barplot_mode != "individual") {
            # ORIGINAL: Add facet (only if character column exists and has valid values)
            if ("character" %in% colnames(d) && length(unique(d$character)) > 0) {
              p <- p + facet_wrap(~character, scales = "free_y", ncol = input$rcbd_barplot_ncol) +
                theme(
                  strip.text.x = element_text(
                    size = input$rcbd_bar_wtextsize,
                    colour = input$barwtextcolr,
                    face = if (!is.null(input$rcbd_barplot_strip_text_bold) && input$rcbd_barplot_strip_text_bold) "bold" else "plain"
                  ),
                  strip.background = element_rect(
                    colour = input$barwbordercolr,
                    fill = input$barwbgcolr,
                    size = if (!is.null(input$rcbd_barplot_strip_border_size)) input$rcbd_barplot_strip_border_size else 0.5
                  )
                )
            }
          }
          
          # ORIGINAL: Conditionally customize axis labels and ticks
          if (input$rcbd_bar_xaxislab == TRUE) {
            p <- p + theme(
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(
                size = if (!is.null(input$rcbd_barplot_y_axis_text_size)) input$rcbd_barplot_y_axis_text_size else 11,
                colour = if (!is.null(input$rcbd_barplot_y_axis_text_color)) input$rcbd_barplot_y_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_y_axis_text_bold) && input$rcbd_barplot_y_axis_text_bold) "bold" else "plain"
              ),
              aspect.ratio = if (!is.null(input$rcbd_barplot_aspect_ratio)) input$rcbd_barplot_aspect_ratio else 1
            )
          } else {
            p <- p + theme(
              axis.text.x = element_text(
                angle = input$rcbd_barplot_angle,
                size = input$rcbd_barplot_elSize,
                hjust = if (input$rcbd_barplot_angle %% 180 == 90) 1 else 0.5,
                vjust = 0.5,
                margin = margin(t = 4),
                colour = if (!is.null(input$rcbd_barplot_x_axis_text_color)) input$rcbd_barplot_x_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_x_axis_text_bold) && input$rcbd_barplot_x_axis_text_bold) "bold" else "plain"
              ),
              axis.text.y = element_text(
                size = if (!is.null(input$rcbd_barplot_y_axis_text_size)) input$rcbd_barplot_y_axis_text_size else 11,
                colour = if (!is.null(input$rcbd_barplot_y_axis_text_color)) input$rcbd_barplot_y_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_y_axis_text_bold) && input$rcbd_barplot_y_axis_text_bold) "bold" else "plain"
              ),
              aspect.ratio = if (!is.null(input$rcbd_barplot_aspect_ratio)) input$rcbd_barplot_aspect_ratio else 1
            )
          }
          p
        } else {
          # ORIGINAL: Regular bar plot
          p <- ggplot(d, aes(x = .data[[Treatment]],
                             y = mean_value,
                             fill = Treatment)) +
            scale_x_discrete(labels = unique(d$Treatment)) +
            scale_fill_manual(values = mycolors) +
            scale_y_continuous(
              expand = expansion(mult = c(
                if (!is.null(input$rcbd_barplot_y_expand_lower)) input$rcbd_barplot_y_expand_lower else 0,
                if (!is.null(input$rcbd_barplot_y_expand_upper)) input$rcbd_barplot_y_expand_upper else 0.2
              ))
            ) +
            xlab(input$x_barplot_rcbd) +
            ylab(input$y_barplot_rcbd) +
            ggtitle(input$title_barplot_rcbd) +
            labs(fill = input$side_barplot_rcbd) +
            rcbd_barplot_theme
          
          # ORIGINAL: Add bars
          p <- p + geom_bar(
            stat = "identity",
            position = position_dodge(0.5),
            width = input$rcbd_barplot_barW,
            colour = if (!is.null(input$rcbd_barplot_bar_border_color)) input$rcbd_barplot_bar_border_color else 'black',
            alpha = if (!is.null(input$rcbd_barplot_bar_alpha)) input$rcbd_barplot_bar_alpha else 1,
            size = if (!is.null(input$rcbd_barplot_bar_border_size)) input$rcbd_barplot_bar_border_size else 0.5
          )
          
          # ORIGINAL: Add error bars
          p <- p + geom_errorbar(aes(ymin = mean_value - sd_value,
                                     ymax = mean_value + sd_value),
                                 width = input$rcbd_barplot_size,
                                 colour = input$rcbd_barerror_colour,
                                 size = input$rcbd_barplot_Esize)
          
          # ORIGINAL: Add letters for groups conditionally
          if (input$rcbd_barplot_letter) {
            p <- p + geom_text(aes(x = .data[[Treatment]],
                                   y = mean_value + (sd_value + (sd_value / 4)),
                                   label = group),
                               color = input$col_barplotLetter_colour,
                               size = input$rcbd_barplot_lettrSZ,
                               vjust = if (!is.null(input$rcbd_bar_vjust)) input$rcbd_bar_vjust else -0.5)
          }
          
          # NEW: Conditional faceting - only add if NOT individual mode
          if (is.null(input$rcbd_barplot_mode) || input$rcbd_barplot_mode != "individual") {
            # ORIGINAL: Add facet
            p <- p + facet_wrap(~character, scales = "free_y", ncol = input$rcbd_barplot_ncol) +
              theme(
                strip.text.x = element_text(
                  size = input$rcbd_bar_wtextsize,
                  colour = input$barwtextcolr,
                  face = if (!is.null(input$rcbd_barplot_strip_text_bold) && input$rcbd_barplot_strip_text_bold) "bold" else "plain"
                ),
                strip.background = element_rect(
                  colour = input$barwbordercolr,
                  fill = input$barwbgcolr,
                  size = if (!is.null(input$rcbd_barplot_strip_border_size)) input$rcbd_barplot_strip_border_size else 0.5
                )
              )
          }
          
          # ORIGINAL: Conditionally customize axis labels and ticks
          if (input$rcbd_bar_xaxislab == FALSE) {
            p <- p + theme(
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(
                size = if (!is.null(input$rcbd_barplot_y_axis_text_size)) input$rcbd_barplot_y_axis_text_size else 11,
                colour = if (!is.null(input$rcbd_barplot_y_axis_text_color)) input$rcbd_barplot_y_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_y_axis_text_bold) && input$rcbd_barplot_y_axis_text_bold) "bold" else "plain"
              ),
              aspect.ratio = if (!is.null(input$rcbd_barplot_aspect_ratio)) input$rcbd_barplot_aspect_ratio else 1
            )
          } else {
            p <- p + theme(
              axis.text.x = element_text(
                angle = input$rcbd_barplot_angle,
                size = input$rcbd_barplot_elSize,
                hjust = if (input$rcbd_barplot_angle %% 180 == 90) 1 else 0.5,
                vjust = 0.5,
                margin = margin(t = 4),
                colour = if (!is.null(input$rcbd_barplot_x_axis_text_color)) input$rcbd_barplot_x_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_x_axis_text_bold) && input$rcbd_barplot_x_axis_text_bold) "bold" else "plain"
              ),
              axis.text.y = element_text(
                size = if (!is.null(input$rcbd_barplot_y_axis_text_size)) input$rcbd_barplot_y_axis_text_size else 11,
                colour = if (!is.null(input$rcbd_barplot_y_axis_text_color)) input$rcbd_barplot_y_axis_text_color else "black",
                face = if (!is.null(input$rcbd_barplot_y_axis_text_bold) && input$rcbd_barplot_y_axis_text_bold) "bold" else "plain"
              ),
              aspect.ratio = if (!is.null(input$rcbd_barplot_aspect_ratio)) input$rcbd_barplot_aspect_ratio else 1
            )
          }
          
          # Return the plot
          p
        }
      }
    })
    
    # NEW: Enhanced observer for bar plot y-values
    observe({
      tryCatch({
        if (exists("rcbd_plotdata") && is.function(rcbd_plotdata)) {
          plotting_data <- rcbd_plotdata()
          
          if (!is.null(plotting_data)) {
            # Get available y-variables from the appropriate summary data based on selected factor
            available_y_vars <- NULL
            
            if (!is.null(input$selected_factor_bar)) {
              if (input$selected_factor_bar == "A" && !is.null(plotting_data$df_summaryA)) {
                available_y_vars <- unique(plotting_data$df_summaryA$character)
              } else if (input$selected_factor_bar == "B" && !is.null(plotting_data$df_summaryB)) {
                available_y_vars <- unique(plotting_data$df_summaryB$character)
              } else if (input$selected_factor_bar == "AB" && !is.null(plotting_data$df_summaryAB)) {
                available_y_vars <- unique(plotting_data$df_summaryAB$character)
              }
            }
            
            if (!is.null(available_y_vars) && length(available_y_vars) > 0) {
              available_y_vars <- available_y_vars[!is.na(available_y_vars)]
              if (length(available_y_vars) > 0) {
                updateSelectInput(session, "rcbd_barplot_selected_yvalues",
                                  choices = available_y_vars,
                                  selected = available_y_vars[1])
              }
            }
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    })
    
    # NEW: Observer to sync singlebarplot with new mode system
    observe({
      if (!is.null(input$rcbd_barplot_mode)) {
        if (input$rcbd_barplot_mode == "single") {
          updateCheckboxInput(session, "singlebarplot", value = TRUE)
        } else {
          updateCheckboxInput(session, "singlebarplot", value = FALSE)
        }
      }
    })
    
    
    # ORIGINAL: Render plot
    output$rcbd_barplot <- renderPlot({
      rcbd_barplot_reactive()
    }, res = 96)
    
    # ENHANCED: Download handler with customizable quality settings
    output$downloadrcbd_barplot <- downloadHandler(
      filename = function() {
        paste0("rcbd_BarPlot-", Sys.Date(), ".", input$rcbd_barplot_plot_format %||% "png")
      },
      content = function(file) {
        # Enhanced: Show loading modal
        showModal(modalDialog(
          title = "Generating high-quality bar plot...",
          tags$img(
            src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fdownloadplot.gif?alt=media",
            style = "display: block; margin: 0 auto; width: 80%; height: auto;"
          ),
          footer = NULL
        ))
        
        tryCatch({
          plot_obj <- isolate(rcbd_barplot_reactive())
          
          # Enhanced: Use custom settings if available, otherwise use original defaults
          plot_width <- input$rcbd_barplot_plot_width %||% 10
          plot_height <- input$rcbd_barplot_plot_height %||% 8
          plot_dpi <- input$rcbd_barplot_plot_dpi %||% 600
          
          # ORIGINAL: ggsave structure with enhanced parameters
          fmt <- input$rcbd_barplot_plot_format %||% "png"
          switch(fmt,
                 "png"  = { png(file,  width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "jpeg" = { jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, bg = "white"); print(plot_obj); dev.off() },
                 "tiff" = { tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw", bg = "white"); print(plot_obj); dev.off() },
                 "pdf"  = { cairo_pdf(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() },
                 "svg"  = { svglite::svglite(file, width = plot_width, height = plot_height); print(plot_obj); dev.off() }
          )
          
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while saving the plot:", e$message),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }, finally = {
          removeModal()
        })
      }
    )
    
    
    
    #############################pairs plot
    rcbd_corrgraph_reactive <- reactive({
      rcbd_tablecorr <- rcbd_table()
      rcbd_corrdata <- rcbd_tablecorr$trans_data %>%
        dplyr::select(-dplyr::any_of(c("Block", "id")))

      ## ##arshi(20-08-26) one-way RM - the pair plot correlates the RESPONSE
      ## variables. trans_data only carries a "Day" factor and a "Subject" id
      ## alongside the responses, so drop those (plus any stale two-way columns)
      ## and correlate what remains. The old "A" branch never built `p` (the
      ## ggpairs call was dropped in the two-way merge) and rcbd_pairsplot_theme
      ## was undefined -> "object 'rcbd_pairsplot_theme' not found".
      rcbd_corrdata <- rcbd_corrdata %>%
        dplyr::select(-dplyr::any_of(c("Day", "Subject", "Treatment", "TreatmentxDay")))

      rcbd_pairsplot_theme <- switch(input$rcbd_pairplot_theme %||% "normal",
                                     "normal"    = theme_bw(),
                                     "economist" = theme_economist(),
                                     "minimal"   = theme_minimal(),
                                     "gray"      = theme_gray(),
                                     "light"     = theme_light(),
                                     "void"      = theme_void(),
                                     "tufte"     = theme_tufte(),
                                     "stata"     = theme_stata(),
                                     "wsj"       = theme_wsj(),
                                     "calc"      = theme_calc(),
                                     "hc"        = theme_hc(),
                                     theme_bw())

      p <- ggpairs(rcbd_corrdata,
                   columns = 1:ncol(rcbd_corrdata),
                   upper = list(continuous = wrap("cor", size = input$rcbd_pplettersize)))

      # Apply theme
      p <- p + rcbd_pairsplot_theme

      # Return the plot
      return(p)
    })
    
    # Render the plot
    output$rcbd_corrplot <- renderPlot({
      rcbd_corrgraph_reactive()
    })
    
    # Download handler
    output$download_corrplot <- downloadHandler(
      filename = function() {
        paste("rcbd_pairsplot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        showModal(modalDialog(
          title = "It may take a few seconds...",
          tags$img(
            src = "downloadplot.gif",
            style = "display: block; margin: 0 auto; width: 80%; height: auto;"
          ),
          footer = NULL
        ))
        
        tryCatch({
          ggsave(file, 
                 plot = rcbd_corrgraph_reactive(), 
                 device = "png", 
                 dpi = 300,
                 width = 10, 
                 height = 8, 
                 units = "in")
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while saving the plot:", e$message),
            easyClose = TRUE
          ))
        }, finally = {
          removeModal()
        })
      }
    )
    
    
    ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##  X axis and Legend update ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    ### by arshida 
    observe_factor_update <- function(factor_input, x_input, side_input = NULL) {
      observeEvent(input[[factor_input]], {
        ## ##arshi(19-08-26) one-way RM - the single factor (Day) lives in the
        ## "A" slot, so the axis/legend label for "A" is Day, not Treatment.
        label <- switch(input[[factor_input]],
                        "A" = "Day",
                        "B" = "Day",
                        "AB" = "TreatmentxDay")
        
        updateTextInput(inputId = x_input, value = label)
        if (!is.null(side_input)) {
          updateTextInput(inputId = side_input, value = label)
        }
      })
    }
    
    # Call for each plot type
    observe_factor_update("selected_factor_box", "x_box_rcbd", "side_box_rcbd")
    observe_factor_update("selected_factor_violin", "x_violin_rcbd", "side_violin_rcbd")
    observe_factor_update("selected_factor_linegraph", "y_line_rcbd", "side_line_rcbd")
    observe_factor_update("selected_factor_linecon", "x_lineCon_rcbd")
    observe_factor_update("selected_factor_bar", "x_barplot_rcbd", "side_barplot_rcbd")
    observe_factor_update("selected_factor_pairplot", "x_pairplot_rcbd", "side_pairplot_rcbd")
    
    ################################# performance analytics end   
    
    
    
    
    ############## return a shiny alert if transwitch in pressed and no transformation selected 
    
    if (input$rcbd_trans_switch == TRUE) { 
      if (is.null(input$rcbd_log_selvar) && is.null(input$rcbd_sq_selvar) && is.null(input$rcbd_sin_selvar)) {
        shinyalert::shinyalert(
          title = "Select your transformation!",
          text = HTML("You have selected the transform option but have not chosen a variable for transformation. 
                      Please either deselect the transform option or select a variable to proceed."),
          type = "warning"
        )
      }
    }
    ############################################## Selected Vaiable Display
    
    
    ################ show variables selected
    output$selected_variables <- renderUI({
      if (is.null(input$rcbd_selvar) || length(input$rcbd_selvar) == 0) {
        HTML("<span style='color: red;'>No variables selected.</span>")
      } else {
        HTML(paste0(
          "<span style='color: #103302;'><b>Variables Selected for multivariate study:</b></span> ",
          "<span style='color: red; font-weight: bold;'>", paste(input$rcbd_selvar, collapse = ", "), "</span>",
          "<p>Click below the required analysis &#128071;</p>"  # 👇 as HTML entity
        ))
        
      }
    })
    ###### ################################display writing for multivariate
    rcbd_multivariate_text <- reactive({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      
      req(rcbd_table())
      data_rcbd <- rcbd_table()
      # 
      # Create output text
      rcbd_multi_text <- paste0(
        "<p>In this section, multivariate tools are available for selecting treatment combinations ",
        "by considering multiple characters simultaneously. These include:</p>",
        "<ul>",
        "<li>Calculating PCA-based index scores</li>",
        "</ul>",
        "<p><b>Principal Component Analysis (PCA)</b> is an exploratory technique that helps to identify patterns ",
        "and reduce dimensionality in multivariate data. It can be used to compute index scores for ranking or selecting ",
        "treatment combinations based on all characters collectively.</p>",
        "<p>It is assumed that the dataset satisfies the assumptions for PCA, including linearity, large sample size, ",
        "and absence of multicollinearity.</p>"
      )
      
      list(multi_introtext = rcbd_multi_text)
    })
    
    
    # multivariate intro text
    output$rcbd_multi_intro <- renderUI({
      req(rcbd_csvfile())
      text_data <- rcbd_multivariate_text()  # Call the reactive expression
      intro_text_multi <- text_data$multi_introtext
      HTML(intro_text_multi)
      
    })
    #################################################
    tags$style(HTML("
  .animated-button {
    background-color: #008CBA; /* Blue */
    border: none;
    color: white;
    padding: 12px 24px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    transition-duration: 0.4s;
    cursor: pointer;
    border-radius: 12px;
  }
  .animated-button:hover {
    background-color: white;
    color: black;
    border: 2px solid #008CBA;
  }
"))
    
    output$multivariate_bttns <- renderUI({
      req(rcbd_table())
      multidata <- rcbd_table()
      rcbd_df = multidata$trans_data
      
      if (ncol(rcbd_df) > 4) {
        fluidRow(
          div(
            actionBttn(
              inputId = "rcbd_pca_bttn",
              label = "Click here for PCA Index", 
              #style = "stretch",
              class = "animated-button",
              #color = "animated-button",
              size = "sm"
            )
          )
          
        )
      } else {
        HTML("<span style='color: blue; font-family: \"Times New Roman\"; font-size: 16px;'>
         You need at least 3 variables to run multivariate analysis
      </span>")
      }
    })
    
    
    ############################# PCA starting----
    rcbd_pca<-reactive({
      if (is.null(input$rcbd_digit)) {
        rcbd_roundvalue <- 2
      } else {
        rcbd_roundvalue <- input$rcbd_digit
      }
      req(input$rcbd_pca_bttn)
      multidata<-rcbd_table()
      rcbd_df = multidata$trans_data
      ## ##arshi(19-08-26) one-way RM - trans_data has 2 id columns (Subject, Day)
      ## then the responses, so PCA needs at least 3 responses => ncol > 4
      ## (was > 7, sized for the two-way module's 5 id columns).
      req(ncol(rcbd_df) > 4)
      ## Keep the within-subject factor (Day) + responses; drop only Subject.
      ## PCA then ranks the Day/time levels across all characters (Treatment in
      ## the two-way version -> Day here).
      rcbd_pca <- rcbd_df[, !(names(rcbd_df) %in% "Subject")]
      rcbd_result_mean <- rcbd_pca %>%
        group_by(Day) %>%
        summarise(across(.cols = everything(), .fns = list(mean = mean)))
      colnames(rcbd_result_mean)<- colnames(rcbd_pca)
      data<-as.data.frame(rcbd_result_mean)
      rownames(data) <- data[[1]]
      data <- data[ , -1] 
      # Order index_1 based on mixed order of row names
      data <- data[gtools::mixedorder(rownames(data)), ]
      ############################################# Performing PCA
      res.pca<-prcomp(data,center = TRUE,scale = TRUE)
      # Reverse the sign of PCA components
      res.pca$x <- (-res.pca$x) # Reverse sign of individuals
      res.pca$rotation<-(-res.pca$rotation)
      
      # Uses singular value decomposition (SVD) to calculate 
      # the covariance and correlations between variables. 
      # SVD is said to have slightly better numerical accuracy
      ################ Eigen values
      eig.val <- get_eigenvalue(res.pca)
      eig.val <-round(as.data.frame(eig.val),rcbd_roundvalue)
      eig.val<-format(eig.val, nsmall = rcbd_roundvalue)
      rownames(eig.val) <- paste0("PC", 1:nrow(eig.val))
      
      ##extract percent variance for plotting
      e1<-eig.val$variance.percent[1]
      e2<-eig.val$variance.percent[2]
      
      eigenValue_display<-eig.val
      colnames(eigenValue_display)<-c("Eigen value","Variance(%)","Cumulative variance")
      load<-res.pca$rotation
      load <-round(as.data.frame(load),rcbd_roundvalue)
      load <-format(load, nsmall = rcbd_roundvalue)
      ########################################## Index
      index1<- res.pca$x[,1]
      index2<- res.pca$x[,2]
      # Rescale index1 and index2 between 0 and 1
      index1_scaled <- rescale(index1, to = c(0, 1))
      index2_scaled <- rescale(index2, to = c(0, 1))
      
      index_1<-cbind(index1,index1_scaled)
      colnames(index_1)<-c("Index Score", "Scaled Index")
      index_1 <-round(as.data.frame(index_1),rcbd_roundvalue)
      index_1 <-format(index_1, nsmall = rcbd_roundvalue)
      # Order index_1 based on mixed order of row names
      index_1 <- index_1[gtools::mixedorder(rownames(index_1)), ]
      
      index_2<-cbind(index2,index2_scaled)
      colnames(index_2)<-c("Index Score", "Scaled Index")
      index_2 <-round(as.data.frame(index_2),rcbd_roundvalue)
      index_2 <-format(index_2, nsmall = rcbd_roundvalue)
      # Order index_2 based on mixed order of row names
      index_2 <- index_2[gtools::mixedorder(rownames(index_2)), ]
      
      ################### write up for tables
      rows_above_one <- rownames(eigenValue_display[eigenValue_display$`Eigen value` > 1, ])
      per_var1<-round(as.numeric(eigenValue_display[1,2]),rcbd_roundvalue)
      per_var2<-round(as.numeric(eigenValue_display[2,2]),rcbd_roundvalue)
      cum_var<-round(as.numeric(eigenValue_display[2,3]),rcbd_roundvalue)
      if (cum_var > 60) {
        if (per_var1 > 40) {
          satement_pcacumvar <- "Since PC1 explains more than 40% of the variance, a PC1-based index score is a strong consideration. Additionally, since both PCs explain more than 60% of the variance in the data, an index score based on both PCs is also appropriate."
        } else {
          satement_pcacumvar <- "Since both PCs explain more than 60% of the variance in the data, an index score based on PCs will make sense."
        }
      } else {
        if (per_var1 > 40) {
          satement_pcacumvar <- "Since PC1 explains more than 40% of the variance, a PC1-based index score is a good consideration. "
        } else {
          satement_pcacumvar <- "Since both PCs explain less than 60% of the variance in the data, an index score based on PCs should be done with caution."
        }
      }
      
      abt_eigen_val <- paste0(
        "The table titled 'Eigen Values PCA' above provides information about the eigenvalues and the percentage of variance explained by each principal component. ",
        "The principal components ", paste(rows_above_one, collapse = ", "),
        " have eigenvalues greater than one and are considered important for further analysis. ",
        "PC1 accounts for approximately ", per_var1, "% of the variance in the dataset, while PC2 accounts for about ",
        per_var2, "% of the variance. ",
        "Together, PC1 and PC2 explain approximately ", cum_var, "% of the total variance (termed as cumulative variance). ",
        satement_pcacumvar,
        " You should look upon the loadings of each variable in the below table and decide which PC-based index needs to be selected. ",
        "The scree plot below illustrates the proportion of variance explained by each principal component."
      )
      
      ################# Loadings writeup
      
      load$PC1 <- as.numeric(load$PC1)
      load$PC2 <- as.numeric(load$PC2)
      positive_PC1 <-rownames(load[load$PC1 > 0, ])
      negative_PC1 <- rownames(load[load$PC1 < 0, ])
      positive_PC2 <-rownames(load[load$PC2 > 0, ])
      negative_PC2 <- rownames(load[load$PC2 < 0, ])                        
      
      
      abt_loading <- paste0(
        "<p>Interpretation of the index score depends on the loadings of each variable on the principal components (PCs). ",
        "You can select the index score based on PC1 or PC2 by considering the loadings and % variance explained by PCs.</p>",
        
        if (length(positive_PC1) > 0) {
          paste0(
            "<p>From the table above (named as 'Loadings of each variable on PCs'), you can see that the variables ",
            paste(positive_PC1, collapse = ", "),
            " have positive loadings in PC1. ",
            "If you are seeking treatments with high values for these variables, consider treatments with a high index score based on PC1. ",
            "Thus, treatments with high index scores based on PC1 can be considered optimal for improving the above variables.</p>"
          )
        } else {
          ""
        },
        
        if (length(negative_PC1) > 0) {
          paste0(
            "<p>Conversely, the variables ",
            paste(negative_PC1, collapse = ", "),
            " have negative loadings in PC1. ",
            "If you aim to improve these variables, look for treatments with a low index score based on PC1.</p>"
          )
        } else {
          ""
        },
        
        if (length(positive_PC2) > 0) {
          paste0(
            "<p>For PC2, the variables ",
            paste(positive_PC2, collapse = ", "),
            " have positive loadings. ",
            "If these variables are your focus, consider an index score based on PC2.</p>"
          )
        } else {
          ""
        },
        
        if (length(negative_PC2) > 0) {
          paste0(
            "<p>Similarly, the variables ",
            paste(negative_PC2, collapse = ", "),
            " have negative loadings on PC2, and treatments with low index scores for PC2 would be suitable for these variables.</p>"
          )
        } else {
          ""
        },
        
        "<p>It is recommended to use variables that are highly correlated for PCA, as this helps in constructing a more reliable and meaningful index.</p>",
        "<p>The biplot below provides a visual representation of the relationships among variables and treatments. ",
        "Treatments with high values for a specific variable are positioned in the direction of that variable. ",
        "The angle between variables in the biplot indicates their correlation—smaller angles suggest high positive correlation, ",
        "while larger angles close to 90 degrees suggest weak or no correlation. ",
        "Thus, the biplot aids in understanding the contributions of variables to each PC and in identifying patterns among treatments.</p>"
      )
      
      abt_indexscore <- paste0(
        "Index scores based on PC1 and PC2 are provided below. ",
        "You can select the desired index based on the explanations given earlier. ",
        "In RAISINS, we calculate a scaled index score by converting the index score to a range of 0 to 1, ",
        "making it easier to interpret and compare. ",
        "This standardized approach ensures consistency in evaluating treatments based on their index scores.<br><br>",
        
        "To refine your selection, use the 'Select cutoff for Scaled Index Score' feature, ",
        "where you can choose the cutoff percentage to select treatments above or below a certain threshold. ",
        "The default cutoff is set at 75%. ",
        "By toggling the up-arrow and down-arrow buttons below the cutoff selection, ",
        "you can select the top or bottom percentage of treatments as per your preference. ",
        "Selected treatments are highlighted in yellow in the table below, providing a clear visual cue. ",
        "Additionally, a plot based on the index scores is also displayed to aid in your analysis."
      )
      
      list(
        rcbd_meanData = data,
        eigen_value = eigenValue_display,
        PCAdata = res.pca,
        loadings = load,
        e1=e1,
        e2=e2,
        index_1=index_1,
        index_2=index_2,
        abt_eigen_table=abt_eigen_val,
        abt_loadings = abt_loading,
        abt_indexscore = abt_indexscore
      )
    })
    
    
    ################## heading PCA
    output$heading_pca <- renderUI({
      req(rcbd_csvfile())  # Ensure file input is provided
      req(rcbd_pca())                 # Ensure PCA results are available
      
      # Render the heading as a styled HTML element (purple, bold, and underlined)
      HTML("<h3 style='color: purple; font-weight: bold; text-decoration: underline;'>PCA-based Index Score</h3>")
    })
    
    
    
    ##################### Eigen Values
    output$rcbd_pca1 <- renderDT({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      req(rcbd_pca())
      data_pca <- rcbd_pca()
      pcaEigen <- data_pca$eigen_value
      multidata<-rcbd_table()
      rcbd_df = multidata$trans_data
      req(rcbd_df)
      
      # Get current date to add to the file name
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      # Render datatable with custom filename for buttons
      DT::datatable(
        pcaEigen,
        extensions = 'Buttons',
        selection = 'none',
        class = 'cell-border stripe table-bordered',
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          dom = 'tB',
          buttons = list(
            list(
              extend = 'copy',
              text = 'Copy to Clipboard',
              filename = paste("PCAeigen_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'csv',
              text = 'Export as CSV',
              filename = paste("PCAeigen_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'excel',
              text = 'Export as Excel',
              filename = paste("PCAeigen_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'pdf',
              text = 'Export as PDF',
              filename = paste("PCAeigen_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            )
          )
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
          "Eigen Values PCA"
        )
      )
      
    })
    
    ######################## writeup for eigen table  
    output$rcbd_eigen_abt <- renderUI({
      req(rcbd_csvfile())
      req(rcbd_pca())
      
      pca_data <- rcbd_pca()
      intro_text_eigen <- pca_data$abt_eigen_table
      
      # Render the table with color #9e9e9d
      HTML(paste0("<div style='color: #9e9e9d;'>", intro_text_eigen, "</div>"))
    })
    
    ######################################### SCREE PLOT       
    # Reactive expression for the plot
    
    
    ############Scree plot by Hisham ########################################
    
    rcbd_PCA_screePlt <- reactive({
      if (is.null(rcbd_csvfile())) {
        return(NULL)
      }
      req(rcbd_pca())
      data_rcbd <- rcbd_pca()
      screePlt_data <- data_rcbd$PCAdata
      
      # Define theme based on user input
      scree_theme <- switch(
        input$scree_theme,  # Dropdown for theme
        "normal" = theme_bw(),
        "economist" = theme_economist(),
        "minimal" = theme_minimal(),
        "gray" = theme_gray(),
        "light" = theme_light(),
        "void" = theme_void(),
        "tufte" = theme_tufte(),
        "stata" = theme_stata(),
        "wsj" = theme_wsj(),
        "calc" = theme_calc(),
        "hc" = theme_hc(),
        theme_bw()
      )
      
      # Generate Scree Plot
      rcbd_scree <- fviz_eig(screePlt_data, 
                             addlabels = TRUE, 
                             barfill = input$rcbd_scree_barfill,
                             barcolor = input$rcbd_scree_barcolor,
                             linecolor = input$rcbd_scree_linecolor,
                             geom = c("bar", "line"),
                             pointsize = input$rcbd_scree_dotsize
      ) +
        geom_point(color = input$rcbd_scree_dotcolor, size = input$rcbd_scree_dotsize, 
                   shape = as.numeric(input$rcbd_scree_dotshape))
      
      
      # Apply customizations
      screeplt <- rcbd_scree +
        scree_theme +  # Apply the selected theme
        ylab("% variance explained") +
        xlab("Principal Components") +
        ggtitle(input$title_scree) +  # Custom plot title
        theme(aspect.ratio = 1,
              plot.background = element_rect(fill = input$background_color_rcbd_scree, color = NA),
              axis.title.x = element_text(size = input$xlab_size_rcbd_scree, color = input$xlab_color_rcbd_scree),  
              axis.title.y = element_text(size = input$ylab_size_rcbd_scree, color = input$ylab_color_rcbd_scree),  
              axis.text.x = element_text(size = input$xtick_size_rcbd_scree, color = input$xtick_color_rcbd_scree),  
              axis.text.y = element_text(size = input$ytick_size_rcbd_scree, color = input$ytick_color_rcbd_scree), 
              plot.title = element_text(size = input$title_size_rcbd_scree, color = input$title_color_rcbd_scree, face = "bold", hjust = 0.5)
        )
      
      screeplt
    })
    
    # Render plot in UI
    output$rcbd_PCA_screePlt <- renderPlot({
      req(rcbd_PCA_screePlt())
      rcbd_PCA_screePlt()
    })
    
    # Download Handler
    output$download_scree_plot <- downloadHandler(
      filename = function() {
        paste("Scree_Plot", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(
          file,
          plot = rcbd_PCA_screePlt(),
          device = "png",
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    )
    
    
    ###################end scree plot by Hisham ###################
    
    ####Loadings
    
    output$rcbd_PCA_loadings <- renderDT({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      req(rcbd_pca())
      data_rcbd<- rcbd_pca()
      rcbd_PCA_loadings<-data_rcbd$loadings 
      
      # Get current date to add to the file name
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      
      DT::datatable(
        rcbd_PCA_loadings,
        extensions = 'Buttons',
        selection = 'none',
        class = 'cell-border stripe table-bordered',
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          dom = 'tB',
          buttons = list(
            list(
              extend = 'copy',
              text = 'Copy to Clipboard',
              filename = paste("PCAloadings_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'csv',
              text = 'Export as CSV',
              filename = paste("PCAloadings_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'excel',
              text = 'Export as Excel',
              filename = paste("PCAloadings_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            ),
            list(
              extend = 'pdf',
              text = 'Export as PDF',
              filename = paste("PCAloadings_", current_date, sep = ""),
              title = ""  # Ensures no extra title text in the exported file
            )
          )
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
          "Loadings of each variable on PCs"
        )
      )
    })      
    ##################################### writeup for loadings  
    output$rcbd_loadings_abt <- renderUI({
      req(rcbd_csvfile()) 
      req(rcbd_pca())                
      pca_data <- rcbd_pca()
      intro_text_loading <- pca_data$abt_loadings
      # Render the content with color #9e9e9d
      HTML(paste0("<div style='color: #9e9e9d;'>", intro_text_loading, "</div>"))
    })
    
    
    ################## biplot by HISHAM #############        
    rcbd_PCA_biplot <- reactive({
      if (is.null(rcbd_csvfile())) {
        return(NULL)
      }
      req(rcbd_pca())  # Ensure PCA data exists
      
      data_rcbd <- rcbd_pca()
      biplot_data <- data_rcbd$PCAdata
      e1 <- data_rcbd$e1
      e2 <- data_rcbd$e2
      
      selected_theme <- switch(
        input$rcbd_biplot_theme,
        "normal" = theme_gray(),
        "economist" = theme_economist(),
        "minimal" = theme_minimal(),
        "gray" = theme_gray(),
        "light" = theme_light(),
        "void" = theme_void(),
        "tufte" = theme_tufte(),
        "stata" = theme_stata(),
        "wsj" = theme_wsj(),
        "calc" = theme_calc(),
        "hc" = theme_hc(),
        theme_gray()  # Default theme
      )
      
      # # Set default or fallback values for inputs
      col.var <- input$rcbd_biplot_var_color %||% "black"
      col.ind <- input$rcbd_biplot_ind_color %||% "black"
      size.ind <- input$rcbd_biplot_ind_size %||% 3
      size.var <- input$rcbd_biplot_var_size %||% 3
      
      
      # # Generate Biplot
      Biplot <- fviz_pca_biplot(
        biplot_data,
        axes = c(1, 2),
        geom.ind = c("point", "text"),
        geom.var = c("arrow", "text"),
        repel = input$rcbd_biplot_repel %||% TRUE,
        col.var = col.var,
        col.ind = col.ind,
        pointsize = input$rcbd_biplot_pointsize %||% 3,  # Adjust point size
        labelsize = input$rcbd_biplot_labelsize %||% 3,  # Adjust label size
        arrowsize = input$rcbd_biplot_arrowsize %||% 0.5,
        alpha.ind = input$rcbd_biplot_alpha_ind %||% 1,
        alpha.var = input$rcbd_biplot_alpha_var %||% 1,
        size.ind <- size.ind,
        size.var <- size.var,
        #addEllipses = input$rcbd_biplot_add_ellipses %||% FALSE,
        #ellipse.level = input$rcbd_biplot_ellipse_level %||% 0.95,
        label = input$rcbd_biplot_label %||% "all",
        invisible = input$rcbd_biplot_invisible %||% "none"
      )
      
      # Customize plot appearance
      rcbd_biplot <- Biplot +
        selected_theme +
        ggtitle(input$rcbd_biplot_title %||% "PCA-Biplot") +
        theme(
          plot.title = element_text(size = input$rcbd_biplot_title_size %||% 16,
                                    color = input$rcbd_biplot_title_color %||% "black",
                                    hjust = 0.5),
          axis.title.x = element_text(size = input$rcbd_biplot_xlab_size %||% 14),
          axis.title.y = element_text(size = input$rcbd_biplot_ylab_size %||% 14),
          text = element_text(size = input$rcbd_biplot_labelsize_var %||% 4),
          axis.text = element_text(size = input$rcbd_biplot_labelsize_ind %||% 4),
          plot.background = element_rect(fill = input$rcbd_biplot_background_color %||% "white", color = NA)
        ) +
        xlab(paste0("PC1 (", e1, "%)")) +
        ylab(paste0("PC2 (", e2, "%)"))
      
      return(rcbd_biplot)
    })
    
    
    # Render the Biplot in the UI
    output$rcbd_biplot <- renderPlot({
      req(rcbd_PCA_biplot())  # Ensure the reactive expression is ready
      rcbd_PCA_biplot()
    })
    
    # Download Handler for saving the Biplot
    output$download_rcbd_biplot <- downloadHandler(
      filename = function() {
        paste("BiplotPCA", Sys.Date(), ".png", sep = "")  
      },
      content = function(file) {
        ggsave(file, plot = rcbd_PCA_biplot(), device = "png", width = 8, height = 6, dpi = 300)  # Save the plot
      }
    )
    ##################################### writeup for indexscore
    output$rcbd_indexscore_abt <- renderUI({
      req(rcbd_csvfile())  
      req(rcbd_pca())                 
      pca_data <- rcbd_pca() 
      intro_text_indexscore <- pca_data$abt_indexscore
      HTML(paste0("<div style='color: #9e9e9d;'>", intro_text_indexscore, "</div>"))
    })
    
    
    ##########################################
    ############# switching button PC selection
    
    ############# Switching Button for PC Selection
    
    output$rcbd_pca_varselect <- renderUI({
      req(input$rcbd_pca_bttn)  # Ensure toggle button is clicked
      req(input$rcbd_pcaCutoff)  # Ensure cutoff value exists
      
      # Convert input to numeric
      cutoff <- as.numeric(input$rcbd_pcaCutoff)
      
      # Create a toggle button to switch between top/bottom PC selection
      prettyToggle(
        inputId = "rcbd_pca_abovecut",
        label_on = sprintf("Select top %.2f%% treatments (Index score)", cutoff * 100),
        icon_on = icon("arrow-up"),
        status_on = "info",
        status_off = "warning",
        label_off = sprintf("Select bottom %.2f%% treatments (Index score)", (1 - cutoff) * 100),
        icon_off = icon("arrow-down"),
        value = TRUE  # Default value
      )
    })
    
    
    
    ######################### Index score
    output$rcbd_PCA_index1 <- renderDT({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      req(input$rcbd_pcaCutoff)
      cutoff <- as.numeric(input$rcbd_pcaCutoff)
      req(rcbd_pca())
      data_rcbd <- rcbd_pca()
      rcbd_PCA_index1 <- data_rcbd$index_1
      
      # Get current date to add to the file name
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      ################################ INDEX score PC1
      is_above_cutoff <- isTRUE(input$rcbd_pca_abovecut)  # Defaults to false if NULL
      if(is_above_cutoff){
        DT::datatable(
          rcbd_PCA_index1,
          extensions = 'Buttons',
          selection = 'none',
          options = list(
            select = 'single',
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE,
            #lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
            ordering = FALSE,
            dom = 'tB',
            buttons = list(
              list(
                extend = 'copy',
                text = 'Copy to Clipboard',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'csv',
                text = 'Export as CSV',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'excel',
                text = 'Export as Excel',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'pdf',
                text = 'Export as PDF',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              )
            ),
            rowCallback = JS(
              sprintf(
                "function(row, data, index) {
          if (parseFloat(data[2]) > %f) {
            $(row).css('background-color', '#fff7a3');
          }
        }", cutoff
              )
            )
          ),
          class = 'cell-border stripe table-bordered',
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
            "Index score based on first PC"
          )
        )
      }
      else{
        DT::datatable(
          rcbd_PCA_index1,
          extensions = 'Buttons',
          selection = 'none',
          options = list(
            select = 'single',
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE,
            #lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
            ordering = FALSE,
            dom = 'tB',
            buttons = list(
              list(
                extend = 'copy',
                text = 'Copy to Clipboard',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'csv',
                text = 'Export as CSV',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'excel',
                text = 'Export as Excel',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'pdf',
                text = 'Export as PDF',
                filename = paste("indexPC1_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              )
            ),
            rowCallback = JS(
              sprintf(
                "function(row, data, index) {
          if (parseFloat(data[2]) < %f) {
            $(row).css('background-color', '#fff7a3');
          }
        }", 1-cutoff
              )
            )
          ),
          class = 'cell-border stripe table-bordered',
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
            "Index score based on first PC"
          )
        )
        
      }
    })
    
    
    ######################### Index score2
    output$rcbd_PCA_index2 <- renderDT({
      if (is.null(rcbd_csvfile())) {
        return()
      }
      req(input$rcbd_pcaCutoff)
      cutoff <- as.numeric(input$rcbd_pcaCutoff)
      req(rcbd_pca())
      data_rcbd <- rcbd_pca()
      rcbd_PCA_index2 <- data_rcbd$index_2
      
      # Get current date to add to the file name
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      is_above_cutoff <- isTRUE(input$rcbd_pca_abovecut)  # Defaults to false if NULL
      if(is_above_cutoff){
        DT::datatable(
          rcbd_PCA_index2,
          extensions = 'Buttons',
          selection = 'none',
          options = list(
            select = 'single',
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            dom = 'tB',
            buttons = list(
              list(
                extend = 'copy',
                text = 'Copy to Clipboard',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'csv',
                text = 'Export as CSV',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'excel',
                text = 'Export as Excel',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'pdf',
                text = 'Export as PDF',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              )
            ),
            rowCallback = JS(
              sprintf(
                "function(row, data, index) {
          if (parseFloat(data[2]) > %f) {
            $(row).css('background-color', '#fff7a3');
          }
        }", cutoff
              )
            )
          ),
          class = 'cell-border stripe table-bordered',
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
            "Index score based on second PC"
          )
        )
      }
      else{
        DT::datatable(
          rcbd_PCA_index2,
          extensions = 'Buttons',
          selection = 'none',
          options = list(
            select = 'single',
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            dom = 'tB',
            buttons = list(
              list(
                extend = 'copy',
                text = 'Copy to Clipboard',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'csv',
                text = 'Export as CSV',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'excel',
                text = 'Export as Excel',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              ),
              list(
                extend = 'pdf',
                text = 'Export as PDF',
                filename = paste("indexPC2_", current_date, sep = ""),
                title = ""  # Ensures no extra title text in the exported file
              )
            ),
            rowCallback = JS(
              sprintf(
                "function(row, data, index) {
          if (parseFloat(data[2]) < %f) {
            $(row).css('background-color', '#fff7a3');
          }
        }", 1-cutoff
              )
            )
          ),
          class = 'cell-border stripe table-bordered',
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
            "Index score based on second PC"
          )
        ) 
      }
      
      
      
      
    })    
    
    ################################# Plot based on Index1 by HISHAM
    rcbd_PCA_Indexplot1 <- reactive({
      if (is.null(rcbd_csvfile())) {
        return(NULL)
      }
      req(input$rcbd_pcaCutoff)
      req(rcbd_pca())
      
      cutoff <- as.numeric(input$rcbd_pcaCutoff)
      data_rcbd <- rcbd_pca()
      data_index <- data_rcbd$rcbd_meanData
      index1_scaled <- data_rcbd$index_1
      data_index$ind1 <- as.numeric(index1_scaled[, 2])
      
      ### Create data for index_1
      is_above_cutoff <- isTRUE(input$rcbd_pca_abovecut)  # Defaults to FALSE if NULL
      if (is_above_cutoff) {
        data_ind1 <- data_index %>%
          mutate(
            sel = ifelse(ind1 > cutoff, "Selected", "Nonselected"),
            Treatment = rownames(data_index)
          ) %>%
          arrange(desc(ind1)) %>% 
          mutate(
            Treatment = factor(Treatment, levels = Treatment)
          )
        
        cutoff_line <- cutoff  # Line for cutoff
        plot_title <- input$rcbd_Indexplot1_title  # Title from input
      } else {
        data_ind1 <- data_index %>%
          mutate(
            sel = ifelse(ind1 < (1 - cutoff), "Selected", "Nonselected"),
            Treatment = rownames(data_index)
          ) %>%
          arrange(ind1) %>% 
          mutate(
            Treatment = factor(Treatment, levels = Treatment)
          )
        
        cutoff_line <- 1 - cutoff  # Line for 1-cutoff
        plot_title <- input$rcbd_Indexplot1_title  # Title from input
      }
      
      # Dynamic theme mapping
      selected_theme <- switch(
        input$rcbd_Indexplot1_theme,
        "normal" = theme_gray(),
        "economist" = theme_economist(),
        "minimal" = theme_minimal(),
        "gray" = theme_gray(),
        "light" = theme_light(),
        "void" = theme_void(),
        "tufte" = theme_tufte(),
        "stata" = theme_stata(),
        "wsj" = theme_wsj(),
        "calc" = theme_calc(),
        "hc" = theme_hc(),
        theme_minimal()  # Default theme
      )
      
      # Generate the Index Plot
      indexplot1 <- ggplot(data_ind1, aes(x = Treatment, y = ind1, color = sel)) +
        
        geom_path(aes(group = 1), color = "black") +  # Path layer first
        geom_point(size = input$rcbd_Indexplot1_dot_size, shape = as.numeric(input$rcbd_Indexplot1_dot_shape)) +
        scale_color_manual(
          name = "",  # Set legend title here
          values = c(
            "Nonselected" = input$rcbd_Indexplot1_non_sel_color,  # Nonselected color
            "Selected" = input$rcbd_Indexplot1_sel_color  # Selected color
          )
        ) +
        geom_hline(
          yintercept = cutoff_line, 
          color = input$rcbd_Indexplot1_cutoff_line_color, 
          linetype = "solid", 
          size = 1
        ) +
        labs(x = "Treatments", y = "PCA scaled index") +
        coord_polar(start = 0) +
        selected_theme +  # Apply user-selected theme
        ggtitle(plot_title) +
        theme(
          plot.title = element_text(
            size = input$rcbd_Indexplot1_title_size,  # Title size from input
            color = input$rcbd_Indexplot1_title_color,  # Title color from input
            hjust = 0.5  # Center the title
          ),
          plot.background = element_rect(fill = input$rcbd_Indexplot1_background_color, color = NA),
          panel.grid.major = element_line(color = input$rcbd_Indexplot1_major_ring_color %||% "lightgray"),
          axis.text.x = element_text(size = input$rcbd_Indexplot1_xlab_size, color = input$rcbd_Indexplot1_xlab_color),
          axis.text.y = element_text(size = input$rcbd_Indexplot1_ylab_size, color = input$rcbd_Indexplot1_ylab_color),
          axis.title.x = element_text(size = input$rcbd_Indexplot1_xaxis_size, color = input$rcbd_Indexplot1_xaxis_color),
          axis.title.y = element_text(size = input$rcbd_Indexplot1_yaxis_size, color = input$rcbd_Indexplot1_yaxis_color)
          
        )
      
      return(indexplot1)
    })
    
    # Render plot in UI
    output$rcbd_Indexplot1 <- renderPlot({
      req(rcbd_PCA_Indexplot1())
      rcbd_PCA_Indexplot1()
    })
    
    # Download Handler
    output$download_rcbd_Indexplot1<- downloadHandler(
      filename = function() {
        paste("Indexplot1", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = rcbd_PCA_Indexplot1(), device = "png", width = 8, height = 6, dpi=300)
      }
    ) 
    ################################# Plot based on Index2 by Hisham
    rcbd_PCA_Indexplot2 <- reactive({
      if (is.null(rcbd_csvfile())) {
        return(NULL)
      }
      req(input$rcbd_pcaCutoff)
      req(rcbd_pca())
      
      cutoff <- as.numeric(input$rcbd_pcaCutoff)
      data_rcbd <- rcbd_pca()
      data_index <- data_rcbd$rcbd_meanData
      index2_scaled <- data_rcbd$index_2
      data_index$ind2 <- as.numeric(index2_scaled[, 2])
      
      ### Create data for index_2
      is_above_cutoff <- isTRUE(input$rcbd_pca_abovecut)  # Defaults to FALSE if NULL
      if (is_above_cutoff) {
        data_ind2 <- data_index %>%
          mutate(
            sel = ifelse(ind2 > cutoff, "Selected", "Nonselected"),
            Treatment = rownames(data_index)
          ) %>%
          arrange(desc(ind2)) %>% 
          mutate(
            Treatment = factor(Treatment, levels = Treatment)
          )
        
        cutoff_line <- cutoff  # Line for cutoff
        plot_title <- input$rcbd_Indexplot2_title  # Title from input
      } else {
        data_ind2 <- data_index %>%
          mutate(
            sel = ifelse(ind2 < (1 - cutoff), "Selected", "Nonselected"),
            Treatment = rownames(data_index)
          ) %>%
          arrange(ind2) %>% 
          mutate(
            Treatment = factor(Treatment, levels = Treatment)
          )
        
        cutoff_line <- 1 - cutoff  # Line for 1-cutoff
        plot_title <- input$rcbd_Indexplot2_title  # Title from input
      }
      
      # Dynamic theme mapping
      selected_theme <- switch(
        input$rcbd_Indexplot2_theme,
        "normal" = theme_gray(),
        "economist" = theme_economist(),
        "minimal" = theme_minimal(),
        "gray" = theme_gray(),
        "light" = theme_light(),
        "void" = theme_void(),
        "tufte" = theme_tufte(),
        "stata" = theme_stata(),
        "wsj" = theme_wsj(),
        "calc" = theme_calc(),
        "hc" = theme_hc(),
        theme_minimal()  # Default theme
      )
      
      # Generate the Index Plot
      indexplot2 <- ggplot(data_ind2, aes(x = Treatment, y = ind2, color = sel)) +
        geom_path(aes(group = 1), color = "black") +  # Path layer first
        geom_point(size = input$rcbd_Indexplot2_dot_size, shape = as.numeric(input$rcbd_Indexplot2_dot_shape)) +
        scale_color_manual(
          name = "",  # Set legend title here
          values = c(
            "Nonselected" = input$rcbd_Indexplot2_non_sel_color,  # Nonselected color
            "Selected" = input$rcbd_Indexplot2_sel_color  # Selected color
          )
        ) +
        geom_hline(
          yintercept = cutoff_line, 
          color = input$rcbd_Indexplot2_cutoff_line_color, 
          linetype = "solid", 
          size = 1
        ) +
        labs(
          x = "Treatments",
          y = "PCA scaled index"
        ) +
        coord_polar(start = 0) +
        selected_theme +  # Apply user-selected theme
        ggtitle(plot_title) +
        theme(
          plot.title = element_text(
            size = input$rcbd_Indexplot2_title_size,  # Title size from input
            color = input$rcbd_Indexplot2_title_color,  # Title color from input
            hjust = 0.5  # Center the title
          ),
          plot.background = element_rect(fill = input$rcbd_Indexplot2_background_color, color = NA),
          panel.grid.major = element_line(color = input$rcbd_Indexplot2_major_ring_color %||% "lightgray"),
          axis.text.x = element_text(size = input$rcbd_Indexplot2_xlab_size, color = input$rcbd_Indexplot2_xlab_color),
          axis.text.y = element_text(size = input$rcbd_Indexplot2_ylab_size, color = input$rcbd_Indexplot2_ylab_color),
          axis.title.x = element_text(size = input$rcbd_Indexplot2_xaxis_size, color = input$rcbd_Indexplot2_xaxis_color),
          axis.title.y = element_text(size = input$rcbd_Indexplot2_yaxis_size, color = input$rcbd_Indexplot2_yaxis_color)
        )
      
      return(indexplot2)
    })
    
    # Render plot in UI
    output$rcbd_Indexplot2 <- renderPlot({
      req(rcbd_PCA_Indexplot2())
      rcbd_PCA_Indexplot2()
    })
    
    # Download Handler
    output$download_rcbd_Indexplot2<- downloadHandler(
      filename = function() {
        paste("Indexplot2", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = rcbd_PCA_Indexplot2(), device = "png", width = 8, height = 6, dpi=300)
      }
    )      
    #################### PCA ending

    
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##        Advanced Plots — 3FCRD additions (ported/adapted from 2FCRD)       ----
    ##  Icon-button driven plots: Summary, Distribution, Raincloud,
    ##  Advanced Raincloud, Circular Bar, QQ. Data comes from rcbd_plotdata()
    ##  which already exposes A / B / C / AB / AC / BC / ABC long-format frames.
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ## invalidation key (kept for parity with the 2FCRD render functions) ----
    reset_key <- reactiveVal(0)

    ## clean treatment labels ----
    rcbd_clean_label <- function(x) {
      x <- as.character(x)
      x <- gsub("[\\._-]+", " ", x)
      x <- gsub("\\s+", " ", x)
      trimws(x)
    }

    ## colour helpers (replace colorspace::lighten / colorspace::darken) ----
    lighten <- function(color, amount = 0.1) {
      sapply(color, function(col) {
        v <- col2rgb(col) / 255
        rgb(min(1, v[1] + amount), min(1, v[2] + amount), min(1, v[3] + amount))
      }, USE.NAMES = FALSE)
    }
    darken <- function(color, amount = 0.1) {
      sapply(color, function(col) {
        v <- col2rgb(col) / 255
        rgb(max(0, v[1] - amount), max(0, v[2] - amount), max(0, v[3] - amount))
      }, USE.NAMES = FALSE)
    }

    ## helper: wide-format df with a Treatment column, from the long frames ----
    get_rmtwoway_treatment_df <- function(factor_choice, plotdata) {
      factor_choice <- factor_choice %||% "A"
      if (!factor_choice %in% c("A", "B", "AB")) factor_choice <- "A"

      data_long <- switch(factor_choice,
        "A"   = plotdata$df_datalongA,
        "B"   = plotdata$df_datalongB,
        "AB"  = plotdata$df_datalongAB
      )
      factor_col <- switch(factor_choice,
        "A"   = "Treatment",
        "B"   = "Day",
        "AB"  = "TreatmentxDay"
      )

      data_long %>%
        group_by(!!sym(factor_col), character) %>%
        mutate(.rep = row_number()) %>%
        ungroup() %>%
        pivot_wider(names_from = character, values_from = value) %>%
        select(-.rep) %>%
        rename(Treatment = !!sym(factor_col))
    }

    ## ~ Summary Plot -----------------------------------------------------------
    create_summary_gt_table <- function(df, input) {

      gt_table <- df %>% gt_plt_summary()

      gt_table <- gt_table %>%
        opt_stylize(
          style = as.numeric(input$rcbd_summary_theme),
          color = input$rcbd_summary_color
        )

      if (isTRUE(input$rcbd_summary_show_title)) {

        title_text <- if (is.null(input$rcbd_summary_title) || input$rcbd_summary_title == "") {
          "Data Summary"
        } else {
          input$rcbd_summary_title
        }

        subtitle_text <- if (is.null(input$rcbd_summary_subtitle) || input$rcbd_summary_subtitle == "") {
          NULL
        } else {
          input$rcbd_summary_subtitle
        }

        gt_table <- gt_table %>%
          tab_header(title = title_text, subtitle = subtitle_text)
      }

      if (isTRUE(input$rcbd_summary_show_source) &&
          !is.null(input$rcbd_summary_source) &&
          input$rcbd_summary_source != "") {
        gt_table <- gt_table %>%
          tab_source_note(source_note = input$rcbd_summary_source)
      }

      gt_table <- gt_table %>%
        tab_options(
          table.font.size = paste0(input$rcbd_summary_font_size, "px"),
          table.font.names = if (input$rcbd_summary_font_family != "default") input$rcbd_summary_font_family else NULL,
          row.striping.include_table_body = input$rcbd_summary_striped_rows,
          table.border.top.style = if (input$rcbd_summary_bordered) "solid" else "none",
          table.border.bottom.style = if (input$rcbd_summary_bordered) "solid" else "none",
          column_labels.border.bottom.style = if (input$rcbd_summary_bordered) "solid" else "none",
          table.width = if (input$rcbd_summary_table_width != "auto") input$rcbd_summary_table_width else NULL
        )

      if (input$rcbd_summary_number_format == "standard") {
        gt_table <- gt_table %>%
          fmt_number(decimals = input$rcbd_summary_decimals, use_seps = input$rcbd_summary_use_separator)
      } else if (input$rcbd_summary_number_format == "percent") {
        gt_table <- gt_table %>% fmt_percent(decimals = input$rcbd_summary_decimals)
      } else if (input$rcbd_summary_number_format == "currency") {
        gt_table <- gt_table %>% fmt_currency(currency = input$rcbd_summary_currency, decimals = input$rcbd_summary_decimals)
      } else if (input$rcbd_summary_number_format == "scientific") {
        gt_table <- gt_table %>% fmt_scientific(decimals = input$rcbd_summary_decimals)
      }

      if (input$rcbd_summary_text_align != "left") {
        gt_table <- gt_table %>% cols_align(align = input$rcbd_summary_text_align)
      }

      gt_table
    }

    summary_table_data <- reactive({
      req(rcbd_plotdata())
      get_rmtwoway_treatment_df(input$selected_factor_summary %||% "A", rcbd_plotdata())
    })

    output$rcbd_summary_plot_table <- render_gt({
      df <- summary_table_data()
      create_summary_gt_table(df, input)
    })

    output$download_summary_html <- downloadHandler(
      filename = function() { paste0("summary_table_", Sys.Date(), ".html") },
      content = function(file) {
        df <- summary_table_data()
        gt_table <- create_summary_gt_table(df, input)
        gt_table %>% gtsave(filename = file)
      }
    )

    ## ~ Distribution Plot ------------------------------------------------------
    create_rcbd_distribution_table <- reactive({
      reset_key()
      req(rcbd_plotdata())

      plot_type <- input$rcbd_dist_plot_type
      fill_col  <- input$rcbd_dist_fill_color
      line_col  <- input$rcbd_dist_line_color
      same_lim  <- input$rcbd_dist_same_limits
      theme_id  <- input$rcbd_dist_theme
      scheme    <- input$rcbd_dist_color_scheme

      df_long <- get_rmtwoway_treatment_df(input$selected_factor_distribution %||% "A", rcbd_plotdata())

      agg_data <- df_long %>%
        group_by(Treatment) %>%
        summarize(across(where(is.numeric), ~ list(.), .names = "{.col}"), .groups = "drop")

      list_cols <- agg_data %>% select(where(is.list)) %>% names()

      if (length(list_cols) > 0) {
        gt_table <- Reduce(
          function(gt_tbl, col_name) {
            gt_plt_dist(gt_tbl, column = !!sym(col_name),
                        type = plot_type, fill_color = fill_col,
                        line_color = line_col, same_limit = same_lim)
          },
          list_cols,
          init = gt(agg_data)
        )
      } else {
        gt_table <- gt(data.frame(Message = "No numeric trait columns found."))
      }

      gt_table <- gt_table %>% opt_stylize(style = as.numeric(theme_id), color = scheme)

      if (isTRUE(input$rcbd_dist_show_title)) {
        title_text    <- if (is.null(input$rcbd_dist_title)    || input$rcbd_dist_title == "")    "Distribution Analysis" else input$rcbd_dist_title
        subtitle_text <- if (is.null(input$rcbd_dist_subtitle) || input$rcbd_dist_subtitle == "") NULL else input$rcbd_dist_subtitle
        gt_table <- gt_table %>% tab_header(title = title_text, subtitle = subtitle_text)
      }

      if (isTRUE(input$rcbd_dist_show_source) && !is.null(input$rcbd_dist_source) && input$rcbd_dist_source != "") {
        gt_table <- gt_table %>% tab_source_note(source_note = input$rcbd_dist_source)
      }

      gt_table <- gt_table %>%
        tab_options(
          table.font.size = paste0(input$rcbd_dist_font_size, "px"),
          table.font.names = if (input$rcbd_dist_font_family != "default") input$rcbd_dist_font_family else NULL,
          row.striping.include_table_body = input$rcbd_dist_striped_rows,
          table.border.top.style    = if (input$rcbd_dist_bordered) "solid" else "none",
          table.border.bottom.style = if (input$rcbd_dist_bordered) "solid" else "none",
          column_labels.border.bottom.style = if (input$rcbd_dist_bordered) "solid" else "none",
          table.width = if (input$rcbd_dist_table_width != "auto") input$rcbd_dist_table_width else NULL,
          data_row.padding = px(input$rcbd_dist_plot_height / 10)
        )

      if (input$rcbd_dist_text_align != "left") {
        gt_table <- gt_table %>% cols_align(align = input$rcbd_dist_text_align)
      }

      return(gt_table)
    })

    output$rcbd_distribution_plot_table <- render_gt({ create_rcbd_distribution_table() })

    output$download_rcbd_distribution_table <- downloadHandler(
      filename = function() { paste0("distribution_analysis_", Sys.Date(), ".html") },
      content  = function(file) { gt::gtsave(create_rcbd_distribution_table(), file) }
    )

    ## ~ Raincloud Plot ---------------------------------------------------------
    create_raincloud_plot <- function(data, facet_cols = 2, facet_scales = "free_y",
                                      aspect_ratio = 1, show_title = TRUE, title = "Raincloud Plots",
                                      show_density = TRUE, density_adjust = 0.6,
                                      density_justification = -0.3, density_alpha = 0.8, density_width = 1,
                                      show_boxplot = TRUE, box_width = 0.1, box_alpha = 0.4,
                                      show_outliers = FALSE, outlier_size = 1.5,
                                      show_points = TRUE, point_type = "jitter", point_width = 0.05,
                                      point_size = 1, point_alpha = 0.5, point_color = "black",
                                      color_palette = "default", show_legend = FALSE,
                                      legend_position = "right", theme_choice = "minimal",
                                      x_label = "", y_label = "Value",
                                      title_size = 14, axis_text_size = 10,
                                      strip_text_size = 11, strip_bold = TRUE, font_family = "") {

      data_long <- data %>%
        pivot_longer(cols = -Treatment, names_to = "character", values_to = "Value") %>%
        mutate(Treatment = as.factor(Treatment))

      p <- ggplot(data_long, aes(y = Value, x = Treatment, fill = Treatment, color = Treatment))

      if (show_density) {
        p <- p + ggdist::stat_halfeye(adjust = density_adjust, justification = density_justification,
                                      .width = 0, point_colour = NA, alpha = density_alpha, scale = density_width)
      }

      if (show_boxplot) {
        outlier_shape <- if (show_outliers) 16 else NA
        p <- p + geom_boxplot(width = box_width, outlier.shape = outlier_shape,
                              outlier.size = if (show_outliers) outlier_size else 0, alpha = box_alpha)
      }

      if (show_points) {
        if (point_type == "jitter") {
          p <- p + geom_jitter(width = point_width, alpha = point_alpha, size = point_size, color = point_color)
        } else if (point_type == "swarm") {
          p <- p + ggbeeswarm::geom_beeswarm(alpha = point_alpha, size = point_size, color = point_color, cex = point_width * 10)
        } else if (point_type == "strip") {
          p <- p + geom_point(position = position_jitter(width = point_width, height = 0),
                              alpha = point_alpha, size = point_size, color = point_color)
        }
      }

      p <- p + facet_wrap(~character, scales = facet_scales, ncol = facet_cols)

      if (theme_choice == "minimal")      p <- p + theme_minimal()
      else if (theme_choice == "classic") p <- p + theme_classic()
      else if (theme_choice == "light")   p <- p + theme_light()
      else if (theme_choice == "dark")    p <- p + theme_dark()
      else if (theme_choice == "void")    p <- p + theme_void()
      else if (theme_choice == "bw")      p <- p + theme_bw()

      if (color_palette != "default") {
        if (color_palette == "viridis") p <- p + scale_fill_viridis_d()
        else                            p <- p + scale_fill_brewer(palette = color_palette)
      }

      p <- p + labs(title = if (show_title) title else NULL, x = x_label, y = y_label)

      p <- p + theme(
        strip.text   = element_text(face = if (strip_bold) "bold" else "plain", size = strip_text_size, family = font_family),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text    = element_text(size = axis_text_size, family = font_family),
        plot.title   = element_text(size = title_size, hjust = 0.5, family = font_family),
        axis.title   = element_text(family = font_family),
        legend.text  = element_text(family = font_family),
        legend.title = element_text(family = font_family),
        aspect.ratio = aspect_ratio,
        legend.position = if (show_legend) legend_position else "none"
      )

      return(p)
    }

    output$rcbd_raincloud_plot <- renderPlot({
      req(rcbd_plotdata(), rcbd_table())
      df <- get_rmtwoway_treatment_df(input$selected_factor_raincloud %||% "A", rcbd_plotdata())
      create_raincloud_plot(
        data = df,
        facet_cols          = as.numeric(input$rcbd_rain_facet_cols %||% 2),
        facet_scales        = input$rcbd_rain_facet_scales %||% "free_y",
        aspect_ratio        = input$rcbd_rain_aspect_ratio %||% 1,
        show_title          = input$rcbd_rain_show_title %||% TRUE,
        title               = input$rcbd_rain_title %||% "Raincloud Plots",
        show_density        = input$rcbd_rain_show_density %||% TRUE,
        density_adjust      = input$rcbd_rain_density_adjust %||% 0.6,
        density_justification = input$rcbd_rain_density_justification %||% -0.3,
        density_alpha       = input$rcbd_rain_density_alpha %||% 0.8,
        density_width       = input$rcbd_rain_density_width %||% 1,
        show_boxplot        = input$rcbd_rain_show_boxplot %||% TRUE,
        box_width           = input$rcbd_rain_box_width %||% 0.1,
        box_alpha           = input$rcbd_rain_box_alpha %||% 0.4,
        show_outliers       = input$rcbd_rain_show_outliers %||% FALSE,
        outlier_size        = input$rcbd_rain_outlier_size %||% 1.5,
        show_points         = input$rcbd_rain_show_points %||% TRUE,
        point_type          = input$rcbd_rain_point_type %||% "jitter",
        point_width         = input$rcbd_rain_point_width %||% 0.05,
        point_size          = input$rcbd_rain_point_size %||% 1,
        point_alpha         = input$rcbd_rain_point_alpha %||% 0.5,
        point_color         = input$rcbd_rain_point_color %||% "black",
        color_palette       = input$rcbd_rain_color_palette %||% "default",
        show_legend         = input$rcbd_rain_show_legend %||% FALSE,
        legend_position     = input$rcbd_rain_legend_position %||% "right",
        theme_choice        = input$rcbd_rain_theme %||% "minimal",
        x_label             = input$rcbd_rain_x_label %||% "",
        y_label             = input$rcbd_rain_y_label %||% "Value",
        title_size          = input$rcbd_rain_title_size %||% 14,
        axis_text_size      = input$rcbd_rain_axis_text_size %||% 10,
        strip_text_size     = input$rcbd_rain_strip_text_size %||% 11,
        strip_bold          = input$rcbd_rain_strip_bold %||% TRUE,
        font_family         = input$rcbd_rain_font_family %||% ""
      )
    })

    output$rcbd_rain_download <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$rcbd_rain_filename), ".", input$rcbd_rain_format)
      },
      content = function(file) {
        req(rcbd_table())
        df <- get_rmtwoway_treatment_df(input$selected_factor_raincloud %||% "A", rcbd_plotdata())
        p <- create_raincloud_plot(
          data = df,
          facet_cols          = as.numeric(input$rcbd_rain_facet_cols %||% 2),
          facet_scales        = input$rcbd_rain_facet_scales %||% "free_y",
          aspect_ratio        = input$rcbd_rain_aspect_ratio %||% 1,
          show_title          = input$rcbd_rain_show_title %||% TRUE,
          title               = input$rcbd_rain_title %||% "Raincloud Plots",
          show_density        = input$rcbd_rain_show_density %||% TRUE,
          density_adjust      = input$rcbd_rain_density_adjust %||% 0.6,
          density_justification = input$rcbd_rain_density_justification %||% -0.3,
          density_alpha       = input$rcbd_rain_density_alpha %||% 0.8,
          density_width       = input$rcbd_rain_density_width %||% 1,
          show_boxplot        = input$rcbd_rain_show_boxplot %||% TRUE,
          box_width           = input$rcbd_rain_box_width %||% 0.1,
          box_alpha           = input$rcbd_rain_box_alpha %||% 0.4,
          show_outliers       = input$rcbd_rain_show_outliers %||% FALSE,
          outlier_size        = input$rcbd_rain_outlier_size %||% 1.5,
          show_points         = input$rcbd_rain_show_points %||% TRUE,
          point_type          = input$rcbd_rain_point_type %||% "jitter",
          point_width         = input$rcbd_rain_point_width %||% 0.05,
          point_size          = input$rcbd_rain_point_size %||% 1,
          point_alpha         = input$rcbd_rain_point_alpha %||% 0.5,
          point_color         = input$rcbd_rain_point_color %||% "black",
          color_palette       = input$rcbd_rain_color_palette %||% "default",
          show_legend         = input$rcbd_rain_show_legend %||% FALSE,
          legend_position     = input$rcbd_rain_legend_position %||% "right",
          theme_choice        = input$rcbd_rain_theme %||% "minimal",
          x_label             = input$rcbd_rain_x_label %||% "",
          y_label             = input$rcbd_rain_y_label %||% "Value",
          title_size          = input$rcbd_rain_title_size %||% 14,
          axis_text_size      = input$rcbd_rain_axis_text_size %||% 10,
          strip_text_size     = input$rcbd_rain_strip_text_size %||% 11,
          strip_bold          = input$rcbd_rain_strip_bold %||% TRUE,
          font_family         = input$rcbd_rain_font_family %||% ""
        )
        ggsave(file, plot = p,
               device = if (identical(input$rcbd_rain_format, "pdf")) cairo_pdf else input$rcbd_rain_format,
               width = input$rcbd_rain_width %||% 10, height = input$rcbd_rain_height %||% 8,
               dpi = input$rcbd_rain_dpi %||% 300, bg = "white")
      }
    )

    ## ~ Advanced Raincloud Plot ------------------------------------------------
    rcbd_raincloud_plot_by_char <- reactive({
      reset_key()
      if (is.null(rcbd_csvfile())) return(NULL)
      req(rcbd_plotdata())
      req(input$rcbd_selvar)

      plot_data     <- rcbd_plotdata()
      factor_choice <- input$selected_factor_adv_raincloud %||% "A"

      long_data_full <- switch(factor_choice,
        "A"   = { d <- plot_data$df_datalongA;   d },
        "B"   = { d <- plot_data$df_datalongB;   names(d)[names(d) == "Day"]           <- "Treatment"; d },
        "AB"  = { d <- plot_data$df_datalongAB;  names(d)[names(d) == "TreatmentxDay"] <- "Treatment"; d },
        {       d <- plot_data$df_datalongA;   d }
      )

      if (!is.null(long_data_full)) {
        character_choices <- unique(long_data_full$character)
        current_selection <- isolate(input$rcbd_rc_variables)
        if (is.null(current_selection) || !current_selection %in% character_choices) {
          updateSelectInput(session, "rcbd_rc_variables", choices = character_choices, selected = character_choices[1])
        } else {
          updateSelectInput(session, "rcbd_rc_variables", choices = character_choices, selected = current_selection)
        }
      }

      req(input$rcbd_rc_variables)
      if (!input$rcbd_rc_variables %in% unique(long_data_full$character)) return(NULL)

      data_long <- long_data_full %>% filter(character == input$rcbd_rc_variables)
      data_long$Treatment <- as.factor(data_long$Treatment)
      treatments <- unique(data_long$Treatment)

      treat_rc_order_mode <- input$rcbd_raincloud_treatment_order %||% "alpha"
      if (treat_rc_order_mode == "data") {
        trt_levels <- unique(as.character(data_long$Treatment))
      } else {
        trt_levels <- gtools::mixedsort(unique(as.character(data_long$Treatment)))
      }
      data_long$Treatment <- factor(as.character(data_long$Treatment), levels = trt_levels)

      if (isTRUE(input$rcbd_rc_custom_colors)) {
        custom_colors <- c(input$rcbd_rc_color1, input$rcbd_rc_color2,
                           input$rcbd_rc_color3, input$rcbd_rc_color4,
                           input$rcbd_rc_color5 %||% "#9467bd",
                           input$rcbd_rc_color6 %||% "#8c564b",
                           input$rcbd_rc_color7 %||% "#e377c2",
                           input$rcbd_rc_color8 %||% "#7f7f7f")
        pal <- custom_colors[1:length(treatments)]
      } else {
        if (input$rcbd_rc_color_palette %in% c("viridis", "plasma", "inferno", "magma")) {
          pal <- viridis::viridis(length(treatments), option = substr(input$rcbd_rc_color_palette, 1, 1))
        } else {
          pal <- tryCatch(
            colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, length(treatments))), input$rcbd_rc_color_palette))(length(treatments)),
            error = function(e) colorRampPalette(RColorBrewer::brewer.pal(min(8, 3), "Set2"))(length(treatments))
          )
        }
      }

      lighten_amount <- input$rcbd_rc_lighten_amount %||% 0.3
      darken_amount  <- input$rcbd_rc_darken_amount  %||% 0.1
      pal_light <- lighten(pal, lighten_amount)
      pal_dark  <- darken(pal,  darken_amount)
      names(pal)       <- trt_levels
      names(pal_light) <- trt_levels
      names(pal_dark)  <- trt_levels

      x_levels <- rev(trt_levels)
      data_long$Treatment <- factor(as.character(data_long$Treatment), levels = x_levels)

      p <- ggplot(data_long, aes(x = Treatment, y = value, color = Treatment, fill = Treatment))

      if (isTRUE(input$rcbd_rc_show_density)) {
        p <- p + ggdist::stat_halfeye(
          adjust = input$rcbd_rc_density_adjust, width = input$rcbd_rc_density_width,
          .width = 0, justification = input$rcbd_rc_density_justification,
          point_color = NA, alpha = input$rcbd_rc_density_alpha
        )
      }

      if (isTRUE(input$rcbd_rc_show_boxplot)) {
        p <- p + geom_boxplot(
          width = input$rcbd_rc_box_width,
          outlier.shape = ifelse(isTRUE(input$rcbd_rc_show_outliers), 19, NA),
          alpha = input$rcbd_rc_box_alpha,
          outlier.size = ifelse(isTRUE(input$rcbd_rc_show_outliers), 1.5, 0),
          fill = NA
        )
      }

      if (isTRUE(input$rcbd_rc_show_points)) {
        p <- p + geom_point(
          fill = input$rcbd_rc_point_fill, shape = 21, stroke = input$rcbd_rc_point_stroke,
          size = input$rcbd_rc_point_size, alpha = input$rcbd_rc_point_alpha,
          position = position_jitter(seed = input$rcbd_rc_jitter_seed, width = input$rcbd_rc_jitter_width)
        )
      }

      if (isTRUE(input$rcbd_rc_show_median)) {
        p <- p + stat_summary(
          fun = median,
          aes(label = round(after_stat(y), input$rcbd_rc_median_digits)),
          geom = "text", vjust = input$rcbd_rc_median_vjust,
          size = input$rcbd_rc_median_size, show.legend = FALSE
        )
      }

      if (isTRUE(input$rcbd_rc_coord_flip)) {
        p <- p + coord_flip(xlim = c(input$rcbd_rc_xlim_start, NA), clip = "off")
      }

      p <- p +
        scale_color_manual(values = pal_dark,  guide = "none") +
        scale_fill_manual( values = pal_light, guide = "none")

      title_text <- if (isTRUE(input$rcbd_rc_show_title)) {
        if (!is.null(input$rcbd_rc_custom_title) && input$rcbd_rc_custom_title != "")
          input$rcbd_rc_custom_title
        else
          paste("Raincloud Plot of", input$rcbd_rc_variables, "by Treatment")
      } else { NULL }

      x_label <- if (!is.null(input$rcbd_rc_x_label) && input$rcbd_rc_x_label != "") input$rcbd_rc_x_label else NULL
      y_label <- if (!is.null(input$rcbd_rc_y_label) && input$rcbd_rc_y_label != "") input$rcbd_rc_y_label else input$rcbd_rc_variables

      p <- p + labs(x = x_label, y = y_label, title = title_text)

      base_theme <- switch(input$rcbd_rc_theme %||% "minimal",
        "minimal"  = theme_minimal, "classic"  = theme_classic, "light" = theme_light,
        "dark"     = theme_dark,    "void"     = theme_void,    "bw"    = theme_bw,
        "linedraw" = theme_linedraw, theme_minimal)

      p <- p + base_theme(base_family = input$rcbd_rc_font_family %||% "",
                          base_size   = input$rcbd_rc_base_size %||% 14)

      p <- p + theme(
        panel.grid.minor   = if (isTRUE(input$rcbd_rc_show_minor_grid))  element_line() else element_blank(),
        panel.grid.major.y = if (isTRUE(input$rcbd_rc_show_major_y_grid)) element_line() else element_blank(),
        axis.ticks         = if (isTRUE(input$rcbd_rc_show_axis_ticks))  element_line() else element_blank(),
        axis.text.x = if (isTRUE(input$rcbd_rc_show_x_text)) element_text(size = input$rcbd_rc_axis_text_size) else element_blank(),
        axis.text.y = if (isTRUE(input$rcbd_rc_show_y_text)) {
          if (isTRUE(input$rcbd_rc_axis_text_colored) && isTRUE(input$rcbd_rc_coord_flip))
            element_text(color = rev(pal_dark), size = input$rcbd_rc_axis_text_size)
          else
            element_text(size = input$rcbd_rc_axis_text_size)
        } else element_blank(),
        plot.title = if (isTRUE(input$rcbd_rc_show_title))
          element_text(face = if (isTRUE(input$rcbd_rc_title_bold)) "bold" else "plain",
                       size = input$rcbd_rc_title_size %||% 18)
        else element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(input$rcbd_rc_top_margin %||% 15, input$rcbd_rc_right_margin %||% 15,
                             input$rcbd_rc_bottom_margin %||% 10, input$rcbd_rc_left_margin %||% 15)
      )

      if (isTRUE(input$rcbd_raincloud_clean_labels)) {
        p <- p + scale_x_discrete(labels = function(x) rcbd_clean_label(x))
      }

      return(p)
    })

    output$rcbd_raincloud_by_char_plot <- renderPlot({
      rcbd_raincloud_plot_by_char()
    }, height = function() { input$rcbd_rc_plot_height %||% 600 })

    output$rcbd_rc_download <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$rcbd_rc_filename), ".", input$rcbd_rc_file_format)
      },
      content = function(file) {
        plot <- rcbd_raincloud_plot_by_char()
        if (!is.null(plot)) {
          ggsave(file, plot = plot,
                 device = if (identical(input$rcbd_rc_file_format, "pdf")) cairo_pdf else input$rcbd_rc_file_format,
                 width = input$rcbd_rc_width %||% 12, height = input$rcbd_rc_height %||% 8,
                 dpi = input$rcbd_rc_dpi %||% 300, bg = "white")
        }
      }
    )

    ## ~ Circular Bar Plot ------------------------------------------------------
    circular_bar_plot <- function(data, id_col, scale_data = TRUE, empty_bar = NULL,
                                  plot_size = 12, use_means = TRUE, alpha = 0.7,
                                  grid_color = "grey", grid_size = 0.3, label_size = 2.2,
                                  group_label_size = 3.5, grid_text_size = 2.5,
                                  show_labels = TRUE, show_legend = TRUE, show_grid = TRUE,
                                  grid_levels = c(0.25, 0.5, 0.75, 1.0), dpi = 300) {

      id_col <- rlang::ensym(id_col)
      numeric_data <- data %>% select(!!id_col, where(is.numeric))

      if (use_means) {
        numeric_data <- numeric_data %>%
          group_by(!!id_col) %>%
          summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
      }

      if (is.null(empty_bar) || is.na(empty_bar)) {
        n_traits  <- ncol(numeric_data) - 1
        empty_bar <- max(n_traits - 1, 0)
      } else {
        empty_bar <- max(as.numeric(empty_bar), 0)
      }

      if (scale_data) {
        numeric_data <- numeric_data %>%
          mutate(across(-!!id_col, \(x) (x - min(x, na.rm = TRUE)) /
                          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
      }

      long_data <- numeric_data %>%
        pivot_longer(-!!id_col, names_to = "Trait", values_to = "value") %>%
        rename(individual = !!id_col)

      if (isTRUE(input$rcbd_circular_clean_labels %||% FALSE)) {
        long_data$individual <- rcbd_clean_label(long_data$individual)
      }

      treat_circular_order_mode <- input$rcbd_circular_treatment_order %||% "alpha"
      if (treat_circular_order_mode == "data") {
        trt_levels <- unique(as.character(long_data$individual))
      } else {
        trt_levels <- gtools::mixedsort(unique(as.character(long_data$individual)))
      }
      long_data$individual <- factor(as.character(long_data$individual), levels = trt_levels)
      long_data <- long_data %>% arrange(individual, Trait)

      traits    <- unique(long_data$Trait)
      codes     <- LETTERS[1:length(traits)]
      trait_map <- setNames(codes, traits)
      long_data$group <- trait_map[long_data$Trait]

      if (empty_bar > 0) {
        n_empty_rows <- empty_bar * length(codes)
        if (n_empty_rows > 0 && !is.na(n_empty_rows)) {
          to_add <- data.frame(matrix(NA, n_empty_rows, ncol(long_data)))
          colnames(to_add) <- colnames(long_data)
          to_add$group <- rep(codes, each = empty_bar)
          long_data <- bind_rows(long_data, to_add) %>% arrange(group)
        }
      }
      long_data$id <- seq_len(nrow(long_data))

      number_of_bar <- nrow(long_data)
      angle <- 90 - 360 * (long_data$id - 0.5) / number_of_bar
      long_data$hjust <- ifelse(angle < -90, 1, 0)
      long_data$angle <- ifelse(angle < -90, angle + 180, angle)

      base_data <- long_data %>%
        group_by(group) %>%
        summarize(start = min(id), end = max(id) - empty_bar, .groups = "drop") %>%
        rowwise() %>%
        mutate(title = mean(c(start, end)))

      ## ##arshi(20-08-26) one-way RM - `group` is the response variable, so a
      ## single-response analysis gives base_data ONE row. The between-group
      ## shift below then indexed end[c(1, 1:0)] -> length 2 into a 1-row tibble
      ## ("Can't recycle input of size 2 to size 1"). Only shift when there are
      ## 2+ groups; with one group the base arcs are used as-is.
      grid_data       <- base_data
      if (nrow(grid_data) > 1) {
        grid_data$end   <- grid_data$end[c(nrow(grid_data), 1:(nrow(grid_data) - 1))] + 1
        grid_data$start <- grid_data$start - 1
        grid_data       <- grid_data[-1, ]
      }

      p <- ggplot(long_data, aes(x = as.factor(id), y = value, fill = group)) +
        geom_bar(stat = "identity", alpha = alpha) +
        ylim(-0.2, 1.2) +
        coord_polar() +
        theme_minimal() +
        theme(
          text           = element_text(family = input$rcbd_circular_font_family %||% ""),
          legend.position = "none",
          axis.text      = element_blank(),
          axis.title     = element_blank(),
          panel.grid     = element_blank(),
          plot.margin    = margin(5, 5, 10, 5)
        )

      if (show_grid) {
        for (level in grid_levels) {
          p <- p + geom_segment(data = grid_data,
                                aes(x = end, y = !!level, xend = start, yend = !!level),
                                color = grid_color, linewidth = grid_size, inherit.aes = FALSE)
        }
        p <- p + annotate("text", x = rep(max(long_data$id), length(grid_levels)), y = grid_levels,
                          label = as.character(grid_levels), color = grid_color,
                          size = grid_text_size, fontface = "bold", hjust = 1)
      }

      if (show_labels) {
        p <- p + geom_text(aes(x = id, y = value + 0.05, label = individual, hjust = hjust),
                           color = "black", fontface = "bold", alpha = 0.7, size = label_size,
                           angle = long_data$angle, inherit.aes = FALSE)
      }

      p <- p +
        geom_segment(data = base_data, aes(x = start, y = -0.05, xend = end, yend = -0.05),
                     color = "black", linewidth = 0.6, inherit.aes = FALSE) +
        geom_text(data = base_data, aes(x = title, y = -0.1, label = group),
                  hjust = 0.5, color = "black", size = group_label_size, fontface = "bold", inherit.aes = FALSE)

      if (show_legend) {
        legend_text <- paste(paste0(codes, ": ", traits), collapse = "   •   ")
        p <- p +
          labs(subtitle = if (scale_data) "Note: Values have been min-max normalized to enable visual comparison across traits." else NULL,
               caption  = legend_text) +
          theme(
            plot.subtitle = element_text(size = 11, hjust = 0, face = "italic", margin = margin(b = 8)),
            plot.caption  = element_text(size = 10.5, hjust = 1, margin = margin(t = 12))
          )
      }

      return(p)
    }

    output$rcbd_circular_bar_plot <- renderPlot({
      req(rcbd_plotdata(), rcbd_table())
      df <- get_rmtwoway_treatment_df(input$selected_factor_circular %||% "A", rcbd_plotdata())
      req("Treatment" %in% names(df))
      grid_levels   <- if (is.null(input$rcbd_circ_grid_levels)) c(0.25, 0.5, 0.75, 1.0) else as.numeric(input$rcbd_circ_grid_levels)
      empty_bar_val <- if (is.null(input$rcbd_circ_empty_bar) || is.na(input$rcbd_circ_empty_bar)) NULL else as.numeric(input$rcbd_circ_empty_bar)
      circular_bar_plot(
        data = df, id_col = Treatment,
        scale_data       = input$rcbd_circ_scale_data %||% TRUE,
        use_means        = input$rcbd_circ_use_means %||% TRUE,
        empty_bar        = empty_bar_val,
        alpha            = input$rcbd_circ_alpha %||% 0.7,
        grid_color       = input$rcbd_circ_grid_color %||% "grey",
        grid_size        = input$rcbd_circ_grid_size %||% 0.3,
        label_size       = input$rcbd_circ_label_size %||% 2.2,
        group_label_size = input$rcbd_circ_group_label_size %||% 3.5,
        grid_text_size   = input$rcbd_circ_grid_text_size %||% 2.5,
        show_labels      = input$rcbd_circ_show_labels %||% TRUE,
        show_legend      = input$rcbd_circ_show_legend %||% TRUE,
        show_grid        = input$rcbd_circ_show_grid %||% TRUE,
        grid_levels      = grid_levels
      )
    })

    output$rcbd_circ_download <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$rcbd_circ_filename), ".", input$rcbd_circular_format)
      },
      content = function(file) {
        req(rcbd_table())
        df <- get_rmtwoway_treatment_df(input$selected_factor_circular %||% "A", rcbd_plotdata())
        req("Treatment" %in% names(df))
        grid_levels   <- if (is.null(input$rcbd_circ_grid_levels)) c(0.25, 0.5, 0.75, 1.0) else as.numeric(input$rcbd_circ_grid_levels)
        empty_bar_val <- if (is.null(input$rcbd_circ_empty_bar) || is.na(input$rcbd_circ_empty_bar)) NULL else as.numeric(input$rcbd_circ_empty_bar)
        p <- circular_bar_plot(
          data = df, id_col = Treatment,
          scale_data       = input$rcbd_circ_scale_data %||% TRUE,
          use_means        = input$rcbd_circ_use_means %||% TRUE,
          empty_bar        = empty_bar_val,
          alpha            = input$rcbd_circ_alpha %||% 0.7,
          grid_color       = input$rcbd_circ_grid_color %||% "grey",
          grid_size        = input$rcbd_circ_grid_size %||% 0.3,
          label_size       = input$rcbd_circ_label_size %||% 2.2,
          group_label_size = input$rcbd_circ_group_label_size %||% 3.5,
          grid_text_size   = input$rcbd_circ_grid_text_size %||% 2.5,
          show_labels      = input$rcbd_circ_show_labels %||% TRUE,
          show_legend      = input$rcbd_circ_show_legend %||% TRUE,
          show_grid        = input$rcbd_circ_show_grid %||% TRUE,
          grid_levels      = grid_levels,
          plot_size        = input$rcbd_circ_plot_size %||% 12,
          dpi              = input$rcbd_circ_dpi %||% 300
        )
        ggsave(file, plot = p,
               device = if (identical(input$rcbd_circular_format, "pdf")) cairo_pdf else input$rcbd_circular_format,
               width = input$rcbd_circ_plot_size %||% 12,
               height = input$rcbd_circ_plot_size %||% 12,
               dpi = input$rcbd_circ_dpi %||% 300,
               units = "in", bg = "white")
      }
    )

    ## ~ QQ Plot ----------------------------------------------------------------
    output$rcbd_qq_plot_container <- renderUI({
      req(input$rcbd_qq_plot_height)
      plotOutput("rcbd_qq_plot_main",
                 height = paste0(input$rcbd_qq_plot_height, "px"),
                 width  = "100%") %>%
        withSpinner(
          image        = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fplotting.gif?alt=media",
          image.width  = 400,
          image.height = 400
        )
    })

    create_qq_plot <- reactive({
      reset_key()
      if (is.null(rcbd_csvfile())) return(NULL)
      req(rcbd_plotdata(), rcbd_table())

      ## ##arshi(20-08-26) one-way RM - trans_data (rcbd_df) has ONLY a "Day"
      ## (the single within-subject factor) and "Subject" column, plus the
      ## responses. The two-way version looked for "Treatment"/"Block"/
      ## "TreatmentxDay" columns that don't exist here, so A/Subj came out
      ## length-0 and lm() failed -> "Could not compute model residuals".
      ## The within-subject residuals of a one-way RM come from the additive
      ## model  y ~ Day + Subject  (treatment factor + subject stratum).
      rcbd_data <- rcbd_table()$trans_data
      req(!is.null(rcbd_data))

      factor_cols <- c("Day", "Subject")
      Trt  <- factor(rcbd_data[["Day"]])
      Subj <- factor(rcbd_data[["Subject"]])

      response_cols <- setdiff(names(rcbd_data), factor_cols)
      response_cols <- response_cols[vapply(rcbd_data[response_cols], is.numeric, logical(1))]

      if (length(response_cols) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "No response variables found", size = 6) +
                 theme_void())
      }

      residuals_list <- lapply(response_cols, function(col) {
        y <- as.numeric(rcbd_data[[col]])
        tryCatch({
          m <- lm(y ~ Trt + Subj)
          data.frame(Variable = col, Residual = residuals(m))
        }, error = function(e) NULL)
      })
      residuals_list <- Filter(Negate(is.null), residuals_list)

      if (length(residuals_list) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "Could not compute model residuals", size = 6) +
                 theme_void())
      }

      plot_df <- do.call(rbind, residuals_list)

      # Update variable selector
      current_selection <- isolate(input$rcbd_qq_variable)
      if (is.null(current_selection) || !current_selection %in% response_cols) {
        updateSelectInput(session, "rcbd_qq_variable",
                          choices  = setNames(response_cols, response_cols),
                          selected = response_cols[1])
      } else {
        updateSelectInput(session, "rcbd_qq_variable",
                          choices  = setNames(response_cols, response_cols),
                          selected = current_selection)
      }

      # Normality-test helper
      perform_normality_tests <- function(data, test_types) {
        results    <- list()
        clean_data <- data[is.finite(data) & !is.na(data)]
        n          <- length(clean_data)
        if (n == 0)                        return("No valid data points")
        if (n > 1 && var(clean_data) == 0) return("No variance in data")
        if ("shapiro" %in% test_types && n >= 3 && n <= 5000) {
          tryCatch({ sw <- shapiro.test(clean_data)
                     results$shapiro <- paste0("SW: p=", round(sw$p.value, 4)) },
                   error = function(e) { results$shapiro <<- "SW: test failed" })
        }
        if ("ad" %in% test_types && requireNamespace("nortest", quietly = TRUE)) {
          if (n >= 7 && var(clean_data) > 0) {
            tryCatch({ ad <- nortest::ad.test(clean_data)
                       results$ad <- paste0("AD: p=", round(ad$p.value, 4)) },
                     error = function(e) { results$ad <<- "AD: failed" })
          } else {
            results$ad <- if (n < 7) paste0("AD: need >7 obs (have ", n, ")") else "AD: no variance"
          }
        }
        if ("ks" %in% test_types && n >= 2 && var(clean_data) > 0) {
          tryCatch({ ks <- ks.test(clean_data, "pnorm", mean(clean_data), sd(clean_data))
                     results$ks <- paste0("KS: p=", round(ks$p.value, 4)) },
                   error = function(e) { results$ks <<- "KS: test failed" })
        }
        if (length(results) == 0) return("No tests could be performed")
        return(paste(unlist(results), collapse = "\n"))
      }

      # Subset to single variable if requested
      plot_type  <- input$rcbd_qq_plot_type %||% "all_individual"
      show_tests <- input$rcbd_qq_show_tests %||% FALSE

      if (plot_type == "single") {
        selected_var <- input$rcbd_qq_variable
        if (is.null(selected_var) || !selected_var %in% plot_df$Variable) {
          return(ggplot() +
                   annotate("text", x = 0.5, y = 0.5, label = "Selected variable not found", size = 6) +
                   theme_void())
        }
        plot_data <- plot_df[plot_df$Variable == selected_var, ]
      } else {
        plot_data <- plot_df
      }

      # Normality tests on residuals per variable
      if (show_tests && !is.null(input$rcbd_qq_test_types)) {
        test_results <- plot_data %>%
          group_by(Variable) %>%
          summarise(test_result = perform_normality_tests(Residual, input$rcbd_qq_test_types),
                    .groups = "drop")
        plot_data <- plot_data %>% left_join(test_results, by = "Variable")
      }

      # Build plot
      p <- ggplot(plot_data, aes(sample = Residual)) +
        geom_qq_line(linewidth = input$rcbd_qq_line_size %||% 0.8) +
        geom_qq(size = input$rcbd_qq_point_size %||% 1.2,
                alpha = input$rcbd_qq_point_alpha %||% 0.7)

      # Normality test annotations
      if (show_tests && "test_result" %in% names(plot_data)) {
        test_size <- input$rcbd_qq_test_text_size %||% 3
        test_pos  <- input$rcbd_qq_test_position  %||% "tl"
        pos_map   <- list(
          "tl" = list(x = -Inf, y =  Inf, hjust = -0.1, vjust =  1.1),
          "tr" = list(x =  Inf, y =  Inf, hjust =  1.1, vjust =  1.1),
          "bl" = list(x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.1),
          "br" = list(x =  Inf, y = -Inf, hjust =  1.1, vjust = -0.1)
        )
        pos       <- pos_map[[test_pos]]
        anno_data <- plot_data %>%
          group_by(Variable) %>%
          summarise(test_result = first(test_result), .groups = "drop")
        p <- p + geom_text(
          data        = anno_data,
          aes(x = pos$x, y = pos$y, label = test_result),
          hjust       = pos$hjust, vjust = pos$vjust,
          size        = test_size, inherit.aes = FALSE, family = "mono"
        )
      }

      # Faceting (only when showing all variables)
      free_scales  <- input$rcbd_qq_free_scales %||% TRUE
      ncol_val     <- input$rcbd_qq_ncol        %||% 2
      scales_param <- if (free_scales) "free" else "fixed"

      if (plot_type != "single") {
        p <- p + facet_wrap(~Variable, scales = scales_param, ncol = ncol_val)
      }

      # Labels
      show_subtitle <- input$rcbd_qq_show_subtitle %||% TRUE
      p <- p + labs(
        title    = input$rcbd_qq_main_title %||% "QQ Plot - Residual Normality Check",
        subtitle = if (show_subtitle)
          input$rcbd_qq_subtitle %||% "Within-subject residuals from Day + Subject: points should follow the diagonal line"
          else NULL,
        x = "Theoretical Quantiles (Normal Distribution)",
        y = "Sample Residuals"
      )

      # Theme
      theme_name <- input$rcbd_qq_theme %||% "minimal"
      p <- switch(theme_name,
        "minimal"   = p + theme_minimal(),
        "classic"   = p + theme_classic(),
        "dark"      = p + theme_dark(),
        "light"     = p + theme_light(),
        "void"      = p + theme_void(),
        "economist" = if (requireNamespace("ggthemes", quietly = TRUE)) p + ggthemes::theme_economist() else p + theme_minimal(),
        "tufte"     = if (requireNamespace("ggthemes", quietly = TRUE)) p + ggthemes::theme_tufte()    else p + theme_minimal(),
        p + theme_minimal()
      )

      title_size <- input$rcbd_qq_title_size %||% 16
      text_size  <- input$rcbd_qq_text_size  %||% 10
      strip_size <- input$rcbd_qq_strip_size %||% 10

      p <- p + theme(
        text          = element_text(family = input$rcbd_qq_font_family %||% ""),
        plot.title    = element_text(hjust = 0.5, size = title_size, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = title_size - 2),
        axis.text     = element_text(size = text_size),
        axis.title    = element_text(size = text_size + 1),
        strip.text    = element_text(size = strip_size, face = "bold"),
        legend.position = "none",
        panel.spacing = unit(0.5, "cm")
      )

      return(p)
    })

    output$rcbd_qq_plot_main <- renderPlot({ create_qq_plot() })

    output$rcbd_qq_download <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$rcbd_qq_filename), ".", input$rcbd_qq_format)
      },
      content = function(file) {
        p <- create_qq_plot()
        ggsave(file, plot = p,
               device = if (identical(input$rcbd_qq_format, "pdf")) cairo_pdf else input$rcbd_qq_format,
               width  = input$rcbd_qq_export_width  %||% 12,
               height = input$rcbd_qq_export_height %||% 12,
               dpi    = input$rcbd_qq_dpi %||% 300,
               units  = "in", bg = "white")
      }
    )
    
    
    output$instr_pca <- renderUI({
      req(rcbd_csvfile()) 
      req(rcbd_pca())   
      list(
        actionBttn(
          inputId = "instr_rcbd_pca",
          label = " Give more details on PCA", 
          style = "stretch",
          color = "success",
          size = "sm"
        )
      )
    })
    
    ## ---- RA-One: main results observer (INSIDE the submit observeEvent) ----
    ##(updation March)
    observe({
      req(reactiveValSubmit())
      req(rcbd_table())

      table_data <- rcbd_table()
      plot_data  <- tryCatch(rcbd_plotdata(), error = function(e) NULL)

      # Correlation matrix over the selected response columns
      data_corr <- tryCatch(
        table_data$rcbd_df1[, table_data$rcbd_char, drop = FALSE],
        error = function(e) NULL
      )
      corr_matrix <- NULL
      if (!is.null(data_corr) && is.data.frame(data_corr) && ncol(data_corr) >= 2) {
        rv <- isolate(if (is.null(input$rcbd_digit)) 2 else as.numeric(input$rcbd_digit))
        if (is.na(rv)) rv <- 2
        corr_matrix <- tryCatch(
          as.data.frame(round(stats::cor(data_corr, use = "pairwise.complete.obs"), rv)),
          error = function(e) NULL
        )
      }

      combined_results <- list(
        # -- Experiment metadata --------------------------------------------
        no_trt                = table_data$no_trt,
        r                     = table_data$r,
        all_equal             = table_data$all_equal,
        org_Treatment         = table_data$org_Treatment,
        org_Day               = table_data$org_Day,
        org_Subject           = table_data$org_Subject,
        t_levels              = table_data$levelnamesA,
        levelnamesA           = table_data$levelnamesA,

        # -- Combined tables + ANOVA ----------------------------------------
        df1                   = table_data$df1,
        df4                   = table_data$df4,

        # -- Letter grouping -------------------------------------------------
        rcbd_groups           = table_data$rcbd_groups,
        allow_letter_grouping = !is.null(table_data$rcbd_groups),

        # -- Estimated marginal means ----------------------------------------
        rcbd_trt_means        = table_data$emm_meanA,
        emm_meanA             = table_data$emm_meanA,
        emm_summaryA          = table_data$emm_summaryA,

        # -- Significance ------------------------------------------------------
        rcbd_char             = table_data$rcbd_char,
        resp_cols             = table_data$rcbd_char,
        rcbd_star             = table_data$rcbd_star,
        rcbd_pval             = table_data$rcbd_pvalA,
        rcbd_pvalA            = table_data$rcbd_pvalA,

        # -- Data ---------------------------------------------------------------
        trans_data            = table_data$trans_data,
        rcbd_df1              = table_data$rcbd_df1,
        data_corr             = data_corr,
        rcbd_correlation_matrix = corr_matrix,
        ## The AI plot engine groups by a column literally named `Treatment`
        ## (its x-axis). In this one-way RM design the plotted factor is `Day`,
        ## so publish raw_df built from the numeric analysis data with
        ## Treatment := Day. Without this the plot engine's group_by(Treatment)
        ## fails and crd/rcbd_ai_make_plot() silently returns NULL (no plot).
        raw_df                = tryCatch({
          d <- table_data$trans_data
          if (is.data.frame(d) && "Day" %in% names(d)) {
            d$Treatment <- as.character(d$Day)
          }
          d
        }, error = function(e) tryCatch(rcbd_csvfile(), error = function(e2) NULL)),

        # -- Plot-ready data (from rcbd_plotdata) -------------------------------
        df_summaryA           = if (!is.null(plot_data)) plot_data$df_summaryA   else NULL,
        df_datalongA          = if (!is.null(plot_data)) plot_data$df_datalongA  else NULL,
        trt_noA               = if (!is.null(plot_data)) plot_data$trt_noA       else NULL,

        # -- User selections -----------------------------------------------------
        sel_subject           = isolate(input$rcbd_Subject),
        sel_day               = isolate(input$rcbd_Day),
        sel_selvar            = isolate(input$rcbd_selvar),
        sel_mcp               = isolate(input$rcbd_mcp),
        sel_level             = isolate(input$rcbd_level),
        sel_correction        = isolate(input$correction_method),
        sel_digit             = isolate(input$rcbd_digit)
      )

      rcbd_results_store(combined_results)
    })

    ## ---- RA-One: append individual ANOVA once the user runs it ----
    ## indv_anova_results() is keyed BY RESPONSE VARIABLE
    ## (res[["Ca"]]$ANOVA, ...), so store the bundle whole -- do not destructure.
    observe({
      res <- tryCatch(indv_anova_results(), error = function(e) NULL)
      if (is.null(res)) return()
      cur <- isolate(rcbd_results_store())
      if (is.null(cur)) return()
      cur$individual_anova <- res
      rcbd_results_store(cur)
    })

    ## ---- RA-One: append PCA results once the user clicks the PCA button ----
    observe({
      res <- tryCatch(rcbd_pca(), error = function(e) NULL)
      if (is.null(res)) return()
      cur <- isolate(rcbd_results_store())
      if (is.null(cur)) return()
      cur$rcbd_pca_meanData       <- res$rcbd_meanData
      cur$rcbd_pca_eigen          <- res$eigen_value
      cur$rcbd_pca_loadings       <- res$loadings
      cur$rcbd_pca_index1         <- res$index_1
      cur$rcbd_pca_index2         <- res$index_2
      cur$rcbd_pca_e1             <- res$e1
      cur$rcbd_pca_e2             <- res$e2
      cur$rcbd_pca_abt_eigen      <- res$abt_eigen_table
      cur$rcbd_pca_abt_loadings   <- res$abt_loadings
      cur$rcbd_pca_abt_indexscore <- res$abt_indexscore
      rcbd_results_store(cur)
    })
    
    
    

  # Presentation and descriptive summaries only; fitted models retain their own data.
  doe_render_report <- doe_report_renderer(input, "rcbd", normalizePath(".", winslash = "/"), exact = reactive({ rcbd_table()$doe_exact_tables }))
  doe_presentation_server(input, output, session, "rcbd",
    exact = reactive({ rcbd_table()$doe_exact_tables }),
    data = reactive({ rcbd_table()$rcbd_df1 }),
    groups = reactive({ c("Day") }),
    variables = reactive({ make.names(input$rcbd_selvar) }),
    log_data = reactive({ rcbd_csvfile() }),
    has_style = FALSE, has_log = FALSE, letters = TRUE, paren = TRUE)
  }) #observe event end for submit file
  
  ## Info popup for the Sphericity Correction input
  observeEvent(input$info_sphericity, {
    showModal(modalDialog(
      title = "Sphericity Correction",
      HTML(
        "<div style='font-family:sans-serif; font-size:14px; line-height:1.6; color:#333;'>
            <p><b>What is sphericity?</b> In a repeated-measures ANOVA, <i>sphericity</i> is the
            assumption that the variances of the differences between every pair of within-subject
            (time / Day) levels are equal. It applies only to within-subject effects that have
            <b>three or more</b> levels (here: Day and Treatment&times;Day).</p>

            <p><b>Why a correction is needed.</b> When sphericity is violated (Mauchly's test is
            significant), the ordinary within-subject F-test becomes too liberal - it rejects
            the null too often. A correction multiplies the degrees of freedom by an estimated factor
            &epsilon; (epsilon, 0 &lt; &epsilon; &le; 1), making the F-test more conservative and its
            p-value valid.</p>

            <p><b>Methods available</b></p>
            <ul>
              <li><b>None</b> - no adjustment; assumes sphericity holds. Appropriate when
              Mauchly's test is <i>not</i> significant.</li>
              <li><b>Greenhouse&ndash;Geisser (GG)</b> - estimates &epsilon; and reduces the
              degrees of freedom. The more conservative option; recommended when sphericity is
              clearly violated (especially when &epsilon; &lt; 0.75).</li>
              <li><b>Huynh&ndash;Feldt (HF)</b> - a less conservative estimate of &epsilon;
              (capped at 1). Recommended when the violation is mild (&epsilon; &ge; 0.75), where GG
              can over-correct.</li>
            </ul>

            <p style='color:#666;'>The chosen correction is applied to the within-subject F-tests in
            the ANOVA table. It does not affect the between-subject effect (Treatment).</p>
          </div>"
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  observeEvent(input$instr_rcbd_pca, {
    showModal(
      modalDialog(
        title = "PCA based index scores",
        tags$iframe(
          src = "pcaindexscore.html",  # Ensure this file is in the www directory
          style = "width:100%; height:500px; border:none;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  active_plot <- reactiveVal("rcbd_box")
  observeEvent(input$show_rcbd_box,   { active_plot("rcbd_box") })
  observeEvent(input$show_rcbd_violin,{ active_plot("rcbd_violin") })
  observeEvent(input$show_rcbd_line,  { active_plot("rcbd_line") })
  observeEvent(input$show_rcbd_linecon,{ active_plot("rcbd_linecon") })
  observeEvent(input$show_rcbd_bar,{ active_plot("rcbd_bar") })
  
  output$activePlot <- reactive(active_plot())
  outputOptions(output, "activePlot", suspendWhenHidden = FALSE)
  
  
  
  active_plot1 <- reactiveVal("rcbd_summary")
  observeEvent(input$show_summary_plot,   { active_plot1("rcbd_summary") })
  observeEvent(input$show_raincloud_plot,{ active_plot1("rcbd_raincloud2") })
  observeEvent(input$show_adv_raincloud_plot,{ active_plot1("rcbd_raincloud") })
  observeEvent(input$show_circular_plot,  { active_plot1("rcbd_circular") })
  observeEvent(input$show_qq_plot,  { active_plot1("rcbd_qq") })
  observeEvent(input$show_pair_corrplot,  { active_plot1("rcbd_pair") })
  observeEvent(input$show_distribution_plot,  { active_plot1("rcbd_dist") })
  
  output$activePlot1 <- reactive(active_plot1())
  outputOptions(output, "activePlot1", suspendWhenHidden = FALSE)
  
  
  #RA-One bubble click handler----
  observeEvent(input$raone_bubble_click, {
    req(input$raone_bubble_click)
    showTab("tabs", "RA-One")
    updateTabsetPanel(session, inputId = "tabs", selected = "RA-One")
  })
  
  
  
  plan_label <- reactive({
    creds <- shared$credentials_rv
    usr   <- tryCatch(current_subscriber(), error = function(e) NULL)
    if (is.null(creds) || is.null(usr)) return(NULL)
    row <- creds[creds$user == usr, ]
    if (nrow(row) == 0) return(NULL)
    as.character(row$comment[1])
  })
  
  
  
  
  ## Chat plugin(updation March) ----
  #### Changed by Claude AI 
  chat_server(
    id      = "rcbd_chat",
    context = rcbd_chat_context(
      results_rv      = rcbd_results_store,
      submit_rv       = reactiveValSubmit,
      file_rv         = rcbd_csvfile,
      treatment_input = reactive(input$rcbd_Day),   # the single within-subject factor
      selvar_input    = reactive(input$rcbd_selvar),
      mcp_input       = reactive(input$rcbd_mcp),
      level_input     = reactive(input$rcbd_level),
      padj_input      = reactive(input$correction_method),
      digit_input     = reactive(input$rcbd_digit)   # user's decimals -> RA-One tables
    ),
    user_rv        = current_subscriber,
    plan_rv        = plan_label,
    email_rv       = current_subscriber,
    parent_session = session,
    results_rv     = rcbd_results_store
  )
  
  
  
  


}
