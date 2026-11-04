library(shinymanager)

iconButtonUI <- function(id, img, label) {
  div(
    style = "text-align:center;",
    actionButton(
      id,
      label = HTML(sprintf(
        '<img src="%s" style="width:90%%; height:90%%;
         object-fit:contain; border-radius:50%%;
         background-color:#fef7ea; padding:10px;">',
        img
      )),
      style = "width:100px; height:100px; border-radius:50%;
               background:linear-gradient(135deg,#e29e3d,#f7d59c);
               border:none; box-shadow:0 2px 5px rgba(0,0,0,0.2); padding:0;"
    ),
    tags$div(label, style = "font-weight:bold; margin-top:8px;")
  )
}


FONT_CHOICES <- c(
  "Default" = "", "Times New Roman" = "Times New Roman", "Arial" = "Arial",
  "Calibri" = "Calibri", "Cambria" = "Cambria", "Georgia" = "Georgia",
  "Garamond" = "Garamond", "Palatino Linotype" = "Palatino Linotype",
  "Courier New" = "Courier New", "Tahoma" = "Tahoma", "Verdana" = "Verdana",
  "Serif" = "serif", "Sans" = "sans", "Helvetica" = "Helvetica"
)

rcbd_ui <- function() {
  fluidPage(
    titlePanel(tags$div(tags$b("Repeated Measures ANOVA One Way", style = "color:#000000"))),
    # Dynamic login page----
    useShinyjs(), # Initialize shinyjs
    introjsUI(),
    # Shared login card (auth_ui + styled remember-me + forgot password): greyskull.
    # Logo/background come from the app config, matching CRD/RBD exactly.
    raisins_login_card(),
    # fab_button(
    #   list(
    #     inputId = "rcbd_logout",
    #     icon = icon("sign-out"),
    #     label = NULL
    #   ),
    #   position = "top-right",
    #   animation = "zoomin",
    #   toggle = "hover",
    #   inputId = NULL
    # ),
    ############################### suppress warning messages
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    ############################################# suppress warning
    ################ centering tabset panel
    tags$head(
      
      # CSS  ##(updation March)
      tags$style(HTML("

.plot-container {
  background: white;
  border-radius: 12px;
  padding: 20px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  margin-bottom: 20px;
  position: relative;
}
.plot-header {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;
  font-weight: 700;
  text-align: center;
  margin-bottom: 15px;
}
.left-dropdown-container { margin-bottom: 10px; }
.download-btn {
  background: linear-gradient(135deg, #667eea, #764ba2);
  color: white; border: none; border-radius: 8px;
  padding: 8px 20px; font-weight: 600;
}

ul.nav-pills{
  display: flex !important;
  justify-content: center !important;
}

/* pickerInput highlight */
.brand-picker1.violet-neon-highlight button.dropdown-toggle,
.brand-picker2.violet-neon-highlight button.dropdown-toggle,
.brand-picker3.violet-neon-highlight button.dropdown-toggle,
.brand-picker4.violet-neon-highlight button.dropdown-toggle {
  background-color: #b8fffa;
  color: black;
  border: 2px solid #a855f7 !important;

  box-shadow:
    0 0 0 3px #7c3aed,
    0 0 18px 6px rgba(139,92,246,0.7),
    0 0 40px 14px rgba(167,139,250,0.45) !important;

  animation: violet-neon-pulse 1s infinite alternate;
}

/* Run Analysis button highlight */
#rcbd_submit.violet-neon-highlight button,
#rcbd_submit_wrapper.violet-neon-highlight button {
  border: 2px solid #a855f7 !important;

  box-shadow:
    0 0 0 3px #7c3aed,
    0 0 18px 6px rgba(139,92,246,0.7),
    0 0 40px 14px rgba(167,139,250,0.45) !important;

  animation: violet-neon-pulse 1s infinite alternate;
}

/* @keyframes violet-neon-pulse now in greyskull.css (shared) */

/* Quick Tour (intro.js) styling now lives in greyskull.css — shared by all apps */
.raone-pointer {
  display: none !important;
}


")),      
      # JavaScript
      tags$script(HTML("

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
                         container.closest('.brand-picker4') ||
                         container;

      var btn = brandWrapper.querySelector('button.dropdown-toggle') ||
          container.querySelector('button.dropdown-toggle') ||
          brandWrapper.querySelector('button') ||
          container.querySelector('button') ||
          container;

      $('.raone-pointer').remove();
      $('.violet-neon-highlight').removeClass('violet-neon-highlight');

      brandWrapper.classList.add('violet-neon-highlight');
      if(btn){
       btn.classList.add('violet-neon-highlight');
       }

      brandWrapper.scrollIntoView({
        behavior: 'smooth',
        block: 'center'
      });

      setTimeout(function(){

       var rect = brandWrapper.getBoundingClientRect();

        var pointer = document.createElement('div');

        pointer.className = 'raone-pointer';
        pointer.innerHTML = '&#x1F448;';
        pointer.style.position = 'fixed';
        pointer.style.top = (rect.top + rect.height/2 - 14) + 'px';
        pointer.style.left = (rect.right + 8) + 'px';
        pointer.style.zIndex = '99999';
        pointer.style.fontSize = '26px';
        pointer.style.pointerEvents = 'none';

        document.body.appendChild(pointer);

      },400);

    } else {

      if(attempts < 30){
        setTimeout(function(){
          tryVioletHighlight(attempts+1);
        },200);
      }

    }

  }

  tryVioletHighlight(0);

});


Shiny.addCustomMessageHandler('removeHighlight', function(message){
  $('.raone-pointer').remove();
  $('.violet-neon-highlight').removeClass('violet-neon-highlight');
});

$(document).on('hidden.bs.select', '#rcbd_selvar', function () {

  var selected = $('#rcbd_selvar').val();

  if(selected && selected.length > 0){
      Shiny.setInputValue('variables_dropdown_closed', Math.random());
  }

});

"))
      
    ),
    
    tags$div(
      id = "welcome_modal_overlay",
      style = "display:none; position:fixed; top:0; left:0; width:100%; height:100%;
           background:rgba(0,0,0,0.55); z-index:9999; justify-content:center;
           align-items:center;",
      tags$div(
        style = "background:#fff; border-radius:12px; padding:40px 36px; max-width:460px;
             width:90%; text-align:center; box-shadow:0 8px 32px rgba(0,0,0,0.25);
             font-family:sans-serif;",
        
        # ← Dynamic greeting injected by JS
        tags$h3(id = "welcome_user_name",
                "Welcome! &#128075;",
                style = "color:#6f2c91; margin-bottom:6px;"),
        
        tags$h4("Welcome to RAISINS Repeated Measures ANOVA!",
                style = "color:#444; margin-bottom:10px; font-weight:normal;"),
        
        tags$p("Would you like a quick guided tour of the app before you begin?",
               style = "color:#555; font-size:14px; margin-bottom:24px;"),
        
        tags$div(
          
          actionButton(
            "welcome_skip_tour",
            HTML("&#9889; Go straight to analysis"),
            style = "background:#28a745; color:#ffffff; border:none;
             padding:10px 20px; border-radius:8px; font-size:14px; cursor:pointer;"
          ),
          
          actionButton(
            "welcome_take_tour",
            HTML("&#128506; Give me a quick tour"),
            style = "background:#6f2c91; color:#ffffff; border:none;
             padding:10px 20px; border-radius:8px; font-size:14px; cursor:pointer;"
          ),
          
          style = "display:flex; gap:14px; justify-content:center;"
        )
      )
    ),
    
    
    ######################
    tabsetPanel(
      id = "subtabs",
      type = "pills",
      tabPanel("Analysis Results",
               icon = icon("american-sign-language-interpreting"),
               tags$br(),
               sidebarPanel(
                 id = "sidebar",
                 uiOutput('uploadfile'),
                 uiOutput("rcbd_var"),
                 # uiOutput("total_duration_display"),
                 conditionalPanel(
                   condition = "input.rcbd_trans_switch > 0",
                   wellPanel(
                     tags$style(
                       HTML("
           .custom-h4 {
             font-weight: bold;
             color: blue; /* Change color to your desired color */
           }
           .custom-h5 {
             font-weight: bold;
             color: green; /* Change color to your desired color */
           }
           ")
                     ),
                     h6("Select the variables to Transform", class = "custom-h5"),
                     uiOutput("rcbd_var_transform"),
                     style = "background-color: #b3c6e6; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
                   )
                 ),
                 tags$br(),
                 tags$br(),
                 uiOutput("rcbd_var_submit"),
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
                 id = "rcbd_content",
                 ## Demo ui tables display---
                 conditionalPanel(
                   condition = "output.submitClicked == true",
                   wellPanel(
                     fluidRow(
                       column(width = 2, uiOutput("rcbd_mcp")),
                       column(width = 2, uiOutput("rcbd_alpha")),
                       column(width = 2, uiOutput("rcbd_digit")),
                       column(width = 2, tagList(uiOutput("rcbd_font"), uiOutput("doe_presentation_controls"), uiOutput("doe_presentation_css"))),
                       column(width = 2, uiOutput("correction_method"))
                     ),
                     style = "background-color: #eef2da; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
                   ),
                 ),
                 div(
                   id = "main_content", # Assigning the div with the name 'main_content'
                   
                   conditionalPanel(
                     condition = "output.submitClicked == true", # Condition to show the panel based on server-side output
                     tags$br(),
                     uiOutput("rcbd_selection_summary"), # Placeholder for rcbd selection summary
                     tags$br(), # Line break
                     
                     conditionalPanel(
                       condition = "!output.rcbd_final_ANOVA && output.submitClicked == true", # Show only if outputs are not rendered
                       div(
                         style = "display: flex; justify-content: center; align-items: center; margin: 6px 0; gap: 10px;", # Added gap for spacing
                         div(
                           style = "
        border: 4px solid #f3f3f3; /* Light grey */
        border-top: 4px solid #3498db; /* Blue */
        border-radius: 50%;
        width: 30px;
        height: 30px;
        animation: spin 1s linear infinite;
      ",
                           tags$style(
                             HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }")
                           )
                         ),
                         span(
                           style = "color:#666; font-style: italic;",
                           "Hang tight, it may take some time...."
                         )
                       )
                     ),
                     
                     uiOutput("rcbd_final_ANOVA"), # Placeholder for final ANOVA output
                     uiOutput("rcbd_mauchly"),     # Mauchly's test of sphericity
                     tags$br(), # Line break
                     fluidRow(
                       column(width = 6, uiOutput("toggle_mean_sd")),
                       column(width = 6,uiOutput("toggle_letter_group"))
                       ),
                     uiOutput("rcbd_table1") %>% withLoader(type = "image", 
                                                            loader = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fcrd.GIF?alt=media")
                     
                     # uiOutput("table1AB")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "output.submitClicked == true",
                   tags$br(),
                   uiOutput("rcbd_var1")%>% withLoader(type = "image", loader = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fcrd.GIF?alt=media")
                 ),
                 uiOutput("rbd_startimage_content"),
                 uiOutput("demo_mode_notice"),
                 uiOutput("demo_dataset_info"),
                 tags$br(),
                 reactableOutput("demo_dataset_preview"),
                 # uiOutput("instr_bttns"),
                 tags$br()
               )
      ),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                             Summary statistics                           ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tabPanel("Summary stats",
               icon = icon("table"),
               conditionalPanel(
                 condition = "output.submitClicked == true",
                 tags$br(),
                 tags$br(),
                 tagList(uiOutput("doe_summary_tables"), uiOutput("doe_exact_tables"))%>% withLoader(type = "image", loader = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_CRD%2Fcrd.GIF?alt=media")
               )
      ), 
      
      tabPanel("Basic Plots",
               icon = icon("bar-chart"),
               tags$br(),
               div(
                 style = "text-align: center;",
                 uiOutput("instr_plots")
               ),
               conditionalPanel(
                 condition = "output.submitClicked == true",
                 tags$br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
                   div(
                     style = "text-align: center;",
                     p(
                       style = "color: #6c757d; line-height: 1.5; margin: 0;",
                       "You're currently viewing plots for single character. Click the settings icon (",
                       HTML("&#9881;"), ") in the top-left corner of the plot to switch between single and multiple character views, or explore the extensive customization options to personalize your visualization experience."
                     )
                   )
                 ),                           tags$br(),
                 div(
                   style = "display:flex; justify-content:center; width:100%;",
                   div(
                     style = "max-width:900px; width:100%;",
                     fluidRow(
                       column(2, iconButtonUI("show_rcbd_box",     "Box.png",     "Boxplot")),
                       column(2, iconButtonUI("show_rcbd_violin",  "Violin.png",  "Violin Plot")),
                       column(2, iconButtonUI("show_rcbd_line",    "Line.png",    "Mean Value Plot")),
                       column(2, iconButtonUI("show_rcbd_linecon", "Linecon.png", "Connected Line Plot")),
                       column(2, iconButtonUI("show_rcbd_bar",     "Bar.png",     "Bar Plot"))
                     )
                   )
                 ),
                 tags$br(),
                 ## ---- BOX PLOT ----
                 conditionalPanel(
                   condition = "output.activePlot == 'rcbd_box'",
                   div(class = "plot-container",
                   h3(class = "plot-header", "Box Plot"),
                   div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           style = "width:300px",
                           h5(style = "color:#667eea; font-weight:600; margin-bottom:15px; text-align:center;",
                              "Customize Your Plot"),

                   bsCollapse(
                     id = "rcbd_boxplot_settings_collapse",
                     open = NULL,
                     
                     ### Section 1: Plot Display Mode
                     bsCollapsePanel(
                       title = "Plot Display Mode", style = "primary",
                       radioButtons("rcbd_box_mode",
                                    "Choose Display Mode:",
                                    choices = list(
                                      "Faceted (All variables in one plot)" = "faceted",
                                      "Individual (Select specific variables)" = "individual"
                                    ),
                                    selected = "individual"),
                       
                       # Conditional panel for individual mode options
                       conditionalPanel(
                         condition = "input.rcbd_box_mode == 'faceted'",
                         sliderInput("rcbd_box_ncol", "Divide the Plots (Columns)", value = 3, min = 1, max = 9)
                       )
                     ),
                     
                     ### Section 2: Title & Labels
                     bsCollapsePanel(
                       title = "Title & Labels", style = "info",
                       fluidRow(
                         column(6, textInput("x_box_rcbd", "X-axis Title", "Treatment")),
                         column(6, textInput("y_box_rcbd", "Y-axis Title", "Value"))
                       ),
                       fluidRow(
                         column(6, textInput("title_box_rcbd", "Plot Title", "Box Plot")),
                         column(6, textInput("side_box_rcbd", "Side Legend", "Treatments"))
                       ),
                       
                       # Title Customization
                       tags$h5("Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_title_size", "Title Size", value = 14, min = 6, max = 24)),
                         column(4, colourpicker::colourInput("rcbd_title_color", "Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_title_bold", "Bold Title", value = FALSE))
                       ),
                       fluidRow(
                         column(6, sliderInput("rcbd_title_hjust", "Title Horizontal Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(6, sliderInput("rcbd_title_vjust", "Title Vertical Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       ),
                       
                       # Axis Title Customization
                       tags$h5("Axis Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_x_title_size", "X-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_x_title_color", "X-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_x_title_bold", "Bold X-axis Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_y_title_size", "Y-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_y_title_color", "Y-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_y_title_bold", "Bold Y-axis Title", value = FALSE))
                       )
                     ),
                     
                     ### Section 3: Colors & Patterns
                     bsCollapsePanel(
                       title = "Colors & Patterns", style = "primary",
                       selectInput("pal.rcbd_BOXP", "Choose Colour Pattern (Plot):",  # Fixed input ID
                                   list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                        `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                        `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                        `Grey Shade` = list("Greys")
                                   ),
                                   selected = "Dark2"
                       ),
                       
                       # Outlier Customization
                       tags$h5("Outlier Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_outlier_color", "Outlier Color", value = "black")),
                         column(4, sliderInput("rcbd_outlier_alpha", "Outlier Transparency", value = 1, min = 0, max = 1, step = 0.1)),
                         column(4, sliderInput("rcbd_box_alpha", "Box Transparency", value = 1, min = 0, max = 1, step = 0.1))
                       )
                     ),
                     
                     ### Section 4: Show/Hide Options
                     bsCollapsePanel(
                       title = "Show/Hide Options", style = "info",
                       awesomeCheckbox(inputId = "rcbd_box_xaxislab", label = "Don't Show X-axis Labels", value = FALSE),
                       awesomeCheckbox(inputId = "rcbd_box_letter", label = "Show Letter Grouping", value = TRUE),
                       
                       # Conditional Panel for Letter Grouping
                       conditionalPanel(
                         condition = "input.rcbd_box_letter == true",
                         fluidRow(
                           column(4, sliderInput("rcbd_sig_size", "Sig. Letter Size", value = 3, min = 0.5, max = 5)),
                           column(4, colourpicker::colourInput("col_box_colour", "Sig. Letter Colour", "black")),
                           column(4, sliderInput("rcbd_box_vjust", "Letter Vertical Position", value = -0.5, min = -2, max = 2, step = 0.1))
                         )
                       ),
                       
                       # Additional Statistical Elements
                       tags$h5("Additional Statistical Elements", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_add_mean_points", label = "Add Mean Points", value = FALSE),
                       conditionalPanel(
                         condition = "input.rcbd_add_mean_points == true",
                         fluidRow(
                           column(4, numericInput("rcbd_mean_point_size", "Mean Point Size", value = 2, min = 0.5, max = 5)),
                           column(4, colourpicker::colourInput("rcbd_mean_point_color", "Mean Point Color", value = "red")),
                           column(4, numericInput("rcbd_mean_point_shape", "Mean Point Shape", value = 16, min = 1, max = 25))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_add_median_line", label = "Add Median Line", value = FALSE),
                       conditionalPanel(
                         condition = "input.rcbd_add_median_line == true",
                         fluidRow(
                           column(4, sliderInput("rcbd_median_line_width", "Median Line Width", value = 0.5, min = 0.1, max = 2)),
                           column(4, colourpicker::colourInput("rcbd_median_line_color", "Median Line Color", value = "blue")),
                           column(4, sliderInput("rcbd_median_line_size", "Median Line Size", value = 0.5, min = 0.1, max = 2))
                         )
                       )
                     ),
                     
                     ### Section 5: Line & Theme Settings
                     bsCollapsePanel(
                       title = "Line & Theme Settings", style = "primary",
                       fluidRow(
                         column(6, selectInput(
                           "rcbd_box_linetype", "Select Line Type:",
                           list("Solid", "Dotted", "Dashed", "Blank", "Two Dash", "Long Dash", "Dot Dash"),
                           selected = "Solid"
                         )),
                         column(6, selectInput(
                           "rcbd_box_theme", "Select Theme:",
                           list("normal","economist","minimal","gray","light","void","tufte","stata","wsj","calc","hc"),
                           selected = "normal"
                         ))
                       ),
                       
                       # Y-axis Expansion
                       tags$h5("Y-axis Expansion", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, sliderInput("rcbd_y_expand_lower", "Lower Y-axis Expansion", value = 0.1, min = 0, max = 0.5, step = 0.05)),
                         column(6, sliderInput("rcbd_y_expand_upper", "Upper Y-axis Expansion", value = 0.2, min = 0, max = 0.5, step = 0.05))
                       )
                     ),
                     
                     ### Section 6: Size & Spacing
                     bsCollapsePanel(
                       title = "Size & Spacing", style = "info",
                       fluidRow(
                         column(6, sliderInput("rcbd_box_size", "Box Line Size", value = 0.2, min = 0.1, max = 1)),
                         column(6, sliderInput("rcbd_outlier_size", "Outlier Size", value = 0.2, min = 0.1, max = 1))
                       ),
                       
                       # Aspect Ratio Control
                       tags$h5("Plot Dimensions", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_use_aspect_ratio", label = "Use Fixed Aspect Ratio", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_use_aspect_ratio == true",
                         sliderInput("rcbd_aspect_ratio", "Aspect Ratio", value = 1, min = 0.5, max = 2, step = 0.1)
                       )
                     ),
                     
                     ### Section 7: Text & Label Settings
                     bsCollapsePanel(
                       title = "Text & Label Settings", style = "primary",
                       fluidRow(
                         column(4, sliderInput("rcbd_boxplot_angle", "Text Angle", value = 90, min = 0, max = 180)),
                         column(4, sliderInput("rcbd_box_elSize", "X-axis Label Size", value = 8, min = 1, max = 20)),
                         column(4, sliderInput("rcbd_box_wtextsize", "Section Label Size", value = 8, min = 1, max = 16))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("boxwtextcolr", "Section Label Colour", "black")),
                         column(4, colourpicker::colourInput("boxwbordercolr", "Section Border Colour", "#FAFAFA")),
                         column(4, colourpicker::colourInput("boxwbgcolr", "Section Background Colour", "#FAFAFA"))
                       ),
                       fluidRow(
                         column(4, checkboxInput("rcbd_box_x_axis_text_bold", "Bold X-axis text", value = FALSE)),
                         column(4, checkboxInput("rcbd_box_y_axis_text_bold", "Bold Y-axis text", value = FALSE)),
                         column(4, checkboxInput("rcbd_strip_text_bold", "Bold Strip Text", value = FALSE))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_box_x_axis_text_color", "X-axis text color", value = "black")),
                         column(4, colourpicker::colourInput("rcbd_box_y_axis_text_color", "Y-axis text color", value = "black")),
                         column(4, sliderInput("rcbd_box_y_axis_text_size", "Y-axis Text Size", value = 8, min = 1, max = 20))
                       ),
                       
                       # Text Positioning
                       tags$h5("Text Positioning", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, sliderInput("rcbd_x_text_hjust", "X-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_x_text_vjust", "X-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_y_text_hjust", "Y-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_y_text_vjust", "Y-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       )
                     ),
                     
                     ### Section 8: Axis & Tick Settings
                     bsCollapsePanel(
                       title = "Axis & Tick Settings", style = "info",
                       fluidRow(
                         column(3, colourpicker::colourInput("rcbd_box_x_axis_tick_color", "X-axis Tick Color", value = "black")),
                         column(3, colourpicker::colourInput("rcbd_box_y_axis_tick_color", "Y-axis Tick Color", value = "black")),
                         column(3, sliderInput("rcbd_x_tick_size", "X-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_y_tick_size", "Y-axis Tick Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Axis Lines
                       tags$h5("Axis Lines", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, colourpicker::colourInput("rcbd_x_axis_line_color", "X-axis Line Color", value = "black")),
                         column(3, colourpicker::colourInput("rcbd_y_axis_line_color", "Y-axis Line Color", value = "black")),
                         column(3, sliderInput("rcbd_x_axis_line_size", "X-axis Line Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_y_axis_line_size", "Y-axis Line Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Strip Settings
                       tags$h5("Strip Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       sliderInput("rcbd_strip_border_size", "Strip Border Size", value = 0.5, min = 0.1, max = 2)
                     ),
                     
                     ### Section 9: Background & Grid Settings
                     bsCollapsePanel(
                       title = "Background & Grid Settings", style = "primary",
                       
                       # Plot Background
                       tags$h5("Plot Background", style = "font-weight: bold; color: #333;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_plot_bg_color", "Plot Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_plot_border_color", "Plot Border Color", value = "white")),
                         column(4, sliderInput("rcbd_plot_border_size", "Plot Border Size", value = 0, min = 0, max = 2))
                       ),
                       
                       # Panel Background
                       tags$h5("Panel Background", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_panel_bg_color", "Panel Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_panel_border_color", "Panel Border Color", value = "grey50")),
                         column(4, sliderInput("rcbd_panel_border_size", "Panel Border Size", value = 0.5, min = 0, max = 2))
                       ),
                       
                       # Grid Settings
                       tags$h5("Grid Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_show_major_grid", label = "Show Major Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_show_major_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_major_grid_color", "Major Grid Color", value = "grey90")),
                           column(4, sliderInput("rcbd_major_grid_size", "Major Grid Size", value = 0.5, min = 0.1, max = 2)),
                           column(4, selectInput("rcbd_major_grid_linetype", "Major Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_show_minor_grid", label = "Show Minor Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_show_minor_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_minor_grid_color", "Minor Grid Color", value = "grey95")),
                           column(4, sliderInput("rcbd_minor_grid_size", "Minor Grid Size", value = 0.25, min = 0.1, max = 1)),
                           column(4, selectInput("rcbd_minor_grid_linetype", "Minor Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       )
                     ),
                     
                     ### Section 10: Legend Settings
                     bsCollapsePanel(
                       title = "Legend Settings", style = "info",
                       fluidRow(
                         column(6, selectInput("rcbd_legend_position", "Legend Position",
                                               list("right", "left", "top", "bottom", "none"),
                                               selected = "right")),
                         column(6, checkboxInput("rcbd_legend_title_bold", "Bold Legend Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_legend_title_size", "Legend Title Size", value = 11, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_legend_title_color", "Legend Title Color", value = "black")),
                         column(4, numericInput("rcbd_legend_text_size", "Legend Text Size", value = 9, min = 6, max = 16))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_legend_text_color", "Legend Text Color", value = "black")),
                         column(4, colourpicker::colourInput("rcbd_legend_bg_color", "Legend Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_legend_border_color", "Legend Border Color", value = "black"))
                       ),
                       sliderInput("rcbd_legend_border_size", "Legend Border Size", value = 0, min = 0, max = 2)
                     ),

                     ### Download Settings
                     bsCollapsePanel(
                       title = "Download Settings", style = "success",
                       fluidRow(
                         column(4, numericInput("rcbd_plot_width",  "Width (inches):",  value = 10,  min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_plot_height", "Height (inches):", value = 8,   min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_plot_dpi",    "DPI:",             value = 300, min = 72, max = 1200, step = 50))
                       )
                     )
                           )
                         ),
                         inputId = "rcbd_drop_box",
                         label = tagList(icon("sliders-h", style = "margin-right:8px;"), "Plot Settings"),
                         status = "primary",
                         icon = icon("cog"),
                         circle = FALSE,
                         size = "sm",
                         width = "350px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance"),
                         right = FALSE
                       )
                   ),
                 selectInput("selected_factor_box", "Select Factor:",
                             choices = c("Day" = "A"),
                             selected = "A"),
                 conditionalPanel(
                   condition = "input.rcbd_box_mode == 'individual'",
                   selectInput("rcbd_selected_yvalues", "Select Y-variable to plot:",
                               choices = NULL, multiple = FALSE, selectize = TRUE)
                 ),
                 tags$br(),

                 div(style = "position:relative; margin-top:20px;",
                     plotOutput('rcbd_boxplot') %>% withSpinner()
                 ),

                 div(style = "text-align:center; margin-top:20px;",
                     radioButtons(inputId = "rcbd_box_plot_format", label = "Format:",
                                  choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                                  selected = "png", inline = TRUE),
                     downloadButton("downloadrcbd_boxplot",
                                    label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Box Plot"),
                                    class = "download-btn")
                 )
                   ) # end .plot-container
                 ), # end conditionalPanel rcbd_box
                 # violin plot ----
                 ## ---- VIOLIN PLOT ----
                 conditionalPanel(
                   condition = "output.activePlot == 'rcbd_violin'",
                   div(class = "plot-container",
                   h3(class = "plot-header", "Violin-Box Plot"),
                   div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           style = "width:300px",
                           h5(style = "color:#667eea; font-weight:600; margin-bottom:15px; text-align:center;",
                              "Customize Your Plot"),

                   bsCollapse(
                     id = "rcbd_violinplot_settings_collapse",
                     open = NULL,
                     
                     ### Section 1: Plot Display Mode
                     bsCollapsePanel(
                       title = "Plot Display Mode", style = "primary",
                       radioButtons("rcbd_violin_mode",
                                    "Choose Display Mode:",
                                    choices = list(
                                      "Faceted (All variables in one plot)" = "faceted",
                                      "Individual (Select specific variables)" = "individual"
                                    ),
                                    selected = "individual"),
                       
                       # Conditional panel for individual mode options
                       conditionalPanel(
                         condition = "input.rcbd_violin_mode == 'faceted'",
                         sliderInput("rcbd_violin_ncol", "Divide the Plots (Columns)", value = 3, min = 1, max = 9)
                       )
                     ),
                     
                     ### Section 2: Title & Labels
                     bsCollapsePanel(
                       title = "Title & Labels", style = "info",
                       fluidRow(
                         column(6, textInput("x_violin_rcbd", "X-axis Title", "Treatment")),
                         column(6, textInput("y_violin_rcbd", "Y-axis Title", "Value"))
                       ),
                       fluidRow(
                         column(6, textInput("title_violin_rcbd", "Plot Title", "Violin Plot")),
                         column(6, textInput("side_violin_rcbd", "Side Legend", "Treatments"))
                       ),
                       
                       # Title Customization
                       tags$h5("Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_violin_title_size", "Title Size", value = 14, min = 6, max = 24)),
                         column(4, colourpicker::colourInput("rcbd_violin_title_color", "Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_violin_title_bold", "Bold Title", value = FALSE))
                       ),
                       fluidRow(
                         column(6, sliderInput("rcbd_violin_title_hjust", "Title Horizontal Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(6, sliderInput("rcbd_violin_title_vjust", "Title Vertical Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       ),
                       
                       # Axis Title Customization
                       tags$h5("Axis Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_violin_x_title_size", "X-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_violin_x_title_color", "X-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_violin_x_title_bold", "Bold X-axis Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_violin_y_title_size", "Y-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_violin_y_title_color", "Y-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_violin_y_title_bold", "Bold Y-axis Title", value = FALSE))
                       )
                     ),
                     
                     ### Section 3: Colors & Patterns
                     bsCollapsePanel(
                       title = "Colors & Patterns", style = "primary",
                       selectInput("pal.rcbd_VIOLINP", "Choose Colour Pattern (Plot):",  # Fixed input ID
                                   list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                        `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                        `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                        `Grey Shade` = list("Greys")
                                   ),
                                   selected = "Dark2"
                       ),
                       
                       # Violin Customization
                       tags$h5("Violin Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, sliderInput("rcbd_violin_alpha", "Violin Transparency", value = 1, min = 0, max = 1, step = 0.1)),
                         column(4, sliderInput("rcbd_violin_dodge", "Violin Dodge", value = 0.9, min = 0, max = 2, step = 0.1)),
                         column(4, selectInput("rcbd_violin_scale", "Violin Scale",
                                               choices = c("area", "count", "width"), selected = "area"))
                       ),
                       fluidRow(
                         column(3, checkboxInput("rcbd_violin_trim", "Trim Violin Tails", value = FALSE)),
                         column(3, checkboxInput("rcbd_violin_show_quantiles", "Show Quantiles", value = FALSE)),
                         column(3, checkboxInput("rcbd_violin_show_boxplot", "Show Box Plot Overlay", value = TRUE)),
                         column(3, sliderInput("rcbd_violinbox_size", "Box Plot Width", value = 0.1, min = 0.01, max = 0.5, step = 0.01))
                       ),
                       
                       # Outlier Customization
                       tags$h5("Outlier Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_violin_outlier_color", "Outlier Color", value = "black")),
                         column(4, sliderInput("rcbd_violin_outlier_alpha", "Outlier Transparency", value = 1, min = 0, max = 1, step = 0.1)),
                         column(4, sliderInput("rcbd_violin_outlier_size", "Outlier Size", value = 1, min = 0.1, max = 3))
                       ),
                       sliderInput("rcbd_violinbox_alpha", "Box Plot Transparency", value = 1, min = 0, max = 1, step = 0.1)
                     ),
                     
                     ### Section 4: Show/Hide Options
                     bsCollapsePanel(
                       title = "Show/Hide Options", style = "info",
                       awesomeCheckbox(inputId = "rcbd_violin_xaxislab", label = "Don't Show X-axis Labels", value = FALSE),
                       awesomeCheckbox(inputId = "rcbd_violin_letter", label = "Show Letter Grouping", value = TRUE),
                       
                       # Conditional Panel for Letter Grouping
                       conditionalPanel(
                         condition = "input.rcbd_violin_letter == true",
                         fluidRow(
                           column(4, sliderInput("rcbd_violin_sig_size", "Sig. Letter Size", value = 3, min = 0.5, max = 5)),
                           column(4, colourpicker::colourInput("col_violin_colour", "Sig. Letter Colour", "black")),
                           column(4, sliderInput("rcbd_violin_vjust", "Letter Vertical Position", value = -0.5, min = -2, max = 2, step = 0.1))
                         )
                       ),
                       
                       # Additional Statistical Elements
                       tags$h5("Additional Statistical Elements", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_violin_add_jitter", label = "Add Jitter Points", value = FALSE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_add_jitter == true",
                         fluidRow(
                           column(4, numericInput("rcbd_violin_jitter_size", "Jitter Point Size", value = 1, min = 0.1, max = 3)),
                           column(4, colourpicker::colourInput("rcbd_violin_jitter_color", "Jitter Point Color", value = "black")),
                           column(4, sliderInput("rcbd_violin_jitter_alpha", "Jitter Transparency", value = 0.5, min = 0, max = 1, step = 0.1))
                         ),
                         fluidRow(
                           column(6, sliderInput("rcbd_violin_jitter_width", "Jitter Width", value = 0.2, min = 0, max = 1, step = 0.05)),
                           column(6, sliderInput("rcbd_violin_jitter_height", "Jitter Height", value = 0, min = 0, max = 0.5, step = 0.05))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_violin_add_mean_points", label = "Add Mean Points", value = FALSE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_add_mean_points == true",
                         fluidRow(
                           column(4, numericInput("rcbd_violin_mean_point_size", "Mean Point Size", value = 2, min = 0.5, max = 5)),
                           column(4, colourpicker::colourInput("rcbd_violin_mean_point_color", "Mean Point Color", value = "red")),
                           column(4, numericInput("rcbd_violin_mean_point_shape", "Mean Point Shape", value = 16, min = 1, max = 25))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_violin_add_median_line", label = "Add Median Line", value = FALSE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_add_median_line == true",
                         fluidRow(
                           column(4, sliderInput("rcbd_violin_median_line_width", "Median Line Width", value = 0.5, min = 0.1, max = 2)),
                           column(4, colourpicker::colourInput("rcbd_violin_median_line_color", "Median Line Color", value = "blue")),
                           column(4, sliderInput("rcbd_violin_median_line_size", "Median Line Size", value = 0.5, min = 0.1, max = 2))
                         )
                       )
                     ),
                     
                     ### Section 5: Line & Theme Settings
                     bsCollapsePanel(
                       title = "Line & Theme Settings", style = "primary",
                       fluidRow(
                         column(6, selectInput(
                           "rcbd_violin_theme", "Select Theme:",
                           list("normal","economist","minimal","gray","light","void","tufte","stata","wsj","calc","hc"),
                           selected = "normal"
                         ))
                       ),
                       
                       # Y-axis Expansion
                       tags$h5("Y-axis Expansion", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, sliderInput("rcbd_violin_y_expand_lower", "Lower Y-axis Expansion", value = 0.1, min = 0, max = 0.5, step = 0.05)),
                         column(6, sliderInput("rcbd_violin_y_expand_upper", "Upper Y-axis Expansion", value = 0.2, min = 0, max = 0.5, step = 0.05))
                       )
                     ),
                     
                     ### Section 6: Size & Spacing
                     bsCollapsePanel(
                       title = "Size & Spacing", style = "info",
                       
                       # Aspect Ratio Control
                       tags$h5("Plot Dimensions", style = "font-weight: bold; color: #333;"),
                       awesomeCheckbox(inputId = "rcbd_violin_use_aspect_ratio", label = "Use Fixed Aspect Ratio", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_use_aspect_ratio == true",
                         sliderInput("rcbd_violin_aspect_ratio", "Aspect Ratio", value = 1, min = 0.5, max = 2, step = 0.1)
                       )
                     ),
                     
                     ### Section 7: Text & Label Settings
                     bsCollapsePanel(
                       title = "Text & Label Settings", style = "primary",
                       fluidRow(
                         column(4, sliderInput("rcbd_violin_angle", "Text Angle", value = 90, min = 0, max = 180)),
                         column(4, sliderInput("rcbd_violin_elSize", "X-axis Label Size", value = 8, min = 1, max = 20)),
                         column(4, sliderInput("rcbd_violin_wtextsize", "Section Label Size", value = 8, min = 1, max = 16))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("violinwtextcolr", "Section Label Colour", "black")),
                         column(4, colourpicker::colourInput("violinwbordercolr", "Section Border Colour", "#FAFAFA")),
                         column(4, colourpicker::colourInput("violinwbgcolr", "Section Background Colour", "#FAFAFA"))
                       ),
                       fluidRow(
                         column(4, checkboxInput("rcbd_violin_x_axis_text_bold", "Bold X-axis text", value = FALSE)),
                         column(4, checkboxInput("rcbd_violin_y_axis_text_bold", "Bold Y-axis text", value = FALSE)),
                         column(4, checkboxInput("rcbd_violin_strip_text_bold", "Bold Strip Text", value = FALSE))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_violin_x_axis_text_color", "X-axis text color", value = "black")),
                         column(4, colourpicker::colourInput("rcbd_violin_y_axis_text_color", "Y-axis text color", value = "black")),
                         column(4, sliderInput("rcbd_violin_y_axis_text_size", "Y-axis Text Size", value = 8, min = 1, max = 20))
                       ),
                       
                       # Text Positioning
                       tags$h5("Text Positioning", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, sliderInput("rcbd_violin_x_text_hjust", "X-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_violin_x_text_vjust", "X-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_violin_y_text_hjust", "Y-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_violin_y_text_vjust", "Y-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       )
                     ),
                     
                     ### Section 8: Axis & Tick Settings
                     bsCollapsePanel(
                       title = "Axis & Tick Settings", style = "info",
                       fluidRow(
                         column(3, colourpicker::colourInput("rcbd_violin_x_axis_tick_color", "X-axis Tick Color", value = "black")),
                         column(3, colourpicker::colourInput("rcbd_violin_y_axis_tick_color", "Y-axis Tick Color", value = "black")),
                         column(3, sliderInput("rcbd_violin_x_tick_size", "X-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_violin_y_tick_size", "Y-axis Tick Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Axis Lines
                       tags$h5("Axis Lines", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, colourpicker::colourInput("rcbd_violin_x_axis_line_color", "X-axis Line Color", value = "black")),
                         column(3, colourpicker::colourInput("rcbd_violin_y_axis_line_color", "Y-axis Line Color", value = "black")),
                         column(3, sliderInput("rcbd_violin_x_axis_line_size", "X-axis Line Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_violin_y_axis_line_size", "Y-axis Line Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Strip Settings
                       tags$h5("Strip Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       sliderInput("rcbd_violin_strip_border_size", "Strip Border Size", value = 0.5, min = 0.1, max = 2)
                     ),
                     
                     ### Section 9: Background & Grid Settings
                     bsCollapsePanel(
                       title = "Background & Grid Settings", style = "primary",
                       
                       # Plot Background
                       tags$h5("Plot Background", style = "font-weight: bold; color: #333;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_violin_plot_bg_color", "Plot Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_violin_plot_border_color", "Plot Border Color", value = "white")),
                         column(4, sliderInput("rcbd_violin_plot_border_size", "Plot Border Size", value = 0, min = 0, max = 2))
                       ),
                       
                       # Panel Background
                       tags$h5("Panel Background", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_violin_panel_bg_color", "Panel Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_violin_panel_border_color", "Panel Border Color", value = "grey50")),
                         column(4, sliderInput("rcbd_violin_panel_border_size", "Panel Border Size", value = 0.5, min = 0, max = 2))
                       ),
                       
                       # Grid Settings
                       tags$h5("Grid Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_violin_show_major_grid", label = "Show Major Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_show_major_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_violin_major_grid_color", "Major Grid Color", value = "grey90")),
                           column(4, sliderInput("rcbd_violin_major_grid_size", "Major Grid Size", value = 0.5, min = 0.1, max = 2)),
                           column(4, selectInput("rcbd_violin_major_grid_linetype", "Major Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_violin_show_minor_grid", label = "Show Minor Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_violin_show_minor_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_violin_minor_grid_color", "Minor Grid Color", value = "grey95")),
                           column(4, sliderInput("rcbd_violin_minor_grid_size", "Minor Grid Size", value = 0.25, min = 0.1, max = 1)),
                           column(4, selectInput("rcbd_violin_minor_grid_linetype", "Minor Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       )
                     ),
                     
                     ### Section 10: Legend Settings
                     bsCollapsePanel(
                       title = "Legend Settings", style = "info",
                       fluidRow(
                         column(6, selectInput("rcbd_violin_legend_position", "Legend Position",
                                               list("right", "left", "top", "bottom", "none"),
                                               selected = "right")),
                         column(6, checkboxInput("rcbd_violin_legend_title_bold", "Bold Legend Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_violin_legend_title_size", "Legend Title Size", value = 11, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_violin_legend_title_color", "Legend Title Color", value = "black")),
                         column(4, numericInput("rcbd_violin_legend_text_size", "Legend Text Size", value = 9, min = 6, max = 16))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_violin_legend_text_color", "Legend Text Color", value = "black")),
                         column(4, colourpicker::colourInput("rcbd_violin_legend_bg_color", "Legend Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_violin_legend_border_color", "Legend Border Color", value = "black"))
                       ),
                       sliderInput("rcbd_violin_legend_border_size", "Legend Border Size", value = 0, min = 0, max = 2)
                     ),

                     ### Download Settings
                     bsCollapsePanel(
                       title = "Download Settings", style = "success",
                       fluidRow(
                         column(4, numericInput("rcbd_violin_plot_width",  "Width (inches):",  value = 10,  min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_violin_plot_height", "Height (inches):", value = 8,   min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_violin_plot_dpi",    "DPI:",             value = 300, min = 72, max = 1200, step = 50))
                       )
                     )
                           )
                         ),
                         inputId = "rcbd_drop_violin",
                         label = tagList(icon("sliders-h", style = "margin-right:8px;"), "Plot Settings"),
                         status = "primary",
                         icon = icon("cog"),
                         circle = FALSE,
                         size = "sm",
                         width = "350px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance"),
                         right = FALSE
                       )
                   ),
                 selectInput("selected_factor_violin", "Select Factor:",
                             choices = c("Day" = "A"),
                             selected = "A"),
                 conditionalPanel(
                   condition = "input.rcbd_violin_mode == 'individual'",
                   selectInput("rcbd_violin_selected_yvalues", "Select Y-variable to plot:",
                               choices = NULL, multiple = FALSE, selectize = TRUE)
                 ),
                 tags$br(),

                 div(style = "position:relative; margin-top:20px;",
                     plotOutput('rcbd_violin') %>% withSpinner()
                 ),

                 div(style = "text-align:center; margin-top:20px;",
                     radioButtons(inputId = "rcbd_violin_plot_format", label = "Format:",
                                  choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                                  selected = "png", inline = TRUE),
                     downloadButton("downloadrcbd_violinplot",
                                    label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Violin Plot"),
                                    class = "download-btn")
                 )
                   ) # end .plot-container
                 ), # end conditionalPanel rcbd_violin
                 ########################### VIOLIN UI END
                 
                 ##Line (Mean value) plot ----
                 ## ---- MEAN VALUE PLOT ----
                 conditionalPanel(
                   condition = "output.activePlot == 'rcbd_line'",
                   div(class = "plot-container",
                   h3(class = "plot-header", "Mean Value Plot"),
                   div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           style = "width:300px",
                           h5(style = "color:#667eea; font-weight:600; margin-bottom:15px; text-align:center;",
                              "Customize Your Plot"),

                   bsCollapse(
                     id = "rcbd_line_settings_collapse",
                     open = NULL,
                     
                     ### Section 1: Plot Display Mode
                     bsCollapsePanel(
                       title = "Plot Display Mode", style = "primary",
                       radioButtons("rcbd_line_mode",
                                    "Choose Display Mode:",
                                    choices = list(
                                      "Faceted (All variables in one plot)" = "faceted",
                                      "Individual (Select specific variables)" = "individual"
                                    ),
                                    selected = "individual"),  # Keep original as default
                       
                       # Conditional panel for individual mode options
                       conditionalPanel(
                         condition = "input.rcbd_line_mode == 'faceted'",
                         sliderInput("rcbd_line_ncol", "Divide the Plots (Columns)", value = 3, min = 1, max = 9)
                       )
                     ),
                     
                     ### Section 2: Title & Labels
                     bsCollapsePanel(
                       title = "Title & Labels", style = "info",
                       fluidRow(
                         column(6, textInput("x_line_rcbd", "X-axis Title", "Mean Value")),
                         column(6, textInput("y_line_rcbd", "Y-axis Title", "Treatments"))
                       ),
                       fluidRow(
                         column(6, textInput("title_line_rcbd", "Plot Title", "Mean Value Plot")),
                         column(6, textInput("side_line_rcbd", "Side Legend", "Treatments"))
                       ),
                       
                       # Title Customization
                       tags$h5("Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_line_title_size", "Title Size", value = 14, min = 6, max = 24)),
                         column(4, colourpicker::colourInput("rcbd_line_title_color", "Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_line_title_bold", "Bold Title", value = FALSE))
                       ),
                       fluidRow(
                         column(6, sliderInput("rcbd_line_title_hjust", "Title Horizontal Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(6, sliderInput("rcbd_line_title_vjust", "Title Vertical Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       ),
                       
                       # Axis Title Customization
                       tags$h5("Axis Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_line_x_title_size", "X-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_line_x_title_color", "X-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_line_x_title_bold", "Bold X-axis Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_line_y_title_size", "Y-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_line_y_title_color", "Y-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_line_y_title_bold", "Bold Y-axis Title", value = FALSE))
                       )
                     ),
                     
                     ### Section 3: Colors & Themes
                     bsCollapsePanel(
                       title = "Colors & Themes", style = "primary",
                       fluidRow(
                         column(6, selectInput("pal.rcbd_lineP", "Choose Colour Pattern (Plot):",
                                               list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                                    `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                                    `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                                    `Grey Shade` = list("Greys")
                                               ),
                                               selected = "Dark2"
                         )),
                         column(6, selectInput("rcbd_line_theme", "Select Theme:",
                                               list("normal", "economist", "minimal", "gray", "light", "void", "tufte", "stata", "wsj", "calc", "hc"),
                                               selected = "normal"
                         ))
                       ),
                       
                       # Point and Error Bar Customization
                       tags$h5("Point & Error Bar Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, sliderInput("rcbd_line_point_size", "Point Size", value = 2, min = 0.5, max = 8, step = 0.5)),
                         column(4, sliderInput("rcbd_line_errorbar_size", "Error Bar Line Size", value = 0.5, min = 0.1, max = 2, step = 0.1)),
                         column(4, sliderInput("rcbd_line_point_alpha", "Point Transparency", value = 1, min = 0, max = 1, step = 0.1))
                       ),
                       fluidRow(
                         column(6, numericInput("rcbd_line_point_shape", "Point Shape", value = 16, min = 1, max = 25)),
                         column(6, colourpicker::colourInput("rcbd_line_errorbar_color_override", "Error Bar Color Override", value = ""))
                       )
                     ),
                     
                     ### Section 4: Show/Hide Options
                     bsCollapsePanel(
                       title = "Show/Hide Options", style = "info",
                       awesomeCheckbox(inputId = "rcbd_line_yaxislab", label = "Don't Show Y-axis Labels", value = FALSE),
                       awesomeCheckbox(inputId = "rcbd_line_letter", label = "Show Letter Grouping", value = TRUE),
                       
                       # Conditional Panel for Letter Grouping
                       conditionalPanel(
                         condition = "input.rcbd_line_letter == true",
                         fluidRow(
                           column(6, sliderInput("rcbd_linesig_size", "Sig. Letter Size", value = 3, min = 0.5, max = 5)),
                           column(6, colourpicker::colourInput("col_line_colour", "Sig. Letter Colour", "black"))
                         )
                       ),
                       
                       # Legend Options
                       tags$h5("Legend Options", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, selectInput("rcbd_line_legend_position", "Legend Position",
                                               list("right", "left", "top", "bottom", "none"),
                                               selected = "right")),
                         column(6, checkboxInput("rcbd_line_show_legend", "Show Legend", value = TRUE))
                       )
                     ),
                     
                     ### Section 5: Size & Spacing
                     bsCollapsePanel(
                       title = "Size & Spacing", style = "primary",
                       fluidRow(
                         column(6, sliderInput("rcbd_line_size", "Error Bar Height", value = 0.2, min = 0.1, max = 0.7)),
                         column(6, sliderInput("rcbd_line_angle", "Text Angle", value = 90, min = 0, max = 180))
                       ),
                       
                       # Plot Dimensions
                       tags$h5("Plot Dimensions", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_line_use_aspect_ratio", label = "Use Fixed Aspect Ratio", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_line_use_aspect_ratio == true",
                         sliderInput("rcbd_line_aspect_ratio", "Aspect Ratio", value = 1, min = 0.5, max = 2, step = 0.1)
                       ),
                       
                       # Axis Expansion
                       tags$h5("Axis Expansion", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, sliderInput("rcbd_line_x_expand_lower", "Lower X-axis Expansion", value = 0.1, min = 0, max = 0.5, step = 0.05)),
                         column(6, sliderInput("rcbd_line_x_expand_upper", "Upper X-axis Expansion", value = 0.2, min = 0, max = 0.5, step = 0.05))
                       )
                     ),
                     
                     ### Section 6: Text & Label Settings
                     bsCollapsePanel(
                       title = "Text & Label Settings", style = "info",
                       fluidRow(
                         column(4, sliderInput("rcbd_line_elSize", "Y-axis Label Size", value = 8, min = 1, max = 20)),
                         column(4, numericInput("rcbd_line_x_axis_text_size", "X-axis Text Size", value = 11, min = 6, max = 20, step = 1)),
                         column(4, sliderInput("rcbd_line_wtextsize", "Section Label Size", value = 8, min = 1, max = 16))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("linewtextcolr", "Section Label Colour", "black")),
                         column(4, colourpicker::colourInput("linewbordercolr", "Section Border Colour", "#FAFAFA")),
                         column(4, colourpicker::colourInput("linewbgcolr", "Section Background Colour", "#FAFAFA"))
                       ),
                       
                       # Axis Text Colors
                       fluidRow(
                         column(6, colourpicker::colourInput("rcbd_line_x_axis_text_color", "X-axis Text Colour", "black")),
                         column(6, colourpicker::colourInput("rcbd_line_y_axis_text_color", "Y-axis Text Colour", "black"))
                       ),
                       
                       # Axis Text Bold Options
                       fluidRow(
                         column(6, checkboxInput("rcbd_line_x_axis_text_bold", "Bold X-axis Text", value = FALSE)),
                         column(6, checkboxInput("rcbd_line_y_axis_text_bold", "Bold Y-axis Text", value = FALSE))
                       ),
                       
                       # Axis Tick Colors
                       fluidRow(
                         column(6, colourpicker::colourInput("rcbd_line_x_axis_tick_color", "X-axis Tick Colour", "black")),
                         column(6, colourpicker::colourInput("rcbd_line_y_axis_tick_color", "Y-axis Tick Colour", "black"))
                       ),
                       
                       # Text Positioning
                       tags$h5("Text Positioning", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, sliderInput("rcbd_line_x_text_hjust", "X-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_line_x_text_vjust", "X-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_line_y_text_hjust", "Y-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_line_y_text_vjust", "Y-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       ),
                       
                       # Strip Text Customization
                       tags$h5("Strip Text Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, checkboxInput("rcbd_line_strip_text_bold", "Bold Strip Text", value = FALSE)),
                         column(6, sliderInput("rcbd_line_strip_border_size", "Strip Border Size", value = 0.5, min = 0.1, max = 2))
                       )
                     ),
                     
                     ### Section 7: Axis & Tick Settings
                     bsCollapsePanel(
                       title = "Axis & Tick Settings", style = "primary",
                       fluidRow(
                         column(3, sliderInput("rcbd_line_x_tick_size", "X-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_line_y_tick_size", "Y-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_line_x_axis_line_size", "X-axis Line Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_line_y_axis_line_size", "Y-axis Line Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Axis Line Colors
                       tags$h5("Axis Line Colors", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, colourpicker::colourInput("rcbd_line_x_axis_line_color", "X-axis Line Color", value = "black")),
                         column(6, colourpicker::colourInput("rcbd_line_y_axis_line_color", "Y-axis Line Color", value = "black"))
                       )
                     ),
                     
                     ### Section 8: Background & Grid Settings
                     bsCollapsePanel(
                       title = "Background & Grid Settings", style = "info",
                       
                       # Plot Background
                       tags$h5("Plot Background", style = "font-weight: bold; color: #333;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_line_plot_bg_color", "Plot Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_line_plot_border_color", "Plot Border Color", value = "white")),
                         column(4, sliderInput("rcbd_line_plot_border_size", "Plot Border Size", value = 0, min = 0, max = 2))
                       ),
                       
                       # Panel Background
                       tags$h5("Panel Background", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_line_panel_bg_color", "Panel Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_line_panel_border_color", "Panel Border Color", value = "grey50")),
                         column(4, sliderInput("rcbd_line_panel_border_size", "Panel Border Size", value = 0.5, min = 0, max = 2))
                       ),
                       
                       # Grid Settings
                       tags$h5("Grid Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_line_show_major_grid", label = "Show Major Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_line_show_major_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_line_major_grid_color", "Major Grid Color", value = "grey90")),
                           column(4, sliderInput("rcbd_line_major_grid_size", "Major Grid Size", value = 0.5, min = 0.1, max = 2)),
                           column(4, selectInput("rcbd_line_major_grid_linetype", "Major Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_line_show_minor_grid", label = "Show Minor Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_line_show_minor_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_line_minor_grid_color", "Minor Grid Color", value = "grey95")),
                           column(4, sliderInput("rcbd_line_minor_grid_size", "Minor Grid Size", value = 0.25, min = 0.1, max = 1)),
                           column(4, selectInput("rcbd_line_minor_grid_linetype", "Minor Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       )
                     ),
                     
                     ### Section 9: Legend Settings
                     bsCollapsePanel(
                       title = "Legend Settings", style = "primary",
                       fluidRow(
                         column(6, checkboxInput("rcbd_line_legend_title_bold", "Bold Legend Title", value = FALSE)),
                         column(6, numericInput("rcbd_line_legend_title_size", "Legend Title Size", value = 11, min = 6, max = 20))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_line_legend_title_color", "Legend Title Color", value = "black")),
                         column(4, numericInput("rcbd_line_legend_text_size", "Legend Text Size", value = 9, min = 6, max = 16)),
                         column(4, colourpicker::colourInput("rcbd_line_legend_text_color", "Legend Text Color", value = "black"))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_line_legend_bg_color", "Legend Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_line_legend_border_color", "Legend Border Color", value = "black")),
                         column(4, sliderInput("rcbd_line_legend_border_size", "Legend Border Size", value = 0, min = 0, max = 2))
                       )
                     ),

                     ### Download Settings
                     bsCollapsePanel(
                       title = "Download Settings", style = "success",
                       fluidRow(
                         column(4, numericInput("rcbd_line_plot_width",  "Width (inches):",  value = 10,  min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_line_plot_height", "Height (inches):", value = 8,   min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_line_plot_dpi",    "DPI:",             value = 600, min = 72, max = 1200, step = 50))
                       )
                     )
                           )
                         ),
                         inputId = "rcbd_drop_line",
                         label = tagList(icon("sliders-h", style = "margin-right:8px;"), "Plot Settings"),
                         status = "primary",
                         icon = icon("cog"),
                         circle = FALSE,
                         size = "sm",
                         width = "350px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance"),
                         right = FALSE
                       )
                   ),
                 selectInput("selected_factor_linegraph", "Select Factor:",
                             choices = c("Day" = "A"),
                             selected = "A"),
                 conditionalPanel(
                   condition = "input.rcbd_line_mode == 'individual'",
                   selectInput("rcbd_line_selected_yvalues", "Select Y-variable to plot:",
                               choices = NULL, multiple = FALSE, selectize = TRUE)
                 ),
                 tags$br(),

                 div(style = "position:relative; margin-top:20px;",
                     plotOutput('rcbd_line') %>% withSpinner()
                 ),

                 div(style = "text-align:center; margin-top:20px;",
                     radioButtons(inputId = "rcbd_line_plot_format", label = "Format:",
                                  choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                                  selected = "png", inline = TRUE),
                     downloadButton("downloadrcbd_line",
                                    label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Mean Value Plot"),
                                    class = "download-btn")
                 )
                   ) # end .plot-container
                 ), # end conditionalPanel rcbd_line
                 
                 ########################line plot end
                 
                 
                 ##connected line plot ----
                 ## ---- CONNECTED LINE PLOT ----
                 conditionalPanel(
                   condition = "output.activePlot == 'rcbd_linecon'",
                   div(class = "plot-container",
                   h3(class = "plot-header", "Connected Line Plot"),
                   div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           style = "width:300px",
                           h5(style = "color:#667eea; font-weight:600; margin-bottom:15px; text-align:center;",
                              "Customize Your Plot"),

                   bsCollapse(
                     id = "rcbd_lineCon_settings_collapse",
                     open = NULL,
                     
                     ### Section 1: Plot Display Mode (ENHANCED)
                     bsCollapsePanel(
                       title = "Plot Display Mode", style = "primary",
                       radioButtons("rcbd_lineCon_mode",
                                    "Choose Display Mode:",
                                    choices = list(
                                      "Faceted (All variables in separate panels)" = "faceted",
                                      "Individual (Select specific variables)" = "individual",
                                      "Grouped (All variables on one plot)" = "grouped"  # Fixed to match server
                                    ),
                                    selected = "individual"),  # Individual as default
                       
                       # Hidden checkbox for backward compatibility (synced by server)
                       div(style = "display: none;",
                           checkboxInput("grouppbylineCon", "Group by Line (Hidden)", value = FALSE)
                       ),
                       
                       # Conditional panel for individual mode options
                       conditionalPanel(
                         condition = "input.rcbd_lineCon_mode == 'faceted'",
                         sliderInput("rcbd_lineCon_ncol", "Divide the Plots (Columns)", value = 3, min = 1, max = 9)
                       )
                     ),
                     
                     ### Section 2: Title & Axes (ORIGINAL)
                     bsCollapsePanel(
                       title = "Title & Axes", style = "info",
                       fluidRow(
                         column(6, textInput("x_lineCon_rcbd", "X-axis Title", "Treatments")),
                         column(6, textInput("y_lineCon_rcbd", "Y-axis Title", "Mean Value"))
                       ),
                       fluidRow(
                         column(12, textInput("title_lineCon_rcbd", "Plot Title", "Connected Line Plot"))
                       )
                     ),
                     
                     ### Section 3: Plot Behavior (ORIGINAL)
                     bsCollapsePanel(
                       title = "Plot Behavior", style = "primary",
                       awesomeCheckbox(
                         inputId = "lineCon_show_ebar",
                         label = "Show error bar",
                         value = TRUE
                       ),
                       awesomeCheckbox(
                         inputId = "rcbd_lineCon_letter",
                         label = "Show letter grouping",
                         value = TRUE
                       )
                     ),
                     
                     ### Section 4: Colors (FIXED CONDITIONAL LOGIC)
                     bsCollapsePanel(
                       title = "Colors", style = "info",
                       conditionalPanel(
                         condition = "input.rcbd_lineCon_mode != 'grouped'",  # Fixed condition
                         fluidRow(
                           column(4, colourpicker::colourInput("col_lineConLine_colour", "Line Colour", "black")),
                           column(4, colourpicker::colourInput("col_lineConerrorbar_colour", "Error Bar Colour", "black")),
                           column(4, colourpicker::colourInput("col_lineConPoint_colour", "Point Colour", "black"))
                         )
                       ),
                       conditionalPanel(
                         condition = "input.rcbd_lineCon_mode == 'grouped'",  # Fixed condition
                         selectInput(
                           "pal.rcbd_lineCon", "Choose Colour Pattern (Plot):",
                           list(
                             `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                             `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                             `Grey Shade` = list("Greys"),
                             `Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
                           ),
                           selected = "Dark2"
                         )
                       )
                     ),
                     
                     ### Section 5: Letter Grouping (ORIGINAL)
                     bsCollapsePanel(
                       title = "Letter Grouping", style = "primary",
                       conditionalPanel(
                         condition = "input.rcbd_lineCon_letter == true",
                         fluidRow(
                           column(6, sliderInput(inputId = "rcbd_lineConsig_size", "Sig. Letter Size", value = 3, min = 0.5, max = 5)),
                           column(6, colourpicker::colourInput("col_lineConLetter_colour", "Sig. Letter Colour", "black"))
                         ),
                         # ADDED: Missing vjust control that server references
                         fluidRow(
                           column(6, sliderInput("rcbd_lineCon_vjust", "Letter Vertical Position", value = -0.5, min = -2, max = 2, step = 0.1)),
                           column(6, "")  # Empty for spacing
                         )
                       )
                     ),
                     
                     ### Section 6: Line & Point Styles (ORIGINAL)
                     bsCollapsePanel(
                       title = "Line & Point Styles", style = "info",
                       fluidRow(
                         column(6, selectInput("rcbd_lineCon_linetype", "Select Line Type:",
                                               list("Solid", "Dotted", "Dashed", "Blank", "Two dash", "Long dash", "Dot dash"),
                                               selected = "Solid")),
                         column(6, sliderInput(inputId = "rcbd_lineCon_poinshapes", label = "Point shapes", value = 16, min = 0, max = 25))
                       ),
                       fluidRow(
                         column(6, sliderInput(inputId = "rcbd_lineCon_pointsize", "Point Size", value = 2, min = 1, max = 5)),
                         column(6, sliderInput(inputId = "rcbd_lineCon_size", "Line/Error Bar Width", value = 0.2, min = 0.1, max = 0.7))
                       )
                     ),
                     
                     ### Section 7: Theme & General Settings (ENHANCED)
                     bsCollapsePanel(
                       title = "Theme & General Settings", style = "primary",
                       selectInput(
                         "rcbd_lineCon_theme", "Statoberry Themes:",
                         list("normal", "economist", "minimal", "gray", "light", "void", "tufte", "stata", "wsj", "calc", "hc"),
                         selected = "normal"
                       ),
                       
                       # Conditional panel for faceted mode settings
                       conditionalPanel(
                         condition = "input.rcbd_lineCon_mode == 'faceted'",
                         fluidRow(
                           column(6, sliderInput(inputId = "rcbd_lineCon_wtextsize", "Section Label Size", value = 8, min = 1, max = 16)),
                           column(6, "")  # Empty for spacing
                         ),
                         fluidRow(
                           column(4, colourpicker::colourInput("lineConwtextcolr", "Section Label Colour", "black")),
                           column(4, colourpicker::colourInput("lineConwbordercolr", "Section Border Colour", "#FAFAFA")),
                           column(4, colourpicker::colourInput("lineConwbgcolr", "Section Background Colour", "#FAFAFA"))
                         )
                       )
                     ),
                     
                     ### Section 8: Text & Label Settings (REORGANIZED)
                     bsCollapsePanel(
                       title = "Text & Label Settings", style = "info",
                       fluidRow(
                         column(6, sliderInput(inputId = "rcbd_lineCon_elSize", "X-axis Label Size", value = 8, min = 1, max = 20)),
                         column(6, sliderInput(inputId = "rcbd_lineCon_angle", "Text Angle", value = 90, min = 0, max = 160))
                       ),
                       
                       # Axis Text Colors
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_lineCon_x_axis_text_color", "X-axis Text Colour", "black")),
                         column(4, colourpicker::colourInput("rcbd_lineCon_y_axis_text_color", "Y-axis Text Colour", "black")),
                         column(4, numericInput("rcbd_lineCon_y_axis_text_size", "Y-axis Text Size", value = 11, min = 6, max = 20, step = 1))
                       ),
                       
                       # Axis Text Bold Options
                       fluidRow(
                         column(6, checkboxInput("rcbd_lineCon_x_axis_text_bold", "Bold X-axis Text", value = FALSE)),
                         column(6, checkboxInput("rcbd_lineCon_y_axis_text_bold", "Bold Y-axis Text", value = FALSE))
                       ),
                       
                       # Axis Tick Colors
                       fluidRow(
                         column(6, colourpicker::colourInput("rcbd_lineCon_x_axis_tick_color", "X-axis Tick Colour", "black")),
                         column(6, colourpicker::colourInput("rcbd_lineCon_y_axis_tick_color", "Y-axis Tick Colour", "black"))
                       )
                     ),

                     ### Download Settings
                     bsCollapsePanel(
                       title = "Download Settings", style = "success",
                       fluidRow(
                         column(4, numericInput("rcbd_lineCon_plot_width",  "Width (inches):",  value = 10,  min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_lineCon_plot_height", "Height (inches):", value = 8,   min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_lineCon_plot_dpi",    "DPI:",             value = 600, min = 72, max = 1200, step = 50))
                       )
                     )
                           )
                         ),
                         inputId = "rcbd_drop_lineCon",
                         label = tagList(icon("sliders-h", style = "margin-right:8px;"), "Plot Settings"),
                         status = "primary",
                         icon = icon("cog"),
                         circle = FALSE,
                         size = "sm",
                         width = "350px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance"),
                         right = FALSE
                       )
                   ),
                 selectInput("selected_factor_linecon", "Select Factor:",
                             choices = c("Day" = "A"),
                             selected = "A"),
                 conditionalPanel(
                   condition = "input.rcbd_lineCon_mode == 'individual'",
                   selectInput("rcbd_lineCon_selected_yvalues", "Select Y-variable to plot:",
                               choices = NULL, multiple = FALSE, selectize = TRUE)
                 ),
                 tags$br(),

                 div(style = "position:relative; margin-top:20px;",
                     plotOutput('rcbd_line_plot') %>% withSpinner()
                 ),

                 div(style = "text-align:center; margin-top:20px;",
                     radioButtons(inputId = "rcbd_lineCon_plot_format", label = "Format:",
                                  choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                                  selected = "png", inline = TRUE),
                     downloadButton("downloadrcbd_lineCon",
                                    label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Connected Plot"),
                                    class = "download-btn")
                 )
                   ) # end .plot-container
                 ), # end conditionalPanel rcbd_linecon
                 ## Bar Plot ----
                 ## ---- BAR PLOT ----
                 conditionalPanel(
                   condition = "output.activePlot == 'rcbd_bar'",
                   div(class = "plot-container",
                   h3(class = "plot-header", "Bar Plot"),
                   div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           style = "width:300px",
                           h5(style = "color:#667eea; font-weight:600; margin-bottom:15px; text-align:center;",
                              "Customize Your Plot"),

                   bsCollapse(
                     id = "rcbd_barplot_settings_collapse",
                     open = NULL,
                     
                     ### Section 1: Plot Display Mode
                     bsCollapsePanel(
                       title = "Plot Display Mode", style = "primary",
                       radioButtons("rcbd_barplot_mode",
                                    "Choose Display Mode:",
                                    choices = list(
                                      "Faceted (All variables in separate panels)" = "faceted",
                                      "Individual (Select specific variables)" = "individual",
                                      "Single Plot (All variables grouped)" = "single"
                                    ),
                                    selected = "individual"),  # Individual as default
                       
                       # ADDED: Hidden checkbox for backward compatibility (synced by server)
                       div(style = "display: none;",
                           checkboxInput("singlebarplot", "Single Bar Plot (Hidden)", value = FALSE)
                       ),
                       
                       # Conditional panel for individual mode options
                       conditionalPanel(
                         condition = "input.rcbd_barplot_mode == 'faceted'",
                         sliderInput("rcbd_barplot_ncol", "Divide the Plots (Columns)", value = 3, min = 1, max = 9)
                       )
                     ),
                     
                     ### Section 2: Title & Labels
                     bsCollapsePanel(
                       title = "Title & Labels", style = "info",
                       fluidRow(
                         column(6, shiny::textInput("x_barplot_rcbd", "X-axis Title", "Treatments")),
                         column(6, shiny::textInput("y_barplot_rcbd", "Y-axis Title", "Mean Value"))
                       ),
                       fluidRow(
                         column(6, shiny::textInput("title_barplot_rcbd", "Plot Title", "Bar Plot with grouping")),
                         column(6, shiny::textInput("side_barplot_rcbd", "Side Legend", "Treatments"))
                       ),
                       
                       # Title Customization
                       tags$h5("Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_barplot_title_size", "Title Size", value = 14, min = 6, max = 24)),
                         column(4, colourpicker::colourInput("rcbd_barplot_title_color", "Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_barplot_title_bold", "Bold Title", value = FALSE))
                       ),
                       fluidRow(
                         column(6, sliderInput("rcbd_barplot_title_hjust", "Title Horizontal Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(6, sliderInput("rcbd_barplot_title_vjust", "Title Vertical Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       ),
                       
                       # Axis Title Customization
                       tags$h5("Axis Title Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, numericInput("rcbd_barplot_x_title_size", "X-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_barplot_x_title_color", "X-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_barplot_x_title_bold", "Bold X-axis Title", value = FALSE))
                       ),
                       fluidRow(
                         column(4, numericInput("rcbd_barplot_y_title_size", "Y-axis Title Size", value = 12, min = 6, max = 20)),
                         column(4, colourpicker::colourInput("rcbd_barplot_y_title_color", "Y-axis Title Color", value = "black")),
                         column(4, checkboxInput("rcbd_barplot_y_title_bold", "Bold Y-axis Title", value = FALSE))
                       )
                     ),
                     
                     ### Section 3: Behavior & Options
                     bsCollapsePanel(
                       title = "Behavior & Options", style = "primary",
                       awesomeCheckbox(
                         inputId = "rcbd_barplot_letter",
                         label = "Show letter grouping",
                         value = TRUE
                       ),
                       conditionalPanel(
                         condition = "input.rcbd_barplot_letter == true",
                         fluidRow(
                           column(4, sliderInput(
                             inputId = "rcbd_barplot_lettrSZ",
                             label = "Letter Size",
                             value = 3,
                             min = 0.5,
                             max = 7
                           )),
                           column(4, colourpicker::colourInput(
                             "col_barplotLetter_colour",
                             "Sig. Letter Colour",
                             "black"
                           )),
                           # ADDED: Missing vjust control that server references
                           column(4, sliderInput("rcbd_bar_vjust", "Letter Vertical Position", value = -0.5, min = -2, max = 2, step = 0.1))
                         )
                       ),
                       
                       # Legend Options
                       tags$h5("Legend Options", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, selectInput("rcbd_barplot_legend_position", "Legend Position",
                                               list("right", "left", "top", "bottom", "none"),
                                               selected = "right")),
                         column(6, checkboxInput("rcbd_barplot_show_legend", "Show Legend", value = TRUE))
                       )
                     ),
                     
                     ### Section 4: Colors & Patterns
                     bsCollapsePanel(
                       title = "Colors & Patterns", style = "info",
                       selectInput("pal.rcbd_barplot", "Choose Colour Pattern:",
                                   list(
                                     `Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                     `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                     `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                     `Grey Shade` = list("Greys")
                                   ),
                                   selected = "Dark2"
                       ),
                       fluidRow(
                         column(4,
                                # Pattern option (available for ALL modes now)
                                awesomeCheckbox(
                                  inputId = "gradientpattern",
                                  label = "Enable pattern type",
                                  value = FALSE
                                )
                         ),
                         column(4, colourpicker::colourInput(
                           "rcbd_barerror_colour",
                           "Error Bar Colour",
                           "black"
                         )),
                         column(4, selectInput(
                           "rcbd_barplot_theme", "Select Theme:",
                           list("normal", "economist", "minimal", "gray", "light", "void", "tufte", "stata", "wsj", "calc", "hc"),
                           selected = "normal"
                         ))
                       ),
                       
                       # Pattern settings (available when pattern is enabled)
                       conditionalPanel(
                         condition = "input.gradientpattern == true",
                         fluidRow(
                           column(6, sliderInput(
                             inputId = "rcbd_bar_pdensity",
                             label = "Pattern Density",
                             value = 0.04,
                             min = 0.01,
                             max = 1
                           )),
                           column(6, sliderInput(
                             inputId = "rcbd_bar_pspacing",
                             label = "Pattern Spacing",
                             value = 0.04,
                             min = 0.01,
                             max = 0.2
                           ))
                         )
                       ),
                       
                       # Bar Customization
                       tags$h5("Bar Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, sliderInput("rcbd_barplot_bar_alpha", "Bar Transparency", value = 1, min = 0, max = 1, step = 0.1)),
                         column(4, colourpicker::colourInput("rcbd_barplot_bar_border_color", "Bar Border Color", value = "black")),
                         column(4, sliderInput("rcbd_barplot_bar_border_size", "Bar Border Size", value = 0.5, min = 0, max = 2, step = 0.1))
                       )
                     ),
                     
                     ### Section 5: Dimensions & Sizing
                     bsCollapsePanel(
                       title = "Dimensions & Sizing", style = "primary",
                       fluidRow(
                         column(4, sliderInput(inputId = "rcbd_barplot_barW", label = "Bar Width", value = 0.5, min = 0.1, max = 1)),
                         column(4, sliderInput(inputId = "rcbd_barplot_size", label = "Error Bar Width", value = 0.2, min = 0.1, max = 1)),
                         column(4, sliderInput(inputId = "rcbd_barplot_Esize", label = "Error Bar Size", value = 0.5, min = 0, max = 1))
                       ),
                       
                       # Plot Dimensions
                       tags$h5("Plot Dimensions", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_barplot_use_aspect_ratio", label = "Use Fixed Aspect Ratio", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_barplot_use_aspect_ratio == true",
                         sliderInput("rcbd_barplot_aspect_ratio", "Aspect Ratio", value = 1, min = 0.5, max = 2, step = 0.1)
                       ),
                       
                       # Axis Expansion
                       tags$h5("Axis Expansion", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, sliderInput("rcbd_barplot_y_expand_lower", "Lower Y-axis Expansion", value = 0, min = 0, max = 0.5, step = 0.05)),
                         column(6, sliderInput("rcbd_barplot_y_expand_upper", "Upper Y-axis Expansion", value = 0.2, min = 0, max = 0.5, step = 0.05))
                       )
                     ),
                     
                     ### Section 6: Axes & Labels
                     bsCollapsePanel(
                       title = "Axes & Labels", style = "info",
                       awesomeCheckbox(
                         inputId = "rcbd_bar_xaxislab",
                         label = "Show X-axis Labels",
                         value = TRUE
                       ),
                       conditionalPanel(
                         condition = "input.rcbd_bar_xaxislab == true",
                         fluidRow(
                           column(6, sliderInput(inputId = "rcbd_barplot_angle", label = "Text Angle", value = 90, min = 0, max = 180)),
                           column(6, sliderInput(inputId = "rcbd_barplot_elSize", label = "X-axis Label Size", value = 8, min = 1, max = 20))
                         )
                       ),
                       
                       # Y-axis text size (always available)
                       numericInput("rcbd_barplot_y_axis_text_size", "Y-axis Text Size", value = 11, min = 6, max = 20, step = 1),
                       
                       # Axis Text Bold Options
                       fluidRow(
                         column(6, awesomeCheckbox(
                           inputId = "rcbd_barplot_x_axis_text_bold",
                           label = "Bold X-axis Text",
                           value = FALSE
                         )),
                         column(6, awesomeCheckbox(
                           inputId = "rcbd_barplot_y_axis_text_bold",
                           label = "Bold Y-axis Text",
                           value = FALSE
                         ))
                       ),
                       
                       # Axis Text Colors
                       fluidRow(
                         column(6, colourpicker::colourInput(
                           inputId = "rcbd_barplot_x_axis_text_color",
                           label = "X-axis Text Color",
                           value = "black"
                         )),
                         column(6, colourpicker::colourInput(
                           inputId = "rcbd_barplot_y_axis_text_color",
                           label = "Y-axis Text Color",
                           value = "black"
                         ))
                       ),
                       
                       # Axis Tick Colors
                       fluidRow(
                         column(6, colourpicker::colourInput(
                           inputId = "rcbd_barplot_x_axis_tick_color",
                           label = "X-axis Tick Color",
                           value = "black"
                         )),
                         column(6, colourpicker::colourInput(
                           inputId = "rcbd_barplot_y_axis_tick_color",
                           label = "Y-axis Tick Color",
                           value = "black"
                         ))
                       ),
                       
                       # Text Positioning
                       tags$h5("Text Positioning", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(3, sliderInput("rcbd_barplot_x_text_hjust", "X-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_barplot_x_text_vjust", "X-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_barplot_y_text_hjust", "Y-text H-Position", value = 0.5, min = 0, max = 1, step = 0.1)),
                         column(3, sliderInput("rcbd_barplot_y_text_vjust", "Y-text V-Position", value = 0.5, min = 0, max = 1, step = 0.1))
                       )
                     ),
                     
                     ### Section 7: Section Labels
                     bsCollapsePanel(
                       title = "Section Labels", style = "primary",
                       # Available for faceted mode
                       conditionalPanel(
                         condition = "input.rcbd_barplot_mode == 'faceted'",  # SIMPLIFIED: Removed complex condition
                         fluidRow(
                           column(6, sliderInput(inputId = "rcbd_bar_wtextsize", label = "Section Label Size", value = 8, min = 1, max = 16))
                         ),
                         fluidRow(
                           column(4, colourpicker::colourInput("barwtextcolr", "Section Label Colour", "black")),
                           column(4, colourpicker::colourInput("barwbordercolr", "Section Border Colour", "#FAFAFA")),
                           column(4, colourpicker::colourInput("barwbgcolr", "Section Background Colour", "#FAFAFA"))
                         ),
                         
                         # Strip Text Customization
                         tags$h5("Strip Text Customization", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                         fluidRow(
                           column(6, checkboxInput("rcbd_barplot_strip_text_bold", "Bold Strip Text", value = FALSE)),
                           column(6, sliderInput("rcbd_barplot_strip_border_size", "Strip Border Size", value = 0.5, min = 0.1, max = 2))
                         )
                       )
                     ),
                     
                     ### Section 8: Axis & Tick Settings
                     bsCollapsePanel(
                       title = "Axis & Tick Settings", style = "info",
                       fluidRow(
                         column(3, sliderInput("rcbd_barplot_x_tick_size", "X-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_barplot_y_tick_size", "Y-axis Tick Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_barplot_x_axis_line_size", "X-axis Line Size", value = 0.5, min = 0.1, max = 2)),
                         column(3, sliderInput("rcbd_barplot_y_axis_line_size", "Y-axis Line Size", value = 0.5, min = 0.1, max = 2))
                       ),
                       
                       # Axis Line Colors
                       tags$h5("Axis Line Colors", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(6, colourpicker::colourInput("rcbd_barplot_x_axis_line_color", "X-axis Line Color", value = "black")),
                         column(6, colourpicker::colourInput("rcbd_barplot_y_axis_line_color", "Y-axis Line Color", value = "black"))
                       )
                     ),
                     
                     ### Section 9: Background & Grid Settings
                     bsCollapsePanel(
                       title = "Background & Grid Settings", style = "primary",
                       
                       # Plot Background
                       tags$h5("Plot Background", style = "font-weight: bold; color: #333;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_barplot_plot_bg_color", "Plot Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_barplot_plot_border_color", "Plot Border Color", value = "white")),
                         column(4, sliderInput("rcbd_barplot_plot_border_size", "Plot Border Size", value = 0, min = 0, max = 2))
                       ),
                       
                       # Panel Background
                       tags$h5("Panel Background", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_barplot_panel_bg_color", "Panel Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_barplot_panel_border_color", "Panel Border Color", value = "grey50")),
                         column(4, sliderInput("rcbd_barplot_panel_border_size", "Panel Border Size", value = 0.5, min = 0, max = 2))
                       ),
                       
                       # Grid Settings
                       tags$h5("Grid Settings", style = "font-weight: bold; color: #333; margin-top: 15px;"),
                       awesomeCheckbox(inputId = "rcbd_barplot_show_major_grid", label = "Show Major Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_barplot_show_major_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_barplot_major_grid_color", "Major Grid Color", value = "grey90")),
                           column(4, sliderInput("rcbd_barplot_major_grid_size", "Major Grid Size", value = 0.5, min = 0.1, max = 2)),
                           column(4, selectInput("rcbd_barplot_major_grid_linetype", "Major Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       ),
                       
                       awesomeCheckbox(inputId = "rcbd_barplot_show_minor_grid", label = "Show Minor Grid", value = TRUE),
                       conditionalPanel(
                         condition = "input.rcbd_barplot_show_minor_grid == true",
                         fluidRow(
                           column(4, colourpicker::colourInput("rcbd_barplot_minor_grid_color", "Minor Grid Color", value = "grey95")),
                           column(4, sliderInput("rcbd_barplot_minor_grid_size", "Minor Grid Size", value = 0.25, min = 0.1, max = 1)),
                           column(4, selectInput("rcbd_barplot_minor_grid_linetype", "Minor Grid Line Type",
                                                 list("solid", "dotted", "dashed", "blank", "twodash", "longdash", "dotdash"),
                                                 selected = "solid"))
                         )
                       )
                     ),
                     
                     ### Section 10: Legend Settings
                     bsCollapsePanel(
                       title = "Legend Settings", style = "info",
                       fluidRow(
                         column(6, checkboxInput("rcbd_barplot_legend_title_bold", "Bold Legend Title", value = FALSE)),
                         column(6, numericInput("rcbd_barplot_legend_title_size", "Legend Title Size", value = 11, min = 6, max = 20))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_barplot_legend_title_color", "Legend Title Color", value = "black")),
                         column(4, numericInput("rcbd_barplot_legend_text_size", "Legend Text Size", value = 9, min = 6, max = 16)),
                         column(4, colourpicker::colourInput("rcbd_barplot_legend_text_color", "Legend Text Color", value = "black"))
                       ),
                       fluidRow(
                         column(4, colourpicker::colourInput("rcbd_barplot_legend_bg_color", "Legend Background Color", value = "white")),
                         column(4, colourpicker::colourInput("rcbd_barplot_legend_border_color", "Legend Border Color", value = "black")),
                         column(4, sliderInput("rcbd_barplot_legend_border_size", "Legend Border Size", value = 0, min = 0, max = 2))
                       )
                     ),

                     ### Download Settings
                     bsCollapsePanel(
                       title = "Download Settings", style = "success",
                       fluidRow(
                         column(4, numericInput("rcbd_barplot_plot_width",  "Width (inches):",  value = 10,  min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_barplot_plot_height", "Height (inches):", value = 8,   min = 1, max = 20, step = 0.5)),
                         column(4, numericInput("rcbd_barplot_plot_dpi",    "DPI:",             value = 600, min = 72, max = 1200, step = 50))
                       )
                     )
                           )
                         ),
                         inputId = "rcbd_drop_barplot",
                         label = tagList(icon("sliders-h", style = "margin-right:8px;"), "Plot Settings"),
                         status = "primary",
                         icon = icon("cog"),
                         circle = FALSE,
                         size = "sm",
                         width = "350px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance"),
                         right = FALSE
                       )
                   ),
                 selectInput("selected_factor_bar", "Select Factor:",
                             choices = c("Day" = "A"),
                             selected = "A"),
                 conditionalPanel(
                   condition = "input.rcbd_barplot_mode == 'individual'",
                   selectInput("rcbd_barplot_selected_yvalues", "Select Y-variable to plot:",
                               choices = NULL, multiple = FALSE, selectize = TRUE)
                 ),
                 tags$br(),

                 div(style = "position:relative; margin-top:20px;",
                     plotOutput('rcbd_barplot') %>% withSpinner()
                 ),

                 div(style = "text-align:center; margin-top:20px;",
                     radioButtons(inputId = "rcbd_barplot_plot_format", label = "Format:",
                                  choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                                  selected = "png", inline = TRUE),
                     downloadButton("downloadrcbd_barplot",
                                    label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Bar Plot"),
                                    class = "download-btn")
                 )
                   ) # end .plot-container
                 ), # end conditionalPanel rcbd_bar
                 
                 tags$br(),
                 tags$br()
               )
      ),
      ##ui for advanced plots  by HISHAM 
      tabPanel("Advanced Plots",
               icon = icon("area-chart"),
               tags$br(),
               tags$head(
                 tags$style(HTML("
      .plot-control-section {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 20px;
        padding: 25px;
        margin: 20px 0;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
        transition: all 0.3s ease;
      }
      .plot-control-section:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 40px rgba(0,0,0,0.3);
      }
      .modern-btn {
        background: linear-gradient(45deg, #FF6B6B, #4ECDC4);
        border: none;
        color: white;
        padding: 15px 30px;
        border-radius: 50px;
        font-weight: bold;
        font-size: 16px;
        transition: all 0.3s ease;
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      .modern-btn:hover {
        background: linear-gradient(45deg, #4ECDC4, #FF6B6B);
        transform: translateY(-3px);
        box-shadow: 0 12px 35px rgba(0,0,0,0.25);
        color: white;
      }
      .plot-container {
        background: rgba(255,255,255,0.95);
        border-radius: 20px;
        padding: 30px;
        margin: 25px 0;
        box-shadow: 0 15px 35px rgba(0,0,0,0.1);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.2);
      }
      .plot-header {
        background: linear-gradient(90deg, #667eea, #764ba2);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        font-family: 'Segoe UI', sans-serif;
        font-weight: 700;
        text-align: center;
        margin-bottom: 20px;
        font-size: 2.2em;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
      }
      .download-section {
        background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%);
        border-radius: 15px;
        padding: 20px;
        margin-top: 20px;
        text-align: center;
      }
      .download-btn {
        background: linear-gradient(45deg, #56ab2f, #a8e6cf);
        border: none;
        color: white;
        padding: 12px 25px;
        border-radius: 25px;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
      }
      .download-btn:hover {
        background: linear-gradient(45deg, #a8e6cf, #56ab2f);
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(0,0,0,0.3);
        color: white;
      }
      .control-card {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin: 10px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        border: 1px solid #e8ecef;
        transition: all 0.3s ease;
      }
      .control-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      .icon-wrapper {
        background: linear-gradient(45deg, #667eea, #764ba2);
        color: white;
        width: 50px;
        height: 50px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto 15px;
        box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
      }
      .left-dropdown-container {
        position: relative;
        text-align: left;
        margin-bottom: 20px;
      }
      .left-dropdown-container .btn {
        background: linear-gradient(45deg, #667eea, #764ba2);
        color: white;
        border: none;
        border-radius: 8px;
        padding: 8px 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.2);
        transition: all 0.3s ease;
      }
      .left-dropdown-container .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.3);
      }
      .left-dropdown-container .dropdown-menu {
        position: absolute;
        left: 0;
        right: auto;
        min-width: 350px;
        max-height: 70vh;
        overflow-y: auto;
        background: white;
        border-radius: 12px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
        border: 1px solid #e8ecef;
        padding: 15px;
        z-index: 1000;
      }
    "))
               ),

               conditionalPanel(
                 condition = "output.submitClicked == true",

                 wellPanel(
                   style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
                            border: 1px solid #dee2e6; border-radius: 12px; padding: 20px;
                            margin: 20px 0; box-shadow: 0 2px 10px rgba(0,0,0,0.05);",
                   div(
                     style = "text-align: center;",
                     p(
                       style = "color: #6c757d; font-size: 16px; margin: 0; line-height: 1.5;",
                       "Generate your desired plot by clicking the buttons below. Each plot offers extensive customization options through the settings panel."
                     )
                   )
                 ),

                 div(
                   style = "display: flex; justify-content: center; width: 100%;",
                   div(
                     style = "max-width: 900px; width: 100%;",
                     fluidRow(
                       column(2, iconButtonUI("show_summary_plot",       "Summary.png",      "Summary Plot")),
                       column(2, iconButtonUI("show_adv_raincloud_plot", "Raincloud.png",    "Advanced Raincloud Plot")),
                       column(2, iconButtonUI("show_raincloud_plot",     "Raincloud2.png",   "Raincloud Plot")),
                       column(2, iconButtonUI("show_circular_plot",      "Circular.png",     "Circular Plot"))
                     ),
                     tags$br(),
                     fluidRow(
                       column(2, iconButtonUI("show_qq_plot",            "QQ.png",          "QQ Plot")),
                       column(2, iconButtonUI("show_distribution_plot",  "Dist.png",        "Distribution Plot")),
                       column(2, iconButtonUI("show_pair_corrplot",      "Pair.png",        "Pair Plot"))
                     )
                   )
                 ),

                 tags$br(),

                 # ~ Summary Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_summary'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Summary Plot"),
                     selectInput(
                       "selected_factor_summary", "Select Factor:",
                       choices = c("Treatment" = "A", "Day" = "B",
                                   "Interaction (Treatment x Day)" = "AB"),
                       selected = "A"
                     ),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;",
                                "Customize Your Table"),
                             bsCollapse(
                               id = "rcbd_summary_collapse", open = "table_style_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Table Style & Theme"),
                                 value = "table_style_panel", style = "primary",
                                 selectInput("rcbd_summary_theme", "Table theme:",
                                             choices = list("Default"=1,"Guardian"=2,"Dot Matrix"=3,"Dark"=4,"Bootstrap"=5,"Minimal"=6),
                                             selected = 1),
                                 selectInput("rcbd_summary_color", "Color scheme:",
                                             choices = list("Blue"="blue","Red"="red","Green"="green","Pink"="pink","Cyan"="cyan","Gray"="gray"),
                                             selected = "blue"),
                                 sliderInput("rcbd_summary_font_size", "Font size (px)", min = 10, max = 18, value = 14, step = 1),
                                 selectInput("rcbd_summary_font_family", "Font family:",
                                             choices = list("Default"="default","Arial"="Arial","Times New Roman"="Times New Roman",
                                                            "Courier New"="Courier New","Georgia"="Georgia"),
                                             selected = "default")
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("heading"), " Headers & Titles"),
                                 value = "header_panel", style = "info",
                                 checkboxInput("rcbd_summary_show_title", "Show table title", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_summary_show_title",
                                                  textInput("rcbd_summary_title", "Table title:", value = "Data Summary"),
                                                  textInput("rcbd_summary_subtitle", "Subtitle (optional):", value = "")),
                                 checkboxInput("rcbd_summary_show_source", "Show source note", value = FALSE),
                                 conditionalPanel(condition = "input.rcbd_summary_show_source",
                                                  textInput("rcbd_summary_source", "Source note:", value = ""))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("calculator"), " Number Formatting"),
                                 value = "format_panel", style = "primary",
                                 sliderInput("rcbd_summary_decimals", "Decimal places", min = 0, max = 5, value = 2, step = 1),
                                 checkboxInput("rcbd_summary_use_separator", "Use thousand separator", value = TRUE),
                                 selectInput("rcbd_summary_number_format", "Number format:",
                                             choices = list("Standard"="standard","Scientific"="scientific","Percentage"="percent","Currency"="currency"),
                                             selected = "standard"),
                                 conditionalPanel(condition = "input.rcbd_summary_number_format == 'currency'",
                                                  selectInput("rcbd_summary_currency", "Currency:",
                                                              choices = list("INR"="INR","USD"="USD","EUR"="EUR","GBP"="GBP"),
                                                              selected = "INR"))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("columns"), " Column Settings"),
                                 value = "column_panel", style = "info",
                                 selectInput("rcbd_summary_text_align", "Text alignment:",
                                             choices = list("Left"="left","Center"="center","Right"="right"), selected = "center"),
                                 checkboxInput("rcbd_summary_striped_rows", "Striped rows", value = TRUE),
                                 checkboxInput("rcbd_summary_bordered", "Show borders", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("download"), " Export Options"),
                                 value = "export_panel", style = "success",
                                 selectInput("rcbd_summary_table_width", "Table width:",
                                             choices = list("Auto"="auto","100%"="100%","75%"="75%","50%"="50%"), selected = "auto"),
                                 sliderInput("rcbd_summary_table_height", "Max table height (px)", min = 300, max = 800, value = 500, step = 50)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_summary_plot",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("cog"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     div(style = "position: relative; margin-top: 20px;",
                         gt_output("rcbd_summary_plot_table") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         downloadButton("download_summary_html",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download Summary Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ Advanced Raincloud Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_raincloud'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Advanced Raincloud Plot"),
                     selectInput(
                       "selected_factor_adv_raincloud", "Select Factor:",
                       choices = c("Treatment" = "A", "Day" = "B",
                                   "Interaction (Treatment x Day)" = "AB"),
                       selected = "A"
                     ),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;", "Customize Your Plot"),
                             bsCollapse(
                               id = "rcbd_rc_collapse", open = "layout_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("database"), " Layout & Data"), value = "layout_panel", style = "primary",
                                 checkboxInput("rcbd_rc_show_title", "Show plot title", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_show_title",
                                                  textInput("rcbd_rc_custom_title", "Custom title:", value = "Raincloud Plot")),
                                 textInput("rcbd_rc_x_label", "X-axis label:", value = ""),
                                 textInput("rcbd_rc_y_label", "Y-axis label:", value = "Value"),
                                 checkboxInput("rcbd_rc_coord_flip", "Flip coordinates", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_coord_flip",
                                                  numericInput("rcbd_rc_xlim_start", "X-axis start limit:", value = 1.2, min = 0.5, max = 3, step = 0.1))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("wave-square"), " Density Curves"), value = "density_panel", style = "info",
                                 checkboxInput("rcbd_rc_show_density", "Show density half-eyes", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_show_density",
                                                  sliderInput("rcbd_rc_density_adjust", "Density smoothing", min = 0.1, max = 2, value = 0.6, step = 0.1),
                                                  sliderInput("rcbd_rc_density_width", "Density width", min = 0.3, max = 1.2, value = 0.6, step = 0.1),
                                                  sliderInput("rcbd_rc_density_justification", "Density position", min = -0.5, max = 0.5, value = -0.3, step = 0.1),
                                                  sliderInput("rcbd_rc_density_alpha", "Density transparency", min = 0.1, max = 1, value = 0.8, step = 0.1),
                                                  sliderInput("rcbd_rc_lighten_amount", "Fill color lightening", min = 0.1, max = 0.8, value = 0.5, step = 0.1))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("square"), " Box Plots"), value = "boxplot_panel", style = "primary",
                                 checkboxInput("rcbd_rc_show_boxplot", "Show box plots", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_show_boxplot",
                                                  sliderInput("rcbd_rc_box_width", "Box plot width", min = 0.1, max = 0.5, value = 0.3, step = 0.05),
                                                  sliderInput("rcbd_rc_box_alpha", "Box plot transparency", min = 0.1, max = 1, value = 0.4, step = 0.1),
                                                  checkboxInput("rcbd_rc_show_outliers", "Show outliers", value = FALSE),
                                                  sliderInput("rcbd_rc_darken_amount", "Box color darkening", min = 0.05, max = 0.3, value = 0.1, step = 0.05))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("circle"), " Data Points"), value = "points_panel", style = "info",
                                 checkboxInput("rcbd_rc_show_points", "Show individual points", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_show_points",
                                                  sliderInput("rcbd_rc_point_size", "Point size", min = 0.5, max = 4, value = 2, step = 0.1),
                                                  sliderInput("rcbd_rc_point_stroke", "Point border width", min = 0.1, max = 1, value = 0.4, step = 0.1),
                                                  sliderInput("rcbd_rc_jitter_width", "Jitter width", min = 0.05, max = 0.3, value = 0.1, step = 0.01),
                                                  sliderInput("rcbd_rc_point_alpha", "Point transparency", min = 0.1, max = 1, value = 0.3, step = 0.1),
                                                  colourpicker::colourInput("rcbd_rc_point_fill", "Point fill color", value = "white"),
                                                  numericInput("rcbd_rc_jitter_seed", "Jitter seed", value = 1, min = 1, max = 100))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("calculator"), " Statistical Labels"), value = "stats_panel", style = "primary",
                                 checkboxInput("rcbd_rc_show_median", "Show median labels", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rc_show_median",
                                                  sliderInput("rcbd_rc_median_size", "Median label size", min = 2, max = 6, value = 4, step = 0.5),
                                                  sliderInput("rcbd_rc_median_vjust", "Median label vertical position", min = -3, max = 3, value = -1.5, step = 0.1),
                                                  numericInput("rcbd_rc_median_digits", "Decimal places for median", value = 2, min = 0, max = 4))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Colors & Theme"), value = "theme_panel", style = "info",
                                 selectInput("rcbd_rc_color_palette", "Color palette:",
                                             choices = list("Dark 3 (Default)"="Dark 3","Set 2"="Set 2","Set 3"="Set 3",
                                                            "Pastel 1"="Pastel 1","Dark 2"="Dark 2","Viridis"="viridis",
                                                            "Plasma"="plasma","Inferno"="inferno","Magma"="magma"),
                                             selected = "Dark 3"),
                                 checkboxInput("rcbd_rc_custom_colors", "Use custom colors", value = FALSE),
                                 conditionalPanel(condition = "input.rcbd_rc_custom_colors",
                                   fluidRow(
                                     column(6, colourpicker::colourInput("rcbd_rc_color1", "Treatment 1:", value = "#1f77b4")),
                                     column(6, colourpicker::colourInput("rcbd_rc_color2", "Treatment 2:", value = "#ff7f0e"))
                                   ),
                                   fluidRow(
                                     column(6, colourpicker::colourInput("rcbd_rc_color3", "Treatment 3:", value = "#2ca02c")),
                                     column(6, colourpicker::colourInput("rcbd_rc_color4", "Treatment 4:", value = "#d62728"))
                                   ),
                                   fluidRow(
                                     column(6, colourpicker::colourInput("rcbd_rc_color5", "Treatment 5:", value = "#9467bd")),
                                     column(6, colourpicker::colourInput("rcbd_rc_color6", "Treatment 6:", value = "#8c564b"))
                                   ),
                                   fluidRow(
                                     column(6, colourpicker::colourInput("rcbd_rc_color7", "Treatment 7:", value = "#e377c2")),
                                     column(6, colourpicker::colourInput("rcbd_rc_color8", "Treatment 8:", value = "#7f7f7f"))
                                   )
                                 ),
                                 selectInput("rcbd_rc_theme", "Plot theme:",
                                             choices = list("Minimal"="minimal","Classic"="classic","Light"="light",
                                                            "Dark"="dark","Void"="void","BW"="bw","Linedraw"="linedraw"),
                                             selected = "minimal")
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("font"), " Text & Labels"), value = "text_panel", style = "primary",
                                 fluidRow(column(6, selectInput("rcbd_rc_font_family", "Font Family", choices = FONT_CHOICES, selected = ""))),
                                 fluidRow(
                                   column(12, radioButtons("rcbd_raincloud_treatment_order", "Arrange Treatments:",
                                                           choices = list("As in uploaded dataset"="data","Alphabetical order"="alpha"),
                                                           selected = "alpha", inline = FALSE)),
                                   column(12, awesomeCheckbox(inputId = "rcbd_raincloud_clean_labels", label = "Clean treatment labels", value = FALSE))
                                 ),
                                 sliderInput("rcbd_rc_base_size", "Base font size", min = 10, max = 20, value = 14, step = 1),
                                 sliderInput("rcbd_rc_title_size", "Title size", min = 14, max = 24, value = 18, step = 1),
                                 sliderInput("rcbd_rc_axis_text_size", "Axis text size", min = 8, max = 16, value = 12, step = 1),
                                 checkboxInput("rcbd_rc_title_bold", "Bold title", value = TRUE),
                                 checkboxInput("rcbd_rc_axis_text_colored", "Color axis text by treatment", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("th"), " Grid & Axes"), value = "grid_panel", style = "info",
                                 checkboxInput("rcbd_rc_show_minor_grid", "Show minor grid lines", value = FALSE),
                                 checkboxInput("rcbd_rc_show_major_y_grid", "Show major Y grid lines", value = FALSE),
                                 checkboxInput("rcbd_rc_show_axis_ticks", "Show axis ticks", value = FALSE),
                                 checkboxInput("rcbd_rc_show_x_text", "Show X-axis text", value = TRUE),
                                 checkboxInput("rcbd_rc_show_y_text", "Show Y-axis text", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("expand-arrows-alt"), " Plot Dimensions"), value = "dimensions_panel", style = "primary",
                                 sliderInput("rcbd_rc_plot_height", "Plot height (px)", min = 400, max = 1000, value = 600, step = 50),
                                 sliderInput("rcbd_rc_top_margin", "Top margin", min = 5, max = 30, value = 15, step = 1),
                                 sliderInput("rcbd_rc_right_margin", "Right margin", min = 5, max = 30, value = 15, step = 1),
                                 sliderInput("rcbd_rc_bottom_margin", "Bottom margin", min = 5, max = 25, value = 10, step = 1),
                                 sliderInput("rcbd_rc_left_margin", "Left margin", min = 5, max = 30, value = 15, step = 1)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("cog"), " Export Settings"), value = "export_panel", style = "success",
                                 textInput("rcbd_rc_filename", "Filename for export", value = "advanced_raincloud_plot.png"),
                                 sliderInput("rcbd_rc_dpi", "Export DPI (resolution)", min = 150, max = 600, value = 300, step = 50),
                                 sliderInput("rcbd_rc_width", "Export width (inches)", min = 6, max = 16, value = 12, step = 1),
                                 sliderInput("rcbd_rc_height", "Export height (inches)", min = 4, max = 12, value = 8, step = 1)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_raincloud_by_char",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("cog"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     selectInput("rcbd_rc_variables", "Select the Variable", choices = NULL),
                     div(style = "position: relative; margin-top: 20px;",
                         plotOutput("rcbd_raincloud_by_char_plot", height = "600px") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         radioButtons(inputId = "rcbd_rc_file_format", label = "Format:",
                                      choices = c("PNG"="png","JPEG"="jpeg","TIFF"="tiff","PDF"="pdf","SVG"="svg"),
                                      selected = "png", inline = TRUE),
                         downloadButton("rcbd_rc_download",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download Advanced Raincloud Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ Raincloud Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_raincloud2'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Raincloud Plot"),
                     selectInput(
                       "selected_factor_raincloud", "Select Factor:",
                       choices = c("Treatment" = "A", "Day" = "B",
                                   "Interaction (Treatment x Day)" = "AB"),
                       selected = "A"
                     ),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;", "Customize Your Plot"),
                             bsCollapse(
                               id = "rcbd_rain_collapse", open = "data_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("database"), " Data & Layout"), value = "data_panel", style = "primary",
                                 selectInput("rcbd_rain_facet_cols", "Number of columns in facet:",
                                             choices = list("1"=1,"2"=2,"3"=3,"4"=4,"5"=5), selected = 2),
                                 selectInput("rcbd_rain_facet_scales", "Facet scales:",
                                             choices = list("Free Y"="free_y","Free X"="free_x","Free Both"="free","Fixed"="fixed"),
                                             selected = "free_y"),
                                 sliderInput("rcbd_rain_aspect_ratio", "Panel aspect ratio", min = 0.2, max = 2, value = 0.5, step = 0.1),
                                 checkboxInput("rcbd_rain_show_title", "Show plot title", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rain_show_title",
                                                  textInput("rcbd_rain_title", "Custom title:", value = "Raincloud Plots"))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("wave-square"), " Distribution Density"), value = "distribution_panel", style = "info",
                                 checkboxInput("rcbd_rain_show_density", "Show density curves", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rain_show_density",
                                                  sliderInput("rcbd_rain_density_adjust", "Density smoothing", min = 0.1, max = 2, value = 0.6, step = 0.1),
                                                  sliderInput("rcbd_rain_density_justification", "Density position", min = -0.5, max = 0.5, value = -0.3, step = 0.1),
                                                  sliderInput("rcbd_rain_density_alpha", "Density transparency", min = 0.1, max = 1, value = 0.8, step = 0.1),
                                                  sliderInput("rcbd_rain_density_width", "Density width", min = 0.5, max = 2, value = 1, step = 0.1))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("square"), " Box Plot"), value = "boxplot_panel", style = "primary",
                                 checkboxInput("rcbd_rain_show_boxplot", "Show box plots", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rain_show_boxplot",
                                                  sliderInput("rcbd_rain_box_width", "Box plot width", min = 0.05, max = 0.3, value = 0.1, step = 0.01),
                                                  sliderInput("rcbd_rain_box_alpha", "Box plot transparency", min = 0.1, max = 1, value = 0.4, step = 0.1),
                                                  checkboxInput("rcbd_rain_show_outliers", "Show outliers", value = FALSE),
                                                  conditionalPanel(condition = "input.rcbd_rain_show_outliers",
                                                                   sliderInput("rcbd_rain_outlier_size", "Outlier size", min = 0.5, max = 3, value = 1.5, step = 0.1)))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("circle"), " Data Points"), value = "points_panel", style = "info",
                                 checkboxInput("rcbd_rain_show_points", "Show individual points", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_rain_show_points",
                                                  selectInput("rcbd_rain_point_type", "Point display type:",
                                                              choices = list("Jitter"="jitter","Swarm (beeswarm)"="swarm","Strip"="strip"),
                                                              selected = "jitter"),
                                                  sliderInput("rcbd_rain_point_width", "Point spread width", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
                                                  sliderInput("rcbd_rain_point_size", "Point size", min = 0.5, max = 3, value = 1, step = 0.1),
                                                  sliderInput("rcbd_rain_point_alpha", "Point transparency", min = 0.1, max = 1, value = 0.5, step = 0.1),
                                                  colourpicker::colourInput("rcbd_rain_point_color", "Point color", value = "black"))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Colors & Theme"), value = "theme_panel", style = "primary",
                                 selectInput("rcbd_rain_color_palette", "Color palette:",
                                             choices = list("Default"="default","Viridis"="viridis","Set1"="Set1","Set2"="Set2",
                                                            "Set3"="Set3","Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1",
                                                            "Pastel2"="Pastel2","Accent"="Accent"),
                                             selected = "default"),
                                 checkboxInput("rcbd_rain_show_legend", "Show legend", value = FALSE),
                                 conditionalPanel(condition = "input.rcbd_rain_show_legend",
                                                  selectInput("rcbd_rain_legend_position", "Legend position:",
                                                              choices = list("Right"="right","Left"="left","Top"="top","Bottom"="bottom"),
                                                              selected = "right")),
                                 selectInput("rcbd_rain_theme", "Plot theme:",
                                             choices = list("Minimal"="minimal","Classic"="classic","Light"="light",
                                                            "Dark"="dark","Void"="void","BW"="bw"),
                                             selected = "minimal")
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("font"), " Text & Labels"), value = "text_panel", style = "info",
                                 textInput("rcbd_rain_x_label", "X-axis label:", value = ""),
                                 textInput("rcbd_rain_y_label", "Y-axis label:", value = "Value"),
                                 selectInput("rcbd_rain_font_family", "Font Family", choices = FONT_CHOICES),
                                 sliderInput("rcbd_rain_title_size", "Title size", min = 10, max = 20, value = 14, step = 1),
                                 sliderInput("rcbd_rain_axis_text_size", "Axis text size", min = 8, max = 16, value = 10, step = 1),
                                 sliderInput("rcbd_rain_strip_text_size", "Facet label size", min = 8, max = 16, value = 11, step = 1),
                                 checkboxInput("rcbd_rain_strip_bold", "Bold Section labels", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("cog"), " Export Settings"), value = "export_panel", style = "success",
                                 textInput("rcbd_rain_filename", "Filename for export", value = "raincloud_plot.png"),
                                 sliderInput("rcbd_rain_dpi", "Export DPI (resolution)", min = 150, max = 600, value = 300, step = 50),
                                 sliderInput("rcbd_rain_width", "Plot width (inches)", min = 6, max = 16, value = 10, step = 1),
                                 sliderInput("rcbd_rain_height", "Plot height (inches)", min = 4, max = 12, value = 8, step = 1)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_raincloud_plot",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("cog"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     div(style = "position: relative; margin-top: 20px;",
                         plotOutput("rcbd_raincloud_plot", height = "600px") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         radioButtons(inputId = "rcbd_rain_format", label = "Format:",
                                      choices = c("PNG"="png","JPEG"="jpeg","TIFF"="tiff","PDF"="pdf","SVG"="svg"),
                                      selected = "png", inline = TRUE),
                         downloadButton("rcbd_rain_download",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download Raincloud Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ Circular Bar Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_circular'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Circular Bar Plot"),
                     selectInput(
                       "selected_factor_circular", "Select Factor:",
                       choices = c("Treatment" = "A", "Day" = "B",
                                   "Interaction (Treatment x Day)" = "AB"),
                       selected = "A"
                     ),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;", "Customize Your Plot"),
                             bsCollapse(
                               id = "rcbd_circular_collapse", open = "data_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("database"), " Data Processing"), value = "data_panel", style = "primary",
                                 checkboxInput("rcbd_circ_scale_data", "Scale data (min-max normalization)", value = TRUE),
                                 checkboxInput("rcbd_circ_use_means", "Use means for grouped data", value = TRUE),
                                 numericInput("rcbd_circ_empty_bar", "Empty bars between groups:", value = NULL, min = 0, max = 10, step = 1)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Visual Styling"), value = "visual_panel", style = "info",
                                 sliderInput("rcbd_circ_alpha", "Bar transparency", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
                                 colourpicker::colourInput("rcbd_circ_grid_color", "Grid color:", value = "grey"),
                                 sliderInput("rcbd_circ_grid_size", "Grid line thickness", min = 0.1, max = 1.0, value = 0.3, step = 0.1),
                                 selectInput("rcbd_circ_grid_levels", "Grid levels:",
                                             choices = list("0.1"=0.1,"0.2"=0.2,"0.25"=0.25,"0.3"=0.3,"0.4"=0.4,"0.5"=0.5,
                                                            "0.6"=0.6,"0.7"=0.7,"0.75"=0.75,"0.8"=0.8,"0.9"=0.9,"1.0"=1.0,
                                                            "1.1"=1.1,"1.2"=1.2,"1.25"=1.25,"1.5"=1.5,"1.75"=1.75,"2.0"=2.0),
                                             selected = c(0.25, 0.5, 0.75, 1.0), multiple = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("font"), " Text & Labels"), value = "text_panel", style = "primary",
                                 fluidRow(column(6, selectInput("rcbd_circular_font_family", "Font Family", choices = FONT_CHOICES, selected = ""))),
                                 fluidRow(
                                   column(12, radioButtons("rcbd_circular_treatment_order", "Arrange Treatments:",
                                                           choices = list("As in uploaded dataset"="data","Alphabetical order"="alpha"),
                                                           selected = "alpha", inline = FALSE)),
                                   column(12, awesomeCheckbox(inputId = "rcbd_circular_clean_labels", label = "Clean treatment labels", value = FALSE))
                                 ),
                                 checkboxInput("rcbd_circ_show_labels", "Show individual labels", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_circ_show_labels",
                                                  sliderInput("rcbd_circ_label_size", "Individual label size", min = 1.0, max = 4.0, value = 2.2, step = 0.1)),
                                 sliderInput("rcbd_circ_group_label_size", "Group label size", min = 1.0, max = 5.0, value = 3.5, step = 0.1),
                                 checkboxInput("rcbd_circ_show_legend", "Show legend", value = TRUE),
                                 checkboxInput("rcbd_circ_show_grid", "Show grid", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_circ_show_grid",
                                                  sliderInput("rcbd_circ_grid_text_size", "Grid text size", min = 1.0, max = 4.0, value = 2.5, step = 0.1))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("download"), " Export Options"), value = "export_panel", style = "success",
                                 textInput("rcbd_circ_filename", "File name:", value = "circular_plot.png"),
                                 sliderInput("rcbd_circ_plot_size", "Plot size (inches)", min = 6, max = 20, value = 12, step = 1),
                                 numericInput("rcbd_circ_dpi", "DPI (resolution):", value = 300, min = 72, max = 600, step = 50)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_circular_plot",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("cog"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     div(style = "position: relative; margin-top: 20px;",
                         plotOutput("rcbd_circular_bar_plot") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         radioButtons(inputId = "rcbd_circular_format", label = "Format:",
                                      choices = c("PNG"="png","JPEG"="jpeg","TIFF"="tiff","PDF"="pdf","SVG"="svg"),
                                      selected = "png", inline = TRUE),
                         downloadButton("rcbd_circ_download",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download Circular Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ QQ Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_qq'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "QQ Plot"),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;", "Customize Your Plot"),
                             bsCollapse(
                               id = "rcbd_qq_collapse", open = "plot_type_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("chart-line"), " Plot Type & Data"), value = "plot_type_panel", style = "primary",
                                 radioButtons("rcbd_qq_plot_type", "Plot Type:",
                                              choices = list("All Variables (Individual)"="all_individual","Single Variable"="single"),
                                              selected = "single"),
                                 checkboxInput("rcbd_qq_show_bands", "Show Confidence Bands", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Visual Appearance"), value = "visual_panel", style = "info",
                                 selectInput("rcbd_qq_theme", "Plot Theme:",
                                             choices = list("Minimal"="minimal","Classic"="classic","Dark"="dark",
                                                            "Economist"="economist","Light"="light","Void"="void","Tufte"="tufte"),
                                             selected = "minimal"),
                                 sliderInput("rcbd_qq_point_size", "Point Size:", min = 0.5, max = 3, value = 1.2, step = 0.1),
                                 sliderInput("rcbd_qq_point_alpha", "Point Transparency:", min = 0.3, max = 1, value = 0.7, step = 0.1),
                                 sliderInput("rcbd_qq_band_alpha", "Band Transparency:", min = 0.1, max = 0.8, value = 0.3, step = 0.1),
                                 sliderInput("rcbd_qq_line_size", "Reference Line Size:", min = 0.3, max = 2, value = 0.8, step = 0.1)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("font"), " Text & Labels"), value = "text_panel", style = "primary",
                                 fluidRow(column(6, selectInput("rcbd_qq_font_family", "Font Family", choices = FONT_CHOICES, selected = ""))),
                                 textInput("rcbd_qq_main_title", "Main Title:", value = "QQ Plot - Residual Normality Check"),
                                 textInput("rcbd_qq_subtitle", "Subtitle:", value = "Residuals from the full model (A + B + C + all interactions): points should follow the diagonal line"),
                                 sliderInput("rcbd_qq_title_size", "Title Size:", min = 12, max = 20, value = 16, step = 1),
                                 sliderInput("rcbd_qq_text_size", "Axis Text Size:", min = 8, max = 16, value = 10, step = 1),
                                 sliderInput("rcbd_qq_strip_size", "Facet Label Size:", min = 8, max = 14, value = 10, step = 1),
                                 checkboxInput("rcbd_qq_show_subtitle", "Show Subtitle", value = TRUE),
                                 checkboxInput("rcbd_qq_show_legend", "Show Legend", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("th"), " Layout Options"), value = "layout_panel", style = "info",
                                 sliderInput("rcbd_qq_plot_height", "Plot Height (px):", min = 400, max = 1000, value = 700, step = 50),
                                 conditionalPanel(condition = "input.rcbd_qq_plot_type == 'all_grid' || input.rcbd_qq_plot_type == 'all_individual'",
                                                  sliderInput("rcbd_qq_ncol", "Number of Columns:", min = 1, max = 4, value = 2, step = 1)),
                                 checkboxInput("rcbd_qq_free_scales", "Free Scales", value = TRUE),
                                 checkboxInput("rcbd_qq_equal_axes", "Equal Axis Ranges", value = FALSE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("calculator"), " Statistical Tests"), value = "stats_panel", style = "primary",
                                 checkboxInput("rcbd_qq_show_tests", "Show Normality Tests", value = FALSE),
                                 conditionalPanel(condition = "input.rcbd_qq_show_tests == true",
                                                  checkboxGroupInput("rcbd_qq_test_types", "Normality Tests to Display:",
                                                                     choices = list("Shapiro-Wilk"="shapiro","Anderson-Darling"="ad","Kolmogorov-Smirnov"="ks"),
                                                                     selected = "shapiro"),
                                                  sliderInput("rcbd_qq_test_text_size", "Test Result Text Size:", min = 2, max = 5, value = 3, step = 0.1),
                                                  radioButtons("rcbd_qq_test_position", "Test Result Position:",
                                                               choices = list("Top-left"="tl","Top-right"="tr","Bottom-left"="bl","Bottom-right"="br"),
                                                               selected = "tl", inline = FALSE))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("download"), " Export Settings"), value = "export_panel", style = "success",
                                 textInput("rcbd_qq_filename", "Filename:", value = "qq_plot_analysis.png"),
                                 sliderInput("rcbd_qq_export_width", "Export Width:", min = 8, max = 20, value = 12, step = 1),
                                 sliderInput("rcbd_qq_export_height", "Export Height:", min = 6, max = 16, value = 8, step = 1),
                                 sliderInput("rcbd_qq_dpi", "Export DPI:", min = 150, max = 600, value = 300, step = 50)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_qq_plot",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("cog"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     conditionalPanel(condition = "input.rcbd_qq_plot_type == 'single' || input.rcbd_qq_plot_type == 'compare_treatments'",
                                      selectInput("rcbd_qq_variable", "Select Variable:", choices = NULL)),
                     div(style = "position: relative; margin-top: 20px;",
                         uiOutput("rcbd_qq_plot_container") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         radioButtons(inputId = "rcbd_qq_format", label = "Format:",
                                      choices = c("PNG"="png","JPEG"="jpeg","TIFF"="tiff","PDF"="pdf","SVG"="svg"),
                                      selected = "png", inline = TRUE),
                         downloadButton("rcbd_qq_download",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download QQ Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ Distribution Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_dist'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Distribution Plot"),
                     selectInput(
                       "selected_factor_distribution", "Select Factor:",
                       choices = c("Treatment" = "A", "Day" = "B",
                                   "Interaction (Treatment x Day)" = "AB"),
                       selected = "A"
                     ),
                     div(class = "left-dropdown-container",
                         dropdownButton(
                           div(
                             h5(style = "color: #667eea; font-weight: 600; margin-bottom: 15px; text-align: center;", "Customize Your Plot"),
                             bsCollapse(
                               id = "rcbd_distribution_collapse", open = "plot_style_panel",
                               bsCollapsePanel(
                                 title = tags$span(icon("chart-area"), " Plot Style & Type"), value = "plot_style_panel", style = "primary",
                                 selectInput("rcbd_dist_plot_type", "Distribution plot type:",
                                             choices = list("Density"="density","Histogram"="histogram","Boxplot"="boxplot"),
                                             selected = "density"),
                                 colourpicker::colourInput("rcbd_dist_fill_color", "Fill color:", value = "#3498db"),
                                 colourpicker::colourInput("rcbd_dist_line_color", "Line color:", value = "#2c3e50")
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("palette"), " Table Theme & Style"), value = "table_theme_panel", style = "info",
                                 selectInput("rcbd_dist_theme", "Table theme:",
                                             choices = list("Default"=1,"Guardian"=2,"Dot Matrix"=3,"Dark"=4,"Bootstrap"=5,"Minimal"=6),
                                             selected = 1),
                                 selectInput("rcbd_dist_color_scheme", "Color scheme:",
                                             choices = list("Blue"="blue","Red"="red","Green"="green","Pink"="pink","Cyan"="cyan","Gray"="gray"),
                                             selected = "blue"),
                                 sliderInput("rcbd_dist_font_size", "Font size (px)", min = 10, max = 18, value = 14, step = 1),
                                 selectInput("rcbd_dist_font_family", "Font family:",
                                             choices = list("Default"="default","Arial"="Arial","Times New Roman"="Times New Roman",
                                                            "Courier New"="Courier New","Georgia"="Georgia"),
                                             selected = "default")
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("heading"), " Text & Labels"), value = "dist_header_panel", style = "primary",
                                 checkboxInput("rcbd_dist_show_title", "Show table title", value = TRUE),
                                 conditionalPanel(condition = "input.rcbd_dist_show_title",
                                                  textInput("rcbd_dist_title", "Table title:", value = "Distribution Analysis"),
                                                  textInput("rcbd_dist_subtitle", "Subtitle (optional):", value = "")),
                                 checkboxInput("rcbd_dist_show_source", "Show source note", value = FALSE),
                                 conditionalPanel(condition = "input.rcbd_dist_show_source",
                                                  textInput("rcbd_dist_source", "Source note:", value = ""))
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("columns"), " Column Settings"), value = "dist_column_panel", style = "info",
                                 selectInput("rcbd_dist_text_align", "Text alignment:",
                                             choices = list("Left"="left","Center"="center","Right"="right"), selected = "center"),
                                 checkboxInput("rcbd_dist_striped_rows", "Striped rows", value = TRUE),
                                 checkboxInput("rcbd_dist_bordered", "Show borders", value = TRUE)
                               ),
                               bsCollapsePanel(
                                 title = tags$span(icon("download"), " Export Options"), value = "dist_export_panel", style = "success",
                                 selectInput("rcbd_dist_table_width", "Table width:",
                                             choices = list("Auto"="auto","100%"="100%","75%"="75%","50%"="50%"), selected = "auto"),
                                 sliderInput("rcbd_dist_table_height", "Max table height (px)", min = 300, max = 800, value = 500, step = 50),
                                 sliderInput("rcbd_dist_plot_height", "Row padding (drive via this)", min = 30, max = 200, value = 60, step = 10)
                               )
                             )
                           ),
                           inputId = "rcbd_drop_distribution_plot",
                           label = tagList(icon("sliders-h", style = "margin-right: 8px;"), "Plot Settings"),
                           status = "primary", icon = icon("gear"), circle = FALSE, size = "sm",
                           width = "350px", tooltip = tooltipOptions(title = "Click to customize plot appearance"), right = FALSE
                         )
                     ),
                     div(style = "position: relative; margin-top: 20px;",
                         gt_output("rcbd_distribution_plot_table") %>% withSpinner()
                     ),
                     div(style = "text-align: center; margin-top: 20px;",
                         downloadButton("download_rcbd_distribution_table",
                                        label = tagList(icon("file-image", style = "margin-right: 8px;"), "Download Distribution Plot"),
                                        class = "download-btn")
                     )
                   )
                 ),

                 # ~ Pair Plot ----
                 conditionalPanel(
                   condition = "output.activePlot1 == 'rcbd_pair'",
                   div(class = "plot-container",
                     h3(class = "plot-header", "Pair Plot"),
                     div(class = "left-dropdown-container",
                       dropdownButton(
                         div(
                           h5(style = "color: #667eea; font-weight: 600; margin-bottom: 12px;",
                              tagList(icon("sliders-h"), " Customize Your Plot")),
                           bsCollapse(
                             id = "rcbd_pp_collapse", open = NULL,
                             bsCollapsePanel(
                               title = tags$span(icon("layer-group"), " Data Settings"), value = "pp_data_panel", style = "primary",
                               selectInput("selected_factor_pairplot", "Select Factor:",
                                           choices = c("Treatment" = "A", "Day" = "B",
                                                       "Interaction (Treatment x Day)" = "AB"),
                                           selected = "A"),
                               awesomeCheckbox("rcbd_pp_corrshow", "Show treatmentwise corr", value = TRUE)
                             ),
                             bsCollapsePanel(
                               title = tags$span(icon("palette"), " Appearance"), value = "pp_theme_panel", style = "info",
                               selectInput("rcbd_pairplot_theme", "Statoberry themes:",
                                           list("normal","economist","minimal","gray","light","void","tufte","stata","wsj","calc","hc")),
                               sliderInput("rcbd_pplettersize", "Adjust letter size", value = 1.5, min = 1, max = 5)
                             )
                           )
                         ),
                         inputId = "rcbd_drop_pairsplot",
                         circle = FALSE, size = "sm", status = "primary",
                         label = "Plot Settings", icon = icon("cog"),
                         width = "400px",
                         tooltip = tooltipOptions(title = "Click to customize plot appearance")
                       )
                     ),
                     plotOutput("rcbd_corrplot") %>% withSpinner(),
                     tags$br(),
                     div(style = "text-align:center; margin-top:20px;",
                       downloadButton("download_corrplot",
                                      label = tagList(icon("file-image", style = "margin-right:8px;"), "Download Pair Plot"),
                                      class = "download-btn")
                     ),
                     tags$br()
                   )
                 ),

                 tags$br()
               )
      ),
      
      
      tabPanel("Individual ANOVA",
               icon = icon("list-alt"),
               tags$br(),
               conditionalPanel(
                 condition = "output.submitClicked == true",
                 uiOutput("indv_text"),
                 tags$br(),
                 uiOutput("char_selector_ui1"),
                 
      #            conditionalPanel(
      #              condition = "!output.anova_table", # Show only if outputs are not rendered
      #              div(
      #                style = "display: flex; justify-content: center; align-items: center; margin: 6px 0; gap: 10px;", # Added gap for spacing
      #                div(
      #                  style = "
      #   border: 4px solid #f3f3f3; /* Light grey */
      #   border-top: 4px solid #3498db; /* Blue */
      #   border-radius: 50%;
      #   width: 30px;
      #   height: 30px;
      #   animation: spin 1s linear infinite;
      # ",
      #                  tags$style(
      #                    HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }")
      #                  )
      #                ),
      #                span(
      #                  style = "color:#666; font-style: italic;",
      #                  "If your data is large, it may take some time to load...."
      #                )
      #              )
      #            ),
                 
                 tags$br(),
                 
                 fluidRow(
                   # column(
                   #   width = 6,  # Left column
                   #   withLoader(uiOutput("anova_table")),
                   #   tags$hr(),
                   #   uiOutput("grouping_Treatment"),
                   #   uiOutput("stat_summary_Treatment"),
                   #   uiOutput("grouping_Day"),
                   #   uiOutput("stat_summary_Day"),
                   #   uiOutput("grouping_TreatmentDay"),
                   #   uiOutput("stat_summary_TreatmentDay"),
                   #   tags$br(),
                   #   uiOutput("rcbd_var2")
                   # ),
                   # column(
                   #   width = 6,  # Right column
                     tags$script(HTML("
// ##arshi(15-08-26) jump to the decimals input from the CD rounding note
window.rcbdGoToDecimals = function(){

  var el = document.getElementById('rcbd_digit');

  if(!el){ return false; }

  var c = el.closest('.shiny-input-container') ||
          el.closest('.form-group') || el;

  c.scrollIntoView({behavior: 'smooth', block: 'center'});

  c.style.transition   = 'box-shadow 0.3s ease';
  c.style.borderRadius = '6px';
  c.style.boxShadow    = '0 0 0 3px #7c3aed, 0 0 18px 6px rgba(139,92,246,0.7)';

  try { el.focus(); el.select(); } catch(e) {}

  setTimeout(function(){ c.style.boxShadow = 'none'; }, 3000);

  return false;
};
")),
                     withLoader(uiOutput("indv_full_anova")),
                     tags$br(),
                     withLoader(uiOutput("pairwise_Treatment")),
                     tags$br(),
                     # withLoader(uiOutput("pairwise_Day")),
                     tags$br(),
                     # uiOutput("toggle_pair"),
                     tags$br(),
                     uiOutput("rcbd_var2"),
                     conditionalPanel(
                       condition = "input.pair_wise == true",
                       withLoader(uiOutput("pairwise_TD"))
                     ),
                     tags$hr()
                   # )
                 ),
                 
                 tags$br()
               )),
      
      ########## AI TABSET STARTING
      tabPanel("Interpretation",
               icon = icon("grav"),
               tags$br(),
               uiOutput("var_AI_path_intro_rcbd"),
               uiOutput("var_checkbox_path_rcbd"),
               uiOutput("var_AI_path_actionbttn_rcbd"),
               ### loading for AI
               
               tags$style("
  #loading-spinner_rcbd {
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
  
  #loading-text_rcbd {
    position: absolute;
    top: 70%;
    left: 50%;
    transform: translate(-50%, -50%);
    font-size: 20px;
    font-weight: bold;
    text-align: center;
    color: #3498db;
  }
  
  #output_path_typing_rcbd {
    white-space: pre-line;
  }
"),
               conditionalPanel(
                 condition = "input.aibttn_path_rcbd > 0 && document.getElementById('output_path_typing_rcbd').innerHTML == ''",
                 tags$div(id = "loading-spinner_rcbd"),
                 tags$div(id = "loading-text_rcbd", "Yes boss I'm reading your data, it will take few minutes, please wait...")
               ),
               tags$br(),
               conditionalPanel(
                 condition = "input.aibttn_path_rcbd > 0",
                 wellPanel(
                   uiOutput("AI_typing_path_rcbd"),
                   tags$div(id = "output_path_typing_rcbd"),
                   style = "background-color: #ffe8b8; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
                 )
               ),
               #### Loading end
               tags$br(),
               tags$br(),
               tags$br(),
               uiOutput("var_AI_path_end_rcbd"),
               tags$br(),
               tags$br()
      ),
      ######### AI tabset panel end
      
      ################################# Multivariate
      tabPanel("Multivariate",
               icon = icon("cubes"),
               tags$br(),
               conditionalPanel(
                 condition = "output.submitClicked == true",
                 uiOutput("rcbd_multi_intro"),
                 uiOutput("selected_variables")
                 
               ),
               tags$style("
  #loading-spinnermanova_rcbd {
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
  
  #loading-textmanova_rcbd {
    position: absolute;
    top: 70%;
    left: 50%;
    transform: translate(-50%, -50%);
    font-size: 20px;
    font-weight: bold;
    text-align: center;
    color: #3498db;
  }
  
  #output_pathmanova_typing_rcbd {
    white-space: pre-line;
  }
"),
               tags$br(),
               conditionalPanel(
                 condition = "output.submitClicked == true",
                 uiOutput("multivariate_bttns"),
                 tags$br(),
                 # uiOutput("manova_warning"),
                 # uiOutput("heading_manova"),
                 # tags$br(),
                 # dataTableOutput("rcbd_manova1") %>% withSpinner(),
                 # tags$br(),
                 # uiOutput("var_AI_manova_intro_rcbd"),
                 # uiOutput("var_checkbox_manova_rcbd"),
                 # uiOutput("var_AI_manova_actionbttn_rcbd"),
                 # conditionalPanel(
                 #   condition = "input.aibttn_manova_rcbd > 0 && document.getElementById('output_pathmanova_typing_rcbd').innerHTML == ''",
                 #   tags$div(id = "loading-spinnermanova_rcbd"),
                 #   tags$div(id = "loading-textmanova_rcbd", "Yes boss I'm reading your data, it will take few minutes, please wait...")
                 # ),
                 # tags$br(),
                 # conditionalPanel(
                 #   condition = "input.aibttn_manova_rcbd > 0",
                 #   wellPanel(
                 #     uiOutput("AI_typing_manova_rcbd"),
                 #     tags$div(id = "output_pathmanova_typing_rcbd"),
                 #     style = "background-color: #ffe8b8; border: 1px solid #ddd; border-radius: 3px; padding: 15px;"
                 #   )
                 # ),
                 # tags$br(),
                 
                 # PCA Outputs triggered by PCA button By Hisham
                 conditionalPanel(
                   condition = "input.rcbd_pca_bttn > 0",  # Check if PCA button has been clicked
                   uiOutput("heading_pca"),
                   tags$br(),
                   # PCA-related outputs
                   dataTableOutput("rcbd_pca1"),
                   tags$br(),
                   uiOutput("rcbd_eigen_abt"),
                   tags$br(),
                   ####################### Scree Plot Settings Dropdown by Hisham###################
                   
                   dropdownButton(
                     tags$div(
                       style = "color: #6f2c91; font-family: 'Times New Roman';",
                       tags$span(
                         tags$i(class = "fa fa-wrench", style = "margin-right: 8px;"),
                         tags$h4(
                           "Scree Plot Settings",
                           style = "font-family: 'Times New Roman'; font-size: small; color: #6f2c91;"
                         ),
                         style = "display: flex; align-items: center;" # Align icon and text horizontally
                       )
                     ),
                     inputId = "scree_plot_settings",
                     
                     # Organize inputs into collapsible sections
                     bsCollapse(
                       id = "scree_plot_settings_collapse",
                       open = NULL,
                       
                       ### Section 1: Plot Title & Theme
                       bsCollapsePanel(
                         title = "Title & Theme",
                         fluidRow(
                           column(6, textInput("title_scree", "Plot Title:", "Scree Plot")),
                           column(6, numericInput("title_size_rcbd_scree", "Title Size", value = 16, min = 12, max = 30))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("title_color_rcbd_scree", "Title Color", value = "#000000")),  # Title color
                           column(6, selectInput(
                             "scree_theme", "Select Theme:",
                             list(
                               "Normal" = "normal",
                               "Economist" = "economist",
                               "Minimal" = "minimal",
                               "Gray" = "gray",
                               "Light" = "light",
                               "Void" = "void",
                               "Tufte" = "tufte",
                               "Stata" = "stata",
                               "WSJ" = "wsj",
                               "Calc" = "calc",
                               "HC" = "hc"
                             ),
                             selected = "stata"
                           ))
                         )
                       ),
                       
                       ### Section 2: Colors (All Colors in One Section)
                       bsCollapsePanel(
                         title = "Colors",
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_scree_barfill", "Bar Fill Color", value = "#4682B4")),
                           column(6, colourpicker::colourInput("rcbd_scree_barcolor", "Bar Border Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_scree_linecolor", "Line Color", value = "#000000")),
                           column(6, colourpicker::colourInput("rcbd_scree_dotcolor", "Dot Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("background_color_rcbd_scree", "Background Color", value = "#FFFFFF"))
                         )
                       ),
                       
                       ### Section 3: Axis Customization
                       bsCollapsePanel(
                         title = "Axis Customization",
                         fluidRow(
                           column(6, numericInput("xlab_size_rcbd_scree", "X-axis Title Size", value = 14, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("xlab_color_rcbd_scree", "X-axis Title Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("ylab_size_rcbd_scree", "Y-axis Title Size", value = 14, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("ylab_color_rcbd_scree", "Y-axis Title Color", value = "#000000"))
                         ),
                         
                         fluidRow(
                           column(6, numericInput("xtick_size_rcbd_scree", "X-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("xtick_color_rcbd_scree", "X-axis Label Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("ytick_size_rcbd_scree", "Y-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("ytick_color_rcbd_scree", "Y-axis Label Color", value = "#000000"))
                         )
                       ),
                       
                       ### Section 4: Dot Customization
                       bsCollapsePanel(
                         title = "Dot Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_scree_dotsize", "Dot Size", value = 3, min = 1, max = 10)),
                           column(6, selectInput("rcbd_scree_dotshape", "Dot Shape",
                                                 choices = list(
                                                   "Circle (filled)" = 19,
                                                   "Circle (bordered)" = 21,
                                                   "Cross" = 4,
                                                   "Diamond" = 5,
                                                   "Plus" = 3
                                                 ),
                                                 selected = 19))
                         )
                       )
                     ),
                     
                     circle = TRUE,
                     size = "sm",
                     status = "danger",
                     icon = icon("gear"),
                     width = "400px",
                     tooltip = tooltipOptions(title = "Customize Scree Plot")
                   ),
                   tags$br(),
                   plotOutput("rcbd_PCA_screePlt") %>% withSpinner(),
                   downloadButton("download_scree_plot", "Download Scree Plot"),
                   
                   tags$br(),
                   dataTableOutput("rcbd_PCA_loadings"),
                   tags$br(),
                   uiOutput("rcbd_loadings_abt"),
                   tags$br(),
                   
                   ################## dropdown for biplot by HISHAM #############
                   dropdownButton(
                     tags$div(
                       style = "color: #6f2c91; font-family: 'Times New Roman';",
                       tags$span(
                         tags$i(class = "fa fa-wrench", style = "margin-right: 8px;"),
                         tags$h4(
                           "Biplot Settings",
                           style = "font-family: 'Times New Roman'; font-size: small; color: #6f2c91;"
                         ),
                         style = "display: flex; align-items: center;" # Align icon and text horizontally
                       )
                     ),
                     inputId = "rcbd_biplot_settings",
                     
                     # Organize inputs into collapsible sections
                     bsCollapse(
                       id = "rcbd_biplot_collapse",
                       open = NULL,
                       
                       ### Section 1: Title & Theme
                       bsCollapsePanel(
                         title = "Title & Theme",
                         fluidRow(
                           column(6, textInput("rcbd_biplot_title", "Plot Title:", "PCA-Biplot")),
                           column(6, numericInput("rcbd_biplot_title_size", "Title Size", value = 16, min = 12, max = 30))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_biplot_title_color", "Title Color", value = "#000000")),
                           column(6, selectInput(
                             "rcbd_biplot_theme", "Select Theme:",
                             list(
                               "Normal" = "normal",
                               "Economist" = "economist",
                               "Minimal" = "minimal",
                               "Gray" = "gray",
                               "Light" = "light",
                               "Void" = "void",
                               "Tufte" = "tufte",
                               "Stata" = "stata",
                               "WSJ" = "wsj",
                               "Calc" = "calc",
                               "HC" = "hc"
                             ),
                             selected = "stata"
                           ))
                         ),
                         fluidRow(
                           column(6, selectInput(
                             "rcbd_biplot_label", 
                             "Elements to Label",  
                             choices = c(
                               "All" = "all",  
                               "None" = "none", 
                               "Individuals Only" = "ind", 
                               "Variables Only" = "var"
                             ), 
                             selected = "all"
                           )),
                           column(6, selectInput(
                             "rcbd_biplot_invisible", 
                             "Elements to Hide",  
                             choices = c(
                               "None" = "none",  
                               "Individuals Only" = "ind", 
                               "Variables Only" = "var"
                             ), 
                             selected = "none"
                           ))
                           
                         ),
                         fluidRow(
                           column(12, checkboxInput("rcbd_biplot_repel", "Repel Labels", value = TRUE))
                         )
                       ),
                       
                       ### Section 2: Axis Customization
                       bsCollapsePanel(
                         title = "Axis Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_biplot_xlab_size", "X-axis Label Size", value = 14, min = 8, max = 30)),
                           column(6, numericInput("rcbd_biplot_ylab_size", "Y-axis Label Size", value = 14, min = 8, max = 30))
                         )
                       ),
                       
                       ### Section 3: Colors
                       bsCollapsePanel(
                         title = "Colors",
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_biplot_var_color", "Variable Arrow Color", value = "#a3a198")),
                           column(6, colourpicker::colourInput("rcbd_biplot_ind_color", "Individual Color", value = "#a905f5"))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_biplot_background_color", "Background Color", value = "#FFFFFF"))
                         )
                       ),
                       
                       ### Section 4: Size & Transparency
                       bsCollapsePanel(
                         title = "Size & Transparency",
                         
                         fluidRow(
                           column(6, sliderInput("rcbd_biplot_alpha_ind", "Individual Transparency", min = 0.1, max = 1, value = 1)),
                           column(6, sliderInput("rcbd_biplot_alpha_var", "Variable Transparency", min = 0.1, max = 1, value = 1))
                         ),
                         fluidRow(
                           column(6, sliderInput("rcbd_biplot_pointsize", "Individual Point Size", min = 1, max = 10, value = 3)),
                           column(6, sliderInput("rcbd_biplot_arrowsize", "Variable Arrow Size", min = 0.1, max = 2, value = 0.5, step = 0.1))
                         ),
                         fluidRow(
                           column(12, sliderInput("rcbd_biplot_labelsize", "Label Size", min = 1, max = 10, value = 3))
                         )
                       )
                       
                       # ### Section 5: Ellipses
                       # bsCollapsePanel(
                       #   title = "Ellipses",
                       #   fluidRow(
                       #     column(6, checkboxInput("rcbd_biplot_add_ellipses", "Add Ellipses", value = FALSE)),
                       #     column(6, sliderInput("rcbd_biplot_ellipse_level", "Ellipse Confidence Level", min = 0.5, max = 0.99, value = 0.95))
                       #   ),
                       #   fluidRow(
                       #   column(6, numericInput("rcbd_biplot_k_clusters", 
                       #                "Number of Clusters:", 
                       #                value = 3, 
                       #                min = 2, 
                       #                max = 10))
                       #   )
                       #   
                       #   
                       # )
                     ),
                     
                     circle = TRUE,
                     size = "sm",
                     status = "warning",
                     icon = icon("gear"),
                     width = "400px",
                     tooltip = tooltipOptions(title = "Customize PCA Biplot")
                   ),
                   
                   tags$br(),
                   tags$br(),
                   plotOutput("rcbd_biplot") %>% withSpinner(),
                   
                   tags$br(),
                   tags$br(),
                   downloadButton("download_rcbd_biplot", "Download Biplot"),
                   tags$br(),
                   uiOutput("rcbd_indexscore_abt"),
                   tags$br(),
                   selectInput("rcbd_pcaCutoff", "Select cutoff for Scaled Indexscore:", 
                               choices = c("0.50", "0.75", "0.80", "0.90", "0.95"), selected = "0.75"),
                   uiOutput("rcbd_pca_varselect"),
                   tags$br(),
                   dataTableOutput("rcbd_PCA_index1"),
                   tags$br(),
                   # #######################dropdown for plot based on pc1 by HISHAM #######
                   dropdownButton(
                     tags$div(
                       style = "color: #6f2c91; font-family: 'Times New Roman';",
                       tags$span(
                         tags$i(class = "fa fa-wrench", style = "margin-right: 8px;"),
                         tags$h4(
                           "Index Plot Settings",
                           style = "font-family: 'Times New Roman'; font-size: small; color: #6f2c91;"
                         ),
                         style = "display: flex; align-items: center;" # Align icon and text horizontally
                       )
                     ),
                     inputId = "rcbd_Indexplot1_settings",
                     
                     # Organize inputs into collapsible sections
                     bsCollapse(
                       id = "rcbd_Indexplot1_collapse",
                       open = NULL,
                       
                       ### Section 1: Title & Theme
                       bsCollapsePanel(
                         title = "Title & Theme",
                         fluidRow(
                           column(6, textInput("rcbd_Indexplot1_title", "Plot Title:", "Index Plot based on PC1")),
                           column(6, numericInput("rcbd_Indexplot1_title_size", "Title Size", value = 16, min = 12, max = 30))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_title_color", "Title Color", value = "#000000")),
                           column(6, selectInput(
                             "rcbd_Indexplot1_theme", "Select Theme:",
                             list(
                               "Normal" = "normal",
                               "Economist" = "economist",
                               "Minimal" = "minimal",
                               "Gray" = "gray",
                               "Light" = "light",
                               "Void" = "void",
                               "Tufte" = "tufte",
                               "Stata" = "stata",
                               "WSJ" = "wsj",
                               "Calc" = "calc",
                               "HC" = "hc"
                             ),
                             selected = "minimal"
                           ))
                         )
                       ),
                       
                       ### Section 2: Colors
                       bsCollapsePanel(
                         title = "Colors",
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_sel_color", "Selected Color", value = "red")),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_non_sel_color", "Nonselected Color", value = "grey"))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_cutoff_line_color", "Cutoff Line Color", value = "red")),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_major_ring_color", "Ring Color", value = "lightgray"))
                         ),
                         fluidRow(
                           column(12, colourpicker::colourInput("rcbd_Indexplot1_background_color", "Background Color", value = NA))
                         )
                       ),
                       
                       ### Section 3: Dot Settings
                       bsCollapsePanel(
                         title = "Dot Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot1_dot_size", "Dot Size", value = 3, min = 1, max = 10)),
                           column(6, selectInput(
                             inputId = "rcbd_Indexplot1_dot_shape",
                             label = "Dot Shape",
                             choices = list(
                               "Hollow Circle" = 1,
                               "Filled Circle" = 16,
                               "Hollow Square" = 0,
                               "Filled Square" = 15,
                               "Hollow Triangle Up" = 2,
                               "Filled Triangle Up" = 17,
                               "Plus Sign" = 3,
                               "Cross Sign" = 4,
                               "Diamond" = 5,
                               "Filled Diamond" = 18
                             ),
                             selected = 16  # Default selection
                           )
                           )
                         )
                       ),
                       
                       ### Section 4: Axis Customization
                       bsCollapsePanel(
                         title = "Axis Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot1_xaxis_size", "X-axis Title Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_xaxis_color", "X-axis Title Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot1_xlab_size", "X-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_xlab_color", "X-axis Label Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot1_yaxis_size", "Y-axis Title Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_yaxis_color", "Y-axis Title Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot1_ylab_size", "Y-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot1_ylab_color", "Y-axis Label Color", value = "#000000"))
                         )
                       )
                     ),
                     
                     circle = TRUE,
                     size = "sm",
                     status = "info",
                     icon = icon("gear"),
                     width = "400px",
                     tooltip = tooltipOptions(title = "Customize Index Plot 1")
                   ),
                   tags$br(),
                   #######################
                   plotOutput("rcbd_Indexplot1")%>% withSpinner(),
                   downloadButton("download_rcbd_Indexplot1", "Download IndexPlot1"),
                   tags$br(),
                   dataTableOutput("rcbd_PCA_index2"),
                   tags$br(),
                   
                   ##################plot based on pc2 by HISHAM
                   ### Dropdown for Index Plot Based on PC2
                   dropdownButton(
                     tags$div(
                       style = "color: #6f2c91; font-family: 'Times New Roman';",
                       tags$span(
                         tags$i(class = "fa fa-wrench", style = "margin-right: 8px;"),
                         tags$h4(
                           "Index Plot Settings (PC2)",
                           style = "font-family: 'Times New Roman'; font-size: small; color: #6f2c91;"
                         ),
                         style = "display: flex; align-items: center;" # Align icon and text horizontally
                       )
                     ),
                     inputId = "rcbd_Indexplot2_settings",
                     
                     # Organize inputs into collapsible sections
                     bsCollapse(
                       id = "rcbd_Indexplot2_collapse",
                       open = NULL,
                       
                       ### Section 1: Title & Theme
                       bsCollapsePanel(
                         title = "Title & Theme",
                         fluidRow(
                           column(6, textInput("rcbd_Indexplot2_title", "Plot Title:", "Index Plot based on PC2")),
                           column(6, numericInput("rcbd_Indexplot2_title_size", "Title Size", value = 16, min = 12, max = 30))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_title_color", "Title Color", value = "#000000")),
                           column(6, selectInput(
                             "rcbd_Indexplot2_theme", "Select Theme:",
                             list(
                               "Normal" = "normal",
                               "Economist" = "economist",
                               "Minimal" = "minimal",
                               "Gray" = "gray",
                               "Light" = "light",
                               "Void" = "void",
                               "Tufte" = "tufte",
                               "Stata" = "stata",
                               "WSJ" = "wsj",
                               "Calc" = "calc",
                               "HC" = "hc"
                             ),
                             selected = "minimal"
                           ))
                         )
                       ),
                       
                       ### Section 2: Colors
                       bsCollapsePanel(
                         title = "Colors",
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_sel_color", "Selected Color", value = "red")),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_non_sel_color", "Nonselected Color", value = "grey"))
                         ),
                         fluidRow(
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_cutoff_line_color", "Cutoff Line Color", value = "red")),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_major_ring_color", "Ring Color", value = "lightgray"))
                         ),
                         fluidRow(
                           column(12, colourpicker::colourInput("rcbd_Indexplot2_background_color", "Background Color", value = NA))
                         )
                       ),
                       
                       ### Section 3: Dot Settings
                       bsCollapsePanel(
                         title = "Dot Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot2_dot_size", "Dot Size", value = 3, min = 1, max = 10)),
                           column(6, selectInput(
                             inputId = "rcbd_Indexplot2_dot_shape",
                             label = "Dot Shape",
                             choices = list(
                               "Hollow Circle" = 1,
                               "Filled Circle" = 16,
                               "Hollow Square" = 0,
                               "Filled Square" = 15,
                               "Hollow Triangle Up" = 2,
                               "Filled Triangle Up" = 17,
                               "Plus Sign" = 3,
                               "Cross Sign" = 4,
                               "Diamond" = 5,
                               "Filled Diamond" = 18
                             ),
                             selected = 16
                           ))
                         )
                       ),
                       
                       ### Section 4: Axis Customization
                       bsCollapsePanel(
                         title = "Axis Customization",
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot2_xaxis_size", "X-axis Title Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_xaxis_color", "X-axis Title Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot2_xlab_size", "X-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_xlab_color", "X-axis Label Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot2_yaxis_size", "Y-axis Title Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_yaxis_color", "Y-axis Title Color", value = "#000000"))
                         ),
                         fluidRow(
                           column(6, numericInput("rcbd_Indexplot2_ylab_size", "Y-axis Label Size", value = 12, min = 8, max = 30)),
                           column(6, colourpicker::colourInput("rcbd_Indexplot2_ylab_color", "Y-axis Label Color", value = "#000000"))
                         )
                       )
                     ),
                     
                     circle = TRUE,
                     size = "sm",
                     status = "success",
                     icon = icon("gear"),
                     width = "400px",
                     tooltip = tooltipOptions(title = "Customize Index Plot 2")
                   ),
                   tags$br(),
                   
                   plotOutput("rcbd_Indexplot2")%>% withSpinner(),
                   downloadButton("download_rcbd_Indexplot2", "Download IndexPlot2")
                 ),
                 ############################end by Hisham############################################         
                 
                 tags$br(),
                 uiOutput("instr_pca"),
                 tags$br(),
                 tags$br()
               )
      )
      ## FAQs and View Data are now top-level tabs supplied to
      ## greyskull::raisins_app_ui() (uniform across all apps), not sub-tabs here.
    )
    
    
    ## Chat bubble----  ##(updation March)
    ,tags$style(HTML("
#raone-bubble {
  position: fixed;
  bottom: 28px;
  right: 28px;
  width: 68px;
  height: 68px;
  border-radius: 50%;
  background: #560466;
  box-shadow: 0 6px 20px rgba(86,4,102,0.55), 0 0 18px rgba(193,139,204,0.45);
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  z-index: 2147483647;
  border: none;
  outline: none;
  -webkit-tap-highlight-color: transparent;
  isolation: isolate;
  animation: raone-float 6s ease-in-out infinite;
  display: none !important;
}
#raone-bubble.raone-visible {
  display: flex !important;
}

@keyframes raone-float {
  0%   { transform: translateY(0) rotate(0deg); }
  70%  { transform: translateY(0) rotate(0deg); }
  75%  { transform: translateY(-3px) rotate(-4deg); }
  80%  { transform: translateY(0) rotate(4deg); }
  85%  { transform: translateY(-3px) rotate(-3deg); }
  90%  { transform: translateY(0) rotate(2deg); }
  100% { transform: translateY(0) rotate(0deg); }
}
#raone-bubble:hover, #raone-bubble:focus-visible {
  animation-play-state: paused;
  box-shadow: 0 8px 28px rgba(86,4,102,0.65), 0 0 22px rgba(193,139,204,0.55);
}
#raone-bubble::after {
  position: absolute;
  right: 78px;
  bottom: 14px;
  background: #2d1a30;
  color: #f2eaf5;
  padding: 7px 13px;
  border-radius: 8px;
  font-size: 13px;
  font-family: Arial, sans-serif;
  white-space: nowrap;
  pointer-events: none;
  box-shadow: 0 2px 10px rgba(0,0,0,0.25);
  content: 'Hi! I am RA-One';
  opacity: 0;
  transform: translateY(8px);
  transition: opacity 0.25s ease, transform 0.25s ease;
}
#raone-bubble::before {
  content: '';
  position: absolute;
  right: 72px;
  bottom: 22px;
  border: 6px solid transparent;
  border-left-color: #2d1a30;
  pointer-events: none;
  opacity: 0;
  transition: opacity 0.25s ease;
}
#raone-bubble:hover::after, #raone-bubble:focus-visible::after {
  content: 'Hi! I am RA-One';
  opacity: 1;
  transform: translateY(0);
}
#raone-bubble:hover::before, #raone-bubble:focus-visible::before { opacity: 1; }
@keyframes raone-tooltip-auto {
  0%,  68% { opacity: 0; transform: translateY(8px); }
  73%, 87% { opacity: 1; transform: translateY(0); }
  100%     { opacity: 0; transform: translateY(8px); }
}
#raone-bubble::after { animation: raone-tooltip-auto 6s ease-in-out infinite; }
#raone-bubble:hover::after, #raone-bubble:focus-visible::after {
  animation: none !important; opacity: 1 !important; transform: translateY(0) !important;
}
#raone-bubble img {
  width: 40px; height: 50px; pointer-events: none;
  filter: drop-shadow(0 1px 3px rgba(0,0,0,0.35));
}
@media (prefers-reduced-motion: reduce) {
  #raone-bubble, #raone-bubble::after { animation: none !important; }
}
  ")),
    
    tags$div(
      id           = "raone-bubble",
      role         = "button",
      tabindex     = "0",
      `aria-label` = "Open RA-One AI Assistant",
      tags$img(
        src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_GENERAL%2Fbot.webp?alt=media",
        alt = "RA-One"
      )
    ),
    
    tags$script(HTML("
// Show bubble only after login
Shiny.addCustomMessageHandler('show_raone_bubble', function(msg) {
  var el = document.getElementById('raone-bubble');
  if (el) { el.classList.add('raone-visible'); }
});
")),
    
    
    
    tags$script(HTML("
(function () {
  function openRAOne() {
    var link = document.querySelector('a[data-value=\"RA-One\"]');
    if (link) { link.click(); return; }
    if (window.Shiny) {
      Shiny.setInputValue('raone_bubble_click', Math.random(), {priority: 'event'});
    }
  }
  document.addEventListener('DOMContentLoaded', function () {
    var el = document.getElementById('raone-bubble');
    if (!el) return;
    el.addEventListener('click', openRAOne);
    el.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); openRAOne(); }
    });
  });
})();
  ")),
  )
}
