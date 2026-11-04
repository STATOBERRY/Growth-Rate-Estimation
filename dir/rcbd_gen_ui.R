rcbd_gen_ui <- function(){
  
  fluidPage(
    titlePanel(tags$div(tags$b('CSV data file creator', style = "color:#000000"))),
    fab_button(
      list(
        inputId = "rcbd_logout",
        icon = icon("sign-out"),
        label = NULL
      ),
      position = "top-right",
      animation = "zoomin",
      toggle = "hover",
      inputId = NULL
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput("experiment_instructions"),
        tags$br(),
        
        # 1. Treatment
        numericInput("treatment_csv_gen_rcbd", 
                     tags$div(tags$b('Enter the number of subjects:'), 
                              tags$br(), 
                              tags$small("(Select the column with subjects)"),
                              style = "color:#8B0000"), 
                     value = 2, min = 2, max = 100),
        
        # 2. Day/Time
        numericInput("daytime_csv_gen_rcbd", 
                     tags$div(tags$b('Enter the number of day/time points:'), 
                              tags$br(), 
                              tags$small("(Select the column that identifies day/time points)"),
                              style = "color:#0c1687"), 
                     value = 2, min = 2, max = 100),
        
        # 3. Variables
        numericInput("response_csv_gen_rcbd",
                     tags$div(tags$b('Enter the number of variables:'), 
                              tags$br(), 
                              tags$small("(You can select all the variables for analysis at once)"),
                              style = "color:#87560c"), 
                     value = 1, min = 1, max = 100),
        
        tags$br(),
        
        div(
          style = "text-align: center;",
          shinyWidgets::actionBttn(
            inputId = "submit_csvgen_rcbd",
            label = "Create",
            color = "success",
            style = "unite"
          )
        ),
        
        tags$br(),
        tags$br(),
        style = "background-color: #fff7db;" # change sidebar panel color
      ),
      mainPanel(
        tabsetPanel(
          type = "tab",
          tabPanel("Data entry Panel",
                   tags$br(),
                   uiOutput("text_csv_gen_rcbd"),
                   tags$br(),
                   uiOutput("toggle_rcbd_gen"),
                   rHandsontableOutput("data_entry_table_rcbd"),
                   tags$br(),
                   tags$br(),
                   uiOutput("var_csv_gen_rcbd"),
                   tags$br(),
                   uiOutput("text2_csv_gen_rcbd"),
                   tags$br(),
                   tags$br(),
                   tags$br()
          ),
          tabPanel(
            title = "Help me",
            icon = icon("question-circle"),
            value = "Help_rcbd",
            HTML(paste0(
              "<p style='color: #345915; font-weight: bold; font-size: 14px; text-align: justify;'> 
This is a user-friendly platform where you can generate CSV files in the required format for analysis which can be used to upload and perform analysis.
</p>
<p style='color: #a65a03; font-weight: bold; font-size: 14px; text-align: justify;'>
Enter the number of subjects, day/time points, and variables you want to work with in the sidebar panel. Upon submission, users can view a model format in the main panel, where you can input the responses by clicking in the cell. </p>
<p style='color: #510487; font-weight: bold; font-size: 14px; text-align: justify;'>
The data frame includes subjects column (identifies subjects), Day/Time column (with day/time points), and observation variables (y1, y2, etc.). You can simply copy observations directly from Excel and paste using Ctrl+V.
</p>
<p style='color: #5f036e; font-weight: bold; font-size: 14px; text-align: justify;'>
After completion of data entry download the CSV file and upload in the analysis tab and complete your analysis.
</p>
<p style='color: #593e15; font-weight: bold; font-size: 14px; text-align: justify;'>
This is designed to make data entry and manipulation more efficient and straightforward, which saves users time and effort. Try this now and experience a faster and more convenient way of handling your experimental data!
</p>"
            ))
          )
        )
      )
    )
  )
}