library(rhandsontable)

rcbd_gen_server = function(input, output, session) {
  observeEvent(session$input$rcbd_logout, {
    session$reload()
  })
  
  # Reactive value to store the generated data frame
  df_reactive <- reactiveVal(NULL)
  
  ############# Instruction appearing in sidebar panel
  output$experiment_instructions <- renderUI({
    if (is.null(input$submit_csvgen_rcbd) || input$submit_csvgen_rcbd == 0) {
      HTML(
        "<p style='color: blue; font-weight: bold; font-size: 12px; text-align: justify;'>
        Please fill in the details of your experiment below. Once you provide the required information, a new panel will appear on the right where you can enter your data and download it as a CSV file. This CSV file can then be uploaded in the Analysis tab.
      </p>"
      )
    }
  })
  
  # Observe event for generating the data frame
  observeEvent(input$submit_csvgen_rcbd, {
    shiny::validate(
      need(input$treatment_csv_gen_rcbd != 0, "Warning 1: Please enter the number of subjects correctly, it should be at least one"),
      need(input$daytime_csv_gen_rcbd != 0, "Warning 2: Please enter the number of day/time points correctly, it should be more than 1"),
      need(input$response_csv_gen_rcbd != 0, "Warning 3: Please enter the number of variables correctly, it should be at least one")
    )

    # Calculate total observations
    total_obs <- input$treatment_csv_gen_rcbd * input$daytime_csv_gen_rcbd

    # Create the data frame: Subject, Day/Time, Variables
    # One-way repeated measures: one row per Subject x Day/Time (no block column)
    df <- data.frame(
      Subject = rep(paste0("T", 1:input$treatment_csv_gen_rcbd), each = input$daytime_csv_gen_rcbd),
      DayTime = rep(paste0("D", 1:input$daytime_csv_gen_rcbd), times = input$treatment_csv_gen_rcbd),
      stringsAsFactors = FALSE
    )
    
    
    for (i in 1:input$response_csv_gen_rcbd) {
      colname <- paste0("y", i)
      df[[colname]] <- as.numeric(rep(NA, total_obs))
    }
    
    # Store the data frame in reactive value
    df_reactive(df)
    
    # Text appear above CSV panel
    output$text_csv_gen_rcbd <- renderUI({
      HTML(
        "<p style='font-family: Times New Roman; color: green; font-weight: bold; font-size: 14px; text-align: justify;'> 
        In the panel below, enter the responses with Subject (identifies Subject groups), Day/Time (day/time column), and observations (y1, y2, etc.). You can type the response directly or copy the data from anywhere including MS Excel by copying and pasting using ctrl+v. Once you are done, click the download button to save the data as a CSV file.
        </p>"
      )
    })
    
    # Text appear below download button
    output$text2_csv_gen_rcbd <- renderUI({
      HTML(
        "<p style='font-family: Times New Roman; color: green; font-weight: bold; font-size: 14px; text-align: justify;'> 
        You can download the CSV file and upload in the analysis tab to perform analysis.
        </p>"
      )
    })
    
    output$toggle_rcbd_gen <- renderUI({
      prettyToggle(
        inputId = "rcbd_gen_order",
        label_on = "Sort by Subject-Day/Time",
        icon_on = icon("sort-up"),
        status_on = "info",
        status_off = "warning",
        label_off = "Sort by Day/Time-Subject",
        icon_off = icon("sort-down"),
        value = TRUE
      )
    })
  })
  
  # Observe event for the toggle and update sorting
  observeEvent(input$rcbd_gen_order, {
    if (!is.null(df_reactive())) {
      df <- df_reactive()
      
      # Sort columns correctly
      df$Subject <- factor(df$Subject, levels = gtools::mixedsort(unique(df$Subject)))
      df$DayTime <- factor(df$DayTime, levels = gtools::mixedsort(unique(df$DayTime)))
      
      # Apply ordering based on input
      if (isTRUE(input$rcbd_gen_order)) {
        # Primary sort: Subject, then Day/Time
        df <- df[order(df$Subject, df$DayTime), ]
      } else {
        # Alternative sort: Day/Time, then Subject
        df <- df[order(df$DayTime, df$Subject), ]
      }
      
      # Convert back to character to avoid sorting arrow issue
      df$Subject <- as.character(df$Subject)
      df$DayTime <- as.character(df$DayTime)
      
      # Store back into reactive value
      df_reactive(df)
    }
  })
  
  # Render data table using the reactive data frame
  output$data_entry_table_rcbd <- renderRHandsontable({
    if (is.null(df_reactive())) {
      return(NULL)
    }
    rhandsontable(df_reactive(), rowHeaders = NULL) %>%
      hot_col("Subject", readOnly = TRUE, renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.backgroundColor = '#E8F4FD';
          td.style.fontWeight = 'bold';
        }") %>%
      hot_col("DayTime", readOnly = TRUE, renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.backgroundColor = '#FFF2CC';
          td.style.fontWeight = 'bold';
        }")
  })
  
  # Output for download button
  output$var_csv_gen_rcbd <- renderUI({
    if (is.null(df_reactive())) {
      return(NULL)
    }
    div(
      style = "text-align: center; margin-top: 15px;",
      downloadButton("download_csvgen_rcbd", "Download CSV file", class = "btn-success")
    )
  })
  
  # Download data as CSV file
  output$download_csvgen_rcbd <- downloadHandler(
    filename = function() {
      paste("data_rmanova_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_rbd <- hot_to_r(input$data_entry_table_rcbd)
      data_rbd[is.na(data_rbd)] <- ""
      write.csv(data_rbd, file, row.names = FALSE)
    }
  )
}