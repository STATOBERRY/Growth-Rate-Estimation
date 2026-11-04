rcbd_dataset_server = function(input, output, session, error_support=NULL) {
  if(!is.null(error_support)) {
    observe <- error_support$observe
    observeEvent <- error_support$observeEvent
    downloadHandler <- error_support$downloadHandler
  }
  
  output$fruit_healthdata_intro <- renderUI({
    HTML(
      '<div style="font-family: Arial, sans-serif; background-color: #ffffff; padding: 20px; margin: 0 auto; max-width: 800px;">
    <h2 style="color: #2e7d32; text-align: center;">Model Datasets</h2>
    <p>Below are example datasets that illustrate how to arrange <strong>time-series</strong> data for the Growth Rate Estimation module. Structure your own file in a similar format for smooth analysis. You can upload any of these directly into the Analysis tab to test the app.</p>
    <h3 style="color: #333;">Dataset 1: Production &amp; Area (national series)</h3>
    <p>A single annual series with one <strong>Year</strong> column followed by numeric columns <strong>Area</strong> (&#39;000 ha), <strong>Production</strong> (million nuts) and <strong>Yield</strong> (nuts/ha), one row per year (2001&ndash;2023).</p>
    <p>Keep column names simple and free of spaces or special characters. Having columns named <strong>Area</strong> and <strong>Production</strong> enables the automatic <strong>area-yield decomposition</strong>. You can download this dataset as CSV using the link below:</p>
    </div>'
    )
  })
  
  fruit_health_data <- read.csv("data/dataset1.csv", row.names = NULL)
  
  output$download_fruit_health_data <- downloadHandler(
    filename = function() {
      paste("fruit_health_data", Sys.Date(), ".csv", sep="")  # The name of the file to be downloaded
    },
    content = function(file) {
      write.csv(fruit_health_data, file, row.names = FALSE) # Copy the dataset from your app data folder to the temporary file
    }
  )
  
  output$storage_data_intro <- renderUI({
    HTML(
      '<div style="font-family: Arial, sans-serif; background-color: #ffffff; padding: 20px; margin: 0 auto; max-width: 800px;">
    <h3 style="color: #333;">Dataset 2: District Panel</h3>
    <p>A panel with a <strong>Year</strong> column, a <strong>District</strong> grouping column (North, Central, South) and numeric <strong>Area</strong> and <strong>Production</strong> columns, one row per district per year.</p>
    <p>When you choose the District column as the optional group filter in the Analysis tab, the app aggregates production and area across districts by year before estimating growth. You can download this dataset as CSV using the link below:</p>
    </div>'
    )
  })
  
  
  storage_data <- read.csv("data/dataset2.csv", row.names = NULL)
  
  output$download_storage_data <- downloadHandler(
    filename = function() {
      paste("storage_data", Sys.Date(), ".csv", sep="")  # The name of the file to be downloaded
    },
    content = function(file) {
      write.csv(storage_data, file, row.names = FALSE) # Copy the dataset from your app data folder to the temporary file
    }
  )  
  
  output$soildata_intro <- renderUI({
    HTML(
      '<div style="font-family: Arial, sans-serif; background-color: #ffffff; padding: 20px; margin: 0 auto; max-width: 800px;">
    <h3 style="color: #333;">Dataset 3: Yield Series</h3>
    <p>A minimal two-column series &mdash; a <strong>Year</strong> column and a single numeric <strong>Yield</strong> column (nuts/ha), one row per year.</p>
    <p>Use this to try trend fitting, the Mann-Kendall test and the instability indices on a single variable. You can download this dataset as CSV using the link below:</p>
    </div>'
    )
  })
  
  soil_data <- read.csv("data/dataset3.csv", row.names = NULL)
  
  output$download_soil_data <- downloadHandler(
    filename = function() {
      paste("soil_data", Sys.Date(), ".csv", sep="")  # The name of the file to be downloaded
    },
    content = function(file) {
      write.csv(soil_data, file, row.names = FALSE) # Copy the dataset from your app data folder to the temporary file
    }
  )     
  
  
  
  
  
  
  
  
  
}