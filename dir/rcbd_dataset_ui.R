doe_dataset_link <- function(outputId, label, ...) {
  shiny::downloadLink(outputId, htmltools::tagList(shiny::icon("download"), label),
    class = "doe-dataset-link", ...)
}
rcbd_dataset_ui <- function(){
  
  fluidPage(
    tags$style(HTML(".doe-dataset-link {display:inline-block;font-size:22px;font-weight:700;color:#4CAF50!important;text-decoration:none!important;border-bottom:3px solid #4CAF50;padding-bottom:4px;margin:10px 0 35px;} .doe-dataset-link:hover,.doe-dataset-link:focus {color:#2E7D32!important;border-color:#2E7D32;} .doe-dataset-link .fa {margin-right:10px;}")),
    
    uiOutput("fruit_healthdata_intro"),
    div(style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        doe_dataset_link("download_fruit_health_data", "Download Production & Area Data (CSV)")
    ),
    uiOutput("storage_data_intro"),
    div(style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        doe_dataset_link("download_storage_data", "Download District Panel Data (CSV)")
    ),
    uiOutput("soildata_intro"),
    div(style = "display: flex; justify-content: center; align-items: center; height: 100%;",
        doe_dataset_link("download_soil_data", "Download Yield Series Data (CSV)")
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br()
    
    
    
    
  )
  
  
  
  
  
  
}
