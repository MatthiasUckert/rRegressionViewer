#' Create App UI
#'
#' Generates the complete Shiny UI for the regression comparison app
#'
#' @param .app_data List with 'data' and 'metadata' elements
#' @return Shiny UI
#' @keywords internal
app_ui <- function(.app_data) {
  
  tab_names <- names(.app_data$data)
  
  # Create tabs dynamically
  tabs <- purrr::map2(
    tab_names,
    .app_data$metadata,
    function(.name, .meta) {
      create_table_tab(.tab_name = .name, .metadata = .meta)
    }
  )
  
  shiny::fluidPage(
    # Custom CSS
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Base styles */
        .navbar {
          margin-bottom: 0px;
        }
        body {
          padding-top: 60px;
          font-family: 'Times New Roman', Times, serif;
        }
        h4, h5 {
          color: #2c3e50;
          font-weight: 600;
          font-family: 'Times New Roman', Times, serif;
        }
        
        /* Button styles */
        .btn-warning {
          background-color: #f39c12;
          border-color: #f39c12;
          color: white;
        }
        .btn-warning:hover {
          background-color: #e67e22;
          border-color: #e67e22;
        }
        .btn-info {
          background-color: #3498db;
          border-color: #3498db;
          color: white;
        }
        .btn-info:hover {
          background-color: #2980b9;
          border-color: #2980b9;
        }
        
        /* Modal styles */
        .modal-header {
          background-color: #3498db;
          color: white;
        }
        .modal-title {
          color: white;
        }
        
        /* Highlighted column styles */
        .column-highlighted {
          font-weight: bold !important;
          border-left: 3px solid #3498db !important;
          border-right: 3px solid #3498db !important;
          background-color: #e8f4f8 !important;
        }
        
        /* Clickable column headers */
        .clickable-column {
          transition: background-color 0.2s;
        }
        .clickable-column:hover {
          background-color: #ddd !important;
        }
      "))
    ),
    
    # Title
    shiny::titlePanel(
      title = "Regression Viewer",
      windowTitle = "Regression Viewer"
    ),
    
    shiny::br(),
    
    # Tab navigation
    do.call(shiny::tabsetPanel, c(list(id = "main_tabs", type = "tabs"), tabs))
  )
}