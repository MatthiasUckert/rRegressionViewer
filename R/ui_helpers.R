#' Create Column Selection Modal
#'
#' Creates a modal dialog for selecting visible columns
#'
#' @param .tab_name Character. Name of the current tab
#' @param .model_cols Character vector of model column names
#' @param .current_selection Character vector of currently selected columns
#' @return Shiny modal dialog
#' @keywords internal
create_column_modal <- function(.tab_name, .model_cols, .current_selection) {
  
  shiny::modalDialog(
    title = shiny::tagList(
      shiny::icon("eye"),
      " Show/Hide Columns"
    ),
    size = "l",
    
    # Control buttons
    shiny::div(
      style = "margin-bottom: 15px;",
      shiny::actionButton(
        inputId = paste0(.tab_name, "_select_all_modal"),
        label = "Select All",
        style = "margin-right: 5px;",
        class = "btn-sm btn-primary"
      ),
      shiny::actionButton(
        inputId = paste0(.tab_name, "_select_none_modal"),
        label = "Select None",
        class = "btn-sm btn-secondary"
      )
    ),
    
    # Checkboxes in scrollable area with multiple columns
    shiny::div(
      style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 15px; border-radius: 4px; column-count: 3; column-gap: 20px;",
      shiny::checkboxGroupInput(
        inputId = paste0(.tab_name, "_visible_cols"),
        label = NULL,
        choices = .model_cols,
        selected = .current_selection,
        width = "100%"
      )
    ),
    
    footer = shiny::div(
      shiny::modalButton("Close"),
      style = "text-align: right;"
    ),
    
    easyClose = TRUE
  )
}


#' Create Tab Panel for a Table
#'
#' Creates a complete tab with one table for viewing regressions
#'
#' @param .tab_name Character. Name of the tab (from list names)
#' @param .metadata List. Metadata for this table
#' @return Shiny tabPanel
#' @keywords internal
create_table_tab <- function(.tab_name, .metadata) {
  shiny::tabPanel(
    title = .tab_name,
    value = .tab_name,
    
    shiny::fluidRow(
      # LEFT SIDEBAR: All Controls
      shiny::column(
        width = 2,
        style = "padding: 20px; background-color: #f8f9fa; border-radius: 4px; margin-left: 15px; margin-top: 20px;",
        
        # === FILTERS SECTION ===
        shiny::h5(shiny::icon("filter"), "Filters", style = "margin-bottom: 20px;"),
        
        # Filters stacked vertically
        purrr::map(.metadata$filter_cols, function(.col) {
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::selectInput(
              inputId = paste0(.tab_name, "_filter_", .col),
              label = paste0(.col, ":"),
              choices = .metadata$filter_values[[.col]],
              selected = .metadata$default_filters[[.col]],
              width = "100%"
            )
          )
        }),
        
        # Reset button
        shiny::actionButton(
          inputId = paste0(.tab_name, "_reset"),
          label = "Reset Filters",
          icon = shiny::icon("refresh"),
          width = "100%",
          class = "btn-warning",
          style = "margin-top: 10px; margin-bottom: 20px;"
        ),
        
        # Divider
        shiny::hr(),
        
        # === FORMAT ROWS SECTION ===
        shiny::h5(shiny::icon("paint-brush"), "Format Rows", style = "margin-bottom: 15px;"),
        
        shiny::selectizeInput(
          inputId = paste0(.tab_name, "_bold_rows"),
          label = "Bold:",
          choices = NULL,  # Will be updated dynamically
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = 'Select rows...')
        ),
        
        shiny::selectizeInput(
          inputId = paste0(.tab_name, "_border_rows"),
          label = "Border:",
          choices = NULL,  # Will be updated dynamically
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = 'Select rows...')
        ),
        
        # Divider
        shiny::hr(),
        
        # === COLUMN VISIBILITY SECTION ===
        shiny::h5(shiny::icon("eye"), "Columns", style = "margin-bottom: 15px;"),
        
        shiny::actionButton(
          inputId = paste0(.tab_name, "_show_column_modal"),
          label = "Show/Hide Columns",
          icon = shiny::icon("eye"),
          width = "100%",
          class = "btn-info",
          style = "margin-bottom: 15px;"
        ),
        
        # Divider
        shiny::hr(),
        
        # === ROW COUNTER ===
        shiny::div(
          style = "padding: 10px; background-color: #ffffff; border-radius: 4px; text-align: center; font-weight: 500;",
          shiny::textOutput(paste0(.tab_name, "_row_count"))
        )
      ),
      
      # MAIN AREA: Just the Table
      shiny::column(
        width = 9,
        style = "padding: 20px;",
        
        # Table (no top bar anymore - all controls are in sidebar)
        reactable::reactableOutput(paste0(.tab_name, "_table"), height = "700px")
      )
    )
  )
}