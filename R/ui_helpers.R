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


#' Create Panel UI
#'
#' Creates the complete UI for one panel
#'
#' @param .tab_name Character. Name of the current tab
#' @param .metadata List. Metadata for the current table
#' @return Shiny UI element
#' @keywords internal
create_panel_ui <- function(.tab_name, .metadata) {
  
  shiny::div(
    style = "padding: 15px; background-color: #f8f9fa; border-radius: 4px; margin-bottom: 15px;",
    
    shiny::fluidRow(
      # Filters section
      shiny::column(
        width = 9,
        shiny::h5(shiny::icon("filter"), "Filters"),
        shiny::fluidRow(
          purrr::map(.metadata$filter_cols, function(.col) {
            shiny::column(
              width = 2,
              shiny::selectInput(
                inputId = paste0(.tab_name, "_filter_", .col),
                label = paste0(.col, ":"),
                choices = .metadata$filter_values[[.col]],
                selected = .metadata$default_filters[[.col]],
                width = "100%"
              )
            )
          })
        )
      ),
      
      # Compact buttons section
      shiny::column(
        width = 3,
        shiny::h5(" "),  # Spacer for alignment
        
        # Column visibility button
        shiny::actionButton(
          inputId = paste0(.tab_name, "_show_column_modal"),
          label = "Show/Hide Columns",
          icon = shiny::icon("eye"),
          width = "100%",
          class = "btn-info",
          style = "margin-bottom: 10px;"
        ),
        
        # Reset button
        shiny::actionButton(
          inputId = paste0(.tab_name, "_reset"),
          label = "Reset Filters",
          icon = shiny::icon("refresh"),
          width = "100%",
          class = "btn-warning",
          style = "margin-bottom: 10px;"
        )
      )
    )
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
    
    # Single table section
    shiny::div(
      style = "padding: 20px;",
      create_panel_ui(.tab_name = .tab_name, .metadata = .metadata),
      reactable::reactableOutput(paste0(.tab_name, "_table"), height = "600px")
    )
  )
}