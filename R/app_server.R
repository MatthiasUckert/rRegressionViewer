#' Create App Server
#'
#' Defines the server logic for the regression comparison app
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param .app_data List with 'data' and 'metadata' elements
#' @param .dir Character. Output directory path
#' @keywords internal
app_server <- function(input, output, session, .app_data, .dir) {
  
  # Get tab names
  tab_names <- names(.app_data$data)
  
  # For each table, create server logic
  purrr::walk(tab_names, function(.tab_name) {
    
    # Get data and metadata for this tab
    df <- .app_data$data[[.tab_name]]
    meta <- .app_data$metadata[[.tab_name]]
    
    # === REACTIVE: Get current filter values ===
    get_current_filters <- shiny::reactive({
      # Collect all filter values from inputs
      filters <- purrr::map(meta$filter_cols, function(.col) {
        input[[paste0(.tab_name, "_filter_", .col)]]
      }) |>
        purrr::set_names(meta$filter_cols)
      
      return(filters)
    })
    
    # === REACTIVE: Get filtered data ===
    filtered_data <- shiny::reactive({
      filters <- get_current_filters()
      shiny::req(all(!sapply(filters, is.null)))
      
      get_filtered_data(.df = df, .filters = filters)
    })
    
    # === REACTIVE: Get visible columns ===
    visible_cols <- shiny::reactive({
      cols <- input[[paste0(.tab_name, "_visible_cols")]]
      if (is.null(cols) || length(cols) == 0) {
        return(character(0))
      }
      return(cols)
    })
    
    # === REACTIVE: Prepare table data ===
    table_data <- shiny::reactive({
      vis_cols <- visible_cols()
      filt_data <- filtered_data()
      
      shiny::req(nrow(filt_data) > 0)
      
      prepare_table_data(
        .df = filt_data,
        .filter_cols = meta$filter_cols,
        .term_col = meta$term_col,
        .visible_model_cols = vis_cols
      )
    })
    
    # === OUTPUT: Row count ===
    output[[paste0(.tab_name, "_row_count")]] <- shiny::renderText({
      filt_data <- filtered_data()
      total_rows <- nrow(df)
      filtered_rows <- nrow(filt_data)
      
      paste0("Showing ", filtered_rows, " of ", total_rows, " rows")
    })
    
    # === OUTPUT: Table ===
    output[[paste0(.tab_name, "_table")]] <- reactable::renderReactable({
      tbl_data <- table_data()
      
      render_comparison_table(
        .df = tbl_data,
        .term_col = meta$term_col
      )
    })
    
    # === OBSERVER: Show column selection modal ===
    shiny::observeEvent(input[[paste0(.tab_name, "_show_column_modal")]], {
      # Get current selection (or default to all if NULL)
      current_selection <- input[[paste0(.tab_name, "_visible_cols")]]
      if (is.null(current_selection)) {
        current_selection <- meta$model_cols
      }
      
      shiny::showModal(
        create_column_modal(
          .tab_name = .tab_name,
          .model_cols = meta$model_cols,
          .current_selection = current_selection
        )
      )
    })
    
    # === OBSERVER: Select All button (in modal) ===
    shiny::observeEvent(input[[paste0(.tab_name, "_select_all_modal")]], {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = paste0(.tab_name, "_visible_cols"),
        selected = meta$model_cols
      )
    })
    
    # === OBSERVER: Select None button (in modal) ===
    shiny::observeEvent(input[[paste0(.tab_name, "_select_none_modal")]], {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = paste0(.tab_name, "_visible_cols"),
        selected = character(0)
      )
    })
    
    # === OBSERVER: Reset button ===
    shiny::observeEvent(input[[paste0(.tab_name, "_reset")]], {
      # Reset all filter dropdowns to default
      purrr::walk(meta$filter_cols, function(.col) {
        shiny::updateSelectInput(
          session = session,
          inputId = paste0(.tab_name, "_filter_", .col),
          selected = meta$default_filters[[.col]]
        )
      })
      
      # Reset column selection to all
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = paste0(.tab_name, "_visible_cols"),
        selected = meta$model_cols
      )
    })
    
  })  # End tab loop
  
}