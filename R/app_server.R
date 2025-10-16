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
  
  # ============================================================================
  # SECTION 1: TABLE TABS LOGIC (existing code)
  # ============================================================================
  
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
      # Default to showing ALL columns if none selected
      if (is.null(cols) || length(cols) == 0) {
        return(meta$model_cols)  # Show all columns by default
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
    
    # === OBSERVER: Update row choices dynamically for all format dropdowns ===
    shiny::observeEvent({
      lapply(meta$filter_cols, function(.col) {
        input[[paste0(.tab_name, "_filter_", .col)]]
      })
    }, {
      filters <- purrr::map(meta$filter_cols, function(.col) {
        input[[paste0(.tab_name, "_filter_", .col)]]
      }) |>
        purrr::set_names(meta$filter_cols)
      
      if (any(sapply(filters, is.null))) {
        return()
      }
      
      filt_data <- get_filtered_data(.df = df, .filters = filters)
      
      if (is.null(filt_data) || nrow(filt_data) == 0) {
        shiny::updateSelectizeInput(session, paste0(.tab_name, "_bold_rows"), choices = character(0), selected = NULL)
        shiny::updateSelectizeInput(session, paste0(.tab_name, "_border_top_rows"), choices = character(0), selected = NULL)
        shiny::updateSelectizeInput(session, paste0(.tab_name, "_border_bottom_rows"), choices = character(0), selected = NULL)
        return()
      }
      
      row_choices <- unique(filt_data[[meta$term_col]])
      
      current_bold <- shiny::isolate(input[[paste0(.tab_name, "_bold_rows")]])
      current_border_top <- shiny::isolate(input[[paste0(.tab_name, "_border_top_rows")]])
      current_border_bottom <- shiny::isolate(input[[paste0(.tab_name, "_border_bottom_rows")]])
      
      preserved_bold <- if (!is.null(current_bold) && length(current_bold) > 0) intersect(current_bold, row_choices) else NULL
      preserved_border_top <- if (!is.null(current_border_top) && length(current_border_top) > 0) intersect(current_border_top, row_choices) else NULL
      preserved_border_bottom <- if (!is.null(current_border_bottom) && length(current_border_bottom) > 0) intersect(current_border_bottom, row_choices) else NULL
      
      shiny::updateSelectizeInput(session, paste0(.tab_name, "_bold_rows"), choices = row_choices, selected = preserved_bold)
      shiny::updateSelectizeInput(session, paste0(.tab_name, "_border_top_rows"), choices = row_choices, selected = preserved_border_top)
      shiny::updateSelectizeInput(session, paste0(.tab_name, "_border_bottom_rows"), choices = row_choices, selected = preserved_border_bottom)
      
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
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
      bold_rows <- input[[paste0(.tab_name, "_bold_rows")]]
      border_top_rows <- input[[paste0(.tab_name, "_border_top_rows")]]
      border_bottom_rows <- input[[paste0(.tab_name, "_border_bottom_rows")]]
      
      render_comparison_table(.df = tbl_data, .term_col = meta$term_col, .bold_rows = bold_rows, .border_top_rows = border_top_rows, .border_bottom_rows = border_bottom_rows)
    })
    
    # === OBSERVER: Show column selection modal ===
    shiny::observeEvent(input[[paste0(.tab_name, "_show_column_modal")]], {
      current_selection <- input[[paste0(.tab_name, "_visible_cols")]]
      if (is.null(current_selection)) current_selection <- meta$model_cols
      shiny::showModal(create_column_modal(.tab_name, meta$model_cols, current_selection))
    })
    
    # === OBSERVER: Select All button ===
    shiny::observeEvent(input[[paste0(.tab_name, "_select_all_modal")]], {
      shiny::updateCheckboxGroupInput(session, paste0(.tab_name, "_visible_cols"), selected = meta$model_cols)
    })
    
    # === OBSERVER: Select None button ===
    shiny::observeEvent(input[[paste0(.tab_name, "_select_none_modal")]], {
      shiny::updateCheckboxGroupInput(session, paste0(.tab_name, "_visible_cols"), selected = character(0))
    })
    
    # === OBSERVER: Reset button ===
    shiny::observeEvent(input[[paste0(.tab_name, "_reset")]], {
      purrr::walk(meta$filter_cols, function(.col) {
        shiny::updateSelectInput(session, paste0(.tab_name, "_filter_", .col), selected = meta$default_filters[[.col]])
      })
      shiny::updateCheckboxGroupInput(session, paste0(.tab_name, "_visible_cols"), selected = meta$model_cols)
    })
    
  })  # End table tabs loop
  
  
  # ============================================================================
  # SECTION 2: SIGNIFICANCE TAB LOGIC (new code)
  # ============================================================================
  
  # === REACTIVE: Get selected table metadata ===
  selected_table_meta <- shiny::reactive({
    selected_table <- input$sig_table_select
    shiny::req(selected_table)
    .app_data$metadata[[selected_table]]
  })
  
  # === REACTIVE: Get selected table data ===
  selected_table_data <- shiny::reactive({
    selected_table <- input$sig_table_select
    shiny::req(selected_table)
    .app_data$data[[selected_table]]
  })
  
  # === OBSERVER: Update filters when table changes ===
  shiny::observeEvent(input$sig_table_select, {
    meta <- selected_table_meta()
    
    # Dynamically create filter inputs
    filter_ui <- purrr::map(meta$filter_cols, function(.col) {
      shiny::div(
        style = "margin-bottom: 15px;",
        shiny::selectInput(
          inputId = paste0("sig_filter_", .col),
          label = paste0(.col, ":"),
          choices = meta$filter_values[[.col]],
          selected = meta$default_filters[[.col]],
          width = "100%"
        )
      )
    })
    
    shiny::removeUI(selector = "#sig_filters_container > *", multiple = TRUE)
    shiny::insertUI(
      selector = "#sig_filters_container",
      where = "beforeEnd",
      ui = filter_ui
    )
    
    # Update variable choices
    all_terms <- meta$initial_row_choices
    shiny::updateSelectInput(session, "sig_variable", choices = all_terms, selected = all_terms[1])
    
    # Update model column choices
    shiny::updateSelectInput(session, "sig_model", choices = meta$model_cols, selected = meta$model_cols[1])
    
    # Update axis choices (filter columns)
    shiny::updateSelectInput(session, "sig_x_axis", choices = meta$filter_cols, selected = meta$filter_cols[1])
    shiny::updateSelectInput(session, "sig_y_axis", choices = meta$filter_cols, selected = meta$filter_cols[min(2, length(meta$filter_cols))])
  })
  
  # === REACTIVE: Get current significance filters ===
  sig_filters <- shiny::reactive({
    meta <- selected_table_meta()
    
    filters <- purrr::map(meta$filter_cols, function(.col) {
      input[[paste0("sig_filter_", .col)]]
    }) |>
      purrr::set_names(meta$filter_cols)
    
    shiny::req(all(!sapply(filters, is.null)))
    
    return(filters)
  })
  
  # === REACTIVE: Extract coefficients from selected table ===
  coefficients_data <- shiny::reactive({
    df <- selected_table_data()
    meta <- selected_table_meta()
    
    extract_coefficients(
      .df = df,
      .term_col = meta$term_col,
      .filter_cols = meta$filter_cols
    )
  })
  
  # === REACTIVE: Prepare plot data ===
  plot_data <- shiny::reactive({
    coef_data <- coefficients_data()
    filters <- sig_filters()
    variable <- input$sig_variable
    model <- input$sig_model
    x_axis <- input$sig_x_axis
    y_axis <- input$sig_y_axis
    
    shiny::req(variable, model, x_axis, y_axis)
    shiny::req(x_axis != y_axis)  # X and Y must be different
    
    # Filter by selected model column
    coef_data <- coef_data |>
      dplyr::filter(model == !!model)
    
    prepare_plot_data(
      .df_coef = coef_data,
      .variable = variable,
      .fixed_filters = filters,
      .x_axis = x_axis,
      .y_axis = y_axis
    )
  })
  
  # === OUTPUT: Significance plot ===
  output$sig_plot <- shiny::renderPlot({
    pdata <- plot_data()
    variable <- input$sig_variable
    model <- input$sig_model
    x_axis <- input$sig_x_axis
    y_axis <- input$sig_y_axis
    
    shiny::req(variable, model, x_axis, y_axis)
    
    create_significance_plot(
      .plot_data = pdata,
      .variable = variable,
      .x_axis = x_axis,
      .y_axis = y_axis
    )
  })
  
}