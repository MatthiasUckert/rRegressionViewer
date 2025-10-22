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
        shiny::updateSelectizeInput(
          session,
          paste0(.tab_name, "_bold_rows"),
          choices = character(0),
          selected = NULL
        )
        shiny::updateSelectizeInput(
          session,
          paste0(.tab_name, "_border_top_rows"),
          choices = character(0),
          selected = NULL
        )
        shiny::updateSelectizeInput(
          session,
          paste0(.tab_name, "_border_bottom_rows"),
          choices = character(0),
          selected = NULL
        )
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
  # SECTION 2: SIGNIFICANCE TAB LOGIC (existing code)
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


  # ============================================================================
  # SECTION 3: TABLE FORMATTER LOGIC (FIXED)
  # ============================================================================

  # === REACTIVE: Template information ===
  template_info <- shiny::reactiveVal(NULL)

  # === OBSERVER: When template is uploaded ===
  shiny::observeEvent(input$fmt_template_file, {
    req(input$fmt_template_file)

    # Read template structure
    info <- read_template_structure(input$fmt_template_file$datapath)
    template_info(info)

    # Update sheet selector
    if (length(info) > 0) {
      shiny::updateSelectInput(
        session,
        "fmt_sheet_select",
        choices = names(info),
        selected = names(info)[1]
      )
    }
  })

  # === OBSERVER: When sheet is selected ===
  shiny::observeEvent(input$fmt_sheet_select, {
    req(template_info(), input$fmt_sheet_select)

    info <- template_info()
    sheet_name <- input$fmt_sheet_select

    if (!sheet_name %in% names(info)) {
      return()
    }

    # Update table mapping choices
    shiny::updateSelectInput(
      session,
      "fmt_table_mapping",
      choices = c("-- Select Table --" = "", names(.app_data$data)),
      selected = ""
    )
  })

  # === OBSERVER: When table is mapped (SIMPLIFIED) ===
  shiny::observeEvent(input$fmt_table_mapping, {

    # Hide sections if no table selected
    if (is.null(input$fmt_table_mapping) || input$fmt_table_mapping == "") {
      shinyjs::hide("fmt_filters_section")
      shinyjs::hide("fmt_columns_section")
      shinyjs::hide("fmt_rows_section")
      shinyjs::hide("fmt_preview_section")
      return()
    }

    # Show all sections (filters populate via renderUI)
    shinyjs::show("fmt_filters_section")
    shinyjs::show("fmt_columns_section")
    shinyjs::show("fmt_rows_section")
    shinyjs::show("fmt_preview_section")

  }, ignoreInit = TRUE)

  # === OUTPUT: Dynamic filters UI (NEW - ROBUST) ===
  output$fmt_filters_ui <- shiny::renderUI({
    req(input$fmt_table_mapping, input$fmt_table_mapping != "")

    table_name <- input$fmt_table_mapping
    sheet_name <- input$fmt_sheet_select
    meta <- .app_data$metadata[[table_name]]

    purrr::map(meta$filter_cols, function(.col) {
      shiny::div(
        style = "margin-bottom: 10px;",
        shiny::selectInput(
          inputId = paste0("fmt_filter_", sheet_name, "_", .col),
          label = paste0(.col, ":"),
          choices = meta$filter_values[[.col]],
          selected = meta$default_filters[[.col]],
          width = "100%"
        )
      )
    })
  })

  # === OUTPUT: Column mapping UI ===
  output$fmt_column_mapping_ui <- shiny::renderUI({
    req(template_info(), input$fmt_sheet_select, input$fmt_table_mapping != "")
    
    info <- template_info()
    sheet_name <- input$fmt_sheet_select
    table_name <- input$fmt_table_mapping
    
    if (!sheet_name %in% names(info)) {
      return(shiny::div("Error: Sheet not found in template"))
    }
    
    sheet_info <- info[[sheet_name]]
    dashboard_cols <- get_available_columns(.app_data, table_name)
    col_headers <- sheet_info$col_headers
    
    if (is.null(col_headers) || length(col_headers) == 0) {
      return(shiny::div(
        shiny::p("No column headers found in template.", style = "color: orange; font-weight: bold;")
      ))
    }
    
    # Create compact inline mapping inputs with auto-matching
    purrr::map(col_headers, function(.col) {
      col_str <- as.character(.col)
      
      # Try to find a matching dashboard column
      default_match <- ""
      exact_match <- dashboard_cols[tolower(dashboard_cols) == tolower(col_str)]
      if (length(exact_match) > 0) {
        default_match <- exact_match[1]
      } else {
        partial_matches <- dashboard_cols[grepl(gsub("[^A-Za-z0-9]", "", col_str),
                                                gsub("[^A-Za-z0-9]", "", dashboard_cols),
                                                ignore.case = TRUE)]
        if (length(partial_matches) > 0) {
          default_match <- partial_matches[1]
        }
      }
      
      shiny::div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        shiny::div(
          style = "font-weight: bold; font-size: 12px; color: #555; min-width: 80px; margin-right: 10px;",
          col_str
        ),
        shiny::selectInput(
          inputId = paste0("fmt_col_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", col_str)),
          label = NULL,
          choices = c("-- Select Column --" = "", dashboard_cols),
          selected = default_match,
          width = "100%"
        )
      )
    })
  })

  # === OUTPUT: Row mapping UI ===
  output$fmt_row_mapping_ui <- shiny::renderUI({
    req(template_info(), input$fmt_sheet_select, input$fmt_table_mapping != "")
    
    info <- template_info()
    sheet_name <- input$fmt_sheet_select
    table_name <- input$fmt_table_mapping
    
    if (!sheet_name %in% names(info)) {
      return(shiny::div("Error: Sheet not found in template"))
    }
    
    sheet_info <- info[[sheet_name]]
    dashboard_rows <- get_available_rows(.app_data, table_name)
    row_labels <- sheet_info$row_labels
    
    if (is.null(row_labels) || length(row_labels) == 0) {
      return(shiny::div(
        shiny::p("No row labels found in template.", style = "color: orange; font-weight: bold;")
      ))
    }
    
    # Create compact inline mapping inputs with auto-matching
    purrr::map(row_labels, function(.row) {
      row_str <- as.character(.row)
      
      # Try to find a matching dashboard row
      default_match <- ""
      exact_match <- dashboard_rows[tolower(dashboard_rows) == tolower(row_str)]
      if (length(exact_match) > 0) {
        default_match <- exact_match[1]
      } else {
        partial_matches <- dashboard_rows[grepl(gsub("[^A-Za-z0-9]", "", row_str),
                                                gsub("[^A-Za-z0-9]", "", dashboard_rows),
                                                ignore.case = TRUE)]
        if (length(partial_matches) > 0) {
          default_match <- partial_matches[1]
        }
      }
      
      shiny::div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        shiny::div(
          style = "font-weight: bold; font-size: 12px; color: #555; min-width: 150px; margin-right: 10px; flex-shrink: 0;",
          row_str
        ),
        shiny::selectInput(
          inputId = paste0("fmt_row_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", row_str)),
          label = NULL,
          choices = c("-- Select Row --" = "", dashboard_rows),
          selected = default_match,
          width = "100%"
        )
      )
    })
  })

  # === DOWNLOAD HANDLER: Fill and download template ===
  output$fmt_download <- shiny::downloadHandler(
    filename = function() {
      paste0("filled_template_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(template_info(), input$fmt_template_file)

      # Collect all mappings
      info <- template_info()

      sheet_mappings <- list()
      column_mappings <- list()
      row_mappings <- list()
      filters <- list()

      for (sheet_name in names(info)) {
        # Sheet to table mapping
        table_input_id <- "fmt_table_mapping"
        if (sheet_name == input$fmt_sheet_select) {
          sheet_mappings[[sheet_name]] <- input[[table_input_id]]
        }

        if (is.null(sheet_mappings[[sheet_name]]) || sheet_mappings[[sheet_name]] == "") {
          next
        }

        table_name <- sheet_mappings[[sheet_name]]
        meta <- .app_data$metadata[[table_name]]

        # Collect filters
        filter_vals <- purrr::map(meta$filter_cols, function(.col) {
          input[[paste0("fmt_filter_", sheet_name, "_", .col)]]
        }) |>
          purrr::set_names(meta$filter_cols)
        filters[[sheet_name]] <- filter_vals

        # Collect column mappings
        col_map <- list()
        for (.col in info[[sheet_name]]$col_headers) {
          input_id <- paste0("fmt_col_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", .col))
          col_map[[.col]] <- input[[input_id]]
        }
        column_mappings[[sheet_name]] <- col_map

        # Collect row mappings
        row_map <- list()
        for (.row in info[[sheet_name]]$row_labels) {
          input_id <- paste0("fmt_row_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", .row))
          row_map[[.row]] <- input[[input_id]]
        }
        row_mappings[[sheet_name]] <- row_map
      }

      # Create mapping object
      mapping <- create_mapping_object(sheet_mappings, column_mappings, row_mappings, filters)

      # Fill template
      fill_excel_template(
        .template_path = input$fmt_template_file$datapath,
        .template_info = info,
        .mapping = mapping,
        .app_data = .app_data,
        .output_path = file
      )
    }
  )
  
  # === OUTPUT: Preview table ===
  output$fmt_preview_table <- shiny::renderUI({
    req(template_info(), input$fmt_sheet_select, input$fmt_table_mapping != "")
    
    info <- template_info()
    sheet_name <- input$fmt_sheet_select
    table_name <- input$fmt_table_mapping
    
    if (!sheet_name %in% names(info)) {
      return(shiny::div("Error: Sheet not found"))
    }
    
    sheet_info <- info[[sheet_name]]
    meta <- .app_data$metadata[[table_name]]
    df <- .app_data$data[[table_name]]
    
    # Collect current filters
    filters <- purrr::map(meta$filter_cols, function(.col) {
      input[[paste0("fmt_filter_", sheet_name, "_", .col)]]
    }) |>
      purrr::set_names(meta$filter_cols)
    
    # Check if all filters are ready
    if (any(sapply(filters, is.null))) {
      return(shiny::div("Loading filters...", style = "color: #666; font-style: italic;"))
    }
    
    # Apply filters to data
    filtered_df <- get_filtered_data(.df = df, .filters = filters)
    
    # Collect column mappings
    col_mapping <- list()
    for (.col in sheet_info$col_headers) {
      input_id <- paste0("fmt_col_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", .col))
      col_mapping[[.col]] <- input[[input_id]]
    }
    
    # Collect row mappings
    row_mapping <- list()
    for (.row in sheet_info$row_labels) {
      input_id <- paste0("fmt_row_map_", sheet_name, "_", gsub("[^A-Za-z0-9]", "_", .row))
      row_mapping[[.row]] <- input[[input_id]]
    }
    
    # Generate preview
    generate_preview_table(
      .sheet_info = sheet_info,
      .df = filtered_df,
      .col_mapping = col_mapping,
      .row_mapping = row_mapping,
      .term_col = meta$term_col
    )
  })
}