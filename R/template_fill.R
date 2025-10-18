#' Fill Excel Template with Data
#'
#' Fills an Excel template with data based on mappings
#'
#' @param .template_path Character. Path to template file
#' @param .template_info List. Template structure info
#' @param .mapping List. Mapping configuration
#' @param .app_data List. Application data
#' @param .output_path Character. Path for output file
#' @return Character. Path to filled file
#' @keywords internal
fill_excel_template <- function(.template_path, .template_info, .mapping, .app_data, .output_path) {
  
  # Load the template workbook
  wb <- openxlsx2::wb_load(.template_path)
  
  # Process each sheet
  for (sheet_name in names(.mapping)) {
    
    # Get dashboard table name for this sheet
    table_name <- .mapping[[sheet_name]]$table
    
    if (is.null(table_name) || table_name == "") {
      next  # Skip unmapped sheets
    }
    
    # Get template info for this sheet
    sheet_info <- .template_info[[sheet_name]]
    
    if (is.null(sheet_info)) {
      warning("Sheet info not found for: ", sheet_name)
      next
    }
    
    # Get data and metadata
    df <- .app_data$data[[table_name]]
    meta <- .app_data$metadata[[table_name]]
    
    # Apply filters
    filters <- .mapping[[sheet_name]]$filters
    if (!is.null(filters)) {
      df <- get_filtered_data(.df = df, .filters = filters)
    }
    
    # Get mappings for this sheet
    col_mapping <- .mapping[[sheet_name]]$columns
    row_mapping <- .mapping[[sheet_name]]$rows
    
    # Fill the data
    wb <- fill_sheet_data(
      .wb = wb,
      .sheet_name = sheet_name,
      .sheet_info = sheet_info,
      .df = df,
      .col_mapping = col_mapping,
      .row_mapping = row_mapping,
      .term_col = meta$term_col
    )
  }
  
  # Save the filled workbook
  openxlsx2::wb_save(wb, .output_path, overwrite = TRUE)
  
  message("Template filled and saved to: ", .output_path)
  
  return(.output_path)
}


#' Fill Single Sheet with Data
#'
#' Fills one sheet of the workbook with mapped data
#'
#' @param .wb Workbook object
#' @param .sheet_name Character. Sheet name
#' @param .sheet_info List. Sheet structure info
#' @param .df Dataframe. Source data
#' @param .col_mapping Named list. Template col to dashboard col
#' @param .row_mapping Named list. Template row to dashboard row
#' @param .term_col Character. Name of term column
#' @return Modified workbook object
#' @keywords internal
fill_sheet_data <- function(.wb, .sheet_name, .sheet_info, .df, .col_mapping, .row_mapping, .term_col) {
  
  markers <- .sheet_info$markers
  
  # Starting position for data (first row after CS, first col after row labels)
  start_row <- markers$cs_row + 1
  start_col <- markers$lu_col + 2  # After LU marker + row label column
  
  # Process each template column
  for (i in seq_along(.sheet_info$col_headers)) {
    
    template_col_name <- .sheet_info$col_headers[i]
    dashboard_col_name <- .col_mapping[[template_col_name]]
    
    if (is.null(dashboard_col_name) || dashboard_col_name == "") {
      next  # Skip unmapped columns
    }
    
    # Process each template row
    for (j in seq_along(.sheet_info$row_labels)) {
      
      template_row_name <- .sheet_info$row_labels[j]
      dashboard_row_name <- .row_mapping[[template_row_name]]
      
      if (is.null(dashboard_row_name) || dashboard_row_name == "") {
        next  # Skip unmapped rows
      }
      
      # Get the value from dashboard data
      value <- get_cell_value(
        .df = .df,
        .term_col = .term_col,
        .row_name = dashboard_row_name,
        .col_name = dashboard_col_name
      )
      
      # Write to workbook (preserve existing formatting)
      if (!is.na(value) && value != "") {
        excel_row <- start_row + j - 1
        excel_col <- start_col + i - 1
        
        .wb <- openxlsx2::wb_add_data(
          .wb,
          sheet = .sheet_name,
          x = value,
          start_row = excel_row,
          start_col = excel_col,
          col_names = FALSE
        )
      }
    }
  }
  
  return(.wb)
}


#' Get Cell Value from Dashboard Data
#'
#' Retrieves a specific cell value from the dataframe
#'
#' @param .df Dataframe
#' @param .term_col Character. Term column name
#' @param .row_name Character. Row term to find
#' @param .col_name Character. Column name to get value from
#' @return Character. Cell value
#' @keywords internal
get_cell_value <- function(.df, .term_col, .row_name, .col_name) {
  
  # Filter to the specific row
  row_data <- .df[.df[[.term_col]] == .row_name, ]
  
  if (nrow(row_data) == 0) {
    return(NA_character_)
  }
  
  # Get the value from the specified column
  if (!.col_name %in% names(row_data)) {
    return(NA_character_)
  }
  
  value <- row_data[[.col_name]][1]
  
  if (is.na(value)) {
    return(NA_character_)
  }
  
  return(as.character(value))
}


#' Generate Preview Table
#'
#' Creates an HTML table preview of what will be filled in the template
#'
#' @param .sheet_info List. Sheet structure info
#' @param .df Dataframe. Source data (filtered)
#' @param .col_mapping Named list. Template col to dashboard col mappings
#' @param .row_mapping Named list. Template row to dashboard row mappings
#' @param .term_col Character. Name of term column
#' @return Shiny HTML tag
#' @keywords internal
generate_preview_table <- function(.sheet_info, .df, .col_mapping, .row_mapping, .term_col) {
  
  # Build table header
  header_row <- shiny::tags$tr(
    shiny::tags$th("", style = "border: 1px solid #ddd; padding: 8px; background-color: #f8f9fa; font-weight: bold;"),
    lapply(.sheet_info$col_headers, function(.col) {
      shiny::tags$th(
        .col,
        style = "border: 1px solid #ddd; padding: 8px; background-color: #e9ecef; font-weight: bold; text-align: center;"
      )
    })
  )
  
  # Build table body
  body_rows <- lapply(.sheet_info$row_labels, function(.row) {
    # Get dashboard row name
    dashboard_row <- .row_mapping[[.row]]
    
    # Build cells for this row
    cells <- lapply(.sheet_info$col_headers, function(.col) {
      # Get dashboard column name
      dashboard_col <- .col_mapping[[.col]]
      
      # Get the value
      if (is.null(dashboard_row) || is.null(dashboard_col) || 
          dashboard_row == "" || dashboard_col == "") {
        cell_value <- "_"
        cell_style <- "border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #fff3cd; color: #999;"
      } else {
        cell_value <- get_cell_value(.df, .term_col, dashboard_row, dashboard_col)
        if (is.na(cell_value) || cell_value == "") {
          cell_value <- "—"
          cell_style <- "border: 1px solid #ddd; padding: 8px; text-align: center; color: #999;"
        } else {
          cell_style <- "border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #d4edda;"
        }
      }
      
      shiny::tags$td(cell_value, style = cell_style)
    })
    
    # Row label cell
    row_label_cell <- shiny::tags$td(
      .row,
      style = "border: 1px solid #ddd; padding: 8px; background-color: #e9ecef; font-weight: bold;"
    )
    
    shiny::tags$tr(c(list(row_label_cell), cells))
  })
  
  # Build complete table
  shiny::tags$table(
    style = "border-collapse: collapse; font-family: 'Times New Roman', Times, serif; font-size: 12px; width: 100%;",
    shiny::tags$thead(header_row),
    shiny::tags$tbody(body_rows)
  )
}