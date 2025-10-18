#' Read Excel Template and Detect Structure
#'
#' Reads an Excel template file and detects the data region using corner markers.
#' 
#' Expected template structure:
#' - LU (Left-Upper): Top-left corner of the table region
#' - RU (Right-Upper): Top-right corner of the table region  
#' - LB (Left-Bottom): Bottom-left corner of the table region
#' - RB (Right-Bottom): Bottom-right corner of the table region
#' - CS (Column Start): Marks the row containing column headers
#' - CE (Column End): Marks the end of column headers in the CS row
#' - "...": Placeholder in cells where data should be written
#'
#' The first column of the table (LU_col + 1) contains row labels (variable names).
#' Column headers are extracted from the CS row, starting from CS_col + 2 (skipping row label column).
#' Row labels are extracted from the first table column (LU_col + 1), between CS row and LB row.
#'
#' @param .path Character. Path to template Excel file
#' @return List with template info for each sheet
#' @keywords internal
read_template_structure <- function(.path) {
  
  # Get all sheet names
  wb <- openxlsx2::wb_load(.path)
  sheet_names <- openxlsx2::wb_get_sheet_names(wb)
  
  # Process each sheet
  template_info <- purrr::map(sheet_names, function(.sheet) {
    
    # Load workbook
    wb <- openxlsx2::wb_load(.path)
    
    # Read full sheet to find markers
    full_data <- openxlsx2::wb_to_df(
      wb,
      sheet = .sheet,
      col_names = FALSE,
      start_row = 1
    )
    
    # Find corner markers
    markers <- find_corner_markers(full_data)
    
    if (is.null(markers)) {
      warning("Could not find corner markers (LU, RU, LB, RB) in sheet: ", .sheet)
      return(NULL)
    }
    
    # ⭐ Extract column headers between CS and CE markers
    col_headers <- character(0)
    if (!is.null(markers$cs_row) && !is.null(markers$ce_row)) {
      # Column headers are in the CS row, between CS and CE columns
      # Skip the first table column (after CS) which contains row labels
      start_col <- markers$cs_col + 2  # Skip CS marker and row label column
      end_col <- markers$ce_col - 1    # Before CE marker
      
      if (start_col <= end_col) {
        col_headers <- as.character(full_data[markers$cs_row, start_col:end_col])
        # Filter out NA, empty strings, and the string "NA"
        col_headers <- col_headers[!is.na(col_headers) & col_headers != "" & col_headers != "NA"]
      }
    }
    
    # ⭐ Extract row labels between CS and LB rows, in the first table column
    # Only include rows that have "..." placeholders in their data cells
    row_labels <- character(0)
    if (!is.null(markers$cs_row) && !is.null(markers$lb_row)) {
      # Row labels are in the first column of the TABLE (column after LU marker)
      # NOT column 1 of the Excel file!
      row_label_col <- markers$lu_col + 1
      
      if ((markers$cs_row + 1) <= (markers$lb_row - 1)) {
        # Read raw data first for debugging
        raw_labels <- full_data[(markers$cs_row + 1):(markers$lb_row - 1), row_label_col]
        
        # Debug: show what we're reading
        message("  DEBUG - Reading row labels from column ", row_label_col)
        message("  DEBUG - Raw labels: ", paste(head(as.character(raw_labels), 5), collapse=" | "))
        
        # For each row, check if it has "..." in any data cell
        # Data cells are from (lu_col + 2) to (ru_col - 1) or similar
        data_start_col <- markers$lu_col + 2
        data_end_col <- markers$ce_col - 1
        
        rows_to_include <- c()
        for (i in seq_along(raw_labels)) {
          row_idx <- markers$cs_row + i
          label <- as.character(raw_labels[i])
          
          # Skip if label is NA or empty
          if (is.na(label) || label == "" || label == "NA") next
          
          # Check if this row has any "_" in data columns
          row_data <- full_data[row_idx, data_start_col:data_end_col]
          has_placeholder <- any(row_data == "_", na.rm = TRUE)
          
          if (has_placeholder) {
            rows_to_include <- c(rows_to_include, label)
          }
        }
        
        row_labels <- rows_to_include
        message("  DEBUG - Filtered to ", length(row_labels), " rows with '...' placeholders")
      }
    }
    
    # Debug output
    message("Sheet: ", .sheet)
    message("  Markers found - LU: (", markers$lu_row, ",", markers$lu_col, 
            "), RU: (", markers$ru_row, ",", markers$ru_col, ")")
    message("  LB: (", markers$lb_row, ",", markers$lb_col, 
            "), RB: (", markers$rb_row, ",", markers$rb_col, ")")
    if (!is.null(markers$cs_row)) {
      message("  CS: (", markers$cs_row, ",", markers$cs_col, 
              "), CE: (", markers$ce_row, ",", markers$ce_col, ")")
      message("  Note: Column headers extracted from row ", markers$cs_row, 
              ", cols ", markers$cs_col + 2, " to ", markers$ce_col - 1)
      message("  Note: Row labels extracted from col ", markers$lu_col + 1, 
              " (first table column), rows ", markers$cs_row + 1, " to ", markers$lb_row - 1)
      message("  Note: Only rows with '...' placeholders in data cells are included")
    }
    message("  Column headers (", length(col_headers), "): ", 
            if (length(col_headers) > 0) paste(head(col_headers, 10), collapse = ", ") else "none",
            if (length(col_headers) > 10) "..." else "")
    message("  Row labels (", length(row_labels), "): ", 
            if (length(row_labels) > 0) paste(head(row_labels, 8), collapse = ", ") else "NONE - check DEBUG output above",
            if (length(row_labels) > 8) paste0("... (", length(row_labels), " total)") else "")
    message("  ---")
    
    list(
      sheet_name = .sheet,
      markers = markers,
      col_headers = col_headers,
      row_labels = row_labels,
      n_cols = length(col_headers),
      n_rows = length(row_labels),
      # Data starts after CS row, in the column after row labels
      data_start_row = if (!is.null(markers$cs_row)) markers$cs_row + 1 else NULL,
      data_start_col = if (!is.null(markers$lu_col)) markers$lu_col + 2 else NULL,  # After LU + row label column
      workbook = wb
    )
  }) |>
    purrr::set_names(sheet_names)
  
  # Remove NULL entries (sheets without markers)
  template_info <- purrr::compact(template_info)
  
  return(template_info)
}


#' Find Corner Markers in Excel Sheet
#'
#' Locates LU, RU, LB, RB, CS, CE markers in the sheet
#'
#' @param .data Dataframe of sheet data
#' @return List with marker positions or NULL if not found
#' @keywords internal
find_corner_markers <- function(.data) {
  
  # Convert to matrix for easier searching
  mat <- as.matrix(.data)
  
  # Find each marker
  lu_pos <- which(mat == "LU", arr.ind = TRUE)
  ru_pos <- which(mat == "RU", arr.ind = TRUE)
  lb_pos <- which(mat == "LB", arr.ind = TRUE)
  rb_pos <- which(mat == "RB", arr.ind = TRUE)
  cs_pos <- which(mat == "CS", arr.ind = TRUE)
  ce_pos <- which(mat == "CE", arr.ind = TRUE)
  
  # Check if all required markers found
  if (nrow(lu_pos) == 0 || nrow(ru_pos) == 0 || 
      nrow(lb_pos) == 0 || nrow(rb_pos) == 0) {
    return(NULL)
  }
  
  # Build result list
  result <- list(
    lu_row = lu_pos[1, 1],
    lu_col = lu_pos[1, 2],
    ru_row = ru_pos[1, 1],
    ru_col = ru_pos[1, 2],
    lb_row = lb_pos[1, 1],
    lb_col = lb_pos[1, 2],
    rb_row = rb_pos[1, 1],
    rb_col = rb_pos[1, 2]
  )
  
  # Add CS/CE if found
  if (nrow(cs_pos) > 0) {
    result$cs_row <- cs_pos[1, 1]
    result$cs_col <- cs_pos[1, 2]
  } else {
    result$cs_row <- NULL
    result$cs_col <- NULL
  }
  
  if (nrow(ce_pos) > 0) {
    result$ce_row <- ce_pos[1, 1]
    result$ce_col <- ce_pos[1, 2]
  } else {
    result$ce_row <- NULL
    result$ce_col <- NULL
  }
  
  return(result)
}


#' Get Available Dashboard Tables
#'
#' Returns list of available tables from app data
#'
#' @param .app_data List with 'data' and 'metadata' elements
#' @return Character vector of table names
#' @keywords internal
get_available_tables <- function(.app_data) {
  names(.app_data$data)
}


#' Get Available Columns for Table
#'
#' Returns available model columns for a given table
#'
#' @param .app_data List with 'data' and 'metadata' elements
#' @param .table_name Character. Name of table
#' @return Character vector of column names
#' @keywords internal
get_available_columns <- function(.app_data, .table_name) {
  .app_data$metadata[[.table_name]]$model_cols
}


#' Get Available Rows for Table
#'
#' Returns available row terms for a given table
#'
#' @param .app_data List with 'data' and 'metadata' elements
#' @param .table_name Character. Name of table
#' @return Character vector of row terms
#' @keywords internal
get_available_rows <- function(.app_data, .table_name) {
  .app_data$metadata[[.table_name]]$initial_row_choices
}


#' Create Mapping Object
#'
#' Creates a structured mapping object from user selections
#'
#' @param .sheet_mappings List. Sheet to table mappings
#' @param .column_mappings List. Column mappings for each sheet
#' @param .row_mappings List. Row mappings for each sheet
#' @param .filters List. Filter values for each sheet
#' @return List. Structured mapping object
#' @keywords internal
create_mapping_object <- function(.sheet_mappings, .column_mappings, .row_mappings, .filters) {
  
  mapping <- list()
  
  for (sheet_name in names(.sheet_mappings)) {
    
    # Clean column mappings - remove unmapped columns
    col_map <- .column_mappings[[sheet_name]]
    col_map <- col_map[col_map != "" & !is.null(col_map) & !is.na(col_map)]
    
    # Clean row mappings - remove unmapped rows
    row_map <- .row_mappings[[sheet_name]]
    row_map <- row_map[row_map != "" & !is.null(row_map) & !is.na(row_map)]
    
    mapping[[sheet_name]] <- list(
      table = .sheet_mappings[[sheet_name]],
      columns = col_map,
      rows = row_map,
      filters = .filters[[sheet_name]]
    )
  }
  
  return(mapping)
}