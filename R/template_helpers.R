#' Read Excel Template and Detect Structure
#'
#' Reads the Excel template matrix exactly as-is and identifies all column headers and row labels
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
    
    # ⭐ Read full sheet as-is - NO assumptions about structure
    full_data <- openxlsx2::wb_to_df(
      wb,
      sheet = .sheet,
      col_names = FALSE,
      start_row = 1
    )
    
    message("\n=== Reading Sheet: ", .sheet, " ===")
    message("Sheet dimensions: ", nrow(full_data), " rows x ", ncol(full_data), " columns")
    
    # Find corner markers in the actual matrix
    markers <- find_corner_markers(full_data)
    
    if (is.null(markers)) {
      warning("Could not find corner markers (LU, RU, LB, RB) in sheet: ", .sheet)
      return(NULL)
    }
    
    message("Markers found:")
    message("  LU: row ", markers$lu_row, ", col ", markers$lu_col)
    message("  RU: row ", markers$ru_row, ", col ", markers$ru_col)
    message("  LB: row ", markers$lb_row, ", col ", markers$lb_col)
    message("  RB: row ", markers$rb_row, ", col ", markers$rb_col)
    message("  CS: row ", markers$cs_row, ", col ", markers$cs_col)
    message("  CE: row ", markers$ce_row, ", col ", markers$ce_col)
    
    # ⭐ STEP 1: Extract column headers from CS row
    # Read from CS column to CE column in the CS row
    cs_row <- markers$cs_row
    start_col <- markers$cs_col + 1  # Skip the CS marker itself
    end_col <- markers$ce_col - 1     # Stop before CE marker
    
    message("\nReading column headers from row ", cs_row, ", columns ", start_col, " to ", end_col)
    
    col_positions <- list()
    for (excel_col in start_col:end_col) {
      header_value <- as.character(full_data[cs_row, excel_col])
      
      # Debug: show what we're reading
      message("  Col ", excel_col, ": '", header_value, "'")
      
      # Skip the first column after CS (that's the row labels column)
      if (excel_col == start_col) {
        message("    -> SKIPPED (row labels column)")
        next
      }
      
      # Only include non-empty headers
      if (!is.na(header_value) && header_value != "" && header_value != "NA") {
        col_positions[[length(col_positions) + 1]] <- list(
          header = header_value,
          excel_col = excel_col
        )
      }
    }
    
    # ⭐ STEP 2: Extract row labels
    # Read from row after CS to row before LB, in the first data column (CS_col + 1)
    row_label_col <- markers$cs_col + 1
    start_row <- markers$cs_row + 1
    end_row <- markers$lb_row - 1
    
    message("\nReading row labels from column ", row_label_col, ", rows ", start_row, " to ", end_row)
    
    row_positions <- list()
    for (excel_row in start_row:end_row) {
      label_value <- as.character(full_data[excel_row, row_label_col])
      
      # Debug: show first 10 rows
      if (excel_row - start_row < 10) {
        message("  Row ", excel_row, ": '", label_value, "'")
      }
      
      # Only include non-empty labels
      if (!is.na(label_value) && label_value != "" && label_value != "NA") {
        row_positions[[length(row_positions) + 1]] <- list(
          label = label_value,
          excel_row = excel_row
        )
      }
    }
    
    if (end_row - start_row >= 10) {
      message("  ... (", length(row_positions), " total row labels)")
    }
    
    # Extract for UI
    col_headers <- sapply(col_positions, function(x) x$header)
    row_labels <- sapply(row_positions, function(x) x$label)
    
    # Summary
    message("\nSummary:")
    message("  Column headers (", length(col_headers), "): ", paste(col_headers, collapse = ", "))
    message("  Row labels (", length(row_labels), "): ", 
            paste(head(row_labels, 5), collapse = ", "),
            if (length(row_labels) > 5) paste0(" ... (", length(row_labels), " total)") else "")
    message("=============================\n")
    
    list(
      sheet_name = .sheet,
      markers = markers,
      col_headers = col_headers,
      row_labels = row_labels,
      col_positions = col_positions,
      row_positions = row_positions,
      n_cols = length(col_headers),
      n_rows = length(row_labels),
      workbook = wb
    )
  }) |>
    purrr::set_names(sheet_names)
  
  # Remove NULL entries
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