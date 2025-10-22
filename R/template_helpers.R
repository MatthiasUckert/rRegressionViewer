#' Read Excel Template and Detect Structure
#'
#' Reads template using column/row identifiers (ID="C", column V) and "_" placeholders
#'
#' @param .path Character. Path to template Excel file
#' @return List with template info for each sheet
#' @keywords internal
read_template_structure <- function(.path) {
  
  wb <- openxlsx2::wb_load(.path)
  sheet_names <- openxlsx2::wb_get_sheet_names(wb)
  
  template_info <- purrr::map(sheet_names, function(.sheet) {
    
    message("\n=== Reading Sheet: ", .sheet, " ===")
    
    # Read with column names from row 1
    tab_ <- openxlsx2::wb_to_df(
      wb,
      sheet = .sheet,
      col_names = TRUE,
      start_row = 1
    )
    
    message("  Sheet dimensions: ", nrow(tab_), " rows x ", ncol(tab_), " columns")
    
    # Check if ID and V columns exist
    if (!"ID" %in% names(tab_) || !"V" %in% names(tab_)) {
      warning("Sheet '", .sheet, "' must have 'ID' and 'V' columns in row 1")
      return(NULL)
    }
    
    # Get column mappings from the "C" row (where ID == "C")
    cols_ <- tab_ |>
      dplyr::filter(ID == "C") |>
      dplyr::select(dplyr::starts_with("C")) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "ColName", values_to = "TabName") |>
      dplyr::left_join(
        tibble::tibble(
          ColNum = seq_len(ncol(tab_)),
          ColName = colnames(tab_)
        ),
        by = "ColName"
      )
    
    # Get row labels from "V" column
    rows_ <- tibble::tibble(
      RowNum = seq_len(nrow(tab_)),
      RowName = tab_$V
    )
    
    # Find all cells with "_" placeholder
    placeholder_cells <- tibble::as_tibble(which(tab_ == "_", arr.ind = TRUE)) |>
      dplyr::rename(RowNum = row, ColNum = col) |>
      dplyr::left_join(cols_, by = "ColNum") |>
      dplyr::left_join(rows_, by = "RowNum") |>
      dplyr::filter(!is.na(TabName), !is.na(RowName)) |>
      dplyr::select(RowNum, ColNum, TabName, RowName) %>% 
      dplyr::mutate(RowNum = RowNum + 1L)
    
    # Convert to list format for compatibility
    placeholder_list <- purrr::pmap(placeholder_cells, function(RowNum, ColNum, TabName, RowName) {
      list(
        excel_row = RowNum,
        excel_col = ColNum,
        col_header = as.character(TabName),
        row_label = as.character(RowName)
      )
    })
    
    # Get unique headers and labels (for UI) - preserve order of first appearance
    col_headers <- character()
    for (cell in placeholder_list) {
      if (!cell$col_header %in% col_headers) {
        col_headers <- c(col_headers, cell$col_header)
      }
    }
    
    row_labels <- character()
    for (cell in placeholder_list) {
      if (!cell$row_label %in% row_labels) {
        row_labels <- c(row_labels, cell$row_label)
      }
    }
    
    message("  Found ", nrow(placeholder_cells), " cells with '_' placeholder")
    message("  Unique columns (", length(col_headers), "): ", paste(col_headers, collapse = ", "))
    message("  Unique rows (", length(row_labels), "): ", 
            paste(head(row_labels, 5), collapse = ", "),
            if (length(row_labels) > 5) paste0(" ... (", length(row_labels), " total)") else "")
    message("=============================\n")
    
    list(
      sheet_name = .sheet,
      col_headers = col_headers,
      row_labels = row_labels,
      placeholder_cells = placeholder_list,
      n_cols = length(col_headers),
      n_rows = length(row_labels)
    )
  }) |>
    purrr::set_names(sheet_names)
  
  template_info <- purrr::compact(template_info)
  
  return(template_info)
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