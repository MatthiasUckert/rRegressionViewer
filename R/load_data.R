#' Load Regression Data from File
#'
#' Loads regression data from either .rds or .xlsx file format
#'
#' @param .path Character. Path to input file (.rds or .xlsx)
#' @return Named list of dataframes
#' @keywords internal
load_regression_data <- function(.path) {
  file_ext <- tools::file_ext(.path)
  
  data <- if (file_ext == "rds") {
    load_from_rds(.path = .path)
  } else if (file_ext == "xlsx") {
    load_from_excel(.path = .path)
  } else {
    stop("Unsupported file format: ", file_ext, call. = FALSE)
  }
  
  message("Successfully loaded ", length(data), " table(s) from: ", basename(.path))
  
  return(data)
}


#' Load Data from RDS File
#'
#' Loads a named list of dataframes from an .rds file
#'
#' @param .path Character. Path to .rds file
#' @return Named list of dataframes
#' @keywords internal
load_from_rds <- function(.path) {
  tryCatch({
    data <- readRDS(.path)
    return(data)
  }, error = function(e) {
    stop("Failed to load .rds file: ", e$message, call. = FALSE)
  })
}


#' Load Data from Excel File
#'
#' Loads multiple sheets from an Excel file as a named list of dataframes
#'
#' @param .path Character. Path to .xlsx file
#' @return Named list of dataframes (one per sheet)
#' @keywords internal
load_from_excel <- function(.path) {
  tryCatch({
    sheet_names <- readxl::excel_sheets(.path)
    
    data <- purrr::map(sheet_names, function(.sheet) {
      readxl::read_excel(.path, sheet = .sheet)
    }) |>
      purrr::set_names(sheet_names)
    
    return(data)
  }, error = function(e) {
    stop("Failed to load Excel file: ", e$message, call. = FALSE)
  })
}


#' Prepare Data for App
#'
#' Processes loaded data and extracts metadata for each table
#'
#' @param .data_list Named list of dataframes
#' @return List with 'data' and 'metadata' elements
#' @keywords internal
prepare_app_data <- function(.data_list) {
  # Extract metadata for each table
  metadata <- purrr::map(.data_list, function(.df) {
    col_info <- detect_column_types(.df = .df)
    default_filters <- get_default_filter_values(.df = .df, .filter_cols = col_info$filter_cols)
    
    # Get ALL unique row names from the entire dataset (not just default filters)
    # This ensures all possible row names are available in the dropdown from the start
    all_row_choices <- unique(.df[[col_info$term_col]])
    
    list(
      filter_cols = col_info$filter_cols,
      term_col = col_info$term_col,
      model_cols = col_info$model_cols,
      filter_values = get_unique_filter_values(.df = .df, .filter_cols = col_info$filter_cols),
      default_filters = default_filters,
      initial_row_choices = all_row_choices,  # All possible row names
      n_rows = nrow(.df),
      n_models = length(col_info$model_cols)
    )
  })
  
  list(
    data = .data_list,
    metadata = metadata
  )
}