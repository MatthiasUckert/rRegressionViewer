#' Validate Input File Path
#'
#' Checks if the file exists and has the correct extension (.rds or .xlsx)
#'
#' @param .path Character. Path to the input file
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_file_path <- function(.path) {
  if (!file.exists(.path)) {
    stop("File not found: ", .path, call. = FALSE)
  }
  
  if (!stringr::str_detect(.path, "\\.(rds|xlsx)$")) {
    stop(".path must be a .rds or .xlsx file. Got: ", .path, call. = FALSE)
  }
  
  return(TRUE)
}


#' Set Up Output Directory
#'
#' Creates output directory if it doesn't exist, or uses a temporary directory
#'
#' @param .dir Character or NULL. Output directory path
#' @return Character. Path to the output directory
#' @keywords internal
setup_output_dir <- function(.dir = NULL) {
  if (is.null(.dir)) {
    .dir <- file.path(
      tempdir(), 
      paste0("regression_compare_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
  }
  
  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE)
    message("Created output directory: ", .dir)
  }
  
  return(normalizePath(.dir, mustWork = TRUE))
}


#' Validate Data Structure
#'
#' Validates that the loaded data is a named list of dataframes with proper structure
#'
#' @param .data_list Named list of dataframes
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_data_structure <- function(.data_list) {
  # Check it's a list
  if (!is.list(.data_list)) {
    stop("Data must be a list of dataframes", call. = FALSE)
  }
  
  # Check it's named
  if (is.null(names(.data_list)) || any(names(.data_list) == "")) {
    stop("All list elements must be named (these become tab names)", call. = FALSE)
  }
  
  # Check each element is a dataframe
  for (i in seq_along(.data_list)) {
    df <- .data_list[[i]]
    name <- names(.data_list)[i]
    
    if (!is.data.frame(df)) {
      stop("List element '", name, "' is not a dataframe", call. = FALSE)
    }
    
    # Check for 'term' column
    if (!"term" %in% names(df)) {
      stop("Dataframe '", name, "' must have a 'term' column", call. = FALSE)
    }
    
    # Check has at least one column before and after term
    term_idx <- which(names(df) == "term")
    if (term_idx == 1) {
      stop("Dataframe '", name, "' must have at least one filter column before 'term'", 
           call. = FALSE)
    }
    if (term_idx == ncol(df)) {
      stop("Dataframe '", name, "' must have at least one model column after 'term'", 
           call. = FALSE)
    }
  }
  
  return(TRUE)
}


#' Get First Unique Value
#'
#' Returns the first unique value from a vector (used for default filter values)
#'
#' @param .x Vector
#' @return First unique value
#' @keywords internal
get_first_value <- function(.x) {
  unique_vals <- unique(.x)
  return(unique_vals[1])
}


#' Detect Column Types in Dataframe
#'
#' Identifies which columns are filters, term, and models
#'
#' @param .df Dataframe with 'term' column
#' @return Named list with 'filter_cols', 'term_col', 'model_cols', 'term_idx'
#' @keywords internal
detect_column_types <- function(.df) {
  term_idx <- which(names(.df) == "term")
  
  list(
    filter_cols = names(.df)[1:(term_idx - 1)],
    term_col = "term",
    model_cols = names(.df)[(term_idx + 1):ncol(.df)],
    term_idx = term_idx
  )
}


#' Get Unique Filter Values
#'
#' Extracts unique values for each filter column
#'
#' @param .df Dataframe
#' @param .filter_cols Character vector of filter column names
#' @return Named list where names are filter columns and values are unique values
#' @keywords internal
get_unique_filter_values <- function(.df, .filter_cols) {
  purrr::map(.filter_cols, function(.col) {
    sort(unique(.df[[.col]]))
  }) |>
    purrr::set_names(.filter_cols)
}


#' Get Default Filter Values
#'
#' Gets the first unique value for each filter column (default selection)
#'
#' @param .df Dataframe
#' @param .filter_cols Character vector of filter column names
#' @return Named list where names are filter columns and values are first unique value
#' @keywords internal
get_default_filter_values <- function(.df, .filter_cols) {
  purrr::map(.filter_cols, function(.col) {
    get_first_value(.df[[.col]])
  }) |>
    purrr::set_names(.filter_cols)
}