#' Apply Filters to Dataframe
#'
#' Filters dataframe based on selected filter values
#'
#' @param .df Dataframe to filter
#' @param .filter_values Named list of filter column:value pairs
#' @return Filtered dataframe
#' @keywords internal
apply_filters <- function(.df, .filter_values) {
  result <- .df
  
  for (col in names(.filter_values)) {
    selected_value <- .filter_values[[col]]
    
    # Filter by selected value
    result <- dplyr::filter(result, !!rlang::sym(col) == selected_value)
  }
  
  return(result)
}


#' Get Filtered Data
#'
#' Main filtering function that applies all filters
#'
#' @param .df Dataframe to filter
#' @param .filters Named list of filter values
#' @return Filtered dataframe
#' @keywords internal
get_filtered_data <- function(.df, .filters) {
  # Apply filters
  filtered <- apply_filters(.df = .df, .filter_values = .filters)
  
  return(filtered)
}


#' Prepare Table Data for Display
#'
#' Selects columns to display based on user selections
#'
#' @param .df Dataframe (already filtered)
#' @param .filter_cols Character vector of filter column names (NOT shown in table)
#' @param .term_col Character. Name of term column
#' @param .visible_model_cols Character vector of model columns to show
#' @return Dataframe with selected columns only
#' @keywords internal
prepare_table_data <- function(.df, .filter_cols, .term_col, .visible_model_cols) {
  # Select columns: term + visible model cols ONLY (filter cols are hidden)
  cols_to_show <- c(.term_col, .visible_model_cols)
  
  # Make sure all columns exist
  cols_to_show <- intersect(cols_to_show, names(.df))
  
  result <- dplyr::select(.df, dplyr::all_of(cols_to_show))
  
  return(result)
}