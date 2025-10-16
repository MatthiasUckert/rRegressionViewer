#' Extract Coefficients and Significance from Formatted Data
#'
#' Extracts coefficient values and significance stars from formatted strings
#'
#' @param .df Dataframe with formatted regression results
#' @param .term_col Character. Name of term column
#' @param .filter_cols Character vector. Names of filter columns
#' @return Dataframe with extracted coefficients and stars
#' @keywords internal
extract_coefficients <- function(.df, .term_col, .filter_cols) {
  
  # Get model columns (everything except term and filter columns)
  model_cols <- setdiff(names(.df), c(.term_col, .filter_cols))
  
  # Pivot longer to get all coefficient values
  df_long <- .df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(model_cols),
      names_to = "model",
      values_to = "value"
    )
  
  # Filter out empty cells and cells without statistics
  df_long <- df_long |>
    dplyr::filter(!value == "") |>
    dplyr::filter(grepl("\n\\(", value))
  
  # Separate coefficient and statistics
  df_long <- df_long |>
    tidyr::separate(value, c("coef", "stats"), sep = "\n")
  
  # Extract stars and clean coefficient
  df_long <- df_long |>
    dplyr::mutate(
      stars = stringi::stri_extract_last_regex(coef, "\\*{1,3}"),
      coef = as.numeric(dplyr::case_when(
        is.na(stars) ~ coef,
        TRUE ~ stringi::stri_replace_first_fixed(coef, stars, "")
      )),
      stars = dplyr::if_else(is.na(stars), "ns", stars)
    )
  
  # Rename columns for consistency
  df_long <- df_long |>
    dplyr::rename(
      Variable = dplyr::all_of(.term_col),
      Estimate = coef,
      Stars = stars
    )
  
  return(df_long)
}


#' Prepare Data for Significance Plot
#'
#' Filters and reshapes data for plotting
#'
#' @param .df_coef Dataframe with extracted coefficients
#' @param .variable Character. Variable/term to plot
#' @param .fixed_filters Named list. Filter values to fix (not X or Y)
#' @param .x_axis Character. Filter column for X-axis
#' @param .y_axis Character. Filter column for Y-axis
#' @return Dataframe ready for plotting
#' @keywords internal
prepare_plot_data <- function(.df_coef, .variable, .fixed_filters, .x_axis, .y_axis) {
  
  # Start with full data
  result <- .df_coef
  
  # Filter to selected variable
  result <- result |>
    dplyr::filter(Variable == .variable)
  
  # Apply fixed filters (all filters except X and Y axes)
  for (filter_col in names(.fixed_filters)) {
    if (filter_col %in% c(.x_axis, .y_axis)) {
      next  # Skip X and Y axes
    }
    
    filter_value <- .fixed_filters[[filter_col]]
    result <- result |>
      dplyr::filter(!!rlang::sym(filter_col) == filter_value)
  }
  
  # Add plotting variables
  result <- result |>
    dplyr::mutate(
      sig_level = dplyr::case_when(
        Stars == "***" ~ 3,
        Stars == "**" ~ 2,
        Stars == "*" ~ 1,
        TRUE ~ 0
      ),
      sign = dplyr::if_else(Estimate >= 0, "Positive", "Negative"),
      abs_estimate = abs(Estimate)
    )
  
  return(result)
}