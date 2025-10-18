#' Global Variables Declaration
#'
#' Declares global variables used in NSE (non-standard evaluation) contexts
#' to avoid R CMD check NOTEs
#'
#' @keywords internal
utils::globalVariables(c(
  # From extract_coefficients function
  "value",
  "coef",
  "stars",
  
  # From prepare_plot_data function
  "Variable",
  "Estimate",
  "Stars",
  "sign",
  "model",
  
  # From create_significance_plot function
  "abs_estimate"
))