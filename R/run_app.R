#' Run Regression Comparison Shiny App
#'
#' Launch an interactive Shiny app to compare regression specifications
#' side-by-side with independent filtering capabilities.
#'
#' @param .path Character. Path to input data file. Supports:
#'   \itemize{
#'     \item \code{.rds} file containing a named list of dataframes
#'     \item \code{.xlsx} file with multiple sheets (each sheet = one table)
#'   }
#' @param .dir Character or NULL. Directory for outputs, cache, and exports.
#'   If NULL (default), creates a temporary directory for the session.
#'   If specified, will save exports and cache to this location.
#'
#' @details
#' Each dataframe must have the following structure:
#' \itemize{
#'   \item Columns before "term": Filter columns (e.g., Cluster, Controls, part)
#'   \item Column "term": Row identifier (always required and visible)
#'   \item Columns after "term": Regression model results
#' }
#'
#' The app provides:
#' \itemize{
#'   \item One tab per table/test
#'   \item Dynamic filtering with dropdown filters
#'   \item Column show/hide functionality
#'   \item Dynamic detection of filter and model columns
#' }
#'
#' @return Launches Shiny app (no return value)
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with RDS
#' run_regression_comparison(.path = "data/regression_results.rds")
#'
#' # With custom output directory
#' run_regression_comparison(
#'   .path = "data/regression_results.rds",
#'   .dir = "output/regression_comparisons"
#' )
#'
#' # With Excel file
#' run_regression_comparison(.path = "data/regressions.xlsx")
#' }
run_regression_comparison <- function(.path, .dir = NULL) {
  
  # 1. Validate inputs
  message("Validating inputs...")
  validate_file_path(.path = .path)
  
  # 2. Set up output directory
  message("Setting up output directory...")
  .dir <- setup_output_dir(.dir = .dir)
  
  # 3. Load data
  message("Loading data...")
  data_list <- load_regression_data(.path = .path)
  
  # 4. Validate data structure
  message("Validating data structure...")
  validate_data_structure(.data_list = data_list)
  
  # 5. Prepare data for app (extract metadata)
  message("Preparing app data...")
  app_data <- prepare_app_data(.data_list = data_list)
  
  # 6. Print summary
  message("\n=== Data Summary ===")
  message("Number of tables: ", length(app_data$data))
  purrr::iwalk(app_data$metadata, function(.meta, .name) {
    message("\nTable: ", .name)
    message("  - Rows: ", .meta$n_rows)
    message("  - Filter columns: ", length(.meta$filter_cols), 
            " (", paste(.meta$filter_cols, collapse = ", "), ")")
    message("  - Model columns: ", .meta$n_models)
  })
  message("\n===================\n")
  
  # 7. Launch app
  message("Launching Regression Comparison App...")
  message("Data loaded from: ", .path)
  message("Output directory: ", .dir)
  message("\n")
  
  shiny::shinyApp(
    ui = app_ui(.app_data = app_data),
    server = function(input, output, session) {
      app_server(input, output, session, .app_data = app_data, .dir = .dir)
    }
  )
}