#' Save Mapping Configuration
#'
#' Saves the current mapping configuration to a JSON file
#'
#' @param .mapping List with mapping configuration
#' @param .dir Character. Directory to save mapping
#' @param .name Character. Name for the mapping file
#' @return Character. Path to saved file
#' @keywords internal
save_mapping_config <- function(.mapping, .dir, .name) {
  
  # Ensure directory exists
  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE)
  }
  
  # Create filename
  filename <- paste0(.name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
  filepath <- file.path(.dir, filename)
  
  # Save as JSON
  jsonlite::write_json(.mapping, filepath, pretty = TRUE, auto_unbox = TRUE)
  
  message("Mapping saved to: ", filepath)
  
  return(filepath)
}


#' Load Mapping Configuration
#'
#' Loads a mapping configuration from a JSON file
#'
#' @param .path Character. Path to mapping JSON file
#' @return List with mapping configuration
#' @keywords internal
load_mapping_config <- function(.path) {
  
  if (!file.exists(.path)) {
    stop("Mapping file not found: ", .path, call. = FALSE)
  }
  
  mapping <- jsonlite::read_json(.path, simplifyVector = TRUE)
  
  message("Mapping loaded from: ", .path)
  
  return(mapping)
}


#' List Available Mappings
#'
#' Lists all saved mapping files in a directory
#'
#' @param .dir Character. Directory containing mappings
#' @return Character vector of mapping file paths
#' @keywords internal
list_available_mappings <- function(.dir) {
  
  if (!dir.exists(.dir)) {
    return(character(0))
  }
  
  files <- list.files(.dir, pattern = "\\.json$", full.names = TRUE)
  
  return(files)
}


#' Create Mapping Object
#'
#' Creates a structured mapping object
#'
#' @param .sheet_mappings List. Sheet name to table name mappings
#' @param .column_mappings List. Column mappings for each sheet
#' @param .row_mappings List. Row mappings for each sheet
#' @param .filters List. Filter values for each sheet
#' @return List with complete mapping configuration
#' @keywords internal
create_mapping_object <- function(.sheet_mappings, .column_mappings, .row_mappings, .filters) {
  
  list(
    created_at = Sys.time(),
    sheet_mappings = .sheet_mappings,
    column_mappings = .column_mappings,
    row_mappings = .row_mappings,
    filters = .filters
  )
}