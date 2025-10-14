#' Render Comparison Table
#'
#' Creates a reactable table for display in the app
#'
#' @param .df Dataframe to display (should only contain term + model columns)
#' @param .term_col Character. Name of term column
#' @return reactable object
#' @keywords internal
render_comparison_table <- function(.df, .term_col) {
  
  if (nrow(.df) == 0) {
    return(
      reactable::reactable(
        data.frame(Message = "No data matches the selected filters"),
        sortable = FALSE,
        searchable = FALSE
      )
    )
  }
  
  # Convert line breaks to HTML in all columns
  df_formatted <- dplyr::rename_with(.df, ~ gsub("\n", "<br>", ., fixed = TRUE))
  for (col in names(df_formatted)) {
    if (is.character(df_formatted[[col]])) {
      # Replace \n with <br> for HTML rendering
      df_formatted[[col]] <- gsub("\n", "<br>", df_formatted[[col]], fixed = TRUE)
    }
  }
  
  # Create column definitions
  col_defs <- list()
  
  # Style term column (sticky on left, emphasized, left-aligned)
  col_defs[[.term_col]] <- reactable::colDef(
    style = list(fontWeight = "500", whiteSpace = "normal"),
    minWidth = 150,
    sticky = "left",
    align = "left",
    html = TRUE  # Allow HTML rendering for line breaks
  )
  
  # All other columns: centered with line break support
  model_cols <- setdiff(names(df_formatted), .term_col)
  for (col in model_cols) {
    col_defs[[col]] <- reactable::colDef(
      align = "center",
      minWidth = 100,
      html = TRUE,  # Allow HTML rendering for line breaks
      style = list(whiteSpace = "normal"),
      headerStyle = list(whiteSpace = "normal", textAlign = "center")
    )
  }
  
  reactable::reactable(
    df_formatted,
    columns = col_defs,
    defaultColDef = reactable::colDef(
      minWidth = 100,
      align = "center",
      headerStyle = list(
        background = "#f7f7f7",
        fontWeight = "600",
        whiteSpace = "normal",
        textAlign = "center"
      )
    ),
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    sortable = FALSE, 
    defaultPageSize = 100,
    showPagination = FALSE,  # No pagination - show all rows
    striped = FALSE,  # No striping - all white background
    compact = TRUE,   # More compact
    wrap = FALSE,
    height = "auto",  # Auto height to show all rows
    theme = reactable::reactableTheme(
      backgroundColor = "#ffffff",  # White background
      borderColor = "#e0e0e0",
      stripedColor = "#ffffff",  # Keep stripes white too
      highlightColor = "#f5f5f5"  # Light gray on hover
    )
  )
}