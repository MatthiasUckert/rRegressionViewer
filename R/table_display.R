#' Render Comparison Table
#'
#' Creates a reactable table for display in the app
#'
#' @param .df Dataframe to display (should only contain term + model columns)
#' @param .term_col Character. Name of term column
#' @param .bold_rows Character vector. Row term values to make bold (NULL if none)
#' @param .border_rows Character vector. Row term values to add borders (NULL if none)
#' @return reactable object
#' @keywords internal
render_comparison_table <- function(.df, .term_col, .bold_rows = NULL, .border_rows = NULL) {
  if (nrow(.df) == 0) {
    return(
      reactable::reactable(
        data.frame(Message = "No data matches the selected filters"),
        sortable = FALSE,
        searchable = FALSE
      )
    )
  }
  
  # Convert line breaks to HTML in column names
  df_formatted <- dplyr::rename_with(.df, ~ gsub("\n", "<br>", ., fixed = TRUE))
  
  # Convert line breaks to HTML in all cell values
  for (col in names(df_formatted)) {
    if (is.character(df_formatted[[col]])) {
      df_formatted[[col]] <- gsub("\n", "<br>", df_formatted[[col]], fixed = TRUE)
    }
  }
  
  # Get the formatted term column name
  term_col_formatted <- gsub("\n", "<br>", .term_col, fixed = TRUE)
  
  # Create column definitions
  col_defs <- list()
  
  # Style term column (sticky on left, emphasized, left-aligned, vertically centered)
  col_defs[[term_col_formatted]] <- reactable::colDef(
    style = list(
      fontWeight = "500",
      whiteSpace = "normal",
      fontFamily = "'Times New Roman', Times, serif",
      verticalAlign = "middle",
      paddingTop = "6px",
      paddingBottom = "6px"
    ),
    minWidth = 150,
    sticky = "left",
    align = "left",
    html = TRUE,
    headerStyle = list(
      fontFamily = "'Times New Roman', Times, serif",
      whiteSpace = "normal",
      borderBottom = "2px solid #333",
      borderTop = "2px solid #333",
      paddingTop = "8px",
      paddingBottom = "8px",
      backgroundColor = "#ffffff",
      fontWeight = "600"
    )
  )
  
  # All other columns: centered with line break support and vertical centering
  model_cols <- setdiff(names(df_formatted), term_col_formatted)
  for (col in model_cols) {
    col_defs[[col]] <- reactable::colDef(
      align = "center",
      minWidth = 100,
      html = TRUE,
      style = list(
        whiteSpace = "normal",
        fontFamily = "'Times New Roman', Times, serif",
        verticalAlign = "middle",
        paddingTop = "6px",
        paddingBottom = "6px"
      ),
      headerStyle = list(
        whiteSpace = "normal",
        textAlign = "center",
        fontFamily = "'Times New Roman', Times, serif",
        borderBottom = "2px solid #333",
        borderTop = "2px solid #333",
        paddingTop = "8px",
        paddingBottom = "8px",
        backgroundColor = "#ffffff",
        fontWeight = "600"
      )
    )
  }
  
  # Create row styling function if formatting is needed
  row_style <- NULL
  has_bold <- !is.null(.bold_rows) && length(.bold_rows) > 0
  has_border <- !is.null(.border_rows) && length(.border_rows) > 0
  
  if (has_bold || has_border) {
    # Convert to HTML format to match the formatted data
    bold_rows_html <- if (has_bold) gsub("\n", "<br>", .bold_rows, fixed = TRUE) else character(0)
    border_rows_html <- if (has_border) gsub("\n", "<br>", .border_rows, fixed = TRUE) else character(0)
    
    row_style <- reactable::JS(sprintf(
      "
      function(rowInfo) {
        var boldRows = %s;
        var borderRows = %s;
        var termValue = rowInfo.row['%s'];

        var style = {};

        if (boldRows.includes(termValue)) {
          style.fontWeight = 'bold';
        }

        if (borderRows.includes(termValue)) {
          style.borderBottom = '1px solid #666';
        }

        if (Object.keys(style).length > 0) {
          return style;
        }
      }
    ",
      jsonlite::toJSON(bold_rows_html),
      jsonlite::toJSON(border_rows_html),
      term_col_formatted
    ))
  }
  
  reactable::reactable(
    df_formatted,
    columns = col_defs,
    defaultColDef = reactable::colDef(
      minWidth = 100,
      align = "center",
      headerStyle = list(
        background = "#ffffff",
        fontWeight = "600",
        whiteSpace = "normal",
        textAlign = "center",
        fontFamily = "'Times New Roman', Times, serif",
        borderBottom = "1px solid #333",
        borderTop = "1px solid #333",
        paddingTop = "6px",
        paddingBottom = "6px"
      ),
      style = list(
        fontFamily = "'Times New Roman', Times, serif",
        verticalAlign = "middle",
        paddingTop = "4px",
        paddingBottom = "4px"
      )
    ),
    rowStyle = row_style,
    bordered = FALSE,  # Remove cell borders
    highlight = TRUE,
    resizable = TRUE,
    sortable = FALSE, 
    defaultPageSize = 100,
    showPagination = FALSE,
    striped = FALSE,
    compact = TRUE,  # Compact mode for tighter spacing
    wrap = FALSE,
    height = "auto",
    theme = reactable::reactableTheme(
      backgroundColor = "#ffffff",
      borderColor = "transparent",  # No internal borders
      stripedColor = "#ffffff",
      highlightColor = "#f8f9fa",
      style = list(
        fontFamily = "'Times New Roman', Times, serif",
        fontSize = "14px"
      ),
      headerStyle = list(
        borderBottom = "1px solid #333",
        borderTop = "1px solid #333"
      )
    )
  )
}