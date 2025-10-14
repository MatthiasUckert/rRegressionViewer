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

  # Style term column (sticky on left, emphasized, left-aligned)
  col_defs[[term_col_formatted]] <- reactable::colDef(
    style = list(
      fontWeight = "500",
      whiteSpace = "normal",
      fontFamily = "'Times New Roman', Times, serif"
    ),
    minWidth = 150,
    sticky = "left",
    align = "left",
    html = TRUE,
    headerStyle = list(
      fontFamily = "'Times New Roman', Times, serif",
      whiteSpace = "normal",
      borderBottom = "2px solid #000000" # Black border below header
    )
  )

  # All other columns: centered with line break support
  model_cols <- setdiff(names(df_formatted), term_col_formatted)
  for (col in model_cols) {
    col_defs[[col]] <- reactable::colDef(
      align = "center",
      minWidth = 100,
      html = TRUE,
      style = list(
        whiteSpace = "normal",
        fontFamily = "'Times New Roman', Times, serif"
      ),
      headerStyle = list(
        whiteSpace = "normal",
        textAlign = "center",
        fontFamily = "'Times New Roman', Times, serif",
        borderBottom = "2px solid #000000" # Black border below header
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
          style.borderBottom = '1px solid #000000';
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
        background = "#f7f7f7",
        fontWeight = "600",
        whiteSpace = "normal",
        textAlign = "center",
        fontFamily = "'Times New Roman', Times, serif",
        borderBottom = "2px solid #000000" # Black border below all headers
      ),
      style = list(
        fontFamily = "'Times New Roman', Times, serif"
      )
    ),
    rowStyle = row_style,
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    sortable = FALSE, 
    defaultPageSize = 100,
    showPagination = FALSE,
    striped = FALSE,
    compact = TRUE,
    wrap = FALSE,
    height = "auto",
    theme = reactable::reactableTheme(
      backgroundColor = "#ffffff",
      borderColor = "#e0e0e0",
      stripedColor = "#ffffff",
      highlightColor = "#f5f5f5",
      style = list(
        fontFamily = "'Times New Roman', Times, serif"
      )
    )
  )
}
