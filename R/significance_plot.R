#' Create Significance Overview Plot
#'
#' Generates a bubble plot showing coefficient significance and magnitude
#'
#' @param .plot_data Dataframe prepared for plotting
#' @param .variable Character. Variable name for title
#' @param .x_axis Character. X-axis column name
#' @param .y_axis Character. Y-axis column name
#' @return ggplot2 object
#' @keywords internal
create_significance_plot <- function(.plot_data, .variable, .x_axis, .y_axis) {
  
  # Check if we have data
  if (nrow(.plot_data) == 0) {
    # Return empty plot with message
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No data available for the selected filters",
          size = 6
        ) +
        ggplot2::theme_void()
    )
  }
  
  # Create the plot
  p <- .plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = !!rlang::sym(.x_axis),
      y = !!rlang::sym(.y_axis)
    )) +
    ggplot2::geom_tile(
      fill = "white",
      color = "grey90",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = abs_estimate, color = sign),
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = dplyr::case_when(
        Stars == "***" ~ "* * *",
        Stars == "**" ~ "* *",
        Stars == "*" ~ "*",
        TRUE ~ ""
      )),
      size = 5,
      color = "black",
      vjust = -1.8
    ) +
    ggplot2::scale_color_manual(
      values = c("Positive" = "#31a354", "Negative" = "#e34a33"),
      name = "Sign"
    ) +
    ggplot2::scale_size_continuous(
      range = c(5, 18),
      name = "Absolute\nCoefficient"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 13),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13, face = "bold"),
      plot.title = ggplot2::element_text(
        family = "Times New Roman",
        face = "bold",
        size = 16
      )
    ) +
    ggplot2::labs(
      title = paste0("Regression Coefficients for ", .variable),
      x = .x_axis,
      y = .y_axis
    )
  
  return(p)
}