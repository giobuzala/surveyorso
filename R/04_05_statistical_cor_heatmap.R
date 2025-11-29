#' **Plot a correlation heatmap**
#'
#' @description
#' This function creates an interactive heatmap of a correlation matrix. Hover labels display variable pairs and their correlation values.
#'
#' @param cor_mat Correlation matrix or a data frame.
#' @param exclude Optional variable(s) to exclude.
#'
#' @details
#' The function accepts the output of `cor_matrix()` and optionally allows excluding selected variables from the visualization.
#'
#' @return
#' A plotly heatmap object.
#'
#' @examples
#' # Basic heatmap pipeline
#' cor_matrix(data = data) %>% cor_heatmap()
#'
#' @export

cor_heatmap <- function(cor_mat, exclude = NULL) {
  # Check required packages ----

  required_pkgs <- c("rlang", "wCorr", "plotly")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Convert to matrix
  cor_mat <- as.matrix(cor_mat)

  # Handle errors
  if (!is.matrix(cor_mat) && !is.data.frame(cor_mat)) {
    stop("Input must be a correlation matrix or data frame.", call. = FALSE)
  }

  if (nrow(cor_mat) != ncol(cor_mat)) {
    stop(
      "Input looks like raw data, not a correlation matrix. Try feeding the output of cor_matrix() instead.",
      call. = FALSE
    )
  }

  if (is.null(rownames(cor_mat)) || is.null(colnames(cor_mat))) {
    stop("Input looks like raw data, not a correlation matrix. Try feeding the output of cor_matrix() instead.", call. = FALSE)
  }

  # Handle exclusions
  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    exclude_expr <- rlang::enquo(exclude)

    exclude_vars <- tryCatch(
      {eval(rlang::get_expr(exclude_expr), envir = parent.frame())},
      error = function(e) {
        as.character(rlang::get_expr(exclude_expr))
      }
    )

    exclude_vars <- as.character(exclude_vars)

    keep <- setdiff(colnames(cor_mat), exclude_vars)
    cor_mat <- cor_mat[keep, keep, drop = FALSE]
  }

  # Hover text
  hover_text <- matrix(
    paste0(
      "Variable 1: ", rep(rownames(cor_mat), times = ncol(cor_mat)),
      "<br>Variable 2: ", rep(colnames(cor_mat), each = nrow(cor_mat)),
      "<br>Correlation: ", round(cor_mat, 3)
    ),
    nrow = nrow(cor_mat), ncol = ncol(cor_mat)
  )

  # Plot ----

  plotly::plot_ly(
    z = cor_mat,
    x = colnames(cor_mat),
    y = rownames(cor_mat),
    type = "heatmap",
    colorscale = "RdBu",
    zmin = -1, zmax = 1,
    reversescale = TRUE,
    text = hover_text,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = list(text = "Correlation Heatmap", x = 0.5, xanchor = "center", yanchor = "top",
                   font = list(size = 20, family = "Helvetica", color = "black")),
      xaxis = list(title = "", tickangle = 45, showgrid = FALSE),
      yaxis = list(title = "", autorange = "reversed", showgrid = FALSE),
      legend = list(x = 0.5, y = -0.2, orientation = "h")
    )
}
