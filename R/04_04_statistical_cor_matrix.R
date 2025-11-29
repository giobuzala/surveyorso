#' **Compute a correlation matrix**
#'
#' @description
#' This function computes a correlation matrix across all numeric variables in a dataset.
#'
#' @param data A data frame containing variables to correlate.
#' @param weight Optional. Name of the weight variable. If NULL, computes unweighted correlations.
#' @param id_var ID column name to exclude (default = Vrid).
#' @param method Correlation method ("pearson", "spearman", or "kendall").
#' @param half Logical; if TRUE (default), displays only the lower triangle. If FALSE, displays the full matrix.
#' @param round Number of decimal places to round correlations to.
#' @param min Optional numeric threshold. If specified, correlations with |r| < min are replaced with NA.
#'
#' @details
#' Only numeric variables are allowed. Non-numeric variables (e.g., factors, characters, or dates) will trigger an error.
#'
#' @return
#' A correlation matrix, as a data frame.
#'
#' @examples
#' # Weighted Pearson (default) correlations, lower half of the matrix shown
#' cor_matrix(data = data_clust, weight = weight_var)
#'
#' # Unweighted Spearman correlations, full matrix shown, with minimum threshold of 0.3
#' cor_matrix(data = data_clust, method = "spearman", half = FALSE, min = 0.3)
#'
#' @export

cor_matrix <- function(data, weight = NULL, id_var = Vrid, method = "pearson", round = 3, half = TRUE, min = NULL) {
  # Check required packages ----

  required_pkgs <- c("rlang", "stats", "wCorr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  # Capture variable expressions
  id_var <- rlang::as_name(rlang::enquo(id_var))
  weight_expr <- rlang::enquo(weight)
  weight_var <- if (!rlang::quo_is_null(weight_expr)) rlang::as_name(weight_expr) else NULL

  # Handle errors
  if (!is.null(weight_var) && !weight_var %in% names(data)) {
    stop("Weight variable ", weight_var, " does not exist in the dataset.", call. = FALSE)
  }

  if (!id_var %in% names(data)) {
    stop("ID variable ", id_var, " does not exist in the dataset.", call. = FALSE)
  }

  # Determine variables to exclude
  exclude_vars <- c(id_var)

  weight_like <- names(data)[tolower(names(data)) == "weight"]
  exclude_vars <- unique(c(exclude_vars, weight_like))

  if (!is.null(weight_var)) {
    exclude_vars <- unique(c(exclude_vars, weight_var))
  }

  dat <- data[setdiff(names(data), exclude_vars)]
  vars <- names(dat)

  # Incorrect types
  bad_types <- vapply(vars, function(v) {
    cls <- class(data[[v]])[1]
    !(cls %in% c("numeric"))
  }, logical(1))

  if (any(bad_types)) {
    stop(
      "The following variable(s) are of a type that is not allowed (factor, character or date): ",
      paste(vars[bad_types], collapse = ", "),
      "\nOnly numeric variables can be included.",
      call. = FALSE
    )
  }

  # Create matrix ----

  if (!is.null(weight_var)) {
    # Weighted correlation matrix
    wcor_mat <- matrix(NA, nrow = length(vars), ncol = length(vars),
                       dimnames = list(vars, vars))
    w <- data[[weight_var]]

    for (i in seq_along(vars)) {
      for (j in seq_along(vars)) {
        if (i >= j) {
          x <- dat[[vars[i]]]
          y <- dat[[vars[j]]]
          keep <- stats::complete.cases(x, y, w)

          if (sum(keep) > 2) {
            wcor_mat[i, j] <- wCorr::weightedCorr(
              x[keep], y[keep], weights = w[keep], method = method
            )
          }
        }
      }
    }

    # Mirror lower triangle to upper to make full symmetric matrix
    wcor_mat[upper.tri(wcor_mat)] <- t(wcor_mat)[upper.tri(wcor_mat)]

    cor_mat <- as.data.frame(wcor_mat)

  } else {
    # Unweighted correlation matrix
    cor_mat <- as.data.frame(stats::cor(dat, use = "pairwise.complete.obs", method = method))
  }

  # Output ----

  cor_mat <- round(cor_mat, round)

  # Apply minimum threshold
  if (!is.null(min)) {
    cor_mat[abs(cor_mat) < min] <- NA
  }

  # Half or full
  if (half) {
    cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA
  }

  return(cor_mat)
}
