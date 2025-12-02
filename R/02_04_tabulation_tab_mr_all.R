#' **Apply `tab_mr()` to multiple multiple response sets**
#'
#' @description
#' This function maps `tab_mr()` across all multiple response set question prefixes in a data frame.
#' Optionally, you can specify a `y` variable for crosstabs and/or exclude prefixes from being tabulated.
#' Additional arguments are passed to `tab_mr()`.
#'
#' Multiple response sets are detected automatically by prefixes before the first underscore (`_`).
#'
#' @param data A data frame containing the survey data.
#' @param y (Optional) A variable to cross-tabulate against a multiple response set. Defaults to `NULL`.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param exclude Variables to exclude from tabulation. Defaults to `NULL`. Always excludes `y` (if specified), `weight` (if specified), and variable literally named "weight".
#' @param ... Additional arguments passed to `tab_mr()`.
#'
#' @return
#' A named list of tables or crosstabs, one per multiple response set prefix.
#'
#' @examples
#' # Unweighted proportions tables of all multiple response sets
#' tab_mr_all(data = survey_data)
#'
#' # Weighted proportions crosstabs of all multiple response sets by region, excluding Q99
#' tab_mr_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#'
#' @export

tab_mr_all <- function(data, y = NULL, weight = NULL, exclude = NULL, ...) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "purrr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Capture variable expressions
  y_expr <- substitute(y)
  weight_expr <- substitute(weight)
  exclude_exprs <- substitute(exclude)

  # Resolve names safely
  y_name <- if (!identical(y_expr, quote(NULL))) rlang::as_string(y_expr) else NULL
  weight_name <- if (!identical(weight_expr, quote(NULL))) rlang::as_string(weight_expr) else NULL

  # Resolve exclude
  if (!identical(exclude_exprs, quote(NULL))) {
    exclude_list <- as.list(exclude_exprs)
    exclude_names <- vapply(exclude_list, rlang::as_string, character(1))
  } else {
    exclude_names <- NULL
  }

  # Always exclude "weight", y, and weight vars
  exclude_all <- c("weight", exclude_names, y_name, weight_name)

  # Identify MR variables and prefixes
  mr_vars <- names(data)[grepl("_", names(data))]
  prefixes <- unique(sub("_.*$", "", mr_vars))

  # Apply exclude
  prefixes <- setdiff(prefixes, exclude_all)

  # If everything is excluded
  if (length(prefixes) == 0) {
    stop("The provided dataset is empty.", call. = FALSE)
  }

  # Identify non-MR variables
  non_mr <- setdiff(names(data), c(mr_vars, exclude_all))
  if (length(non_mr) > 0) {
    stop("One or more variables in the provided dataset are not multiple response sets.", call. = FALSE)
  }

  # Function ----

  # Loop across prefixes
  purrr::map(prefixes, function(pref) {
    if (is.null(y_name)) {
      tab_mr(data = data, x = !!rlang::sym(pref), weight = !!weight_expr, ...)
    } else {
      tab_mr(data = data, x = !!rlang::sym(pref), y = !!rlang::sym(y_name), weight = !!weight_expr, ...)
    }
  }) %>%
    purrr::set_names(prefixes)
}
