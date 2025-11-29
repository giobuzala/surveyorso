#' **Apply `tab()` to multiple variables**
#'
#' @description
#' This function maps `tab()` across all variables in a data frame.
#' Optionally, you can specify a `y` variable for crosstabs and/or exclude variables from being tabulated.
#' Additional arguments are passed to `tab()`.
#'
#' @param data A data frame containing the survey data.
#' @param y (Optional) A variable to cross-tabulate against all `x` variables.
#' @param exclude Variables to exclude from tabulation. Defaults to `NULL`. Always excludes `y` (if specified), `weight` (if specified), and variable literally named "weight".
#' @param ... Additional arguments passed to `tab()`.
#'
#' @return
#' A named list of frequency tables or crosstabs, one per variable.
#'
#' @examples
#' # Unweighted frequency tables of all questions
#' tab_all(data = survey_data)
#'
#' # Weighted crosstabs of all questions by region, excluding Q99
#' tab_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#' @export

tab_all <- function(data, y = NULL, weight = NULL, exclude = NULL, ...) {
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

  # Resolve y and weight
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

  # Variables to tabulate
  vars <- setdiff(names(data), exclude_all)

  # If everything is excluded
  if (length(vars) == 0) {
    stop("The provided dataset is empty.", call. = FALSE)
  }

  # Function ----

  # Loop
  purrr::map(vars, function(var) {
    x_sym <- rlang::sym(var)

    if (is.null(y_name)) {
      tab(data = data, x = !!x_sym, weight = !!weight_expr, ...)
    } else {
      tab(data = data, x = !!x_sym, y = !!rlang::sym(y_name), weight = !!weight_expr, ...)
    }
  }) %>%
    purrr::set_names(vars)
}
