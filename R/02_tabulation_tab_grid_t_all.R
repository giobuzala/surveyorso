#' **Apply `tab_grid_t()` to all grid questions**
#'
#' @description
#' This function maps `tab_grid_t()` across all grid question prefixes in a data frame.
#' Optionally, you can specify a `y` variable for crosstabs and/or exclude prefixes from being tabulated.
#' Additional arguments are passed to `tab_grid_t()`.
#'
#' Grid questions are detected automatically by prefixes before the first underscore (`_`).
#' Multiple response (pick-any) sets are skipped, since those should be handled with `tab_mr()`.
#'
#' @param data A data frame containing the survey data.
#' @param y (Optional) A variable to cross-tabulate against a grid set. Defaults to `NULL`.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param exclude Variables to exclude from tabulation. Defaults to `NULL`. Always excludes `y` (if specified), `weight` (if specified), and variable literally named "weight".
#' @param ... Additional arguments passed to `tab_grid_t()`.
#'
#' @return
#' A named list of frequency tables or crosstabs, one per grid question prefix.
#'
#' @examples
#' # Frequency tables of all grid questions
#' tab_grid_t_all(data = survey_data)
#'
#' # Weighted crosstabs of all grid questions by region, excluding Q99
#' tab_grid_t_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#' @export

tab_grid_t_all <- function(data, y = NULL, weight = NULL, exclude = NULL, ...) {
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

  # Resolve exclude (allow symbols or strings)
  if (!identical(exclude_exprs, quote(NULL))) {
    exclude_list <- as.list(exclude_exprs)
    exclude_names <- vapply(exclude_list, rlang::as_string, character(1))
  } else {
    exclude_names <- NULL
  }

  # Always exclude "weight", y, and weight vars
  exclude_all <- c("weight", exclude_names, y_name, weight_name)

  # Identify all prefixes (everything before the first underscore, or full name if no underscore)
  all_prefixes <- unique(sub("_.*$", "", names(data)))

  # Exclude
  prefixes <- setdiff(all_prefixes, exclude_all)

  # If everything is excluded
  if (length(prefixes) == 0) {
    stop("The provided dataset is empty.", call. = FALSE)
  }

  # Check for invalid prefixes (those with fewer than 2 vars)
  bad_prefixes <- prefixes[vapply(prefixes, function(pref) {
    length(grep(paste0("^", pref, "(_|$)"), names(data), value = TRUE)) < 2
  }, logical(1))]

  if (length(bad_prefixes) > 0) {
    stop("One or more variables in the provided dataset are not grid questions.", call. = FALSE)
  }

  # Function ----

  # Loop across valid prefixes
  purrr::map(prefixes, function(pref) {
    if (is.null(y_name)) {
      tab_grid_t(data = data, x = !!rlang::sym(pref), weight = !!weight_expr, ...)
    } else {
      tab_grid_t(
        data = data,
        x = !!rlang::sym(pref),
        y = !!rlang::sym(y_name),
        weight = !!weight_expr,
        ...
      )
    }
  }) %>%
    purrr::set_names(prefixes)
}
