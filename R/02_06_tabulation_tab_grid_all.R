#' **Apply `tab_grid()` to all grid questions**
#'
#' @description
#' This function maps `tab_grid()` across all grid question prefixes in a data frame.
#' Additional arguments are passed to `tab_grid()`.
#'
#' Grid questions are detected automatically by prefixes before the first underscore (`_`).
#' Multiple response (pick-any) sets are skipped, since those should be handled with `tab_mr()`.
#'
#' @param data A data frame containing the survey data.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param exclude Variables or prefixes to exclude from tabulation. Defaults to `NULL`.  Always excludes `weight` (if specified) and any variable literally named "weight".
#' @param ... Additional arguments passed to `tab_grid()`.
#'
#' @return
#' A named list of grid frequency tables, one per grid questions prefix.
#'
#' @examples
#' # Unweighted frequency tables of all grid questions
#' tab_grid_all(data = survey_data)
#'
#' # Weighted frequency tables of all grid questions, excluding Q99
#' tab_grid_all(data = survey_data, weight = weight_var, exclude = "Q99")
#'
#' @export

tab_grid_all <- function(data, weight = NULL, exclude = NULL, ...) {
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
  weight_expr <- substitute(weight)
  exclude_exprs <- substitute(exclude)

  weight_name <- if (!identical(weight_expr, quote(NULL))) rlang::as_string(weight_expr) else NULL

  # Turn exclude into character vector
  if (!identical(exclude_exprs, quote(NULL))) {
    # If multiple vars are supplied, turn into vector
    exclude_syms <- as.list(exclude_exprs)
    exclude_names <- vapply(exclude_syms, rlang::as_string, character(1))
  } else {
    exclude_names <- NULL
  }

  # Always exclude "weight" and weight vars
  exclude_all <- c("weight", exclude_names, weight_name)

  # Identify prefixes (before first underscore, or full name if none)
  all_prefixes <- unique(sub("_.*$", "", names(data)))

  # Exclude
  prefixes <- setdiff(all_prefixes, exclude_all)

  # If everything is excluded
  if (length(prefixes) == 0) {
    stop("The provided dataset is empty.", call. = FALSE)
  }

  # Check for invalid prefixes (need ≥ 2 vars)
  bad_prefixes <- prefixes[vapply(prefixes, function(pref) {
    length(grep(paste0("^", pref, "(_|$)"), names(data), value = TRUE)) < 2
  }, logical(1))]

  if (length(bad_prefixes) > 0) {
    stop("One or more variables in the provided dataset are not grid questions.", call. = FALSE)
  }

  # Function ----

  # Loop across valid prefixes
  purrr::map(prefixes, function(pref) {
    tab_grid(
      data = data,
      x = !!rlang::sym(pref),
      weight = !!weight_expr,
      ...
    )
  }) %>%
    purrr::set_names(prefixes)
}
