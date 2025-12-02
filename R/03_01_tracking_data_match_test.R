#' **Check alignment of variables between two datasets**
#'
#' @description
#' This function compares the structure and metadata of corresponding variables in two datasets, typically different survey waves.
#'
#' It performs three types of checks:
#'
#' - **Variable presence:** Identifies variables that appear in both datasets, only in the new dataset, or only in the old dataset (when `vars = "All"`).
#'
#' - **Metadata alignment:** For shared variables, the function tests whether the variable labels, classes, and (for factors) the factor levels match between datasets.
#'
#' - **Factor-level diagnostics:** When factor levels differ, the function reports which levels in the new dataset are not present in the old dataset.
#'
#' @param new_data A data frame for the new dataset.
#' @param old_data A data frame for the old dataset.
#' @param vars Character, either `"Shared"` (default) or `"All"`:
#'   - `"Shared"`: only variables present in both datasets.
#'   - `"All"`: include all variables, marking location as "Both", "New", or "Old".
#'
#' @return
#' A tibble containing one row per variable, including detailed diagnostics that make it easy to scan for inconsistencies before merging survey waves.
#'
#' @examples
#' # Compare variable alignment between Wave 1 and Wave 2
#' data_match <- data_match_test(new_data = wave_2, old_data = wave_1, vars = "All")
#'
#' @export

data_match_test <- function(new_data, old_data, vars = c("Shared", "All")) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "dplyr", "purrr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  vars <- match.arg(vars)

  # Column names
  cols_new <- colnames(new_data)
  cols_old <- colnames(old_data)

  # New columns not matched to old
  new_not_matched <- cols_new[!cols_new %in% cols_old]

  # Old columns not matched to new
  old_not_matched <- cols_old[!cols_old %in% cols_new]

  # Matching columns
  match_vars <- cols_new[cols_new %in% cols_old]

  # Shared variables ----

  # Compare shared variables for label, class, and factor-level alignment
  temp_list <- list(
    match_vars,
    new_data[, match_vars],
    old_data[, match_vars]
  )

  match_test <- purrr::pmap_dfr(temp_list, function(v, new, old) {
    # Compare variable label
    new_label <- attr(new, "label")
    old_label <- attr(old, "label")
    label_test <- identical(new_label, old_label)

    # Compare class
    new_class <- class(new)
    old_class <- class(old)
    class_test <- identical(new_class, old_class)

    # Compare factor levels if applicable
    if (class_test) {
      if (identical(new_class, "factor")) {
        new_levels <- levels(new)
        old_levels <- levels(old)
        levels_test <- identical(new_levels, old_levels)

        if (!levels_test) {
          new_levels_not_matched <- stringr::str_c(new_levels[!new_levels %in% old_levels], collapse = ", ")
        } else {
          new_levels_not_matched <- NA
        }

        match_full <- all(c(label_test, class_test, levels_test))

      } else {
        levels_test <- NA
        new_levels <- NA
        old_levels <- NA
        new_levels_not_matched <- NA
        match_full <- all(c(label_test, class_test))
      }

    } else {
      levels_test <- NA
      new_levels <- NA
      old_levels <- NA
      new_levels_not_matched <- NA
      match_full <- all(c(label_test, class_test))
    }

    # Output one row per variable
    tibble::tibble(
      var = v,
      location = "Both",
      match_full,
      label_test,
      new_label,
      old_label,
      class_test,
      new_class,
      old_class,
      levels_test,
      new_levels = stringr::str_c(new_levels, collapse = ", "),
      old_levels = stringr::str_c(old_levels, collapse = ", "),
      new_levels_not_matched
    )
  })

  # All variables ----

  if (vars == "All") {

    # Variables only in new dataset
    only_new <- if (length(new_not_matched) > 0) {
      tibble::tibble(
        var = new_not_matched,
        location = "New",
        match_full = NA,
        label_test = NA,
        new_label = purrr::map_chr(new_not_matched, ~ attr(new_data[[.x]], "label") %||% NA_character_),
        old_label = NA_character_,
        class_test = NA,
        new_class = purrr::map_chr(new_not_matched, ~ class(new_data[[.x]])[1]),
        old_class = NA_character_,
        levels_test = NA,
        new_levels = purrr::map_chr(new_not_matched, ~ if (is.factor(new_data[[.x]])) stringr::str_c(levels(new_data[[.x]]), collapse = ", ") else NA_character_),
        old_levels = NA_character_,
        new_levels_not_matched = NA_character_
      )
    } else {
      tibble::tibble(
        var = character(),
        location = character(),
        match_full = logical(),
        label_test = logical(),
        new_label = character(),
        old_label = character(),
        class_test = logical(),
        new_class = character(),
        old_class = character(),
        levels_test = logical(),
        new_levels = character(),
        old_levels = character(),
        new_levels_not_matched = character()
      )
    }

    # Variables only in old dataset
    only_old <- if (length(old_not_matched) > 0) {
      tibble::tibble(
        var = old_not_matched,
        location = "Old",
        match_full = NA,
        label_test = NA,
        new_label = NA_character_,
        old_label = purrr::map_chr(old_not_matched, ~ attr(old_data[[.x]], "label") %||% NA_character_),
        class_test = NA,
        new_class = NA_character_,
        old_class = purrr::map_chr(old_not_matched, ~ class(old_data[[.x]])[1]),
        levels_test = NA,
        new_levels = NA_character_,
        old_levels = purrr::map_chr(old_not_matched, ~ if (is.factor(old_data[[.x]])) stringr::str_c(levels(old_data[[.x]]), collapse = ", ") else NA_character_),
        new_levels_not_matched = NA_character_
      )
    } else {
      tibble::tibble(
        var = character(),
        location = character(),
        match_full = logical(),
        label_test = logical(),
        new_label = character(),
        old_label = character(),
        class_test = logical(),
        new_class = character(),
        old_class = character(),
        levels_test = logical(),
        new_levels = character(),
        old_levels = character(),
        new_levels_not_matched = character()
      )
    }

    # Combine results and preserve new dataset order first
    match_test <- dplyr::bind_rows(match_test, only_new, only_old)
    order <- c(cols_new, old_not_matched)
    match_test <- match_test %>%
      dplyr::mutate(var = factor(var, levels = order)) %>%
      dplyr::arrange(var)
  }

  # Output ----

  return(match_test)
}
