#' **Summarize dataset metadata**
#'
#' @description
#' This function produces a summary of each variable in a dataset, including variable name, class, label, number of non-missing values, percentage of non-missing values, and factor levels (if applicable).
#'
#' @param data A data frame to summarize.
#'
#' @return
#' A tibble with a summary row for each variable.
#'
#' @examples
#' # Summarize survey_data
#' data_sum <- dat_sum_fn(survey_data)
#'
#' @export

dat_sum_fn <- function(data) {
  # Check required packages ----

  required_pkgs <- c("tibble", "purrr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Function ----

  # Extract column names
  cols <- colnames(data)

  # Map through each column
  result <- purrr::map2_dfr(data, cols, function(x, v) {

    # Class of variable (collapse if multiple)
    class <- stringr::str_c(class(x), collapse = " ")

    # Extract label (if none, assign NA)
    label <- attr(x, "label")

    if (is.null(label) || length(label) == 0) {
      label <- NA
    }

    # Number and percentage of non-missing values
    n <- sum(!is.na(x))
    pct <- round(n / length(x), 2)

    # Collapse levels if factor
    levels <- if ("factor" %in% class(x)) {
      stringr::str_c(levels(x), collapse = ", ")
    } else {
      NA
    }

    # Return summary row
    tibble::tibble(
      variable = v,
      class = class,
      label = label,
      n = n,
      pct = pct,
      levels = levels
    )
  })

  return(result)
}
