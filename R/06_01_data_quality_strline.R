#' **Identify straightliners in grid questions**
#'
#' @description
#' This function flags respondents who selected the same response across all items in a grid question.
#'
#' @param data A data frame containing the survey data.
#' @param x One or more variable prefixes identifying the grid question(s).
#'
#' @details
#' The function automatically detects all variables that begin with the specified prefix followed by an underscore.
#'
#' It excludes multiple response sets (e.g., binary 0/1 or Unchecked/Checked grids).
#'
#' @return
#' The original data frame with an additional logical column named `prefix_strline`, indicating whether each respondent provided identical responses across all items in the grid.
#'
#' @examples
#' # Identify straightliners for one grid question
#' data <- strline(data = survey_data, x = Q1)
#'
#' # Identify straightliners for multiple grid questions
#' data <- strline(data = survey_data, c(Q1, Q2))
#'
#' @export

strline <- function(data, x) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "purrr", "stringr", "stats")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Capture variable expressions
  x_expr <- rlang::enexpr(x)

  # Handle unquoted or quoted prefixes correctly
  if (rlang::is_call(x_expr, "c")) {
    prefixes <- rlang::call_args(x_expr) %>%
      purrr::map_chr(rlang::as_string)
  } else {
    prefixes <- rlang::as_string(x_expr)
  }

  # Check that each prefix exists
  missing_prefixes <- prefixes[!sapply(prefixes, function(p) {
    any(stringr::str_detect(names(data), paste0("^", p, "_")))
  })]
  if (length(missing_prefixes) > 0) {
    stop(
      "Variables with the following prefixes do not exist in the dataset: ",
      paste(missing_prefixes, collapse = ", "),
      call. = FALSE
    )
  }

  # Function ----

  for (prefix in prefixes) {
    vars <- names(data)[stringr::str_detect(names(data), paste0("^", prefix, "_"))]

    # Stop if multiple response set detected
    bad_sets <- list(
      c("Checked", "Unchecked"), c("Unchecked", "Checked"),
      c("Selected", "Not Selected"), c("Not Selected", "Selected"),
      c("1", "0"), c("0", "1")
    )

    is_bad <- any(sapply(vars, function(v) {
      if (is.factor(data[[v]])) {
        lvls <- levels(data[[v]])
        any(sapply(bad_sets, function(b) setequal(lvls, b)))
      } else if (is.numeric(data[[v]])) {
        all(sort(unique(stats::na.omit(data[[v]]))) %in% c(0, 1))
      } else {
        FALSE
      }
    }))

    if (is_bad) {
      stop(prefix, " looks like a multiple response set. Please use a grid question instead.", call. = FALSE)
    }

    # Compute straightliner flag

    new_var <- paste0(prefix, "_strline")

    data <- data %>%
      dplyr::mutate(
        # Create a new variable for the straightliner flag
        !!new_var := purrr::pmap_dbl(
          # Pass all grid items for each respondent as arguments to the function
          dplyr::select(., dplyr::all_of(vars)),
          function(...) {
            # Combine all responses for the grid into a single vector
            vals <- c(...)

            # Remove missing responses before evaluating
            vals <- vals[!is.na(vals)]

            # If fewer than 2 valid responses remain, return NA (cannot assess straightlining)
            if (length(vals) < 2) return(NA_real_)

            # Flag as 1 if all valid responses are identical, otherwise 0
            as.numeric(length(unique(vals)) == 1)
          }
        )
      )

    # Assign SPSS-style variable label
    attr(data[[new_var]], "label") <- paste(prefix, "straightliner")
  }

  return(data)
}
