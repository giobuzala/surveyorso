#' **Combine split variables into a single variable**
#'
#' @description
#' This function merges two variables that represent split versions of the same question (e.g., `Q1_1` asked to one group and `Q1_2` to another) into a single combined variable.
#' The function fills in missing values from one variable with non-missing values from the other, preserving factor levels and SPSS-style attributes.
#'
#' @param data A data frame containing the variables to combine.
#' @param var_1 First variable to merge.
#' @param var_2 Second variable to merge.
#' @param new_name Optional name for the new variable. If omitted, defaults to `"var_1_var_2_COMB"`.
#' @param new_label Optional SPSS-style variable label. If omitted, the label from `var_1` is copied.
#'
#' @details
#' - If both variables have valid values for the same row, `var_1` takes precedence (a warning is issued).
#' - Factor levels are inherited from `var_1`.
#' - Existing variables with the same name are overwritten.
#'
#' To apply this function across multiple pairs of split variables, you can map it using the code block below:
#'
#' \preformatted{
#' pairs <- list(
#'   list("Q1_1", "Q1_2"),
#'   list("Q2_1", "Q2_2")
#' )
#'
#' dat <- reduce(
#'   pairs,
#'   function(d, vars) {
#'     combine(d, !!sym(vars[[1]]), !!sym(vars[[2]]))
#'   },
#'   .init = dat
#' )
#' }
#'
#' @return
#' A modified data frame with a new combined variable placed immediately after `var_2`.
#'
#' @examples
#' # Combine Q1_1 and Q2_2
#' data <- combine(data = survey_data, var_1 = Q1_1, var_2 = Q1_2)
#'
#' @export

combine <- function(data, var_1, var_2, new_name = NULL, new_label = NULL) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  # Capture variable expressions
  var_1_quo <- rlang::enquo(var_1)
  var_2_quo <- rlang::enquo(var_2)
  new_name_quo <- rlang::enquo(new_name)

  var_1_name <- rlang::as_name(var_1_quo)
  var_2_name <- rlang::as_name(var_2_quo)

  x1 <- data[[var_1_name]]
  x2 <- data[[var_2_name]]

  # Set new variable name
  if (rlang::quo_is_null(new_name_quo)) {
    new_name_sym <- rlang::sym(paste0(var_1_name, "_", var_2_name, "_COMB"))
  } else {
    new_name_sym <- rlang::ensym(new_name)
  }

  new_name_str <- rlang::as_string(new_name_sym)

  # Check both exist
  missing_vars <- setdiff(c(var_1_name, var_2_name), names(data))
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "The following variables were not found in the dataset: ",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Check for type mismatch
  type1 <- class(x1)
  type2 <- class(x2)

  if (type1 != type2) {
    stop(
      paste0(
        "Type mismatch between ", var_1_name, " (", type1, ") and ",
        var_2_name, " (", type2, "). Both variables must be the same type to combine."
      ),
      call. = FALSE
    )
  }

  # Warn if not a true split
  overlap_rows <- which(!is.na(x1) & !is.na(x2))
  if (length(overlap_rows) > 0) {
    warning(paste0("Non-exclusive split detected. Valid cases of ", var_1_name, " will overwrite ", var_2_name, "."), call. = FALSE)
  }

  # Combine and format ----

  # Combine
  new_vec <- dplyr::coalesce(
    if (is.factor(x1)) as.character(x1) else x1,
    if (is.factor(x2)) as.character(x2) else x2
  )

  # Preserve factor levels if var_1 is a factor
  if (is.factor(x1)) {
    new_vec <- factor(new_vec, levels = levels(x1))
  }

  # Create or overwrite a variable
  data[[new_name_str]] <- new_vec

  # Copy SPSS-style attributes from var_1
  label <- if (!is.null(new_label)) {
    new_label
  } else if (!is.null(attr(x1, "label"))) {
    attr(x1, "label")
  } else {
    NULL
  }

  if (!is.null(label)) {
    attr(data[[new_name_str]], "label") <- label
  }

  # Relocate new variable after var_2
  data <- dplyr::relocate(data, all_of(new_name_str), .after = all_of(var_2_name))

  return(data)
}
