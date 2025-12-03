#' **Standardize numeric and factor variables (z-scores)**
#'
#' @description
#' This function standardizes variables to have mean of 0 and standard deviation of 1.
#'
#' @param data A data frame containing variables to standardize.
#' @param vars Variables to standardize. Supports tidyselect syntax (e.g., `starts_with()`, `c(var1, var2)`).
#' @param flip Logical; if `TRUE`, reverses the direction of valid response levels before standardization.
#' @param keep_all Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only standardized variables.
#' @param outside_scale Character vector of response levels (typically DK/PNTS) that lie outside of the scale and should be treated as neutral midpoint (`0.5`).
#'
#' @details
#' - Numeric variables are z-scored.
#' - Factor variables are converted to numeric order and then standardized.
#' - Levels in `outside_scale` are assigned to the midpoint of the valid range before transformation.
#'
#' @return
#' A data frame with standardized variables.
#' - If `keep_all = TRUE`, the full dataset is returned with standardized variables.
#' - If `keep_all = FALSE`, only standardized variables are returned.
#'
#' @examples
#' # Standardize Q1 variable set with
#' standardize(data = data, vars = starts_with("Q1_"))
#'
#' # Standardize Q1 and Q2 variable set with the scale order
#' standardize(data = data, vars = c(Q1, Q2), flip = TRUE)
#'
#' @export

standardize <- function(data, vars = everything(), flip = FALSE, keep_all = TRUE,
                        outside_scale = c("Don't know", "Prefer not to say")) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "tidyselect", "dplyr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  `%>%` <- dplyr::`%>%`

  vars_expr <- rlang::enexpr(vars)

  if (rlang::is_symbol(vars_expr) && exists(as.character(vars_expr), envir = parent.frame())) {
    vars <- get(as.character(vars_expr), envir = parent.frame()) %>% as.character()
  } else {
    vars <- tryCatch(
      tidyselect::eval_select(vars_expr, data) %>% names(),
      error = function(e) rlang::eval_tidy(vars_expr) %>% as.character()
    )
  }

  # Handle errors ----

  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop(
      "The following variables were not found in the dataset: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  bad_types <- vapply(vars, function(v) {
    cls <- class(data[[v]])[1]
    !(cls %in% c("numeric", "factor"))
  }, logical(1))

  if (any(bad_types)) {
    stop(
      "The following variable types are not allowed (character or date): ",
      paste(vars[bad_types], collapse = ", "),
      "\nOnly factor and numeric variables can be standardized.",
      call. = FALSE
    )
  }

  # Standardize ----

  std_list <- lapply(vars, function(v) {
    x <- data[[v]]

    if (is.factor(x)) {
      x_num <- as.numeric(x)
      lvls <- levels(x)

      # Identify specials
      specials <- which(lvls %in% outside_scale)
      is_special <- x_num %in% specials

      # Flip valid values if requested
      if (flip) {
        max_val <- max(x_num, na.rm = TRUE)
        x_num[!is.na(x_num)] <- max_val + 1 - x_num[!is.na(x_num)]
      }

      # Set specials to midpoint of valid range
      valid_vals <- x_num[!(is_special | is.na(x_num))]
      if (length(valid_vals) > 0) {
        midpoint <- mean(range(valid_vals, na.rm = TRUE))
        x_num[is_special] <- midpoint
      }

      # Standardize
      mu <- mean(x_num, na.rm = TRUE)
      sigma <- sd(x_num, na.rm = TRUE)
      if (sigma == 0) {
        result <- rep(0, length(x_num))
      } else {
        result <- (x_num - mu) / sigma
      }

    } else if (is.numeric(x)) {
      mu <- mean(x, na.rm = TRUE)
      sigma <- sd(x, na.rm = TRUE)
      if (sigma == 0) {
        result <- rep(0, length(x))
      } else {
        result <- (x - mu) / sigma
      }
    } else {
      stop("Unexpected type for variable: ", v, call. = FALSE)
    }

    # Preserve label
    attr(result, "label") <- attr(x, "label")
    result
  })

  # Output ----

  # Assign standardized values
  data[vars] <- std_list

  # Return depending on keep_all
  if (keep_all) {
    return(data)
  } else {
    return(tibble::as_tibble(data[vars]))
  }
}
