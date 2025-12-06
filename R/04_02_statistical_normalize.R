#' **Normalize numeric and factor variables to a 0–1 scale**
#'
#' @description
#' This function rescales variables to the 0 to 1 range. It supports flipping of scales and preserves variable labels.
#'
#' @param data A data frame containing variables to normalize.
#' @param vars Variables to normalize. Supports tidyselect syntax (e.g., `starts_with()`, `c(var1, var2)`).
#' @param flip Logical; if `TRUE`, reverses the direction of valid response levels before scaling.
#' @param keep_all Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only normalized variables.
#' @param outside_scale Character vector of response levels (typically DK/PNTS) that lie outside of the scale and should be treated as neutral midpoint (`0.5`).
#'
#' @details
#' - Numeric variables are min–max scaled to 0 to 1.
#' - Factor variables are converted to numeric order and evenly spaced between 0 and 1.
#' - Levels listed in `outside_scale` (e.g., `Don't know`) are assigned a value of 0.5.
#'
#' @return
#' A data frame with normalized variables.
#' - If `keep_all = TRUE`, the full dataset is returned with normalized variables.
#' - If `keep_all = FALSE`, only normalized variables are returned.
#'
#' @examples
#' # Normalize Q1 and Q2 with flipping the scale order
#' normalize(data = data, vars = c(Q2, Q3), flip = TRUE)
#'
#' @export

normalize <- function(data, vars = everything(), flip = FALSE, keep_all = TRUE,
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

  # Missing variables
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop(
      "The following variables were not found in the dataset: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Incorrect types
  bad_types <- vapply(vars, function(v) {
    cls <- class(data[[v]])[1]
    !(cls %in% c("numeric", "factor"))
  }, logical(1))

  if (any(bad_types)) {
    stop(
      "The following variable types are not allowed (character or date): ",
      paste(vars[bad_types], collapse = ", "),
      "\nOnly factor and numeric variables can be rescaled.",
      call. = FALSE
    )
  }

  # Normalize ----

  nrm_list <- lapply(vars, function(v) {
    x <- data[[v]]

    if (is.numeric(x) && is.null(levels(x))) {
      # Numeric input - min-max rescale
      result <- (x - min(x, na.rm = TRUE)) /
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    } else {
      # Factor input - discrete scale
      x_num <- as.numeric(x)
      lvls <- levels(x)

      # Identify out-of-scale levels
      specials <- which(lvls %in% outside_scale)
      is_special <- x_num %in% specials

      # Identify within-scale levels
      valid_vals <- sort(unique(x_num[!(is_special | is.na(x_num))]))
      n_valid <- length(valid_vals)

      # Flip valid levels if requested
      if (flip) valid_vals <- rev(valid_vals)

      # Map valid values evenly 0 to 1
      mapping <- setNames(seq(0, 1, length.out = n_valid), valid_vals)
      result <- rep(NA_real_, length(x_num))
      result[!is_special & !is.na(x_num)] <- mapping[as.character(x_num[!is_special & !is.na(x_num)])]

      # Place out-of-scale levels in the middle
      result[is_special] <- 0.5
    }

    # Preserve label
    attr(result, "label") <- attr(x, "label")
    result
  })

  # Output ----

  # Assign standardized values
  data[vars] <- nrm_list

  # Return depending on keep_all
  if (keep_all) {
    return(data)
  } else {
    return(tibble::as_tibble(data[vars]))
  }
}
