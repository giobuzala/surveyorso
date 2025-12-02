#' **Convert rank variables to factors**
#'
#' @description
#' This function converts a set of ranking variables (e.g., Q1_1, Q1_2, …) into labeled factors with "Rank" prefixes.
#'
#' The input ranking variables must be numeric values representing rank positions (e.g., 1, 2, 3…).
#'
#' @param data A data frame containing the ranking variables.
#' @param x A prefix used to identify ranking variables (e.g., Q1).
#'
#' @return
#' A data frame with the specified ranking variables converted to factors.
#'
#' @examples
#' # Convert ranking variables with prefix Q1 to factors
#' data <- rank(data = survey_data, Q1)
#'
#' @export

rank <- function(data, x) {
  # Check required packages ----

  required_pkgs <- c("purrr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  x_expr <- rlang::enexpr(x)

  # Evaluate safely to string
  x <- tryCatch(
    rlang::as_name(x_expr),
    error = function(e) {
      if (is.character(x_expr)) x_expr else as.character(x_expr)
    }
  )

  # Function ----

  # Identify variables to process
  cols <- colnames(data)
  vars <- cols[stringr::str_detect(cols, paste0("^", x, "_"))]

  if (length(vars) == 0) {
    stop("No questions found with prefix ", x, ".", call. = FALSE)
  }

  # Stop if a variable is already factor
  already_factor <- vars[vapply(data[vars], is.factor, logical(1))]

  if (length(already_factor) > 0) {
    stop("The provided variable is already a factor. Ensure that the variable you are converting is a numeric ranking question.", call. = FALSE)
  }

  # Number of levels/options ranked (can be less than # of vars)
  n_levels <- max(purrr::map_dbl(data[, vars], function(x) max(x, na.rm = TRUE)))

  # Subset of just the ranking variables
  temp <- data[, vars]

  # Base indicator - respondents who answered at least one item
  base_ind <- apply(temp, 1, function(row) any(!is.na(row)))

  # Convert to factors with ranking levels
  data[, vars] <- purrr::map_dfc(data[, vars], function(x) {
    # Get the label
    lab <- attr(x, "label")

    # Change 0 to missing
    x[x %in% 0] <- NA

    # Add "Rank" prefix
    x <- stringr::str_c("Rank ", x)

    # Define factor levels
    lvls <- stringr::str_c("Rank ", 1:n_levels)

    # If ranking a subset of options, add "Not Ranked"
    if (n_levels < length(vars)) {
      x[is.na(x) & base_ind] <- "Not Ranked"
      lvls <- c(lvls, "Not Ranked")
    }

    # Convert to factor
    x <- factor(x, levels = lvls)

    # Redefine attributes
    attributes(x) <- list(
      levels = levels(x),
      class = class(x),
      label = lab,
      format.spss = "F5.0",
      display_width = 0
    )

    x
  })

  invisible(data)
}
