#' **One-hot encode categorical variables**
#'
#' @description
#' This function converts one or more categorical variables in a data frame into one-hot encoded variables.
#'
#' @param data A data frame containing the variables to encode.
#' @param vars A character vector (quoted or unquoted) of variables to one-hot encode.
#' @param keep_all Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only one-hot encoded variables.
#'
#' @details
#' Binary factors labeled `Unchecked`/`Checked` are encoded as a single 0–1 variable.
#'
#' Multi-category factors are expanded into multiple one-hot encoded variables (one per level).
#'
#' Original variable labels are preserved and extended to the generated one-hot encoded variables.
#'
#' @return
#' - A data frame with one-hot encoded variables.
#' - If `keep_all = TRUE`, the full dataset is returned with one-hot encoded variables replacing originals.
#' - If `keep_all = FALSE`, only one-hot encoded variables are returned.
#'
#' @examples
#' # One-hot encode region and Q1_1 while keeping all original variables
#' data <- one_hot_encode(data = data, c(region, Q1_1))
#'
#' # One-hot encode region and Q1_1, returning only the encoded columns
#' data_new <- one_hot_encode(data = data, c(region, Q1_1), keep_all = FALSE)
#'
#' @export

one_hot_encode <- function(data, vars, keep_all = TRUE) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "tidyselect", "dplyr", "purrr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  # Capture variable expressions
  `%>%` <- dplyr::`%>%`

  vars_expr <- rlang::enexpr(vars)

  vars <- tryCatch(
    tidyselect::eval_select(vars_expr, data = data) %>% names(),
    error = function(e) {
      if (is.character(vars_expr)) vars_expr else as.character(vars)
    }
  )

  # Error handling
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop(
      "The following variables were not found in the dataset: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Convert to factors if needed
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~ as.factor(.x)))

  # Encode ----

  out_list <- purrr::map(vars, function(v) {
    x <- data[[v]]
    lvls <- levels(x)
    v_lab <- attr(data[[v]], "label", exact = TRUE) %||% v

    if (length(lvls) == 2 && all(c("Unchecked", "Checked") %in% lvls)) {
      # Binary case
      result <- tibble::tibble("{v}_Checked" := as.numeric(x == "Checked"))
      attr(result[[1]], "label") <- paste0(v_lab, ": Checked")
      return(result)

    } else {
      # Multi-category case
      dummies <- stats::model.matrix(~ x - 1, data = tibble::tibble(x))

      # Clean names
      names_clean <- colnames(dummies) %>%
        stringr::str_remove("^x") %>%
        stringr::str_replace_all("[[:punct:][:space:]]+", "_") %>%
        stringr::str_replace_all("_+", "_") %>%
        stringr::str_remove("^_") %>%
        stringr::str_remove("_$") %>%
        paste0(v, "_", .)

      result <- tibble::as_tibble(dummies)
      names(result) <- names_clean

      # Assign labels
      for (i in seq_along(names(result))) {
        level_label <- lvls[i]
        attr(result[[i]], "label") <- paste0(v_lab, ": ", level_label)
      }

      return(result)
    }
  })

  # Output ----

  result <- dplyr::bind_cols(out_list)

  if (keep_all) {
    for (v in rev(vars)) {
      dummy_cols <- names(result)[startsWith(names(result), paste0(v, "_"))]
      insert_after <- match(v, names(data))
      data <- data %>%
        tibble::add_column(!!!result[dummy_cols], .after = insert_after)
    }
    data <- data %>% dplyr::select(-dplyr::all_of(vars))
    return(data)
  } else {
    return(result)
  }
}
