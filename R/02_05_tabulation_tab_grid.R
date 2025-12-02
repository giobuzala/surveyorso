#' **Create a full distribution table for grid questions**
#'
#' @description
#' This function produces a table for grid survey questions, showing the full distribution across all response levels.
#'
#' - If only `x` is specified, the output is a full-distribution table for each item in the grid question.
#' - Use the `weight` argument to produce weighted tables; if omitted, results are unweighted.
#' - Set `prop = TRUE` (default) to output proportions, or `prop = FALSE` to output frequencies.
#' - Use the `sort` argument (`asc` or `desc`) to order rows based on top/bottom box scores defined by `top_bottom`. Set `sort = NULL` for no sorting.
#' - Use the `top_bottom` argument (e.g., `T2`, `B3`) to specify how many top or bottom levels to use when sorting grid items.
#' - Use the `round` argument to control decimal precision in proportions tables.
#' - If `numeric = TRUE`, the function removes the base column and coerces values to numeric, making the output math-ready.
#'
#' @param data A data frame containing the survey data.
#' @param x A variable prefix identifying the grid question (e.g., `Q3` for `Q3_1`, `Q3_2`, ...).
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param prop Logical; if `TRUE` (default), outputs proportions. If `FALSE`, outputs frequencies.
#' @param sort Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting). Defaults to `NULL`.
#' @param top_bottom A string of the form `TN` or `BN` indicating how many top or bottom levels should be used for sorting rows.
#' @param round Integer; number of decimal places for proportions. Defaults to `3`.
#' @param numeric Logical; if `TRUE`, returns a data frame with numeric columns, with the n column removed. Defaults to `FALSE`.
#'
#' @return
#' A table showing the full distribution across levels, as a data frame..
#'
#' @examples
#' # Weighted proportions table of a grid question, sorted descending by T2
#' tab_grid(data = survey_data, x = Q3, weight = weight_var, sort = desc, top_bottom = T2)
#'
#' @export

tab_grid <- function(data, x, weight = NULL, prop = TRUE, sort = NULL, top_bottom = T2, round = 3, numeric = FALSE) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "dplyr", "purrr", "crayon")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  `%>%` <- dplyr::`%>%`

  x_enquo <- rlang::enquo(x)
  x <- rlang::as_name(x_enquo)

  w_enquo <- rlang::enquo(weight)
  w_name <- if (rlang::quo_is_null(w_enquo)) NULL else rlang::as_name(w_enquo)

  # Handle errors ----

  if (!is.null(w_name) && !w_name %in% colnames(data)) {
    stop("Weight variable ", w_name, " was not found in the dataset.", call. = FALSE)
  }
  if (!is.null(w_name)) {
    if (!(is.numeric(data[[w_name]]) || is.logical(data[[w_name]]))) {
      stop("Weight must be numeric or logical.", call. = FALSE)
    }
  }

  # Parse top_bottom
  tb_expr <- rlang::enexpr(top_bottom)
  top_bottom <- rlang::as_label(tb_expr)
  if (!grepl("^[tTbB][0-9]+$", top_bottom)) {
    stop("top_bottom must be in the form 'TN' or 'BN'.", call. = FALSE)
  }

  direction <- tolower(substr(top_bottom, 1, 1))
  n_val <- as.integer(substr(top_bottom, 2, nchar(top_bottom)))
  if (is.na(n_val) || n_val <= 0) {
    stop("N in top_bottom must be a positive integer.", call. = FALSE)
  }

  # Handle grid variables ----

  # Identify
  vars <- grep(paste0("^", x, "_"), names(data), value = TRUE)
  if (length(vars) == 0) stop("No grid questions found with prefix ", x, ".", call. = FALSE)
  vars <- vars[sapply(data[vars], is.factor)]
  if (length(vars) == 0) stop("No factor grid questions found with prefix ", x, ".", call. = FALSE)

  # Extract labels
  get_label <- function(v) {
    lab <- attr(data[[v]], "label")
    if (!is.null(lab)) sub(":.*$", "", lab) else v
  }
  labels <- sapply(vars, get_label, USE.NAMES = FALSE)

  # Use levels from the first grid variable

  all_levels <- levels(data[[vars[1]]])

  # Validate top_bottom against scale
  exclude_box <- c("Don't know", "Dont know", "I don't know", "I dont know", "Prefer not to say", "Prefer to not say")
  valid_levels <- setdiff(all_levels, exclude_box)

  max_lvls <- length(valid_levels)
  if (n_val > max_lvls) {
    stop("Requested ", top_bottom, " but only ", max_lvls, " valid levels available.", call. = FALSE)
  }

  # Build tables ----

  result <- purrr::map_dfr(seq_along(vars), function(i) {
    v <- vars[i]; lab <- labels[i]
    xvec <- factor(data[[v]], levels = all_levels)
    wvec <- if (!is.null(w_name)) data[[w_name]] else rep(1, length(xvec))

    counts <- tapply(wvec, xvec, sum, na.rm = TRUE)
    counts <- counts[all_levels]

    base <- sum(counts, na.rm = TRUE)
    vals <- if (prop) counts / base else counts

    tibble::tibble(Label = lab, !!!as.list(vals), `n` = base)
  }) %>%
    tibble::column_to_rownames("Label")

  # Format ----

  if (prop && !numeric) {
    result <- dplyr::mutate(result, dplyr::across(dplyr::all_of(all_levels),
                                                  ~ formatC(as.numeric(.), format = "f", digits = round)))
    result[["n"]] <- formatC(as.numeric(result[["n"]]), format = "f", digits = 0)
  } else if (!prop && !numeric) {
    result <- dplyr::mutate(result, dplyr::across(dplyr::all_of(all_levels), ~ round(as.numeric(.))))
    result[["n"]] <- formatC(as.numeric(result[["n"]]), format = "f", digits = 0)
  }

  # Sort ----

  sort_enquo <- rlang::enquo(sort)
  sort_val <- rlang::as_label(sort_enquo)

  # Validate sort input
  valid_sorts <- c("asc", "desc", "NULL")
  if (!sort_val %in% valid_sorts) {
    stop("Sort must be one of: asc, desc, or NULL.", call. = FALSE)
  }

  if (!identical(sort_val, "NULL")) {
    hidden_total <- purrr::map_dbl(seq_along(vars), function(i) {
      v <- vars[i]
      xvec <- factor(data[[v]], levels = all_levels)
      wvec <- if (!is.null(w_name)) data[[w_name]] else rep(1, length(xvec))
      keep <- !is.na(xvec)
      xvec <- xvec[keep]; wvec <- wvec[keep]
      box_idx <- if (direction == "t") 1:n_val else (length(valid_levels) - n_val + 1):length(valid_levels)
      in_box <- xvec %in% valid_levels[box_idx]
      n <- sum(wvec[in_box], na.rm = TRUE)
      base <- sum(wvec, na.rm = TRUE)
      if (prop) ifelse(base > 0, n / base, NA) else n
    })
    names(hidden_total) <- labels

    result <- result[order(if (identical(sort_val, "desc"))
      -hidden_total[rownames(result)]
      else hidden_total[rownames(result)]), , drop = FALSE]
  }

  # Output ----

  if (numeric) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(all_levels), ~ {
        v <- as.numeric(.x)
        if (prop) round(v, round) else round(v, 0)
      })) %>%
      dplyr::select(-`n`)
  }

  # Attach attributes for printing info message
  attr(result, "vars") <- list(
    x = x,
    weight = w_name,
    sort = sort_val,
    top_bottom = top_bottom,
    levels = valid_levels
  )

  class(result) <- c("tab_grid_result", class(result))

  return(result)
}

#' Custom print method for tab_grid() results
#' @export
print.tab_grid_result <- function(x, ...) {
  vars <- attr(x, "vars")

  ital <- function(txt) if (requireNamespace("crayon", quietly = TRUE)) crayon::italic(txt) else txt

  tb_text <- vars$top_bottom
  tb_disp <- NULL

  if (!is.null(tb_text) && grepl("^[tTbB][0-9]+$", tb_text)) {
    direction <- tolower(substr(tb_text, 1, 1))
    n_val <- as.integer(substr(tb_text, 2, nchar(tb_text)))
    lvls_box <- vars$levels

    if (!is.null(lvls_box) && n_val <= length(lvls_box)) {
      chosen <- if (direction == "t") lvls_box[1:n_val] else utils::tail(lvls_box, n_val)
      tb_disp <- paste0(tb_text, " (", paste(shQuote(chosen), collapse = " + "), ")")
    }
  }

  # Sorting info
  sort_dir <- attr(x, "vars")$sort
  if (is.null(sort_dir) || identical(sort_dir, "NULL")) {
    sort_msg <- "not sorted"
  } else if (identical(sort_dir, "desc")) {
    sort_msg <- paste0("sorted descending by ", tb_disp)
  } else {
    sort_msg <- paste0("sorted ascending by ", tb_disp)
  }

  # Build header
  if (!is.null(vars$weight)) {
    header <- paste0("Grid frequency table for ", ital(vars$x),
                     ", weighted by ", ital(vars$weight),
                     ", ", sort_msg, ".")
  } else {
    header <- paste0("Grid frequency table for ", ital(vars$x),
                     ", unweighted, ", sort_msg, ".")
  }

  base::cat(header, "\n\n")
  NextMethod("print")
}
