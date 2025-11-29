#' **Create a multiple response (pick-any) set frequency table or crosstab**
#'
#' @description
#' This function produces a frequency table or a crosstab for multiple response (pick-any) set questions.
#'
#' - If only `x` is specified, the output is a frequency table of the items in the multiple response set.
#' - If both `x` and `y` are specified, the output is a crosstab of the multiple response set by `y`.
#' - Use the `weight` argument to produce weighted tables; if omitted, results are unweighted.
#' - Set `prop = TRUE` (default) to output percentages, or `prop = FALSE` to output counts.
#' - Use the `total` argument to add a total column when `y` is specified. Ignored for one-way tables.
#' - Use the `sort` argument (`desc` or `asc`) to order rows.  Set `sort = NULL` for no sorting.
#' - Use the `round` argument to control decimal precision in percentage tables.
#' - If `numeric = TRUE`, the function removes the base row and coerces values to numeric, making the output math-ready.
#'
#' Supported coding schemes for multiple response items include:
#' - `Unchecked` / `Checked`
#' - `Not Selected` / `Selected`
#' - `0` / `1`
#'
#' @param data A data frame containing the survey data.
#' @param x A variable prefix identifying the multiple response set (e.g., `Q2` for `Q2_1`, `Q2_2`, ...).
#' @param y (Optional) A variable to cross-tabulate against the multiple response set. Defaults to `NULL`.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param prop Logical; if `TRUE` (default), outputs percentages. If `FALSE`, outputs counts.
#' @param total Logical; if `TRUE` (default), adds a total column when `y` is specified. Ignored for single-variable tables.
#' @param sort Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting). Defaults to `NULL`.
#' @param round Integer; number of decimal places for percentages. Defaults to `3`.
#' @param numeric Logical; if `TRUE`, returns a data frame with numeric columns, with the n row removed. Defaults to `FALSE`.
#'
#' @return
#' A frequency table (if `y = NULL`) or a crosstab (if `y` is specified), as a data frame.
#'
#' @examples
#' # Unweighted frequency table of a multiple response set, showing percentages
#' tab_mr(data = survey_data, x = Q2)
#'
#' # Weighted crosstab of Q2 by gender, showing counts and total column
#' tab_mr(data = survey_data, x = Q2, y = gender, weight = weight_var, prop = FALSE, total = TRUE)
#' @export

tab_mr <- function(data, x, y = NULL, weight = NULL, prop = TRUE, total = TRUE, sort = NULL, round = 3, numeric = FALSE) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "dplyr", "tidyr", "purrr", "stringr", "haven")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  `%>%` <- dplyr::`%>%`

  x_enquo <- rlang::enquo(x)
  y_enquo <- rlang::enquo(y)
  w_enquo <- rlang::enquo(weight)

  x_name <- rlang::as_name(x_enquo)
  y_name <- if (rlang::quo_is_null(y_enquo)) NULL else rlang::as_name(y_enquo)
  weight_name <- if (rlang::quo_is_null(w_enquo)) NULL else rlang::as_name(w_enquo)

  # Handle errors ----

  cols <- colnames(data)

  # If y or weight is provided, make sure they exist
  provided <- c(
    if (!is.null(y_name)) y_name,
    if (!is.null(weight_name)) weight_name
  )

  missing <- setdiff(provided, cols)
  if (length(missing) > 0) {
    stop(
      paste0(
        "The following variables were not found in the dataset: ",
        paste(missing, collapse = ", "),
        "\nProvided: y = ", if (is.null(y_name)) "NULL" else y_name,
        ", weight = ", if (is.null(weight_name)) "NULL" else weight_name
      ),
      call. = FALSE
    )
  }

  # If weight is provided, it must be numeric or logical
  if (!is.null(weight_name)) {
    if (!(is.numeric(data[[weight_name]]) || is.logical(data[[weight_name]]))) {
      stop(paste0("Weight must be numeric or logical."), call. = FALSE)
    }
  }

  # Prevent crossing x by itself
  if (!is.null(y_name) && identical(x_name, y_name)) {
    stop("Cannot cross a multiple response set by itself.", call. = FALSE)
  }

  # Identify and handle multiple response sets ----

  # Find specified x variable
  vars <- grep(paste0("^", x_name, "_"), names(data), value = TRUE)
  if (length(vars) == 0) stop("No multiple response sets found with prefix ", x_name, ".", call. = FALSE)

  vars <- vars[sapply(data[vars], function(v) is.factor(v) || inherits(v, "haven_labelled"))]
  if (length(vars) == 0) stop("No factor or haven_labelled multiple response sets found with prefix ", x_name, ".", call. = FALSE)

  # Coerce haven_labelled to factor
  data[vars] <- lapply(data[vars], function(v) {
    if (inherits(v, "haven_labelled")) haven::as_factor(v) else v
  })

  # Stop if unrecognized coding detected
  check_levels <- function(v) {
    lvls <- levels(v)
    if (all(c("Checked", "Unchecked") %in% lvls)) {
      return("checked_unchecked")
    } else if (all(c("Selected", "Not Selected") %in% lvls)) {
      return("selected_notselected")
    } else if (all(c("0", "1") %in% lvls)) {
      return("zero_one")
    } else {
      return(NULL)
    }
  }
  codings <- sapply(data[vars], check_levels)

  if (any(sapply(codings, is.null))) {
    stop(
      "Unrecognized multiple response set levels in one or more variables. Supported levels are Unchecked/Checked, Not Selected/Selected, or 0/1.",
      call. = FALSE
    )
  }

  is_checked <- function(v, coding) {
    if (coding == "checked_unchecked") {
      v == "Checked"
    } else if (coding == "selected_notselected") {
      v == "Selected"
    } else if (coding == "zero_one") {
      v == "1"
    } else {
      rep(FALSE, length(v))
    }
  }

  # Coerce y to factor
  .to_factor_once <- function(var) {
    if (is.factor(var)) return(var)
    if (inherits(var, "Date")) return(factor(format(var, "%Y-%m-%d")))
    factor(var)
  }

  if (!is.null(y_name)) {
    if (is.character(data[[y_name]])) {
      data[[y_name]] <- dplyr::na_if(stringr::str_trim(data[[y_name]]), "")
    }
    data[[y_name]] <- .to_factor_once(data[[y_name]])
  }

  # Extract labels

  get_label <- function(v) {
    lab <- attr(data[[v]], "label")
    if (!is.null(lab)) sub(":.*$", "", lab) else v
  }

  labels <- sapply(vars, get_label, USE.NAMES = FALSE)
  labels_unique <- make.unique(labels, sep = "_")

  # Build tables ----

  if (is.null(y_name)) {
    # Single variable
    result <- purrr::map_dfr(seq_along(vars), function(i) {
      v <- vars[i]; coding <- codings[i]
      n <- if (is.null(weight_name)) {
        sum(is_checked(data[[v]], coding), na.rm = TRUE)
      } else {
        sum(data[[weight_name]][is_checked(data[[v]], coding)], na.rm = TRUE)
      }
      base <- if (is.null(weight_name)) {
        sum(!is.na(data[[v]]))
      } else {
        sum(data[[weight_name]][!is.na(data[[v]])], na.rm = TRUE)
      }
      tibble::tibble(Label = labels_unique[i], n = n, base = base)
    }) %>%
      dplyr::mutate(val = if (prop) n / base else n) %>%
      dplyr::select(Label, val) %>%
      tibble::column_to_rownames("Label")

    # Add Base n row
    if (!numeric) {
      base_n <- if (is.null(weight_name)) {
        sum(apply(data[vars], 1, function(row) any(!is.na(row))), na.rm = TRUE)
      } else {
        sum(data[[weight_name]][apply(data[vars], 1, function(row) any(!is.na(row)))], na.rm = TRUE)
      }
      base_row <- tibble::tibble(Label = "Base n", val = base_n) %>%
        tibble::column_to_rownames("Label")
      result <- dplyr::bind_rows(result, base_row)
    }

    # Name the column properly
    if (!is.null(weight_name)) {
      names(result) <- if (prop) "Weighted %" else "Weighted Count"
    } else {
      names(result) <- if (prop) "Unweighted %" else "Unweighted Count"
    }

  } else {
    # Ensure y is factor with full levels
    data[[y_name]] <- factor(
      dplyr::na_if(stringr::str_trim(data[[y_name]]), ""),
      levels = levels(data[[y_name]])
    )
    y_levels <- levels(data[[y_name]])

    # Crosstab
    result <- purrr::map_dfr(seq_along(vars), function(i) {
      v <- vars[i]; coding <- codings[i]
      result <- tibble::tibble(Label = labels_unique[i])
      for (yv in y_levels) {
        mask <- data[[y_name]] == yv & !is.na(data[[y_name]])
        n <- if (is.null(weight_name)) {
          sum(is_checked(data[[v]], coding) & mask, na.rm = TRUE)
        } else {
          sum(data[[weight_name]][is_checked(data[[v]], coding) & mask], na.rm = TRUE)
        }
        base <- if (is.null(weight_name)) {
          sum(!is.na(data[[v]]) & mask)
        } else {
          sum(data[[weight_name]][!is.na(data[[v]]) & mask], na.rm = TRUE)
        }
        val <- if (prop) ifelse(base > 0, n / base, NA) else n
        result[[yv]] <- val
      }
      result
    }) %>%
      tibble::column_to_rownames("Label")

    # Add total column if requested
    if (total) {
      total_col <- purrr::map_dfr(seq_along(vars), function(i) {
        v <- vars[i]; coding <- codings[i]
        n <- if (is.null(weight_name)) {
          sum(is_checked(data[[v]], coding), na.rm = TRUE)
        } else {
          sum(data[[weight_name]][is_checked(data[[v]], coding)], na.rm = TRUE)
        }
        base <- if (is.null(weight_name)) {
          sum(!is.na(data[[v]]))
        } else {
          sum(data[[weight_name]][!is.na(data[[v]])], na.rm = TRUE)
        }
        tibble::tibble(Label = labels_unique[i], Total = if (prop) n / base else n)
      }) %>%
        tibble::column_to_rownames("Label")

      # Insert total as first column, rename appropriately
      col_label <- if (!is.null(weight_name)) {
        if (prop) "Weighted %" else "Weighted Count"
      } else {
        if (prop) "Unweighted %" else "Unweighted Count"
      }
      result <- cbind(total_col[rownames(result), , drop = FALSE], result)
      names(result)[1] <- col_label
    }

    # Add Base n row
    if (!numeric) {
      base_row <- tibble::tibble(Label = "Base n")
      for (yv in y_levels) {
        mask <- data[[y_name]] == yv & !is.na(data[[y_name]])
        base_n <- if (is.null(weight_name)) {
          sum(apply(data[vars], 1, function(row) any(!is.na(row))) & mask, na.rm = TRUE)
        } else {
          sum(data[[weight_name]][apply(data[vars], 1, function(row) any(!is.na(row))) & mask], na.rm = TRUE)
        }
        base_row[[yv]] <- base_n
      }
      if (total) {
        base_row[[names(result)[1]]] <- if (is.null(weight_name)) {
          sum(apply(data[vars], 1, function(row) any(!is.na(row))), na.rm = TRUE)
        } else {
          sum(data[[weight_name]][apply(data[vars], 1, function(row) any(!is.na(row)))], na.rm = TRUE)
        }
      }
      result <- result %>%
        tibble::rownames_to_column("Label") %>%
        dplyr::bind_rows(base_row) %>%
        tibble::column_to_rownames("Label")
    }
  }

  # Format ----

  if (prop && !numeric) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ formatC(as.numeric(.), format = "f", digits = round)))

    if ("Base n" %in% rownames(result)) {
      result["Base n", ] <- lapply(result["Base n", ], function(v)
        formatC(as.numeric(v), format = "f", digits = 0))
    }

  } else if (!prop && !numeric) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ formatC(round(as.numeric(.)), format = "f", digits = 0)))
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
    has_base <- "Base n" %in% rownames(result)
    if (has_base) {
      base_row <- result["Base n", , drop = FALSE]
      result <- result[rownames(result) != "Base n", , drop = FALSE]
    }

    # Always calculate hidden total for sorting
    hidden_total <- purrr::map_dbl(seq_along(vars), function(i) {
      v <- vars[i]; coding <- codings[i]
      n <- if (is.null(weight_name)) {
        sum(is_checked(data[[v]], coding), na.rm = TRUE)
      } else {
        sum(data[[weight_name]][is_checked(data[[v]], coding)], na.rm = TRUE)
      }
      base <- if (is.null(weight_name)) {
        sum(!is.na(data[[v]]))
      } else {
        sum(data[[weight_name]][!is.na(data[[v]])], na.rm = TRUE)
      }
      if (prop) ifelse(base > 0, n / base, NA) else n
    })
    names(hidden_total) <- labels_unique

    # Sort by hidden total
    if (identical(sort_val, "desc")) {
      result <- result[order(-hidden_total[rownames(result)]), , drop = FALSE]
    } else if (identical(sort_val, "asc")) {
      result <- result[order(hidden_total[rownames(result)]), , drop = FALSE]
    }

    # Reapply special ordering
    other_idx <- grepl("^Other", rownames(result), ignore.case = TRUE)
    dont_idx <- grepl("^(I don't|I do not)", rownames(result), ignore.case = TRUE)
    none_idx <- grepl("^None", rownames(result), ignore.case = TRUE)
    dk_idx <- grepl("Don't know", rownames(result), ignore.case = TRUE)

    other_rows <- result[other_idx, , drop = FALSE]
    dont_rows <- result[dont_idx, , drop = FALSE]
    none_rows <- result[none_idx, , drop = FALSE]
    dk_rows <- result[dk_idx, , drop = FALSE]

    core <- result[!(other_idx | dont_idx | none_idx | dk_idx), , drop = FALSE]
    result <- rbind(core, other_rows, dont_rows, none_rows, dk_rows)

    if (has_base) result <- rbind(result, base_row)
  }

  # Output ----

  if (numeric) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(as.numeric(.), digits = round)))

    if ("Base n" %in% rownames(result)) {
      result[rownames(result) == "Base n", ] <- lapply(
        result[rownames(result) == "Base n", ],
        function(v) round(as.numeric(v), 0)
      )
    }
  }

  # Attach attributes for printing info message
  attr(result, "vars") <- list(x = x_name, y = y_name, weight = weight_name)
  class(result) <- c("tab_mr_result", class(result))

  return(result)
}

#' Custom print method for tab_mr() results
#' @export
print.tab_mr_result <- function(x, ...) {
  vars <- attr(x, "vars")

  ital <- function(txt) if (requireNamespace("crayon", quietly = TRUE)) crayon::italic(txt) else txt

  if (is.null(vars$y)) {
    if (!is.null(vars$weight)) {
      header <- paste0("Frequency table of ", ital(vars$x), ", weighted by ", ital(vars$weight), ".")
    } else {
      header <- paste0("Frequency table of ", ital(vars$x), ", unweighted.")
    }
  } else {
    if (!is.null(vars$weight)) {
      header <- paste0("Crosstab of ", ital(vars$x), " by ", ital(vars$y),
                       ", weighted by ", ital(vars$weight), ".")
    } else {
      header <- paste0("Crosstab of ", ital(vars$x), " by ", ital(vars$y), ", unweighted.")
    }
  }

  cat(header, "\n\n")
  NextMethod("print")
}
