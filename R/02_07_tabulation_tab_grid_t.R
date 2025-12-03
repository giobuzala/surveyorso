#' **Create a top/bottom box table or crosstab for a grid question**
#'
#' @description
#' This function produces a top- or bottom-box summary for grid survey questions.
#'
#' - If only `x` is specified, the output is a top- or bottom-box table for each item in the grid question.
#' - If both `x` and `y` are specified, the output is a crosstab of the selected top/bottom box by `y`.
#' - Use the `top_bottom` argument (e.g., `T2`, `B3`) to define how many top or bottom response levels are included in the box.
#' - Use the `weight` argument to produce weighted tables; if omitted, results are unweighted.
#' - Set `prop = TRUE` (default) to output proportions, or `prop = FALSE` to output frequencies.
#' - Use the `total` argument to include a total column when a `y` variable is specified. Ignored for one-way tables.
#' - Use the `sort` argument (`asc` or `desc`) to order rows by their box score. Set `sort = NULL` for no sorting.
#' - Use the `round` argument to control decimal precision in proportions tables.
#' - If `numeric = TRUE`, the function removes the base row and coerces all values to numeric, making the output math-ready.
#'
#' @param data A data frame containing the survey data.
#' @param x A variable prefix identifying the grid question (e.g., `Q3` for `Q3_1`, `Q3_2`, ...).
#' @param y (Optional) A variable name to cross-tabulate against the grid question. Defaults to `NULL`.
#' @param top_bottom A string of the form `TN` or `BN` indicating how many top or bottom levels to include in the box (e.g., `T2` = top 2). Defaults to `T2`.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param prop Logical; if `TRUE` (default), outputs proportions. If `FALSE`, outputs frequencies.
#' @param total Logical; if `TRUE` (default), adds a total column when `y` is specified.
#' @param sort Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting). Defaults to `NULL`.
#' @param round Integer; number of decimal places for proportions. Defaults to `3`.
#' @param numeric Logical; if `TRUE`, returns a data frame with numeric columns, with the base n row removed. Defaults to `FALSE`.
#'
#' @return
#' A table (if `y = NULL`) or a crosstab (if `y` is specified), as a data frame.
#'
#' @examples
#' # Unweighted proportions table of a grid question, showing top 2 box
#' tab_grid_t(data = survey_data, x = Q4, top_bottom = T2)
#'
#' # Weighted proportions crosstab of a grid question by region, showing bottom 2 box
#' tab_grid_t(data = survey_data, x = Q4, y = region, top_bottom = B2, weight = weight_var)
#'
#' @export

tab_grid_t <- function(data, x, y = NULL, top_bottom = T2, weight = NULL, prop = TRUE, total = TRUE, sort = NULL, round = 3, numeric = FALSE) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "dplyr", "tidyr", "purrr", "stringr", "stats", "crayon")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  `%>%` <- dplyr::`%>%`

  x_enquo <- rlang::enquo(x)
  x <- rlang::as_name(x_enquo)

  y_enquo <- rlang::enquo(y)
  y_name <- if (rlang::quo_is_null(y_enquo)) NULL else rlang::as_name(y_enquo)

  w_enquo <- rlang::enquo(weight)
  w_name <- if (rlang::quo_is_null(w_enquo)) NULL else rlang::as_name(w_enquo)

  # Handle errors ----

  cols <- colnames(data)

  provided <- c(
    if (!is.null(y_name)) y_name,
    if (!is.null(w_name)) w_name
  )

  missing <- setdiff(provided, cols)
  if (length(missing) > 0) {
    stop(
      paste0(
        "The following variables were not found in the dataset: ",
        paste(missing, collapse = ", "),
        "\nProvided: y = ", if (is.null(y_name)) "NULL" else y_name,
        ", weight = ", if (is.null(w_name)) "NULL" else w_name
      ),
      call. = FALSE
    )
  }

  if (!is.null(w_name)) {
    if (!(is.numeric(data[[w_name]]) || is.logical(data[[w_name]]))) {
      stop("Weight must be numeric or logical.", call. = FALSE)
    }
  }

  # Clean y variable if provided
  if (!is.null(y_name)) {
    if (!y_name %in% names(data)) {stop("The specified y variable ", y_name, " does not exist in the dataset.", call. = FALSE)}
    if (is.character(data[[y_name]])) {
      data[[y_name]] <- factor(dplyr::na_if(stringr::str_trim(data[[y_name]]), ""))
    } else if (is.factor(data[[y_name]])) {
      lvls <- levels(data[[y_name]])
      data[[y_name]][data[[y_name]] == ""] <- NA
      data[[y_name]] <- factor(data[[y_name]], levels = lvls)
    }
  }

  # Parse top_bottom
  tb_expr <- rlang::enexpr(top_bottom)
  top_bottom <- rlang::as_label(tb_expr)
  if (!grepl("^[tTbB][0-9]+$", top_bottom)) {stop("top_bottom must be in the form 'TN' or 'BN'.", call. = FALSE)}
  direction <- tolower(substr(top_bottom, 1, 1))
  n_val <- as.integer(substr(top_bottom, 2, nchar(top_bottom)))
  if (is.na(n_val) || n_val <= 0) {stop("N in top_bottom must be a positive integer.", call. = FALSE)}

  # Handle grid variables ----

  # Identify
  vars <- grep(paste0("^", x, "_"), names(data), value = TRUE)
  if (length(vars) == 0) stop("No grid questions found with prefix ", x, ".", call. = FALSE)
  vars <- vars[sapply(data[vars], is.factor)]
  if (length(vars) == 0) stop("No factor grid questions found with prefix ", x, ".", call. = FALSE)

  # Stop if multiple response set provided
  bad_sets <- list(
    c("Checked", "Unchecked"),
    c("Unchecked", "Checked"),
    c("Selected", "Not Selected"),
    c("Not Selected", "Selected"),
    c("1", "0"), c("0", "1")
  )

  is_bad <- any(sapply(vars, function(v) {
    if (is.factor(data[[v]])) {
      lvls <- levels(data[[v]])
      any(sapply(bad_sets, function(b) setequal(lvls, b)))
    } else if (is.numeric(data[[v]])) {
      # Also treat binary numeric 0/1 as multiple-response
      all(sort(unique(stats::na.omit(data[[v]]))) %in% c(0, 1))
    } else {
      FALSE
    }
  }))
  if (is_bad) stop(x, " looks like a multiple response set.", call. = FALSE)

  # Extract labels
  get_label <- function(v) {
    lab <- attr(data[[v]], "label")
    if (!is.null(lab)) sub(":.*$", "", lab) else v
  }
  labels <- sapply(vars, get_label, USE.NAMES = FALSE)
  labels_unique <- make.unique(labels, sep = "_")

  exclude_box <- c("Don't know", "Dont know", "I don't know", "I dont know", "Prefer not to say", "Prefer to not say")

  # Validate top_bottom against scale
  min_lvls <- min(sapply(vars, function(v) {
    lvls <- setdiff(levels(data[[v]]), exclude_box)
    length(lvls)
  }))
  if (n_val > min_lvls) {
    stop("Requested ", top_bottom, " but only ", min_lvls, " valid levels available.", call. = FALSE)
  }

  # Build tables ----

  result <- purrr::map_dfr(seq_along(vars), function(i) {
    v <- vars[i]
    lab <- labels[i]

    xvec <- data[[v]]
    if (!is.factor(xvec)) xvec <- factor(xvec)

    keep <- !is.na(xvec)
    xvec <- droplevels(xvec[keep])
    wvec <- if (!is.null(w_name)) data[[w_name]][keep] else rep(1, length(xvec))
    yvec <- if (!is.null(y_name)) data[[y_name]][keep] else NULL

    lvls <- levels(xvec)
    lvls_box <- setdiff(lvls, exclude_box)

    box_idx <- if (direction == "t") {
      1:n_val
    } else {
      (length(lvls_box) - n_val + 1):length(lvls_box)
    }

    in_box <- xvec %in% lvls_box[box_idx]

    if (is.null(y_name)) {
      n <- sum(wvec[in_box], na.rm = TRUE)
      base <- sum(wvec, na.rm = TRUE)
      val_out <- if (prop) n / base else n
      tibble::tibble(Label = lab, val = val_out)
    } else {
      tibble::tibble(Label = lab, y = yvec, in_box = in_box, w = wvec) %>%
        dplyr::group_by(Label, y, .drop = FALSE) %>%
        dplyr::summarise(
          n = sum(w[in_box], na.rm = TRUE),
          base = sum(w, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(val = if (prop) ifelse(base > 0, n / base, NA_real_) else n) %>%
        dplyr::select(Label, y, val)
    }
  })

  if (nrow(result) == 0) stop("No valid results.", call. = FALSE)

  # Reshape
  if (is.null(y_name)) {
    result <- result %>%
      dplyr::select(Label, val) %>%
      dplyr::arrange(match(Label, labels)) %>%
      tibble::column_to_rownames("Label")
  } else {
    y_lvls <- levels(data[[y_name]])
    result <- result %>%
      tidyr::pivot_wider(names_from = y, values_from = val) %>%
      dplyr::arrange(match(Label, labels)) %>%
      tibble::column_to_rownames("Label")

    # Add back missing y columns
    missing_cols <- setdiff(y_lvls, colnames(result))
    for (mc in missing_cols) result[[mc]] <- NA_real_
    result <- result[, c(intersect(y_lvls, colnames(result)), setdiff(colnames(result), y_lvls)), drop = FALSE]

    # Add total column if requested
    if (total) {
      total_res <- purrr::map_dfr(seq_along(vars), function(i) {
        v <- vars[i]
        lab <- labels[i]

        xvec <- data[[v]]
        if (!is.factor(xvec)) xvec <- factor(xvec)
        keep <- !is.na(xvec)
        xvec <- droplevels(xvec[keep])
        wvec <- if (!is.null(w_name)) data[[w_name]][keep] else rep(1, length(xvec))

        lvls <- levels(xvec)
        lvls_box <- setdiff(lvls, exclude_box)
        box_idx <- if (direction == "t") 1:n_val else (length(lvls_box) - n_val + 1):length(lvls_box)

        in_box <- xvec %in% lvls_box[box_idx]
        n <- sum(wvec[in_box], na.rm = TRUE)
        base <- sum(wvec, na.rm = TRUE)
        val_out <- if (prop) n / base else n

        tibble::tibble(Label = lab, Total = val_out)
      }) %>%
        dplyr::arrange(match(Label, labels)) %>%
        tibble::column_to_rownames("Label")

      total_col_name <- if (!is.null(w_name)) {
        if (prop) "Weighted %" else "Weighted Count"
      } else {
        if (prop) "Unweighted %" else "Unweighted Count"
      }
      colnames(total_res) <- total_col_name
      result <- cbind(total_res, result)
    }
  }

  # Add base row
  if (!numeric) {
    if (is.null(y_name)) {
      base_n <- if (!is.null(w_name)) sum(data[[w_name]], na.rm = TRUE) else nrow(data)
      base_row <- tibble::tibble(Label = "Base n", val = base_n) %>%
        tibble::column_to_rownames("Label")
    } else {
      if (!is.null(w_name)) {
        base_row <- data %>%
          dplyr::group_by(.data[[y_name]]) %>%
          dplyr::summarise(val = sum(.data[[w_name]], na.rm = TRUE), .groups = "drop")
      } else {
        base_row <- data %>%
          dplyr::group_by(.data[[y_name]]) %>%
          dplyr::summarise(val = dplyr::n(), .groups = "drop")
      }
      base_row <- base_row %>%
        tidyr::pivot_wider(names_from = !!y_name, values_from = val) %>%
        as.data.frame()
      rownames(base_row) <- "Base n"

      missing_cols <- setdiff(colnames(result), colnames(base_row))
      for (mc in missing_cols) base_row[[mc]] <- 0
      base_row <- base_row[, colnames(result), drop = FALSE]

      if (total) {
        total_col_name <- if (!is.null(w_name)) {
          if (prop) "Weighted %" else "Weighted Count"
        } else {
          if (prop) "Unweighted %" else "Unweighted Count"
        }
        base_row[[total_col_name]] <- if (!is.null(w_name)) {
          sum(data[[w_name]], na.rm = TRUE)
        } else {
          nrow(data)
        }
        base_row <- base_row[, colnames(result), drop = FALSE]
      }
    }
    result <- dplyr::bind_rows(result, base_row)
  }

  # Format ----

  if (!is.null(w_name)) {
    col_label <- if (prop) "Weighted %" else "Weighted Count"
  } else {
    col_label <- if (prop) "Unweighted %" else "Unweighted Count"
  }
  if (is.null(y_name)) names(result) <- col_label

  if (prop && !numeric) {
    result <- dplyr::mutate(result, dplyr::across(dplyr::everything(), ~ formatC(as.numeric(.), format = "f", digits = round)))
    if ("Base n" %in% rownames(result)) {
      result["Base n", ] <- lapply(result["Base n", ], function(v) formatC(as.numeric(v), format = "f", digits = 0))
    }
  } else if (!prop && !numeric) {
    result <- dplyr::mutate(result, dplyr::across(dplyr::everything(), ~ round(as.numeric(.))))
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

    # Compute hidden totals
    hidden_total <- purrr::map_dbl(seq_along(vars), function(i) {
      v <- vars[i]
      xvec <- data[[v]]
      if (!is.factor(xvec)) xvec <- factor(xvec)
      keep <- !is.na(xvec)
      xvec <- droplevels(xvec[keep])
      wvec <- if (!is.null(w_name)) data[[w_name]][keep] else rep(1, length(xvec))
      lvls <- levels(xvec)
      lvls_box <- setdiff(lvls, exclude_box)
      box_idx <- if (direction == "t") 1:n_val else (length(lvls_box) - n_val + 1):length(lvls_box)
      in_box <- xvec %in% lvls_box[box_idx]
      n <- sum(wvec[in_box], na.rm = TRUE)
      base <- sum(wvec, na.rm = TRUE)
      if (prop) ifelse(base > 0, n / base, NA) else n
    })
    names(hidden_total) <- labels

    if (identical(sort_val, "desc")) {
      result <- result[order(-hidden_total[rownames(result)]), , drop = FALSE]
    } else if (identical(sort_val, "asc")) {
      result <- result[order(hidden_total[rownames(result)]), , drop = FALSE]
    }

    if (has_base) result <- rbind(result, base_row)
  }

  # Output ----

  if (numeric) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ {
        v <- as.numeric(.x)
        if (prop) round(v, round) else round(v, 0)
      }))
  }

  # Attach attributes for printing info message
  attr(result, "vars") <- list(
    x = x,
    y = y_name,
    weight = w_name,
    top_bottom = top_bottom,
    levels = setdiff(levels(data[[vars[1]]]), exclude_box)
  )

  class(result) <- c("tab_grid_t_result", class(result))

  return(result)
}

#' **Custom print method for `tab_grid_t()` results**
#'
#' @description
#' Provides a readable console summary for `tab_grid_t()` output by displaying a descriptive header followed by the table.
#'
#' @export

print.tab_grid_t_result <- function(x, ...) {
  vars <- attr(x, "vars")

  ital <- function(txt) if (requireNamespace("crayon", quietly = TRUE)) crayon::italic(txt) else txt

  tb_text <- vars$top_bottom
  tb_disp <- tb_text

  if (!is.null(tb_text) && grepl("^[tTbB][0-9]+$", tb_text)) {
    direction <- tolower(substr(tb_text, 1, 1))
    n_val <- as.integer(substr(tb_text, 2, nchar(tb_text)))
    lvls_box <- vars$levels

    if (!is.null(lvls_box) && n_val <= length(lvls_box)) {
      if (direction == "t") {
        chosen <- lvls_box[1:n_val]
      } else {
        chosen <- tail(lvls_box, n_val)
      }
      tb_disp <- paste0(tb_text, " = ", paste(shQuote(chosen), collapse = " + "))
    }
  }

  # Build header
  if (is.null(vars$y)) {
    if (!is.null(vars$weight)) {
      header <- paste0("Frequency table of ", ital(vars$x), " (", tb_disp,
                       "), weighted by ", ital(vars$weight), ".")
    } else {
      header <- paste0("Frequency table of ", ital(vars$x), " (", tb_disp,
                       "), unweighted.")
    }
  } else {
    if (!is.null(vars$weight)) {
      header <- paste0("Crosstab of ", ital(vars$x), " (", tb_disp,
                       ") by ", ital(vars$y),
                       ", weighted by ", ital(vars$weight), ".")
    } else {
      header <- paste0("Crosstab of ", ital(vars$x), " (", tb_disp,
                       ") by ", ital(vars$y),
                       ", unweighted.")
    }
  }

  cat(header, "\n\n")
  NextMethod("print")
}
