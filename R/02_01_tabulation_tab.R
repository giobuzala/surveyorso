#' **Create a frequency table or crosstab**
#'
#' @description
#' This function produces a frequency table (for a single variable) or a crosstab (for two variables).
#'
#' - If only `x` is specified, the output is a frequency table of `x`.
#' - If both `x` and `y` are specified, the output is a crosstab of `x` by `y`.
#' - Use the `weight` argument to produce weighted tables; if omitted, results are unweighted.
#' - Set `prop = TRUE` (default) to output percentages, or `prop = FALSE` to output counts.
#' - Use the `round` argument to control decimal precision in percentage tables.
#' - If `numeric = TRUE`, the function removes the total row and coerces all values to numeric, making the output math-ready (e.g., can be multiplied or added directly).
#'
#' @param data A data frame containing the survey data.
#' @param x The variable for which to compute the frequency table (or the row variable in a crosstab).
#' @param y (Optional) The variable to cross-tabulate against `x`. Defaults to `NULL`.
#' @param weight (Optional) A numeric weighting variable. If `NULL` (default), results are unweighted.
#' @param prop Logical; if `TRUE` (default), outputs percentages. If `FALSE`, outputs counts.
#' @param total Logical; if `TRUE` (default), adds a total column when `y` is specified. Ignored for single-variable tables.
#' @param round Integer; number of decimal places for percentages. Defaults to `3`.
#' @param numeric Logical; if `TRUE`, returns a data frame with numeric columns, with the n row removed. Defaults to `FALSE`.
#'
#' @return
#' A frequency table (if `y = NULL`) or a crosstab (if `y` is specified), as a data frame.
#'
#' @examples
#' # Weighted frequency table of Q1, showing percentages
#' tab(data = survey_data, x = Q1, weight = weight_var)
#'
#' # Weighted crosstab of Q1 by region, showing counts
#' tab(data = survey_data, x = Q1, y = region, weight = weight_var, prop = FALSE)
#' @export

tab <- function(data, x, y = NULL, weight = NULL, prop = TRUE, total = TRUE, round = 3, numeric = FALSE) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "dplyr", "tidyr", "stringr", "crayon")
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

  # Ensure that specified variables exist
  cols <- colnames(data)

  provided <- c(
    if (!rlang::quo_is_null(x_enquo)) x_name,
    if (!rlang::quo_is_null(y_enquo)) y_name,
    if (!rlang::quo_is_null(w_enquo)) weight_name
  )

  missing <- setdiff(provided, cols)
  if (length(missing) > 0) {
    stop(
      paste0(
        "The following variables were not found in the dataset: ",
        paste(missing, collapse = ", "),
        "\nProvided: x = ", if (rlang::quo_is_null(x_enquo)) "NULL" else x_name,
        ", y = ", if (rlang::quo_is_null(y_enquo)) "NULL" else y_name,
        ", weight = ", if (rlang::quo_is_null(w_enquo)) "NULL" else weight_name
      ),
      call. = FALSE
    )
  }

  # If weight is provided, it must be numeric or logical
  if (!rlang::quo_is_null(w_enquo)) {
    if (!(is.numeric(data[[weight_name]]) || is.logical(data[[weight_name]]))) {
      stop(
        paste0("Weight must be numeric or logical. Column '", weight_name,
               "' is of type ", class(data[[weight_name]])[1], "."),
        call. = FALSE
      )
    }
  }

  # Helpers ----

  # If x or y is the weight column, duplicate to protect numeric weights
  if (!rlang::quo_is_null(w_enquo)) {
    if (!rlang::quo_is_null(x_enquo) && identical(x_name, weight_name)) {
      tmp_x <- "..x_fac.."
      data[[tmp_x]] <- data[[x_name]]
      x_enquo <- rlang::quo(!!rlang::sym(tmp_x))
      x_name <- tmp_x
    }

    if (!rlang::quo_is_null(y_enquo) && identical(y_name, weight_name)) {
      tmp_yw <- "..y_fac.."
      data[[tmp_yw]] <- data[[y_name]]
      y_enquo <- rlang::quo(!!rlang::sym(tmp_yw))
      y_name <- tmp_yw
    }
  }

  # If x and y arguments are the same
  if (!rlang::quo_is_null(y_enquo) && identical(x_name, y_name)) {
    tmp_y <- "..y_dup.."
    data[[tmp_y]] <- data[[y_name]]
    y_enquo <- rlang::quo(!!rlang::sym(tmp_y))
    y_name <- tmp_y
  }

  # Helper function - coerce selected variables to factor if they're not factor
  .to_factor_once <- function(var) {
    if (is.factor(var)) return(var)
    if (inherits(var, "Date")) return(factor(format(var, "%Y-%m-%d")))
    factor(var)
  }

  # Overwrite x and y as factors
  if (!is.null(x_name)) {
    if (is.character(data[[x_name]])) {
      data[[x_name]] <- dplyr::na_if(stringr::str_trim(data[[x_name]]), "")
    }
    data[[x_name]] <- .to_factor_once(data[[x_name]])
  }

  if (!is.null(y_name)) {
    if (is.character(data[[y_name]])) {
      data[[y_name]] <- dplyr::na_if(stringr::str_trim(data[[y_name]]), "")
    }
    data[[y_name]] <- .to_factor_once(data[[y_name]])
  }

  # Build tables ----

  # Single variable
  if (rlang::quo_is_null(y_enquo)) {

    # Counts
    if (!prop) {
      if (rlang::quo_is_null(w_enquo)) {
        # Unweighted counts
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          dplyr::group_by(.data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Unweighted Count` = dplyr::n(), .groups = "drop") %>%
          dplyr::bind_rows(
            tibble::tibble(!!x_name := "n",
                           `Unweighted Count` = sum(!is.na(data[[x_name]])))
          ) %>%
          tibble::column_to_rownames(var = x_name)

      } else {
        # Weighted counts
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          dplyr::group_by(.data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Weighted Count` = round(sum(.data[[weight_name]])), .groups = "drop") %>%
          dplyr::bind_rows(
            tibble::tibble(!!x_name := "n",
                           `Weighted Count` = round(sum(data[[weight_name]][!is.na(data[[x_name]])])))
          ) %>%
          tibble::column_to_rownames(var = x_name)
      }

      # Percentages
    } else {
      if (rlang::quo_is_null(w_enquo)) {
        # Unweighted %
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          dplyr::group_by(.data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Unweighted %` = dplyr::n() / sum(!is.na(data[[x_name]])), .groups = "drop") %>%
          dplyr::bind_rows(
            tibble::tibble(!!x_name := "n",
                           `Unweighted %` = sum(!is.na(data[[x_name]])))
          ) %>%
          dplyr::mutate(
            `Unweighted %` = ifelse(
              !!rlang::sym(x_name) == "n",
              formatC(`Unweighted %`, format = "f", digits = 0),
              formatC(`Unweighted %`, format = "f", digits = round)
            )
          ) %>%
          tibble::column_to_rownames(var = x_name)

      } else {
        # Weighted %
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          dplyr::group_by(.data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Weighted %` = sum(.data[[weight_name]]) / sum(data[[weight_name]][!is.na(data[[x_name]])]), .groups = "drop") %>%
          dplyr::bind_rows(
            tibble::tibble(!!x_name := "n",
                           `Weighted %` = sum(data[[weight_name]][!is.na(data[[x_name]])]))
          ) %>%
          dplyr::mutate(
            `Weighted %` = ifelse(
              !!rlang::sym(x_name) == "n",
              formatC(`Weighted %`, format = "f", digits = 0),
              formatC(`Weighted %`, format = "f", digits = round)
            )
          ) %>%
          tibble::column_to_rownames(var = x_name)
      }
    }

    # Crosstab
  } else {

    # Counts
    if (!prop) {
      if (rlang::quo_is_null(w_enquo)) {
        # Unweighted counts
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]])) %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Unweighted Count` = dplyr::n(), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Unweighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::bind_rows(
            tibble::tibble(` ` = "n", !!!as.list(colSums(dplyr::select(., -` `), na.rm = TRUE)))
          ) %>%
          dplyr::mutate(across(-1, ~ round(.x))) %>%
          tibble::column_to_rownames(var = " ")

      } else {
        # Weighted counts
        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]])) %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Weighted Count` = sum(.data[[weight_name]]), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Weighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::bind_rows(
            tibble::tibble(` ` = "n", !!!as.list(colSums(dplyr::select(., -` `), na.rm = TRUE)))
          ) %>%
          dplyr::mutate(dplyr::across(-1, ~ round(.x))) %>%
          tibble::column_to_rownames(var = " ")
      }

      # Percentages
    } else {
      if (rlang::quo_is_null(w_enquo)) {
        # Unweighted %
        totals <- data %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Unweighted Count` = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Unweighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::bind_rows(
            tibble::tibble(` ` = "n", !!!as.list(colSums(dplyr::select(., -` `), na.rm = TRUE)))
          ) %>%
          dplyr::mutate(dplyr::across(-1, ~ round(.x))) %>%
          tibble::column_to_rownames(var = " ")

        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]])) %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Unweighted Count` = dplyr::n(), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Unweighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::mutate(dplyr::across(-1, ~ .x / sum(.x))) %>%
          dplyr::mutate(dplyr::across(-1, ~ ifelse(is.nan(.x), NA, .x))) %>%
          dplyr::bind_rows(totals[nrow(totals), ] %>% tibble::rownames_to_column(" ")) %>%
          dplyr::mutate(
            dplyr::across(-1, ~ ifelse(
              ` ` == "n",
              formatC(., format = "f", digits = 0),
              formatC(., format = "f", digits = round)
            ))
          ) %>%
          tibble::column_to_rownames(var = " ")

      } else {
        # Weighted %
        totals <- data %>%
          dplyr::filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]])) %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Weighted Count` = sum(.data[[weight_name]]), .groups = "drop") %>%
          dplyr::filter(!is.na(.data[[x_name]])) %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Weighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::bind_rows(
            tibble::tibble(` ` = "n", !!!as.list(colSums(dplyr::select(., -` `), na.rm = TRUE)))
          ) %>%
          dplyr::mutate(dplyr::across(-1, ~ round(.x))) %>%
          tibble::column_to_rownames(var = " ")

        result <- data %>%
          dplyr::filter(!is.na(.data[[x_name]]), !is.na(.data[[y_name]])) %>%
          dplyr::group_by(.data[[y_name]], .data[[x_name]], .drop = FALSE) %>%
          dplyr::summarise(`Weighted Count` = sum(.data[[weight_name]]), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = !!y_enquo, values_from = `Weighted Count`) %>%
          dplyr::rename(" " = !!x_enquo) %>%
          dplyr::mutate(dplyr::across(-1, ~ .x / sum(.x))) %>%
          dplyr::mutate(dplyr::across(-1, ~ ifelse(is.nan(.x), NA, .x))) %>%
          dplyr::bind_rows(totals[nrow(totals), ] %>% tibble::rownames_to_column(" ")) %>%
          dplyr::mutate(
            dplyr::across(-1, ~ ifelse(
              ` ` == "n",
              formatC(., format = "f", digits = 0),
              formatC(., format = "f", digits = round)
            ))
          ) %>%
          tibble::column_to_rownames(var = " ")
      }
    }
  }

  # Add totals column if requested
  if (!rlang::quo_is_null(y_enquo) && total) {
    if (rlang::quo_is_null(w_enquo)) {
      total_tbl <- tab(
        data = data,
        x = !!rlang::sym(x_name),
        prop = prop,
        round = round,
        numeric = FALSE,
        total = FALSE
      )
    } else {
      total_tbl <- tab(
        data = data,
        x = !!rlang::sym(x_name),
        weight = !!rlang::sym(weight_name),
        prop = prop,
        round = round,
        numeric = FALSE,
        total = FALSE
      )
    }

    total_col <- total_tbl[, 1, drop = FALSE]
    result <- cbind(total_col, result)
  }

  # Output ----

  if (numeric) {
    result <- result %>%
      dplyr::slice_head(n = nrow(.) - 1) %>%
      dplyr::mutate(dplyr::across(everything(), ~ suppressWarnings(as.numeric(.x))))
  }

  # Attach attributes for printing info message
  attr(result, "vars") <- list(
    x = as.character(x_name),
    y = if (!rlang::quo_is_null(y_enquo)) as.character(y_name) else NULL,
    weight = if (!rlang::quo_is_null(w_enquo)) as.character(weight_name) else NULL
  )

  class(result) <- c("tab_result", class(result))

  return(result)
}

#' Custom print method for tab() results
#' @export
print.tab_result <- function(x, ...) {
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
      header <- paste0("Crosstab of ", ital(vars$x), " by ", ital(vars$y), ", weighted by ", ital(vars$weight), ".")
    } else {
      header <- paste0("Crosstab of ", ital(vars$x), " by ", ital(vars$y), ", unweighted.")
    }
  }

  cat(header, "\n\n")
  NextMethod("print")
}
