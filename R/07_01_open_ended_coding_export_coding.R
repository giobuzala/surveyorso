#' **Export a coding workbook**
#'
#' @description
#' This function creates an Excel-based coding workbook for open-ended survey questions.
#'
#' - To export a workbook for a single variable, set `x = var`.
#' - To export for multiple variables at once, provide a vector, e.g., `x = c(var1, var2, var3)`.
#' - Optionally, you can include a `filter` variable that will appear in the workbook for easier sorting or filtering.
#'
#' @param data A data frame containing the survey responses. Must include a `Vrid` column and the variables specified in `x`.
#' @param x A single variable or multiple variables specifying the open-ended question(s) to export.
#' @param path File path where the workbook(s) will be saved. Defaults to `"Data"`.
#' @param id_var ID variable in the dataset. Defaults to `Vrid`.
#' @param filter (Optional) A single variable used to group or filter responses in the exported workbook.
#'
#' @details
#' Each workbook includes:
#' - A `Coding Workbook` sheet containing responses, along with columns for codes and bins.
#' - A `Codes` sheet pre-populated with standard codes (`97` = `Other`, `98` = `None`, `99` = `Don't know`) and space for custom codes.
#'
#' The workbook also includes Excel formulas for bin lookups, counts, and percentages, with conditional formatting to highlight invalid codes.
#'
#' **NOTE:** The formula in cell D2 of the first sheet must be filled down to the end of the column. Populating the formula programmatically in R causes Excel to slow down.
#'
#' @return
#' One or more Excel workbooks saved to the specified path.
#'
#' @examples
#' # Export coding workbook for Q5
#' export_coding(data = survey_data, x = Q5)
#'
#' # Export separate coding workbooks for Q5 and Q6
#' export_coding(data = survey_data, x = c(Q5, Q6))
#'
#' @export

export_coding <- function(data, x, path = "Data", id_var = Vrid, filter = NULL) {
  # Check required packages ----

  required_pkgs <- c("rlang", "tibble", "tidyselect", "dplyr", "openxlsx")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Capture variable expressions ----

  `%>%` <- dplyr::`%>%`

  if (missing(x)) stop("You must specify at least one variable.", call. = FALSE)

  x_expr <- substitute(x)

  if (is.character(x_expr)) {
    vars <- x_expr
  } else if (is.symbol(x_expr)) {
    vars <- as.character(x_expr)
  } else if (is.call(x_expr) && identical(x_expr[[1]], as.name("c"))) {
    vars <- as.character(as.list(x_expr)[-1])
  } else {
    vars <- tryCatch(
      tidyselect::eval_select(x_expr, data = data) %>% names(),
      error = function(e) as.character(x)
    )
  }

  x <- vars

  id_var_name <- rlang::as_name(rlang::enquo(id_var))

  if (missing(filter) || is.null(substitute(filter))) {
    filter <- NULL
  } else {
    filter_expr <- substitute(filter)

    if (is.character(filter_expr)) {
      vars <- filter_expr
    } else if (is.symbol(filter_expr)) {
      vars <- as.character(filter_expr)
    } else if (is.call(filter_expr) && identical(filter_expr[[1]], as.name("c"))) {
      vars <- as.character(as.list(filter_expr)[-1])
    } else {
      vars <- tryCatch(
        tidyselect::eval_select(filter_expr, data = data) %>% names(),
        error = function(e) as.character(filter_expr)
      )
    }

    if (length(vars) > 1) {
      stop("`filter` must refer to exactly one variable.", call. = FALSE)
    }

    filter <- vars
  }

  # Handle errors ----

  # Check that the specified variables exist in the dataset
  missing_q <- setdiff(x, colnames(data))
  if (length(missing_q) > 0) {
    stop("The following variables do not exist in the dataset: ",
         paste(missing_q, collapse = ", "),
         call. = FALSE)
  }

  # Check that the specified variables are character
  classes <- vapply(x, function(q) class(data[[q]])[1], character(1))
  not_char <- x[classes != "character"]
  if (length(not_char) > 0) {
    details <- paste0(" - ", not_char, " - ", classes[match(not_char, x)], collapse = "\n")
    stop("The following variable(s) must be of type character:\n", details,
         call. = FALSE)
  }

  # Check that ID variable exists if provided
  if (!(id_var_name %in% colnames(data))) {
    stop("ID variable ", id_var_name, " does not exist in the dataset.", call. = FALSE)
  }

  # Check that filter variable exists if provided
  if (!is.null(filter) && !filter %in% colnames(data)) {
    stop("The filter variable ", filter, " does not exist in the dataset.", call. = FALSE)
  }

  # Create workbook(s) and export ----

  for (q in x) {
    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Styles
    header_style <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "center")
    even_row_style <- openxlsx::createStyle(fgFill = "#F2F2F2", wrapText = TRUE,
                                            border = "TopBottomLeftRight", borderColour = "#E0E0E0")
    odd_row_style <- openxlsx::createStyle(fgFill = "#FFFFFF", wrapText = TRUE,
                                           border = "TopBottomLeftRight", borderColour = "#E0E0E0")
    vert_center_style <- openxlsx::createStyle(valign = "center")

    # Prepare coding data
    var_label <- attr(data[[q]], "label") # Determine question label; if not, fallback to variable name
    if (is.null(var_label) || !nzchar(var_label)) var_label <- q

    coding_sheet <- data %>%
      dplyr::select(dplyr::all_of(id_var_name), dplyr::all_of(q), dplyr::all_of(filter)) %>%
      dplyr::filter(.data[[q]] != "") %>%
      dplyr::rename(!!var_label := dplyr::all_of(q)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ { attr(.x, "label") <- NULL; .x })) %>%
      dplyr::arrange(!!rlang::sym(id_var_name)) %>%
      dplyr::mutate(`Code(s)` = NA_character_,
                    `Bin(s)` = NA_character_)

    if (!is.null(filter)) {
      coding_sheet <- coding_sheet %>%
        dplyr::relocate(dplyr::all_of(filter), .after = dplyr::last_col())
    }


    # Coding Workbook sheet
    openxlsx::addWorksheet(wb, "Coding Workbook")
    openxlsx::writeData(wb, "Coding Workbook", coding_sheet, withFilter = TRUE)

    openxlsx::addStyle(wb, "Coding Workbook", header_style, rows = 1, cols = 1:ncol(coding_sheet), gridExpand = TRUE)
    for (i in 1:nrow(coding_sheet)) {
      style <- if (i %% 2 == 0) even_row_style else odd_row_style
      openxlsx::addStyle(wb, "Coding Workbook", style, rows = i + 1, cols = 1:ncol(coding_sheet), gridExpand = TRUE)
    }

    openxlsx::setColWidths(wb, "Coding Workbook", cols = c(2, 4), widths = 100)
    if (!is.null(filter)) {openxlsx::setColWidths(wb, "Coding Workbook", cols = 5, widths = 20)}
    openxlsx::freezePane(wb, "Coding Workbook", firstRow = TRUE)


    # Codes sheet
    codes_sheet <- tibble::tibble(
      Code = c(97, 98, 99, rep(NA, 27)),
      Bin = c("Other", "None", "Don't know", rep(NA_character_, 27)),
      Description = c(
        "Response does not fit into any existing categories or represents a unique situation not captured by other codes.",
        "Response is irrelevant, nonsensical, or provides no meaningful information (e.g., gibberish, off-topic, or empty text).",
        "Respondent expresses uncertainty, confusion, or lack of an opinion or knowledge about the topic.",
        rep(NA_character_, 27)
      ),
      Count = NA,
      Percentage = NA,
    )

    openxlsx::addWorksheet(wb,"Codes")
    openxlsx::writeData(wb,"Codes", codes_sheet, withFilter = FALSE)

    openxlsx::addStyle(wb,"Codes", header_style, rows = 1, cols = 1:ncol(codes_sheet), gridExpand = TRUE)
    for (i in 1:nrow(codes_sheet)) {
      style <- if (i %% 2 == 0) even_row_style else odd_row_style
      openxlsx::addStyle(wb,"Codes", style, rows = i + 1, cols = 1:ncol(codes_sheet), gridExpand = TRUE)
      openxlsx::addStyle(wb,"Codes", vert_center_style, i + 1, cols = 1:ncol(codes_sheet), gridExpand = TRUE, stack = TRUE)
    }

    openxlsx::setColWidths(wb,"Codes", cols = 2, widths = 50)
    openxlsx::setColWidths(wb,"Codes", cols = 3, widths = 80)

    # Bin lookup formula (written to Excel as text)
    openxlsx::writeData(
      wb, "Coding Workbook",
      x = paste0(
        '=IF($C2="", "", TEXTJOIN("; ", , MAP(TEXTSPLIT($C2, ","), LAMBDA(code,',
        ' IF(TRIM(code)="", "", TRIM(IFERROR(',
        ' XLOOKUP(TRIM(code)+0, Codes!$A$2:$A$31, Codes!$B$2:$B$31),',
        ' "CODE " & TRIM(code) & " DOES NOT EXIST")))))))'
      ),
      startCol = 4, startRow = 2
    )

    openxlsx::conditionalFormatting(
      wb, "Coding Workbook", cols = 4, rows = 2:(nrow(coding_sheet) + 1),
      rule = "DOES NOT EXIST", type = "contains",
      style = openxlsx::createStyle(fontColour = "#FF0000")
    )

    # Count & percentage formulas
    count_formula <- paste0(
      'IF($A', 2:(nrow(codes_sheet) + 1), '="", "", ',
      'SUMPRODUCT(--(ISNUMBER(SEARCH("," & $A', 2:(nrow(codes_sheet) + 1),
      ' & ",", "," & SUBSTITUTE(\'Coding Workbook\'!$C$2:$C$3000, " ", "") & ",")))))'
    )

    perc_formula <- paste0(
      'IF($A', 2:(nrow(codes_sheet) + 1), '="", "", ',
      'SUMPRODUCT(--ISNUMBER(SEARCH("," & A', 2:(nrow(codes_sheet) + 1),
      ' & ",", "," & SUBSTITUTE(\'Coding Workbook\'!C$2:C$3000, " ", "") & ","))) / MAX(1, COUNTA(\'Coding Workbook\'!B$2:B$3000)))'
    )

    openxlsx::writeFormula(wb, "Codes", x = count_formula, startCol = 4, startRow = 2)
    openxlsx::writeFormula(wb, "Codes", x = perc_formula, startCol = 5, startRow = 2)

    openxlsx::addStyle(wb,"Codes", openxlsx::createStyle(numFmt = "0%"),
                       rows = 2:(nrow(codes_sheet) + 1), cols = 5, gridExpand = TRUE, stack = TRUE)


    # Save workbook(s)
    file_out <- file.path(path, paste0(q, " Coding Workbook.xlsx"))
    openxlsx::saveWorkbook(wb, file = file_out, overwrite = TRUE)
  }
}
