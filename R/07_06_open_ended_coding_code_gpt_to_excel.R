#' **Write `code_gpt()` results to Excel workbook**
#'
#' @description
#' This function inserts the coded responses (`Code(s)` column) from a `code_gpt()` result table into the corresponding Excel coding workbook created for that question.
#'
#' @param coded_tbl A dataset returned by `code_gpt()` with coding results.
#' @param path Directory containing the coding workbook. Defaults to `"Data"`.
#'
#' @details
#' - The function locates the workbook named `X Coding Workbook.xlsx`, matches coded responses by respondent ID, and writes the codes into the `Code(s)` of the coding sheet.
#' - `import_coding()` function can be used to import and process the coding workbook.
#'
#' Use this function immediately after `code_gpt()` in a pipeline to write the coded results directly to the corresponding Excel workbook.
#'
#' You do not need to save it as a dataset, as the workbook is updated in place.
#'
#' For example:
#'
#' \preformatted{
#' code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
#'   code_gpt_to_excel()
#' }
#'
#' This will update `Q5 Coding Workbook` in place with the coded responses, which can then be imported using `import_coding()`.
#'
#' @return
#' The Excel workbook is updated in place.
#'
#' @examples
#' # Update "Q5 Coding Workbook" in place with the coded responses
#' code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
#'   code_gpt_to_excel()
#'
#' @export

code_gpt_to_excel <- function(coded_tbl, path = "Data") {
  # Check required packages ----

  required_pkgs <- c("openxlsx")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  # Pull metadata
  x_name <- attr(coded_tbl, "x_name")
  id_name <- attr(coded_tbl, "id_name")

  # Build file path
  file_in <- file.path(path, paste0(x_name, " Coding Workbook.xlsx"))
  if (!file.exists(file_in)) {
    stop(file_in, " was not found in the specified folder.", call. = FALSE)
  }

  # Load workbook
  wb <- openxlsx::loadWorkbook(file_in)
  q_sheet <- openxlsx::read.xlsx(file_in,
                                 sheet = paste0(x_name, " Coding Workbook"),
                                 check.names = FALSE)

  # Process ----

  # Extract workbook IDs
  ids <- if (id_name %in% names(q_sheet)) q_sheet[[id_name]] else seq_len(nrow(q_sheet))

  # Match coding results to workbook order
  match_idx <- match(ids, coded_tbl[[id_name]])
  codes_to_write <- rep(NA_character_, length(ids))
  codes_to_write[!is.na(match_idx)] <- coded_tbl$`Code(s)`[match_idx[!is.na(match_idx)]]

  # Output ----

  # Write codes into column C
  openxlsx::writeData(wb,
                      sheet = paste0(x_name, " Coding Workbook"),
                      x = codes_to_write,
                      startCol = 3, startRow = 2,
                      colNames = FALSE, rowNames = FALSE,
                      withFilter = FALSE)

  # Save workbook
  openxlsx::saveWorkbook(wb, file_in, overwrite = TRUE)

  invisible(NULL)
}
