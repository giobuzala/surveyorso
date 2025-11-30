#' **Process `code_gpt()` output to the dataset**
#'
#' @description
#' This function takes a `code_gpt()` result table and merges binary Unchecked/Checked code variables back into the original survey dataset, preserving labels and metadata.
#'
#' @param coded_tbl A dataset returned by `code_gpt()` with coding results.
#'
#' @details
#' Use this function immediately after `code_gpt()` in a pipeline, and save the result as a dataset.
#'
#' For example:
#'
#' \preformatted{
#' data <- code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
#'   code_gpt_to_df()
#' }
#'
#' This will append binary `coded_` variables for each code in the list.
#'
#' @return
#' The original dataset specified in `code_gpt()` with factor-coded variables (levels: `Unchecked`, `Checked`) appended for each code.
#'
#' @examples
#' # Code Q5 and add to the dataset
#' data <- code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
#'   code_gpt_to_df()
#'
#' @export

code_gpt_to_df <- function(coded_tbl) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "tidyr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Pull metadata
  data <- attr(coded_tbl, "data")
  x_name <- attr(coded_tbl, "x_name")
  id_name <- attr(coded_tbl, "id_name")
  theme_list <- attr(coded_tbl, "theme_list")
  q_label <- attr(coded_tbl, "q_label")

  # Process ----

  # Reshape coded data to long format
  long <- coded_tbl %>%
    dplyr::select(dplyr::all_of(id_name), `Code(s)`) %>%
    dplyr::filter(!is.na(`Code(s)`) & `Code(s)` != "") %>%
    tidyr::separate_rows(`Code(s)`, sep = ",") %>%
    dplyr::mutate(Code = suppressWarnings(as.numeric(stringr::str_trim(`Code(s)`)))) %>%
    dplyr::left_join(theme_list, by = "Code")

  # Pivot to wide format with Unchecked/Checked flags
  wide <- long %>%
    dplyr::mutate(value = "Checked") %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_name),
      names_from = Code,
      values_from = value,
      values_fill = "Unchecked"
    )

  # Ensure all codes exist
  all_codes <- as.character(sort(theme_list$Code))
  missing_codes <- setdiff(all_codes, names(wide))
  for (c in missing_codes) wide[[c]] <- "Unchecked"

  # Rename, convert to factor, add labels
  wide <- wide %>%
    dplyr::select(dplyr::all_of(id_name), dplyr::all_of(all_codes)) %>%
    dplyr::rename_with(~ stringr::str_c("coded_", x_name, "_", .x), -dplyr::all_of(id_name))

  for (col in setdiff(names(wide), id_name)) {
    wide[[col]] <- factor(wide[[col]], levels = c("Unchecked", "Checked"))
    code_num <- as.numeric(stringr::str_remove(col, paste0("coded_", x_name, "_")))
    bin_label <- theme_list$Bin[match(code_num, theme_list$Code)]
    attr(wide[[col]], "label") <- stringr::str_c(bin_label, ": ", q_label)
  }

  # Merge ----

  result <- data %>%
    dplyr::left_join(wide, by = id_name) %>%
    dplyr::relocate(dplyr::starts_with(paste0("coded_", x_name, "_")), .after = dplyr::all_of(x_name))

  invisible(result)
}
