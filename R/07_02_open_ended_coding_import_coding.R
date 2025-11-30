#' **Import a coding workbook and merge coded variable into the dataset**
#'
#' @description
#' This function imports a coded Excel workbook for a specific open-ended question and merges processed coded variables back into the dataset.
#'
#' @param x A variable to be imported (e.g., Q5).
#' @param path Directory containing the workbook. Defaults to "Data".
#' @param data A data frame containing at least the id_var and original question column. Defaults to dat.
#' @param id_var ID column name in the dataset. Defaults to "Vrid".
#'
#' @details
#' This function handles one question at a time.
#'
#' @return
#' A data frame merged with new coded columns. If unknown codes are detected, the function prints a table of respondent IDs and problematic codes and then stops with an error.
#'
#' @examples
#' # Import coding workbook for variable Q5 and merge the results into the dataset
#' data <- import_coding(data = survey_data, x = Q5, path = "Data", id_var = Vrid)

import_coding <- function(data, x, path = "Data", id_var = Vrid) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "tidyr", "stringr", "openxlsx")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  # Capture variable expressions
  x_name <- rlang::as_name(rlang::enquo(x))
  id_var_name <- rlang::as_name(rlang::enquo(id_var))

  # Build file path
  file_in <- file.path(path, paste0(x_name, " Coding Workbook.xlsx"))

  # Error if file not found
  if (!file.exists(file_in)) {
    stop(paste0(x_name, " Coding Workbook.xlsx doesn't exist in the folder ", path, "."), call. = FALSE)
  }

  # Error if id_var not found
  if (!(id_var_name %in% names(data))) {
    stop("ID variable ", id_var_name, " does not exist in the coding workbook.", call. = FALSE)
  }

  # Load and clean coding sheet
  coded <- openxlsx::read.xlsx(file_in, sheet = paste0(x_name, " Coding Workbook")) %>%
    dplyr::rename(Code = `Code(s)`) %>%
    dplyr::mutate(
      Code = Code %>%
        stringr::str_trim() %>%                      # Remove leading/trailing whitespace
        stringr::str_remove("^,|,$") %>%             # Remove leading/trailing commas
        stringr::str_replace_all(" ", "") %>%        # Remove all spaces
        stringr::str_replace_all("(,{2,})", ",") %>% # Replace multiple commas with a single comma
        stringr::str_replace_all("\\.", ",")         # Replace periods with commas
    )

  # Load key for code-to-bin mapping
  key <- openxlsx::read.xlsx(file_in, sheet = paste0(x_name, " Codes")) %>%
    dplyr::select(Code, Bin)

  # Process coded variables ----

  # Reshape coded data to long format
  long <- coded %>%
    dplyr::select(-`Bin(s)`) %>%
    tidyr::separate_rows(Code, sep = ",") %>%
    dplyr::filter(Code != "") %>%
    dplyr::mutate(Code = suppressWarnings(as.numeric(Code))) %>%
    dplyr::left_join(key, "Code")

  # Check for any codes not in key
  bad_codes <- long %>%
    dplyr::filter(is.na(Bin)) %>%
    dplyr::select(dplyr::all_of(id_var_name), Code) %>%
    dplyr::arrange(!!rlang::sym(id_var_name), Code)

  if (nrow(bad_codes) > 0) {
    bad_codes_df <- as.data.frame(bad_codes)

    print(bad_codes_df, row.names = FALSE)

    stop("The codes above were not found in the key for ", x_name, ".", call. = FALSE)
  }

  # All possible codes from key
  all_codes <- sort(unique(key$Code))

  # Remove duplicate codes per respondent
  long <- long %>%
    dplyr::distinct(!!rlang::sym(id_var_name), Code, .keep_all = TRUE)

  # Pivot to wide format with Unchecked/Checked flags
  wide <- long %>%
    dplyr::mutate(value = "Checked") %>%
    tidyr::pivot_wider(
      id_cols = c(!!rlang::sym(id_var_name)),
      names_from = Code,
      values_from = value,
      values_fill = "Unchecked",
      names_sort = TRUE
    )

  # Ensure all codes from key are present as columns
  missing_cols <- setdiff(as.character(all_codes), names(wide))
  for (col in missing_cols) {
    wide[[col]] <- "Unchecked"
  }

  # Reorder columns
  code_cols <- setdiff(colnames(wide), id_var_name) %>%
    as.numeric() %>%
    sort(na.last = TRUE)

  wide <- wide %>%
    dplyr::select(dplyr::all_of(c(id_var_name, as.character(code_cols))))

  # Rename wide columns for clarity
  wide <- wide %>%
    dplyr::rename_with(~ stringr::str_c("coded_", x_name, "_", .x), -c(!!rlang::sym(id_var_name)))

  # Convert values to factors and add labels
  for (col in setdiff(names(wide), c(id_var_name, "row_id"))) {
    wide[[col]] <- factor(wide[[col]], levels = c("Unchecked", "Checked"))
    attr(wide[[col]], "label") <- stringr::str_c(
      key$Bin[match(stringr::str_remove(col, paste0("coded_", x_name, "_")), key$Code)],
      ": ", attr(data[[x_name]], "label")
    )
  }

  # Merge coded variables ----

  data <- data %>%
    dplyr::left_join(wide, by = id_var_name) %>%
    dplyr::relocate(dplyr::starts_with(paste0("coded_", x_name, "_")), .after = all_of(x_name))

  invisible(data)
}
