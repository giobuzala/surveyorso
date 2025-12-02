#' **Copy to clipboard**
#'
#' @description
#' This function copies a data frame, matrix, or vector to the system clipboard as a tab-separated table (Windows only).
#'
#' @param x Object to copy.
#' @param row.names Include row names. Defaults to `FALSE`.
#' @param col.names Include column names. Defaults to `TRUE`.
#'
#' @examples
#' # Copy a data frame to clipboard
#' copy2clip(data)
#'
#' @export

copy2clip <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  # Check for Windows OS ----

  if (.Platform$OS.type != "windows") {stop("copy2clip() currently only works on Windows systems.", call. = FALSE)}

  # Function ----

  utils::write.table(x, "clipboard-16384", sep = "\t", row.names = row.names, col.names = col.names, na = "", ...)

}
