#' **Merge two survey waves**
#'
#' @description
#' This function merges a new survey wave into an old one while preserving variable labels and allowing flexible control over which variables are retained.
#'
#' It provides two options for which variables to retain:
#' - `"New and shared old"` (default): keeps all variables from the new dataset, plus shared variables from the old dataset, and any additional old-only variables specified in `keep_old`.
#' - `"All"`: keeps all variables from both datasets.
#'
#' Variable labels are reapplied after merging:
#' - For variables present in the new dataset, labels are taken from `new_data`.
#' - For old-only variables (when included), labels are taken from `old_data`.
#'
#' @param new_data A data frame for the new wave.
#' @param old_data A data frame for the old wave.
#' @param vars Character string, either `"New and shared old"` (default) or `"All"`.
#' @param keep_old Optional character vector of old-only variables to keep when `vars = "New and shared old"`.
#'
#' @return
#' A merged data frame with harmonized variable labels.
#'
#' @examples
#' # Merge new data and shared variables from old data
#' merged_data <- merge_waves(new_data = new_data, old_data = old_data, vars = "New and shared old")
#'
#' @export

merge_waves <- function(new_data, old_data, vars = c("New and shared old", "All"), keep_old = NULL) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "purrr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  `%>%` <- dplyr::`%>%`

  vars <- match.arg(vars)

  # Shared and unmatched variables
  shared_vars <- intersect(names(new_data), names(old_data))
  new_only <- setdiff(names(new_data), names(old_data))
  old_only <- setdiff(names(old_data), names(new_data))

  # Decide which variables to keep
  keep_vars <- switch(
    vars,
    "New and shared old" = c(
      names(new_data),
      shared_vars,
      intersect(old_only, rlang::`%||%`(keep_old, character(0)))
    ),
    "All" = union(names(new_data), names(old_data))
  )

  # Merge ----

  # Bind rows
  data <- dplyr::bind_rows(new_data, old_data)

  # Subset
  data <- data %>% dplyr::select(dplyr::all_of(keep_vars))

  # Apply labels ----

  # Reapply labels from new dataset
  new_keep <- intersect(names(data), names(new_data))
  if (length(new_keep) > 0) {
    data[, new_keep] <- purrr::map2_dfc(data[, new_keep], new_data[, new_keep], function(x, v) {
      attr(x, "label") <- attr(v, "label")
      x
    })
  }

  # Reapply labels from old dataset (for old-only vars included via "All" or keep_old)
  old_keep <- setdiff(intersect(names(data), names(old_data)), names(new_data))
  if (length(old_keep) > 0) {
    data[, old_keep] <- purrr::map2_dfc(data[, old_keep], old_data[, old_keep], function(x, v) {
      attr(x, "label") <- attr(v, "label")
      x
    })
  }

  # Output ----

  return(data)
}
