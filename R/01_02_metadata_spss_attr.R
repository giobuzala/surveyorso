#' **Applies SPSS attributes to a variable**
#'
#' @description
#' This function inspects a vector and assigns SPSS-style attributes based on its data type (factor, character, numeric, or integer).
#' These attributes are used to preserve variable metadata such as labels, formats, and display widths when exporting data to SPSS-compatible files.
#'
#' @param x A vector to which SPSS-style attributes will be applied. Must be one of the following types: factor, character, numeric, or integer.
#' @param label A character string specifying the variable label to attach.
#'
#' @return
#' A named list of SPSS-compatible attributes appropriate for the variable type.
#'
#' @examples
#' # Apply SPSS-style label to Q1
#' attributes(data$Q1) <- spss_attr(data$Q1, label = "Question 1")
#'
#' @export

spss_attr <- function(x, label) {
  # Function ----

  x_class <- class(x)

  if ("factor" %in% x_class) {
    # Factor
    result <- list(
      levels = levels(x),
      class = x_class,
      label = label,
      format.spss = "F5.0",
      display_width = 0
    )

  } else if (x_class[1] %in% c("numeric", "integer")) {
    # Numeric / Integer
    result <- list(
      label = label,
      format.spss  = "F8.2",
      display_width = 0
    )

  } else if ("character" %in% x_class) {
    # Character
    result <- list(
      label = label,
      format.spss = "A100",
      display_width = 0
    )

  } else {
    stop("The provided variable is not factor, numeric, integer, or character.",
         call. = FALSE)
  }

  return(result)
}
