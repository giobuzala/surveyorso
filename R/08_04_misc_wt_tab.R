#' **Create a weighted frequency or proportion table**
#'
#' @description
#' This function outputs weighted counts or weighted proportions for a vector.
#'
#' @param x A vector (typically categorical or factor).
#' @param weight A numeric vector of weights.
#' @param prop Logical; if `TRUE` (default), return weighted proportions; otherwise return weighted counts.
#'
#' @return
#' A named numeric vector with weighted proportions or counts for each level/value.
#'
#' @examples
#' # Weighted distribution of Q1, showing percentages
#' wt_tab(x = data$Q1, weight = data$weight, prop = TRUE)
#'
#' @export

wt_tab <- function(x, weight, prop = TRUE) {
  # Function ----
  if (prop) {
    # Weighted proportions
    vec <- weights::wpct(x, weight = weight)
  } else {
    # Weighted counts
    temp <- Hmisc::wtd.table(x, weights = weight)
    vec <- temp$sum.of.weights
    names(vec) <- temp$x
  }

  # Ensure all factor levels are represented
  if ("factor" %in% class(x)) {
    lvls <- levels(x)
    result <- purrr::map_dbl(lvls, function(l, vals = vec) {
      if (l %in% names(vals)) vals[l] else 0
    })
    names(result) <- lvls
  } else {
    result <- vec
  }

  return(result)
}
