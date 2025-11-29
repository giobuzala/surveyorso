#' **Run multiple k-means clustering models**
#'
#' @description
#' This function tests multiple k-means clustering solutions, optionally after applying principal component analysis (PCA) for dimensionality reduction.
#'
#' - Models are always run on the raw standardized data (no PCA).
#' - If `pc_range` is provided, additional models are run using the first N principal components.
#' - Results include cluster sizes, silhouette statistics, and within-cluster sum of squares (WSS).
#' - Use `mean_sil` and `total_wss` to identify optimal cluster configurations.
#'
#' @param data A data frame of numeric variables to cluster. Non-numeric columns are dropped.
#' @param pc_range Optional numeric vector of principal component counts. If `NULL`, no PCA is applied.
#' @param k_range Numeric vector of cluster counts to test. Defaults to `2:6`.
#' @param id_var ID variable to drop before clustering. Defaults to `Vrid`.
#'
#' @details
#' The function automatically excludes the variable named "weight" from the clustering input.
#'
#' @return
#' A data frame summarizing results for each combination of PCs and clusters.
#'
#' @examples
#' # K-means runs (2-5 k-range) with and without PCA (2-4 PCs range)
#' kmeans_results <- kmeans_multi(data = data_clust, pc_range = 2:4, k_range = 2:5)
#'
#' @export

kmeans_multi <- function(data, pc_range = NULL, k_range = 2:6, id_var = Vrid) {
  # Check required packages ----

  required_pkgs <- c("rlang", "dplyr", "cluster")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  # Set up ----

  # Error checks
  if (!is.null(pc_range)) {
    if (!is.numeric(pc_range) || any(pc_range <= 0)) {
      stop("pc_range must contain positive numeric values.", call. = FALSE)
    }
  }
  if (!is.numeric(k_range) || any(k_range <= 0)) {
    stop("k_range must contain positive numeric values.", call. = FALSE)
  }

  # Drop ID and weight variables if present
  id_var <- rlang::as_name(rlang::enquo(id_var))
  drop_cols <- intersect(c(id_var, "weight"), names(data))
  if (length(drop_cols) > 0) {
    data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  }

  # Keep only numeric columns
  data <- data[, sapply(data, is.numeric), drop = FALSE]
  if (ncol(data) == 0) stop("No numeric columns available for clustering.", call. = FALSE)

  # PCA (optional) ----

  if (!is.null(pc_range)) {
    pca <- stats::prcomp(data, scale. = TRUE)
    pca_data <- as.data.frame(pca$x)

    # Ensure pc_range does not exceed available PCs
    max_pcs <- ncol(pca_data)
    if (any(pc_range > max_pcs)) {
      warning(
        "Requested PCs exceed available components (", max_pcs, "). Truncating pc_range.",
        call. = FALSE
      )
      pc_range <- pc_range[pc_range <= max_pcs]
    }
  }

  # Initialize results ----

  n.rows <- length(k_range) * (if (is.null(pc_range)) 1 else (1 + length(pc_range)))
  col_names <- c(
    "n_PCs", "n_cluster",
    paste0("cluster_", 1:max(k_range), ".cnt"),
    paste0("cluster_", 1:max(k_range), ".avg_sil"),
    paste0("cluster_", 1:max(k_range), ".wss"),
    "min_sil", "median_sil", "mean_sil", "max_sil",
    "total_wss"
  )
  result <- as.data.frame(matrix(NA, nrow = n.rows, ncol = length(col_names)))
  colnames(result) <- col_names

  # Prepare datasets ----

  dat_list <- list(raw = as.matrix(data))
  pc_labels <- numeric(0)

  if (!is.null(pc_range)) {
    for (pc in pc_range) {
      dat_list[[paste0("PC", pc)]] <- as.matrix(pca_data[, 1:pc, drop = FALSE])
      pc_labels <- c(pc_labels, pc)
    }
  }

  # Run models ----

  count <- 1
  for (i in seq_along(dat_list)) {
    dat_input <- dat_list[[i]]
    pc_label <- if (i == 1) NA else pc_labels[i - 1]

    for (k in k_range) {
      set.seed(123)
      km <- suppressWarnings(stats::kmeans(dat_input, centers = k, nstart = 50, iter.max = 500))

      sil <- summary(cluster::silhouette(km$cluster, dist(dat_input)))

      result$n_PCs[count] <- pc_label
      result$n_cluster[count] <- k

      # Cluster counts
      ind_cnt <- which(colnames(result) == "cluster_1.cnt")
      result[count, ind_cnt:(ind_cnt + k - 1)] <- as.numeric(sil$clus.sizes)

      # Average silhouette per cluster
      ind_sil <- which(colnames(result) == "cluster_1.avg_sil")
      result[count, ind_sil:(ind_sil + k - 1)] <- round(as.numeric(sil$clus.avg.widths), 3)

      # Within-cluster sum of squares
      ind_wss <- which(colnames(result) == "cluster_1.wss")
      result[count, ind_wss:(ind_wss + k - 1)] <- round(as.numeric(km$withinss), 0)

      # Silhouette summary
      result$min_sil[count] <- round(as.numeric(sil$si.summary["Min."]), 3)
      result$median_sil[count] <- round(as.numeric(sil$si.summary["Median"]), 3)
      result$mean_sil[count] <- round(as.numeric(sil$si.summary["Mean"]), 3)
      result$max_sil[count] <- round(as.numeric(sil$si.summary["Max."]), 3)

      # Total WSS
      result$total_wss[count] <- round(sum(km$withinss), 0)

      count <- count + 1
    }
  }

  # Output ----

  result <- dplyr::mutate(
    result,
    dplyr::across(
      dplyr::any_of(c(
        "n_PCs", "n_cluster",
        "min_sil", "median_sil", "mean_sil", "max_sil",
        "total_wss"
      )),
      as.numeric
    )
  )

  return(result)
}
