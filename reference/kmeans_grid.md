# **Run a grid of k-means clustering models**

This function tests multiple k-means clustering solutions, optionally
after applying principal component analysis (PCA) for dimensionality
reduction.

- Models are always run on the raw standardized data (no PCA).

- If `pc_range` is provided, additional models are run using the first N
  principal components.

- Results include cluster sizes, silhouette statistics, and
  within-cluster sum of squares (WSS).

- Use `mean_sil` and `total_wss` to identify optimal cluster
  configurations.

## Usage

``` r
kmeans_grid(data, pc_range = NULL, k_range = 2:6, id_var = NULL)
```

## Arguments

- data:

  A data frame of numeric variables to cluster. Non-numeric columns are
  dropped.

- pc_range:

  Optional numeric vector of principal component counts. If `NULL`, no
  PCA is applied.

- k_range:

  Numeric vector of cluster counts to test. Defaults to `2:6`.

- id_var:

  ID variable to drop before clustering.

## Value

A data frame summarizing results for each combination of PCs and
clusters.

## Details

The function automatically excludes the variable named "weight" from the
clustering input.

## Examples

``` r
# K-means runs (2-5 k-range) with and without PCA (2-4 PCs range)
kmeans_results <- kmeans_grid(data = data_clust, pc_range = 2:4, k_range = 2:5)
#> Error: These packages are required but not installed: dplyr
```
