# **Compute a correlation matrix**

This function computes a correlation matrix across all numeric variables
in a dataset.

## Usage

``` r
cor_matrix(
  data,
  weight = NULL,
  id_var,
  method = "pearson",
  round = 3,
  half = TRUE,
  min = NULL
)
```

## Arguments

- data:

  A data frame containing variables to correlate.

- weight:

  (Optional) Name of the weight variable. If `NULL`, computes unweighted
  correlations.

- id_var:

  ID column name to exclude.

- method:

  Correlation method (`"pearson"`, `"spearman"`, or `"kendall"`).

- round:

  Number of decimal places to round correlations to.

- half:

  Logical; if `TRUE` (default), displays only the lower triangle. If
  `FALSE`, displays the full matrix.

- min:

  (Optional) Numeric threshold. If specified, correlations with \|r\| \<
  min are replaced with NA.

## Value

A correlation matrix, as a data frame.

## Details

Only numeric variables are allowed. Non-numeric variables (e.g.,
factors, characters, or dates) will trigger an error.

## Examples

``` r
# Weighted Pearson (default) correlations, lower half of the matrix shown
cor_matrix(data = data_clust, weight = weight_var)
#> Error: These packages are required but not installed: wCorr

# Unweighted Spearman correlations, full matrix shown, with minimum threshold of 0.3
cor_matrix(data = data_clust, method = "spearman", half = FALSE, min = 0.3)
#> Error: These packages are required but not installed: wCorr
```
