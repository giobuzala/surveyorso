# **Plot a correlation heatmap**

This function creates an interactive heatmap of a correlation matrix.
Hover labels display variable pairs and their correlation values.

## Usage

``` r
cor_heatmap(cor_mat, exclude = NULL)
```

## Arguments

- cor_mat:

  Correlation matrix or a data frame.

- exclude:

  (Optional) Variable(s) to exclude.

## Value

A plotly heatmap object.

## Details

The function accepts the output of
[`cor_matrix()`](https://giobuzala.github.io/surveyorso/reference/cor_matrix.md)
and optionally allows excluding selected variables from the
visualization.

## Examples

``` r
# Basic heatmap pipeline
cor_matrix(data = data) %>% cor_heatmap()
#> Error in cor_matrix(data = data) %>% cor_heatmap(): could not find function "%>%"
```
