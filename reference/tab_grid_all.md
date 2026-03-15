# **Apply `tab_grid()` to multiple grid questions**

This function maps
[`tab_grid()`](https://giobuzala.github.io/surveyorso/reference/tab_grid.md)
across all grid question prefixes in a data frame. Additional arguments
are passed to
[`tab_grid()`](https://giobuzala.github.io/surveyorso/reference/tab_grid.md).

- Grid questions are detected automatically by prefixes before the first
  underscore (`_`).

- Multiple response (pick-any) sets are skipped, since those should be
  handled with
  [`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md).

## Usage

``` r
tab_grid_all(data, weight = NULL, exclude = NULL, ...)
```

## Arguments

- data:

  A data frame containing the survey data.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- exclude:

  Variables or prefixes to exclude from tabulation. Defaults to `NULL`.
  Always excludes `weight` (if specified) and any variable literally
  named "weight".

- ...:

  Additional arguments passed to
  [`tab_grid()`](https://giobuzala.github.io/surveyorso/reference/tab_grid.md).

## Value

A named list of tables, one per grid question's prefix.

## Details

This function assumes that the specified data frame only contains grid
questions. Any non-grid question should be excluded using the `exclude`
argument.

## Examples

``` r
# Unweighted proportions tables of all grid questions
tab_grid_all(data = survey_data)
#> Error: These packages are required but not installed: dplyr

# Weighted proportions tables of all grid questions, excluding Q99
tab_grid_all(data = survey_data, weight = weight_var, exclude = "Q99")
#> Error: These packages are required but not installed: dplyr
```
