# **Apply `tab_grid_t()` to multiple grid questions**

This function maps
[`tab_grid_t()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_t.md)
across all grid question prefixes in a data frame. Optionally, you can
specify a `y` variable for crosstabs and/or exclude prefixes from being
tabulated. Additional arguments are passed to
[`tab_grid_t()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_t.md).

- Grid questions are detected automatically by prefixes before the first
  underscore (`_`).

- Multiple response (pick-any) sets are skipped, since those should be
  handled with
  [`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md).

## Usage

``` r
tab_grid_t_all(data, y = NULL, weight = NULL, exclude = NULL, ...)
```

## Arguments

- data:

  A data frame containing the survey data.

- y:

  (Optional) A variable to cross-tabulate against a grid set. Defaults
  to `NULL`.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- exclude:

  Variables to exclude from tabulation. Defaults to `NULL`. Always
  excludes `y` (if specified), `weight` (if specified), and variable
  literally named "weight".

- ...:

  Additional arguments passed to
  [`tab_grid_t()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_t.md).

## Value

A named list of tables or crosstabs, one per grid question prefix.

## Details

This function assumes that the specified data frame only contains grid
questions. Any non-grid question should be excluded using the `exclude`
argument.

## Examples

``` r
# Unweighted proportions tables of all grid questions
tab_grid_t_all(data = survey_data)
#> Error: These packages are required but not installed: dplyr

# Weighted proportions crosstabs of all grid questions by region, excluding Q99
tab_grid_t_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#> Error: These packages are required but not installed: dplyr
```
