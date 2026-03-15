# **Apply `tab_mr()` to multiple multiple response sets**

This function maps
[`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md)
across all multiple response set question prefixes in a data frame.
Optionally, you can specify a `y` variable for crosstabs and/or exclude
prefixes from being tabulated. Additional arguments are passed to
[`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md).

Multiple response sets are detected automatically by prefixes before the
first underscore (`_`).

## Usage

``` r
tab_mr_all(data, y = NULL, weight = NULL, exclude = NULL, ...)
```

## Arguments

- data:

  A data frame containing the survey data.

- y:

  (Optional) A variable to cross-tabulate against a multiple response
  set. Defaults to `NULL`.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- exclude:

  Variables to exclude from tabulation. Defaults to `NULL`. Always
  excludes `y` (if specified), `weight` (if specified), and variable
  literally named "weight".

- ...:

  Additional arguments passed to
  [`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md).

## Value

A named list of tables or crosstabs, one per multiple response set
prefix.

## Details

This function assumes that the specified data frame only contains
multiple response sets Any non-multiple response set should be excluded
using the `exclude` argument.

## Examples

``` r
# Unweighted proportions tables of all multiple response sets
tab_mr_all(data = survey_data)
#> Error: These packages are required but not installed: dplyr

# Weighted proportions crosstabs of all multiple response sets by region, excluding Q99
tab_mr_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#> Error: These packages are required but not installed: dplyr
```
