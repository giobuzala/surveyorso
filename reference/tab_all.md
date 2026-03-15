# **Apply `tab()` to multiple variables**

This function maps
[`tab()`](https://giobuzala.github.io/surveyorso/reference/tab.md)
across all variables in a data frame. Optionally, you can specify a `y`
variable for crosstabs and/or exclude variables from being tabulated.
Additional arguments are passed to
[`tab()`](https://giobuzala.github.io/surveyorso/reference/tab.md).

## Usage

``` r
tab_all(data, y = NULL, weight = NULL, exclude = NULL, ...)
```

## Arguments

- data:

  A data frame containing the survey data.

- y:

  (Optional) A variable to cross-tabulate against all `x` variables.

- exclude:

  Variables to exclude from tabulation. Defaults to `NULL`. Always
  excludes `y` (if specified), `weight` (if specified), and variable
  literally named "weight".

- ...:

  Additional arguments passed to
  [`tab()`](https://giobuzala.github.io/surveyorso/reference/tab.md).

## Value

A named list of tables or crosstabs, one per variable.

## Examples

``` r
# Unweighted proportions tables of all questions
tab_all(data = survey_data)
#> Error: These packages are required but not installed: dplyr

# Weighted proportions crosstabs of all questions by region, excluding Q99
tab_all(data = survey_data, y = region, weight = weight_var, exclude = "Q99")
#> Error: These packages are required but not installed: dplyr
```
