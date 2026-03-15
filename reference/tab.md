# **Create a table or crosstab**

This function produces a table (for a single variable) or a crosstab
(for two variables).

- If only `x` is specified, the output is a table of `x`.

- If both `x` and `y` are specified, the output is a crosstab of `x` by
  `y`.

- Use the `weight` argument to produce weighted tables; if omitted,
  results are unweighted.

- Set `prop = TRUE` (default) to output proportions, or `prop = FALSE`
  to output frequencies.

- Use the `round` argument to control decimal precision in proportions
  tables.

- If `numeric = TRUE`, the function removes the total row and coerces
  all values to numeric, making the output math-ready (e.g., can be
  multiplied or added directly).

## Usage

``` r
tab(
  data,
  x,
  y = NULL,
  weight = NULL,
  prop = TRUE,
  total = TRUE,
  round = 3,
  numeric = FALSE
)
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  The variable for which to compute the table (or the row variable in a
  crosstab).

- y:

  (Optional) The variable to cross-tabulate against `x`. Defaults to
  `NULL`.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- prop:

  Logical; if `TRUE` (default), outputs proportions If `FALSE`, outputs
  frequencies.

- total:

  Logical; if `TRUE` (default), adds a total column when `y` is
  specified. Ignored for single-variable tables.

- round:

  Integer; number of decimal places for proportions. Defaults to `3`.

- numeric:

  Logical; if `TRUE`, returns a data frame with numeric columns, with
  the n row removed. Defaults to `FALSE`.

## Value

A table (if `y = NULL`) or a crosstab (if `y` is specified), as a data
frame.

## Examples

``` r
# Weighted proportions table of Q1
tab(data = survey_data, x = Q1, weight = weight_var)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, crayon

# Weighted frequency crosstab of Q1 by region
tab(data = survey_data, x = Q1, y = region, weight = weight_var, prop = FALSE)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, crayon
```
