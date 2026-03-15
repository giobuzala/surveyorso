# **Create a full distribution table for a grid question**

This function produces a table for grid survey questions, showing the
full distribution across all response levels.

- If only `x` is specified, the output is a full-distribution table for
  each item in the grid question.

- Use the `weight` argument to produce weighted tables; if omitted,
  results are unweighted.

- Set `prop = TRUE` (default) to output proportions, or `prop = FALSE`
  to output frequencies.

- Use the `sort` argument (`asc` or `desc`) to order rows based on
  top/bottom box scores defined by `top_bottom`. Set `sort = NULL` for
  no sorting.

- Use the `top_bottom` argument (e.g., `T2`, `B3`) to specify how many
  top or bottom levels to use when sorting grid items.

- Use the `round` argument to control decimal precision in proportions
  tables.

- If `numeric = TRUE`, the function removes the base column and coerces
  values to numeric, making the output math-ready.

## Usage

``` r
tab_grid(
  data,
  x,
  weight = NULL,
  prop = TRUE,
  sort = NULL,
  top_bottom = T2,
  round = 3,
  numeric = FALSE
)
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  A variable prefix identifying the grid question (e.g., `Q3` for
  `Q3_1`, `Q3_2`, ...).

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- prop:

  Logical; if `TRUE` (default), outputs proportions. If `FALSE`, outputs
  frequencies.

- sort:

  Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting).
  Defaults to `NULL`.

- top_bottom:

  A string of the form `TN` or `BN` indicating how many top or bottom
  levels should be used for sorting rows.

- round:

  Integer; number of decimal places for proportions. Defaults to `3`.

- numeric:

  Logical; if `TRUE`, returns a data frame with numeric columns, with
  the n column removed. Defaults to `FALSE`.

## Value

A table showing the full distribution across levels, as a data frame..

## Examples

``` r
# Weighted proportions table of a grid question, sorted descending by T2
tab_grid(data = survey_data, x = Q3, weight = weight_var, sort = desc, top_bottom = T2)
#> Error: These packages are required but not installed: dplyr, crayon
```
