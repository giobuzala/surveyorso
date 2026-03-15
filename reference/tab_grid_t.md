# **Create a top/bottom box table or crosstab for a grid question**

This function produces a top- or bottom-box summary for grid survey
questions.

- If only `x` is specified, the output is a top- or bottom-box table for
  each item in the grid question.

- If both `x` and `y` are specified, the output is a crosstab of the
  selected top/bottom box by `y`.

- Use the `top_bottom` argument (e.g., `T2`, `B3`) to define how many
  top or bottom response levels are included in the box.

- Use the `weight` argument to produce weighted tables; if omitted,
  results are unweighted.

- Set `prop = TRUE` (default) to output proportions, or `prop = FALSE`
  to output frequencies.

- Use the `total` argument to include a total column when a `y` variable
  is specified. Ignored for one-way tables.

- Use the `sort` argument (`asc` or `desc`) to order rows by their box
  score. Set `sort = NULL` for no sorting.

- Use the `round` argument to control decimal precision in proportions
  tables.

- If `numeric = TRUE`, the function removes the base row and coerces all
  values to numeric, making the output math-ready.

## Usage

``` r
tab_grid_t(
  data,
  x,
  y = NULL,
  top_bottom = T2,
  weight = NULL,
  prop = TRUE,
  total = TRUE,
  sort = NULL,
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

- y:

  (Optional) A variable name to cross-tabulate against the grid
  question. Defaults to `NULL`.

- top_bottom:

  A string of the form `TN` or `BN` indicating how many top or bottom
  levels to include in the box (e.g., `T2` = top 2). Defaults to `T2`.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- prop:

  Logical; if `TRUE` (default), outputs proportions. If `FALSE`, outputs
  frequencies.

- total:

  Logical; if `TRUE` (default), adds a total column when `y` is
  specified.

- sort:

  Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting).
  Defaults to `NULL`.

- round:

  Integer; number of decimal places for proportions. Defaults to `3`.

- numeric:

  Logical; if `TRUE`, returns a data frame with numeric columns, with
  the base n row removed. Defaults to `FALSE`.

## Value

A table (if `y = NULL`) or a crosstab (if `y` is specified), as a data
frame.

## Examples

``` r
# Unweighted proportions table of a grid question, showing top 2 box
tab_grid_t(data = survey_data, x = Q4, top_bottom = T2)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, crayon

# Weighted proportions crosstab of a grid question by region, showing bottom 2 box
tab_grid_t(data = survey_data, x = Q4, y = region, top_bottom = B2, weight = weight_var)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, crayon
```
