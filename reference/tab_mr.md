# **Create a table or crosstab for a multiple response (pick-any) set**

This function produces a table or a crosstab for multiple response
(pick-any) set questions.

- If only `x` is specified, the output is a table of the items in the
  multiple response set.

- If both `x` and `y` are specified, the output is a crosstab of the
  multiple response set by `y`.

- Use the `weight` argument to produce weighted tables; if omitted,
  results are unweighted.

- Set `prop = TRUE` (default) to output proportions, or `prop = FALSE`
  to output frequencies

- Use the `total` argument to add a total column when `y` is specified.
  Ignored for one-way tables.

- Use the `sort` argument (`desc` or `asc`) to order rows. Set
  `sort = NULL` for no sorting.

- Use the `round` argument to control decimal precision in percentage
  tables.

- If `numeric = TRUE`, the function removes the base row and coerces
  values to numeric, making the output math-ready.

Supported coding schemes for multiple response items include:

- `Unchecked` / `Checked`

- `Not Selected` / `Selected`

- `0` / `1`

## Usage

``` r
tab_mr(
  data,
  x,
  y = NULL,
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

  A variable prefix identifying the multiple response set (e.g., `Q2`
  for `Q2_1`, `Q2_2`, ...).

- y:

  (Optional) A variable to cross-tabulate against the multiple response
  set. Defaults to `NULL`.

- weight:

  (Optional) A numeric weighting variable. If `NULL` (default), results
  are unweighted.

- prop:

  Logical; if `TRUE` (default), outputs proportions. If `FALSE`, outputs
  frequencies.

- total:

  Logical; if `TRUE` (default), adds a total column when `y` is
  specified. Ignored for single-variable tables.

- sort:

  Sorting order for rows: `desc`, `asc`, or `NULL` (no sorting).
  Defaults to `NULL`.

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
# Unweighted proportions table of a multiple response set
tab_mr(data = survey_data, x = Q2)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, haven

# Weighted frequency crosstab of Q2 by gender, showing total column
tab_mr(data = survey_data, x = Q2, y = gender, weight = weight_var, prop = FALSE, total = TRUE)
#> Error: These packages are required but not installed: dplyr, tidyr, stringr, haven
```
