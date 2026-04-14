# **Standardize numeric and factor variables (z-scores)**

This function standardizes variables to have mean of 0 and standard
deviation of 1.

## Usage

``` r
standardize(
  data,
  vars = everything(),
  flip = FALSE,
  keep_all = TRUE,
  outside_scale = c("Don't know", "Prefer not to say")
)
```

## Arguments

- data:

  A data frame containing variables to standardize.

- vars:

  Variables to standardize. Supports tidyselect syntax (e.g.,
  `starts_with()`, `c(var1, var2)`).

- flip:

  Logical; if `TRUE`, reverses the direction of valid response levels
  before standardization.

- keep_all:

  Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only
  standardized variables.

- outside_scale:

  Character vector of response levels (typically DK/PNTS) that lie
  outside of the scale and should be treated as neutral midpoint
  (`0.5`).

## Value

A data frame with standardized variables.

- If `keep_all = TRUE`, the full dataset is returned with standardized
  variables.

- If `keep_all = FALSE`, only standardized variables are returned.

## Details

- Numeric variables are z-scored.

- Factor variables are converted to numeric order and then standardized.

- Levels in `outside_scale` (e.g., `Don't know`) are assigned to the
  midpoint of the valid range before transformation.

## Examples

``` r
# Standardize Q1 variable set with
standardize(data = data, vars = starts_with("Q1_"))
#> Error: These packages are required but not installed: dplyr, tidyselect

# Standardize Q1 and Q2 variable set with the scale order
standardize(data = data, vars = c(Q1, Q2), flip = TRUE)
#> Error: These packages are required but not installed: dplyr, tidyselect
```
