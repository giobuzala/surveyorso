# **Normalize numeric and factor variables to a 0–1 scale**

This function rescales variables to the 0 to 1 range. It supports
flipping of scales and preserves variable labels.

## Usage

``` r
normalize(
  data,
  vars = everything(),
  flip = FALSE,
  keep_all = TRUE,
  outside_scale = c("Don't know", "Prefer not to say")
)
```

## Arguments

- data:

  A data frame containing variables to normalize.

- vars:

  Variables to normalize. Supports tidyselect syntax (e.g.,
  `starts_with()`, `c(var1, var2)`).

- flip:

  Logical; if `TRUE`, reverses the direction of valid response levels
  before scaling.

- keep_all:

  Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only
  normalized variables.

- outside_scale:

  Character vector of response levels (typically DK/PNTS) that lie
  outside of the scale and should be treated as neutral midpoint
  (`0.5`).

## Value

A data frame with normalized variables.

- If `keep_all = TRUE`, the full dataset is returned with normalized
  variables.

- If `keep_all = FALSE`, only normalized variables are returned.

## Details

- Numeric variables are min–max scaled to 0 to 1.

- Factor variables are converted to numeric order and evenly spaced
  between 0 and 1.

- Levels listed in `outside_scale` (e.g., `Don't know`) are assigned a
  value of 0.5.

## Examples

``` r
# Normalize Q1 and Q2 with flipping the scale order
normalize(data = data, vars = c(Q2, Q3), flip = TRUE)
#> Error: These packages are required but not installed: dplyr, tidyselect
```
